#####################################################################
# Date: April 2, 2021
# Author: Steven Shechter  
# Email: steven.shechter@sauder.ubc.ca
# Description:
# Monte Carlo Simulation Examples
# This is intended to provide some fairly simple intros 
# to both Monte Carlo simulation in R, and embedding a simulation model in a Shiny app.
# A stand-alone R script version for the Newsvendor example, which is stripped of all Shiny aspects (e.g., 
# no user interface, reactivity, etc), can be found in Newsvendor.R 

#####################################################################

library(shiny)
library(ggplot2)
library(DT)

# this function obtains the desired confidence interval bounds around a sample average
confidence_interval <- function(vector, interval) {
    # Standard deviation of sample
    vec_sd <- sd(vector)
    # Sample size
    n <- length(vector)
    # Mean of sample
    vec_mean <- mean(vector)
    # Error according to t distribution
    error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
    # Confidence interval as a vector
    result <- c(vec_mean - error, vec_mean + error)
    return(result)
}

#####################################################################
# USER INTERFACE section of Shiny
##################################################################### 
ui <- navbarPage("Monte Carlo Simulation Examples", 
    
    tabPanel("Intro",
          h4("This site provides some examples of interactive Monte Carlo simulation models, shown along the different tabs.")),
  
    #####################################################################
    #NEWSVENDOR PROBLEM part of UI:
    #####################################################################   
    tabPanel("Newsvendor Problem",    
             
      h4("This example simulates the classic newsvendor problem"),
      p("Begin by entering the purchase cost per unit, resale price per unit, and demand distribution (with associated parameters)"),
      hr(),
      numericInput("purchase_cost", label = h4("Purchase Cost"), value = .25),
      numericInput("sale_price", label = h4("Sale Price"), value = 1), 
      
      radioButtons("dist", h4("Select Demand Distribution"),
                   list("Normal" = "norm", "Uniform" = "unif")),
      
      # depending on demand distribution chosen, different parameters of the distribution are required.
      # hence the conditional panels
      conditionalPanel(condition = "input.dist == 'unif'", inputPanel(
          numericInput("minU", "Min", value = 100),
          numericInput("maxU", "Max", value = 200))),
      conditionalPanel(condition = "input.dist == 'norm'", inputPanel(
          numericInput("mean", "Mean", value = 150),
          numericInput("sd", "Standard Deviation", value = 25))),
      hr(),    
      # users can either run "evaluation" or "optimize" modes
      # for "evaluate", they will get to enter an order quantity, which will be evaluated as a stand-alone decision 
      # for "optimize", the program will loop through a range of choices, evaluate each one, and return the best of these

      p("Now choose whether you want to 'Evaluate' the performance of a single order quantity (and what that order is),
        or have the model 'Optimize' by searching over a range of order quantities and return the best one."),
      
      radioButtons("run_mode", h4("Select Run Mode"),
                   list("Evaluate (tests one specific order quantity)" = "eval", "Optimize (searches over a range)" = "opt")),
      conditionalPanel(condition = "input.run_mode == 'eval'", inputPanel(
          numericInput("order_q", "Order quantity to evaluate", value = 150))),
      hr(),
      
      p("The number of replications is the number of IID sample profits that are simulated from each order quantity that is evaluated. "),
      numericInput("reps", label = h4("Number of Replications"), value = 50),
      
      actionButton("run_sim", "Run Simulation"),
      htmlOutput("summary"),
      plotOutput("avg_and_cis")

    ),# end tabpanel "Newsvendor Problem"

    
    #####################################################################
    #PROJECT MANAGEMENT part of UI:
    ##################################################################### 
    tabPanel("Project Management", 

      h4("This example demonstrates a project management simulation"),
      p("Choose whether you want to upload a file or run in interactive mode."),
      p("In either case, you will enter the dependency structure, with the first column indicating the activity index."), 
      p("If an activity does not have a predecessor, enter a '-1' (this is the default for interactive mode)."),
      p("Otherwise, enter the activity (or up to three activities) that immediately precedes the activity of that row."),
      p("The model assumes you have labeled activities so that any predecessor activities' indices are lower than the current activity index."),
      p("The Average Duration and Percent Uncertainty show default values in the interactive mode, which you can change."),
      p("The simulations will draw random durations for each activity that lie within the Average +/-  (% Uncertainty * Average)."),
      p("For example, with the default values shown in the interactive mode, each activity will have a Uniform[18, 22] duration."),
      p("If upoading a .csv, first take a look at the column headings for the interactive mode, and have a first row of your .csv that matches these headings."),
      hr(),
      
      radioButtons("run_mode_PM", h4("Run Mode"),
                   list("Interactive" = "interactive", "CSV file upload" = "upload")),
      
      #the following indicates what to show if the run mode is interactive
      conditionalPanel(condition = "input.run_mode_PM == 'interactive'", #inputPanel(
        
        p("Enter the number of activites in this project."),
        numericInput("num_activities", label = h4("Number of project activities"), value = 0),
        
        actionButton("generate_table", "Generate Project Table"),
        
        conditionalPanel(condition = "input.generate_table", inputPanel(
          DTOutput("my_datatable"))) #ends conditional panel
        
     ), #end inputpanel conditional on run model = interactive
  
     #the following indicates what to show if the run mode is based on a .csv file upload
     conditionalPanel(condition = "input.run_mode_PM == 'upload'", 
        fileInput("file1", "Choose CSV File", multiple = FALSE, accept = c(".csv")),
        tableOutput(outputId = 'table.output')
     ),

     actionButton("run_sim_PM", "Run Simulation"),
     p("(200 replications will be run)"),
     
     conditionalPanel(condition = "input.run_sim_PM", 
        htmlOutput("summary_PM"),
        htmlOutput("FoA"),
        htmlOutput("FoA2"),
        plotOutput("PM_Plots"),
        p("Move the slider below to change the percentile shading (by default it shows red for the lower and upper 5th percentiles)."),
        inputPanel(sliderInput("percentiles", "Percentile Slider", value = c(5, 95), min = 0, max = 100)),
        htmlOutput("percentile_info"),
      )

    )# end tabpanel "Project Management" 
                 
) #end UI section 

#####################################################################
# SERVER section of Shiny
#####################################################################
server <- function(input, output) {
  
  #####################################################################
  #NEWSVENDOR PROBLEM part of server:
  ##################################################################### 
    #initialize
    profit <- 0
    sales_quantity <- 0
    expected_profit <- 0
    profit_vector <- c()
    expected_profit_vector <- c()
    order_quantity_vector <- c()
    lower_ci_vector <- c()
    upper_ci_vector <- c()
    
    # nearly all server activity takes place after user enters "run simulation" on UI
    observeEvent(input$run_sim, {
        
        set.seed(2) # for reproducability
        
        # The following if/else logic is to obtain the lower and upper ends of an outer
        # for loop that will evaluate different order quantities.
        # In the case that the user wants to evaluate a single order quantity of interest,
        # the lower and upper limits will be the same, so it will run that one decision
        if (input$run_mode == 'eval')
        {
            lower <- input$order_q
            upper <- input$order_q
        }
        else #running in optimization mode
        {
            if (input$dist == 'unif')
            {
                lower <- input$minU
                upper <- input$maxU
            }
            else if (input$dist == 'norm')
            {
                lower <- as.integer(input$mean - 2*input$sd)
                upper <- as.integer(input$mean + 2*input$sd)
            }
        }
        
        # this creates a progress bar so user can get sense of how far along simulations are
        withProgress(message = 'Running Simulations', value = 0, {
            
            # the .05 is me hard coding basically that we will splice the outer evaluation
            # loop into 20 intervals.  So our "optimization" mode is to be understood as
            # optimizing at that level of granularity. Could make inc_amount = 1 if want to evaluate every 
            # order quantity between lower and upper limits
            inc_amount <- as.integer(.05*(upper - lower))
            for (order_quantity in seq(lower, upper, inc_amount))
            {
                profit_vector <- c()
                
                # inner loop that evaluates each order quantity over "reps" number of 
                # simulation replications
                for (i in 1:input$reps)
                {
                    if (input$dist == 'norm')
                    {
                        demand <- round(rnorm(1,input$mean, input$sd))
                    }
                    else if (input$dist == 'unif')
                    {
                        demand <- round(runif(1, input$minU, input$maxU))
                    }
                    
                    sales_quantity <- min(demand,order_quantity)
                    profit <- input$sale_price * sales_quantity - input$purchase_cost * order_quantity
                    profit_vector[i] <- profit # store value
                }#end inner for loop
                
                # append results to vectors that store summary results from the "reps" replications of each order quantity
                order_quantity_vector <- c(order_quantity_vector, order_quantity)
                expected_profit <- mean(profit_vector)
                expected_profit_vector <- c(expected_profit_vector, expected_profit)
                ci <- confidence_interval(profit_vector, .95)
                lower_ci_vector <- c(lower_ci_vector, ci[1])
                upper_ci_vector <- c(upper_ci_vector, ci[2])
                
                # Increment the progress bar, and update the detail text.
                incProgress(inc_amount/(upper - lower), detail = paste("Order Quant = ", order_quantity))
            } #end outer for loop over order quantities
        })# end progress bar
        
        # find order quant that maximizes expected profit
        index <- which.max(expected_profit_vector)
        opt_order_q <- order_quantity_vector[index]
        opt_profit <-  expected_profit_vector[index]
        # the +/- 95% CI half width
        plus_minus <-  upper_ci_vector[index] - expected_profit_vector[index] 
        # the above will be written out in the summary results
        
        # The next parts build a dataframe to feed to ggplot.  Note, because ggplot uses "grouping"
        # to create different curves on same plot, there is one column of y-values in this df,
        # given by "value_vector".  
        # df1 creates a df where these values are the sample averages and we label it
        # with "avg" as the type.
        # We will create two other dfs, one for each of lower and upper CI limits, and
        # then combine the 3 through rbind.  Then see how we use the type_vector in ggplot
        
        value_vector <- expected_profit_vector
        type_vector <- rep("avg", times = length(value_vector) )
        df1 <- data.frame(order_quantity_vector, value_vector, type_vector)
        
        value_vector <- lower_ci_vector
        type_vector <- rep("lower 95% CI", times = length(lower_ci_vector) )
        df2 <- data.frame(order_quantity_vector, value_vector, type_vector)
        
        value_vector <- upper_ci_vector
        type_vector <- rep("upper 95% CI", times = length(upper_ci_vector) )
        df3 <- data.frame(order_quantity_vector, value_vector, type_vector)
        
        #stack these on top of each other:
        df4 <- rbind(df1, df2, df3)
        
        # if we are running in opt mode, then we will write out summary with slightly different
        # text, and we will also plot the results
        if (input$run_mode == "opt")  
        {
            # for writing summary results:
            output$summary <- renderText({
                paste(h4("The average profit maximizing order quantity is: ", opt_order_q),  
                      h4("The average profit (and 95% CI half-width) from this is: ", round(opt_profit,2), 
                         " +/- ", round(plus_minus,2)))
            })
            
            # for plotting:
            output$avg_and_cis <- renderPlot({  
                ggplot(df4, aes(x = order_quantity_vector, y = value_vector, color=type_vector)) + geom_line() + geom_point() +
                    scale_color_manual(breaks = c("lower 95% CI", "avg","upper 95% CI"),
                                       values=c("blue", "red", "blue")) +
                    labs(x = "Order quantity", y = "Average profit") +
                    theme(legend.title = element_blank()) 
            }, res = 100)
        }# end if "opt"
        else # running in "evaluate" mode
        {
            # just write summary results; no plotting for evaluate mode
            output$summary <- renderText({
                paste(h4("The average profit (and 95% CI half-width) is: ", round(opt_profit,2), " +/- ", round(plus_minus,2)))
            })
        }# end if/else
        
    }) #end observeEvent (input$run_sim, 
    # all of the above was the server code for what happen when someone clicks button "run simulation"
  
    # the following clears the summary and plot areas if we switch from opt to eval or back for run mode
    observeEvent(input$run_mode, {
        output$summary <- renderText({  })
        
        output$avg_and_cis <- renderPlot({  })
    })
    
    #####################################################################
    #PROJECT MANAGEMENT part of server 
    #####################################################################
    reactive_vals <- reactiveValues(data = NULL, df_temp = NULL, num_acts = 0)

    #if run mode is interactive, then the user will end up clicking "generate table", which will trigger this
    #default creation of the editable table
    observeEvent(input$generate_table, {
      reactive_vals$data = data.frame("Activities" = rep(-1,input$num_activities), "Predecessor_1" = rep(-1,input$num_activities), "Predecessor_2" = rep(-1,input$num_activities), "Predecessor_3" = rep(-1,input$num_activities), "Average_Duration" = rep(20,input$num_activities), "Percent_Uncertainty" = rep(10, input$num_activities))
      #for some reason, I cannot put "Activities" = seq(1:input$num_activities) in the above line
      #so my way around this is to fill it out first with -1s, and then do the following:
      for (i in 1:input$num_activities)
      {
        reactive_vals$data[i,1] <- i
      }
    })
    
    
    #if run model is "file upload", then the folllowing will store the uploaded CSV into a dataframe
    mydata <- reactive({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      tbl <- read.csv(inFile$datapath, header=TRUE)
    
      return(tbl)
    })
    
    #for showing/confirming the project someone uploaded
    output$table.output <- renderTable({
      mydata()
    })
    
    #output the datatable based on the dataframe (and make it editable)
    output$my_datatable <- renderDT({
        DT::datatable(reactive_vals$data, editable = TRUE, options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE))
        # options can be found here: https://datatables.net/reference/option/
    })
    
    # this updates the entries of the table dataframe if the user edits an entry
    observeEvent(input$my_datatable_cell_edit, {
       info = input$my_datatable_cell_edit
       i = as.numeric(info$row)
       j = as.numeric(info$col)
       val = as.numeric(info$value)
       reactive_vals$data[i,j] <- val
    })
    
    #hard coding simulation replications and maximum number of immediate predecessors:
    PM_sim_reps <- 200  
    max_preds <- 3 # the maximum number of immediate predecessor activities 
    
    observeEvent(input$run_sim_PM, {
      set.seed(2) # for reproducability
      
      if (input$run_mode_PM == "upload") {
        reactive_vals$num_acts = length(mydata()$Activities)
        reactive_vals$df_temp = mydata()
      } else{
        reactive_vals$num_acts = input$num_activities
        reactive_vals$df_temp = reactive_vals$data
      }
      
      withProgress(message = 'Running Simulations', value = 0, {
        project_durations <- c() # vector updated after each replication
  
        #for Flaw of Averages calculation 
        activity_start_times_FoA <- rep(0, reactive_vals$num_acts) 
        activity_end_times_FoA <- rep(0, reactive_vals$num_acts) 
        
        # simulation replications
        for (i in 1:PM_sim_reps)
        {
          #intialize vectors of each activity start/end times within each replication
          activity_start_times <- rep(0, reactive_vals$num_acts) 
          activity_end_times <- rep(0, reactive_vals$num_acts) 
    
          for (j in 1:reactive_vals$num_acts) 
          {
            #get random duration of each activity
            avg <- reactive_vals$df_temp[j, "Average_Duration"] 
            uncert_distance <- avg*(reactive_vals$df_temp[j, "Percent_Uncertainty"])/100 
            activity_duration <- runif(1, avg-uncert_distance, avg+uncert_distance)
            
            #determine activity start time by checking for precedences
            for (k in 1:max_preds)  # for now just hard coding the columns. Improve this later so that we don't have to do the k+1 below if there is a first column with activity indices
            {
              prec_activity_index <- reactive_vals$df_temp[j, k+1]  #the +1 is because first column is Activities
              if (prec_activity_index > 0) #i.e., someone entered a precedence activity
              {
                # keep updating activity start time to be max between what is currently recorded and the precedent activity's end time;
                # i.e., activity start time = max(end time over all precedenty activities to this activity j)
                activity_start_times[j] <- max(activity_start_times[j], activity_end_times[prec_activity_index])
                #for flaw of averages calculation
                if (i == 1)
                {
                  activity_start_times_FoA[j]<- max(activity_start_times_FoA[j], activity_end_times_FoA[prec_activity_index])
                }
              }
            }#end for k in 1:max_preds
            
            activity_end_times[j] = activity_start_times[j] + activity_duration
            #for flaw of averages calculation
            if(i == 1)
            {
              activity_end_times_FoA[j] = activity_start_times_FoA[j] + avg
            }
          } #end for (j in 1:input$num_activities)
  
          # record the project duration time of this replication as the end time of the final activity
          project_durations[i] <- activity_end_times[reactive_vals$num_acts] 

          incProgress(1, detail = paste("Rep # = ", i))       
        }#end for (i in 1:PM_sim_reps)
      }) # end progress bar 
      
      #Flaw of Averages duration
      project_duration_FoA <- activity_end_times_FoA[reactive_vals$num_acts] 

      avg <- round(mean(project_durations), 2)
      ci <- confidence_interval(project_durations, .95)
      plus_minus <- round(ci[2] - avg, 2) #plus_minus is the 95% CI half-width
      
      output$summary_PM <- renderText({
        paste(h4("The average project completion time (and 95% CI half-width) is: ", avg ,  " +/- ", plus_minus))
      })
      output$FoA <- renderText({
        paste(h4("The average project completion time based on the Flaw of Averages (where every activity is assumed to take exactly its average duration) is: ", project_duration_FoA))
      })   
      output$FoA2 <- renderText({
        paste(h4("The probability of the project duration exceeding ", project_duration_FoA, "is: ", round(length(project_durations[project_durations > project_duration_FoA])/length(project_durations),2)))
      })  
      
      #dataframe created for feeding into ggplot:
      df_PM = data.frame(project_durations)

      #histogram of output with sliders for tail percentiles
      output$PM_Plots <- renderPlot({  
        ggplot(df_PM, aes(x = project_durations))  + geom_histogram(binwidth = .5, color="darkblue", fill="lightblue") +
          geom_histogram(data = subset(df_PM, project_durations <= quantile(project_durations, input$percentiles[1]/100)), binwidth = .5, color="darkblue", fill="red")  +
          geom_histogram(data = subset(df_PM, project_durations >= quantile(project_durations, input$percentiles[2]/100)), binwidth = .5, color="darkblue", fill="red")
      }, res = 100)
      
      output$percentile_info <- renderText({
        paste(h4("The ", input$percentiles[1], " percentile is", round(quantile(project_durations, input$percentiles[1]/100), 2), ", and the ", input$percentiles[2], " percentile is", round(quantile(project_durations, input$percentiles[2]/100),2)))
      })
  }) # end ObserveEvent input$run_sim_PM (run Proj Mgmnt simulations)

}# end Server() section


# Run the application 
shinyApp(ui = ui, server = server)

