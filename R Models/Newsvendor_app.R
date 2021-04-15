#####################################################################
# Date: March 25, 2021
# Author: Steven Shechter
# Email: steven.shechter@sauder.ubc.ca
# Description:
# Newsvendor Simulation App
# This is intended to be a simple intro (e.g., a "first model" and "app") 
# to both Monte Carlo simulation in R, and embedding a simulation model in a Shiny app.
# A stand-alone R script version, which is stripped of all Shiny aspects (e.g., 
# no user interface, reactivity, etc), can be found in Newsvendor.R 

#####################################################################

library(shiny)
library(dplyr)
library(ggplot2)

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

# Define UI for application 
ui <- navbarPage("Monte Carlo Simulation Examples",    #fluidPage(

   tabPanel("Intro",
            h4("This site provides some simple examples of interactive Monte Carlo simulation models, shown along the different tabs.")),
  
   tabPanel("Newsvendor Problem",    

    sidebarLayout(

        sidebarPanel(
          p("This example is the classic newsvendor problem"),
          p("Begin by entering the purchast cost per unit, resale price per unit, and demand distribution (with associated parameters)"),
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
      
          # users can either run "evaluation" or "optimize" modes
          # for "evaluate", they will get to enter an order quantity, which will be evaluated as a stand-alone decision 
          # for "optimize", the program will loop through a range of choices, evaluate each one, and return the best of these
          
          hr(),
          p("Now choose whether you want to 'Evaluate' the performance of a single order quantity (and what that order is),
            or have the model 'Optimize' by searching over a range of order quantities and return the best one."),
      
          radioButtons("run_mode", h4("Select Run Mode"),
                  list("Evaluate (tests one specific order quantity)" = "eval", "Optimize (searches over a range)" = "opt")),
          conditionalPanel(condition = "input.run_mode == 'eval'", inputPanel(
              numericInput("order_q", "Order quantity to evaluate", value = 150))),
      
          hr(),
          p("The number of replications is the number of IID sample profits that are simulated from each order quantity that is evaluated. "),
          numericInput("reps", label = h4("Number of Replications"), value = 50),
      
          actionButton("run_sim", "Run Simulation")
        ), # closes sidebarPanel

    # Show a plot of the generated distribution
    mainPanel(
        htmlOutput("summary"),
        plotOutput("avg_and_cis")
    )
    )# closes sidebarLayout
   ),# closes tabpanel (Newsvendor)
   
   tabPanel("Project Management", 
            p("coming soon"))
   
) #closes UI fluidpage 

    server <- function(input, output) {
      
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

          set.seed(1) # for reproducability
          
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
          withProgress(message = 'Evaluating decisions', value = 0, {

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
          
          # building dataframe ts feed to ggplot.  Note, because ggplot uses "grouping"
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

        }) #end eventReactive(input$run_sim, 
            # all of the above was the server code for what happends when someone clicks button "run simulation"
   
            
        # the following clears the summary and plot areas if we switch from opt to eval or back for run mode
        observeEvent(input$run_mode, {
          output$summary <- renderText({  })
          
          output$avg_and_cis <- renderPlot({  })
        })
          
    }# end Server() section


    # Run the application 
    shinyApp(ui = ui, server = server)

    # can depot app from within Rstudio to the right of "Run App" above
