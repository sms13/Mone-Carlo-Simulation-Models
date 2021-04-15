#####################################################################
# Date: March 25, 2021
# Author: Steven Shechter
# Email: steven.shechter@sauder.ubc.ca
# Description:
# Newsvendor Simulation Example
# This is intended to be a simple intro (e.g., a "first model") 
# to Monte Carlo simulation in R.  It is a de-Shiny'd version of the sibling
# code found in Newsvendor_app.R
# Instead of the user-driven entries of the UI in the app, the modeler
# enters parameters at the top section here.
# Note: this code runs much faster than the app. I'm not sure why.
# e.g., choosing "opt" and "unif" between 100 and 200, the plots at the end display
# much quicker in this code than in the same parameter choices for the Shiny app.R version
#####################################################################

# user inputs:
purchase_cost <- 0.25
sale_price <- 1
dist <- "unif"  # could be "norm" or "unif"
#if running normal dist:
mean <- 150
sd <- 25
# if running unif dist:
minU <- 100
maxU <- 200
reps <- 1000

# if running in Evaluate model
order_q <- 175 # order quanity to test 
run_mode <- "opt" # could be "opt" or "eval"
# can either run "evaluation" or "optimize" modes
# for "evaluate", they will get to enter an order quantity, which will be evaluated as a stand-alone decision 
# for "optimize", the program will loop through a range of choices, evaluate each one, and return the best of these

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

# starting main body of code:
  
  #initialize
  profit <- 0
  sales_quantity <- 0
  expected_profit <- 0
  profit_vector <- c()
  expected_profit_vector <- c()
  order_quantity_vector <- c()
  lower_ci_vector <- c()
  upper_ci_vector <- c()
  set.seed(1)
  
  # The following if/else logic is to obtain the lower and upper ends of an outer
  # for loop that will evaluate different order quantities.
  # In the case that the user wants to evaluate a single order quantity of interest,
  # the lower and upper limits will be the same, so it will run that one decision

  if (run_mode == 'eval')
  {
    lower <- order_q
    upper <- order_q
  }  else {   #running in optimization mode
    if (dist == 'unif')
    {
      lower <- minU
      upper <- maxU
    } else if (dist == 'norm')
    {
      lower <- as.integer(mean - 2*sd)
      upper <- as.integer(mean + 2*sd)
    }
  }# end else in eval mode or not
    
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
    for (i in 1:reps)
    {
      if (dist == 'norm')
      {
        demand <- round(rnorm(1,mean, sd))
      } else if (dist == 'unif')
      {
        demand <- round(runif(1, minU, maxU))
      }
      
      sales_quantity <- min(demand,order_quantity)
      profit <- sale_price * sales_quantity - purchase_cost * order_quantity
      profit_vector[i] <- profit # store value
    }#end inner for loop
    
    # append results to vectors that store summary results from the "reps" replications of each order quantity
    order_quantity_vector <- c(order_quantity_vector, order_quantity)
    expected_profit = mean(profit_vector)
    expected_profit_vector <- c(expected_profit_vector, expected_profit)
    ci <- confidence_interval(profit_vector, .95)
    lower_ci_vector = c(lower_ci_vector, ci[1])
    upper_ci_vector = c(upper_ci_vector, ci[2])
    
  } #end outer for loop over order quantities

  # find order quant that maximizes expected profit
  index <- which.max(expected_profit_vector)
  opt_order_q <- order_quantity_vector[index]
  opt_profit <-  expected_profit_vector[index]
  # the +/- 95% CI half width
  plus_minus <-  upper_ci_vector[index] - expected_profit_vector[index] 
  # the above will be written out in the summary results
    
  # building dataframe to feed to ggplot.  Note, because ggplot uses "grouping"
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
  if (run_mode == "opt")  
  {
    sprintf("The average profit maximizing order quantity is: %d\n", opt_order_q)
    sprintf("The average profit (and 95%% CI half-width) from this is: %.2f +/- %.2f\n", opt_profit, plus_minus)

    # for plotting:
    ggplot(df4, aes(x = order_quantity_vector, y = value_vector, color=type_vector)) + geom_line() + geom_point() +
      scale_color_manual(breaks = c("lower 95% CI", "avg","upper 95% CI"),
                         values=c("blue", "red", "blue")) +
      labs(x = "Order quantity", y = "Average profit") +
      theme(legend.title = element_blank()) 
  } else {# running in "evaluate" mode
    # just write summary results; no plotting for evaluate mode
    sprintf("The average profit (and 95%% CI half-width) is: %.2f +/- %.2f\n ", opt_profit, plus_minus)
  }# end if/else
    

































