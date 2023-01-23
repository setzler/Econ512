
# set up the workspace
rm(list=ls())
setwd("~/github/Econ512/RDD/")
library(data.table)

# define the simulator function
RDD_simulation = function(random_seed, sample_size, c_threshold, beta_goal, bias_weight){
  
  # prepare the simulation environment
  set.seed(random_seed)
  RDD_data = data.table()
  
  # simulated primitives
  RDD_data$epsilon = rnorm(n = sample_size, mean = 0, sd = 1)
  RDD_data$X_running = bias_weight * RDD_data$epsilon + 
    (1-bias_weight) * rnorm(n = sample_size, sd = 1)
  
  # observables
  RDD_data$D_choice = as.integer(RDD_data$X_running >= c_threshold)
  RDD_data$Y_outcome = with(RDD_data, D_choice * beta_goal + epsilon)
  # RDD_data[, Y_outcome := D_choice * beta_goal + epsilon] # data.table syntax
  
  # finish
  return(RDD_data)
  
}

# check that OLS is biased
check_OLS_examples = function(random_seed){
  RDD_case = RDD_simulation(random_seed=random_seed, sample_size=1e3, 
                             c_threshold=0, beta_goal=0.7, bias_weight=0.5)
  
  OLS_reg = lm(Y_outcome ~ D_choice, data=RDD_case)
  return(OLS_reg)
}

