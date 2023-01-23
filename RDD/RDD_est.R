
# set up the workspace
rm(list=ls())
setwd("~/github/Econ512/RDD/")
library(data.table)
library(ggplot2)
source("RDD_sim.R")

# simulate data
RDD_data = RDD_simulation(random_seed=10101, sample_size=1e3, 
               c_threshold=0, beta_goal=0.5, bias_weight=0.5)

# visualize
RDD_plot = ggplot(aes(x=X_running, y=Y_outcome), data=as.data.frame(RDD_data)) + 
  geom_point() + theme_bw(base_size = 20) 
print(RDD_plot)

# simple RDD
simple_RDD = function(threshold_margin = 0.1){
  alpha_pos = RDD_data[D_choice==1 & X_running < threshold_margin, mean(Y_outcome)]
  alpha_neg = RDD_data[D_choice==0 & X_running > -threshold_margin, mean(Y_outcome)]
  beta_simple = alpha_pos - alpha_neg
  reg_simple = lm(Y_outcome ~ D_choice, data=RDD_data[(X_running < threshold_margin) & 
                                           (X_running > -threshold_margin)])
  summary(reg_simple)
}

# define the residuals for the local linear regression
resid_localreg = function(params_guess, d_value = 1, bandwidth_h = 0.5, c_thresold = 0.0){
  alpha_guess = params_guess[1]
  gamma_guess = params_guess[2]
  RDD_data[, z_value := (X_running - c_thresold)/bandwidth_h]
  RDD_data[, kernel_value := 1 - abs(z_value)]
  RDD_data[abs(z_value) >= 1, kernel_value := 0]
  RDD_data[, resid_value := (Y_outcome - alpha_guess - 
                               gamma_guess*(X_running - c_thresold))^2]
  RDD_data[D_choice==d_value, sum(kernel_value * resid_value)]
}

localreg_RDD = function(bandwidth_h){
  # D=1 case
  params_Dequals1 = optim(par=c(0.1,0.1), fn=resid_localreg, method = "BFGS", 
                          d_value = 1, bandwidth_h = bandwidth_h, c_thresold = 0.0)
  # D=0 case
  params_Dequals0 = optim(par=c(0.1,0.1), fn=resid_localreg, method = "BFGS", 
                          d_value = 0, bandwidth_h = bandwidth_h, c_thresold = 0.0)
  # finish
  beta_localreg = params_Dequals1$par[1] - params_Dequals0$par[1]
  return(beta_localreg)
}

# example
print(simple_RDD(threshold_margin = 0.1))
print(localreg_RDD(bandwidth_h = 1))
