
################ 1. Prepare the workspace ################
rm( list = ls() )
setwd("~/github/Econ512/RDD/")
set.seed(123)
library( data.table )
library( ggplot2 )
source("RDD-sim.R")
source("RDD-est.R")

################ 2. Test the data simulator ################
simulated_data <- RDD_simulator( beta_treatment_effect = 3, N_sample_size = 1000, endogeneity_weight = 0.5 )
print(simulated_data[1:10])

# plot the data
gg = ggplot( data = simulated_data, aes( x = x_running_variable, y = y_outcome ) ) + 
    geom_point( aes( color = d_treatment ) ) + 
    geom_smooth( method = "lm", se = FALSE ) + 
    theme_bw(base_size = 20) +
    labs( x = "Running Variable", y = "Outcome", color = "Treatment Assignment:" ) + 
    scale_color_manual( values = c( "red", "black" ), labels = c( "Control", "Treatment" ) ) +
    theme( legend.position = "bottom" )
ggsave( filename = "RDD_simulated_data.png", plot = gg, width = 8, height = 6, dpi = 300 )


################ 3. Test the RDD estimator with uniform kernel ################
trial_data <- RDD_simulator( beta_treatment_effect = 1, N_sample_size = 1000, endogeneity_weight = 0.7 )
beta_hat_uniform <- RDD_estimator_uniform( inputdata = trial_data, tau_bandwidth = .1, c_threshold_point = 0 )
print(beta_hat_uniform)

################ 4. Test the RDD estimator with triangular kernel ################
trial_data <- RDD_simulator( beta_treatment_effect = 1, N_sample_size = 1000, endogeneity_weight = 0.7 )
beta_hat_triangular <- RDD_estimator_triangular( inputdata = trial_data, tau_bandwidth = .8, c_threshold_point = 0 )
print(beta_hat_triangular)
