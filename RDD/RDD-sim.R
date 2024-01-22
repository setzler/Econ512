
################ 1. Define a data simulator ################

# simulating an endogenous treatment effect model in an RDD setting
# the model is: y_i = beta * d_i + epsilon_i, d_i = 1(x_i >= 0)
RDD_simulator <- function( beta_treatment_effect, N_sample_size, endogeneity_weight ){

    # simulate primitives (x, eps)
    epsilon_outcome_unobservable <- rnorm( N_sample_size, mean = 0, sd = 1 )
    x_running_variable <- endogeneity_weight * epsilon_outcome_unobservable + 
        (1-endogeneity_weight) * rnorm( N_sample_size, mean = 0, sd = 1 )

    # treatment assignment
    d_treatment <- x_running_variable >= 0

    # potential outcome
    y_outcome <- beta_treatment_effect * d_treatment + epsilon_outcome_unobservable

    # combine variables into a data table
    return( data.table( y_outcome, d_treatment, x_running_variable ) )

}

################ 2. Try out the data simulator ################
setwd("~/github/Econ512/RDD/")
set.seed(123)
library( data.table )
library( ggplot2 )
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
