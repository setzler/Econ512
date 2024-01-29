
################ 1. Define a RDD with uniform kernel ################

# estimator based on uniform interval around the discontinuity point c
# beta = E[ Y | (c+tau) >= x >= c  ] - E[ Y | (c-tau) <= x <= c]
RDD_estimator_uniform <- function( inputdata, tau_bandwidth, c_threshold_point = 0 ){

    # subset the data around the threshold point
    data_subset <- inputdata[ abs( x_running_variable - c_threshold_point ) <= tau_bandwidth ]
    
    # estimate the treatment effect
    RDD_beta_hat <- mean( data_subset[ d_treatment == TRUE, y_outcome ] ) - 
        mean( data_subset[ d_treatment == FALSE, y_outcome ] )
    
    # collect sample statistics
    RDD_beta_hat_stats <- list( RDD_beta_hat = RDD_beta_hat,
                                N_treatment =  data_subset[ d_treatment == TRUE, .N ] ,
                                N_control =  data_subset[ d_treatment == FALSE, .N ] 
                                )

    # return the estimate
    return( RDD_beta_hat_stats  )
    
}

################ 2. Define an RDD estimator with triangular kernel  ################

RDD_estimator_triangular <- function( inputdata, tau_bandwidth, c_threshold_point = 0 ){

    # construct the kernel weights
    inputdata[, kernel_weights := 1 - abs( x_running_variable - c_threshold_point ) / tau_bandwidth ]
    inputdata[ kernel_weights < 0, kernel_weights := 0 ]

    # subset the data around the threshold point
    data_subset_treated <- inputdata[ d_treatment == TRUE & kernel_weights > 0 ] 
    data_subset_control <- inputdata[ d_treatment == FALSE & kernel_weights > 0 ]
    
    # define residuals for a guess of the parameters for treated group
    treated_objective_function <- function(parameter_guess){
        # unpack parameters
        alpha_hat = parameter_guess[1]
        gamma_hat = parameter_guess[2]
        # compute residuals
        data_subset_treated[, residual := y_outcome - alpha_hat - gamma_hat * abs( x_running_variable - c_threshold_point ) ]
        # compute objective function
        objective_value = data_subset_treated[, sum( kernel_weights * residual^2 ) ]
        return( objective_value )
    }

    # execute the minimization of the objective function for the treated group
    treated_estimates = optim( par = c( 0, 0 ), fn = treated_objective_function, method = "BFGS" )
    treated_alpha_hat = treated_estimates$par[1]


    # define residuals for a guess of the parameters for control group  
    control_objective_function <- function(parameter_guess){
        # unpack parameters
        alpha_hat = parameter_guess[1]
        gamma_hat = parameter_guess[2]
        # compute residuals
        data_subset_control[, residual := y_outcome - alpha_hat - gamma_hat * abs( x_running_variable - c_threshold_point ) ]
        # compute objective function
        objective_value = data_subset_control[, sum( kernel_weights * residual^2 ) ]
        return( objective_value )
    }

    # execute the minimization of the objective function for the control group
    control_estimates = optim( par = c( 0, 0 ), fn = control_objective_function, method = "BFGS" )
    control_alpha_hat = control_estimates$par[1]

    # estimate the treatment effect
    RDD_beta_hat = treated_alpha_hat - control_alpha_hat

    # collect sample statistics
    RDD_beta_hat_stats <- list( RDD_beta_hat = RDD_beta_hat,
                                N_treatment =  data_subset_treated[ , .N ] ,
                                N_control =  data_subset_control[ , .N ] 
                                )

    return( RDD_beta_hat_stats )    
    
}



