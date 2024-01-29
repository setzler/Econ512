
# simulating an endogenous treatment effect model in an RDD setting
# the model is: y_i = beta * d_i + epsilon_i, d_i = 1(x_i >= 0)
RDD_simulator <- function( beta_treatment_effect, N_sample_size, endogeneity_weight, set_seed = 123){

    # set the seed
    set.seed(set_seed)

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

