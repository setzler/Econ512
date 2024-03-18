
# set up the workspace
rm(list=ls())
library(data.table)

simulate_AR1 <- function(sample_size, time_periods, beta_effect, gamma_selection, rho_persistence) {

    # skeleton dataset with the right dimension (N*T) 
    dd = as.data.table( expand.grid( ID = 1: sample_size, year = 1:time_periods) )

    # generate the random walk process
    dd[, omega_it := rnorm(.N)]
    dd[, nu_it := cumsum(omega_it), ID]

    # generate the AR(1) process
    dd[, eta_it := rnorm(.N)]
    dd[, epsilon_it := 0.0]
    dd[year == 1, epsilon_it := eta_it / (1 - rho_persistence^2)]

    for (tt in 2:time_periods) {
        dd_lag = dd[year == tt - 1][, list(ID, epsilon_it_lag = epsilon_it)]
        dd = merge(dd, dd_lag, by = "ID", all.x = TRUE)
        dd[year == tt, epsilon_it := rho_persistence * epsilon_it_lag + eta_it]
        dd[, epsilon_it_lag := NULL]
        dd_lag = NULL
    }

    # simulate the observable data
    dd[, x_it := 2.0 + gamma_selection * epsilon_it + nu_it]
    dd[, y_it := 1.0 + beta_effect * x_it + epsilon_it]

    # finish
    return(dd[, list(ID, year, y_it, x_it)])

}


# try out the simulator
#set.seed(123)
#test_data = simulate_AR1(sample_size = 100, time_periods = 5, beta_effect = 1.0, gamma_selection = 0.0, rho_persistence = 0.5)
#lm( y_it ~ x_it, data = test_data)

