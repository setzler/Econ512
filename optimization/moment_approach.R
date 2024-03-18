
moment_solver <- function(paneldata) {

    # collect the lagged values
    paneldata_lag = paneldata[, list(ID, year = year + 1, x_it_lag = x_it, y_it_lag = y_it)]
    paneldata_lag2 = paneldata[, list(ID, year = year + 2, x_it_lag2 = x_it, y_it_lag2 = y_it)]

    # merge the lagged values to the original dataset
    paneldata = merge(paneldata, paneldata_lag, by = c("ID", "year"))
    paneldata = merge(paneldata, paneldata_lag2, by = c("ID", "year"))

    # calculate the moment conditions
    Amat = NULL
    for (zz in c("x_it_lag2", "y_it_lag2")){
        Aj = c()
        for (var in c("y_it", "y_it_lag", "x_it", "x_it_lag")){
            Aj = c(Aj, paneldata[, cov(get(zz), get(var))])
        }
        Amat = rbind(Amat, Aj)
    }

    evaluate_g <- function(params) {
        beta_param = params[1]
        rho_param = params[2]
        pvec = c(1, -rho_param, -beta_param, beta_param*rho_param)
        g_eval = Amat %*% matrix(pvec)
        return(as.vector(g_eval))
    }

    quadratic_g <- function(params) {
        g_eval = evaluate_g(params)
        return(sum(g_eval^2))
    }


    # estimate the parameters
    start_params = c(3, 1)
    res = optim(par = start_params, fn = quadratic_g, method = "BFGS")
    #res = nloptr::newuoa(x0 = start_params, fn = quadratic_g)
    #res = pso::psoptim(par = start_params, fn = quadratic_g, lower = c(-1, 0), upper = c(1, 1), control = list(maxit = 1000, trace = 1))
    #res = nleqslv::nleqslv(start_params, evaluate_g, method = "Newton", global = "cline")
    #res = DEoptim::DEoptim( fn = quadratic_g, lower = c(-5, -1), upper = c(5, 1))

}



library(PanelReg)

simulated_data = PanelRegSim(panel_model = "AR1", seed = 123, sample_size = 100000, noise_sd = 0, min_year = 2003, max_year = 2008, true_beta = c(1.0))

setnames(simulated_data, c("unit_id","time_id","outcome", "endog_var1"), c("ID", "year","y_it", "x_it"))

print(moment_solver(simulated_data) )
