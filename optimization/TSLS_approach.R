
TSLS_AR1 <- function(paneldata) {

    # collect the lagged values
    paneldata_lag = paneldata[, list(ID, year = year + 1, x_it_lag = x_it, y_it_lag = y_it)]
    paneldata_lag2 = paneldata[, list(ID, year = year + 2, x_it_lag2 = x_it, y_it_lag2 = y_it)]

    # merge the lagged values to the original dataset
    paneldata = merge(paneldata, paneldata_lag, by = c("ID", "year"))
    paneldata = merge(paneldata, paneldata_lag2, by = c("ID", "year"))

    # estimate two-stage least-squares
    #library(fixest)
    reg = feols(y_it ~ y_it_lag + x_it_lag | x_it ~ y_it_lag2 + x_it_lag2, data = paneldata)
    #reg = lm( y_it ~ x_it + x_it_lag + y_it_lag, data = paneldata)
    return(reg)

}

test_data = simulate_AR1(sample_size = 10000, time_periods = 5, beta_effect = 1.0, gamma_selection = 1.0, rho_persistence = 0.5)

library(PanelReg)
varnames = list(
    id_name = "unit_id",
    time_name = "time_id",
    outcome_name = "outcome",
    endogenous_names = "endog_var1")

AR1_options = list(
  AR1_method = "PanelIV",
  AR1_IV_outcome = TRUE
)



# devtools::install_github("setzler/PanelReg")
library(PanelReg)

simulated_data = PanelRegSim(panel_model = "AR1", seed = 123, sample_size = 100000, noise_sd = 0, min_year = 2003, max_year = 2008, true_beta = c(1.0))


output = PanelReg(panel_data = simulated_data, panel_model = "AR1", varnames = varnames, AR1_options = AR1_options)



setnames(simulated_data, c("unit_id","time_id","outcome", "endog_var1"), c("ID", "year","y_it", "x_it"))

reg = TSLS_AR1(paneldata = simulated_data)
print(reg)
