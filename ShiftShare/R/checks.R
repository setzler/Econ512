
#' Demonstrate the over-rejection problem using our DGP.
#' @param num_draws Number of random draws from DGP.
#' @param num_locations Number of locations to simulate.
#' @param num_industries Number of industries to simulate.
#' @param regionshock_sd Std Dev of region shocks (set to zero to remove over-rejection problem).
#' @export
overrejection_check = function(num_draws = 20, num_locations=750, num_industries=300, regionshock_sd = 0.1, level_of_significance = 0.1){

  pvalues = data.table()

  for(draw in 1:num_draws){

    simdata = shiftshare_environment(seed=draw, II = num_locations, NN = num_industries,
                                     effect_of_interest = 0, weight_on_IV = 0.5,
                                     eps_sd = 0.1, regionshock_sd = regionshock_sd)

    # construct location-level IV, merge with outcomes, and execute location IV regression
    IV = construct_shiftshare_IV(simdata$exposure_shares, simdata$industry_shocks)
    thisdata = merge(simdata$outcomes_data, IV, by="location")
    ivreg_location = feols(y_i ~ 1 | x_i ~ Z_i, data=thisdata)
    pval_location = as.data.table(summary(ivreg_location)$coeftable)[2,"Pr(>|t|)"]
    pval_location = as.numeric(pval_location)

    # use BHJ industry-level IV regression
    ivreg_industry = BHJ_IV(simdata$outcomes_data, simdata$exposure_shares, simdata$industry_shocks)
    pval_industry = as.data.table(summary(ivreg_industry)$coeftable)[2,"Pr(>|t|)"]
    pval_industry = as.numeric(pval_industry)

    pvalues = rbindlist(list(pvalues, data.table(seed=draw, pval_location=pval_location, pval_industry=pval_industry)))

  }

  rejection_rate_location = pvalues[, mean(pval_location <= level_of_significance, na.rm=TRUE)]
  rejection_rate_industry = pvalues[, mean(pval_industry <= level_of_significance, na.rm=TRUE)]

  return(list(rejection_rate_location=rejection_rate_location,rejection_rate_industry=rejection_rate_industry))

}


#' Demonstrate that ssaggregate and our approach are identical.
#' @param seed Random seed.
#' @export
ssaggregate_check <- function(seed=1){

  requireNamespace("ssaggregate")

  # simulate some shift-share data
  simdata = shiftshare_environment(seed=seed, II = 750, NN = 300,
                                   effect_of_interest = 1, weight_on_IV = 0.5,
                                   eps_sd = 0.1, regionshock_sd = 0.1)

  # apply our estimator
  IVreg_BHJ = BHJ_IV(outcomes_data = simdata$outcomes_data, exposure_shares = simdata$exposure_shares, industry_shocks = simdata$industry_shocks)

  # apply the ssaggregate function to aggregate
  industry = ssaggregate(
    data = simdata$outcomes_data,
    shares = simdata$exposure_shares,
    vars = ~ y_i + x_i ,
    n = "industry",
    s = "exposure_share_in",
    l = "location"
  )
  industry = merge(industry, simdata$industry_shocks, by="industry")
  IVreg_ssaggregate = feols(y_i ~ 1 | 0 | x_i ~ industry_shock_n, data = industry, weights = ~ s_n, vcov = "hc1")

  return(rbind(as.data.table(IVreg_BHJ$coeftable)[2,][, approach := "ours"],
               as.data.table(IVreg_ssaggregate$coeftable)[2,][, approach := "ssaggregate"]))

}

