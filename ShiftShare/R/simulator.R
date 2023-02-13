

#' Simulator of the Shift-Share Environment
#' @param seed Sets the random seed.
#' @param II Number of locations to simulate.
#' @export
shiftshare_environment = function(seed=1, II = 100, NN = 30,
                                  effect_of_interest = 1, weight_on_IV = 0.5,
                                  eps_sd = 0.1, regionshock_sd = 0.1){

  # manually set the seed for this simulation
  set.seed(seed)

  # II locations, each with a random region assignment
  location_sim = data.table(location = 1:II)
  location_sim[, region := sample(1:10, size=II, replace=TRUE)]

  # draw random shocks for the location and region
  location_sim[, epsilon_i := rnorm(II, sd = eps_sd)]
  location_sim[, regionshock := rnorm(1, sd = regionshock_sd), region] # common region shock
  location_sim[, epsilon_i := epsilon_i + regionshock] # add region shock into location shock

  # create exposure shares between industry and location
  location_industry_sim = setDT(expand.grid(location = 1:II, industry = 1:NN))
  location_industry_sim = merge(location_industry_sim , location_sim[,list(location,region)], by="location")

  # construct shares (i = location, n = industry)
  II_NN = nrow(location_industry_sim)
  location_industry_sim[, exposure_share_in := runif(II_NN)]
  location_industry_sim[, region_exposure := runif(1), list(region,industry)]
  location_industry_sim[, exposure_share_in := exposure_share_in + region_exposure]
  location_industry_sim[, exposure_share_in := exposure_share_in/sum(exposure_share_in), location]

  # clean up data sets
  location_sim = location_sim[, list(location, epsilon_i)]
  location_industry_sim = location_industry_sim[, list(location, industry, exposure_share_in)]

  # construct industry shock g
  industry_sim = data.table(industry = 1:NN)
  industry_sim[, industry_shock_n := rnorm(NN, sd = 1)]

  # construct the observables
  location_industry_sim = merge(location_industry_sim , industry_sim , by="industry")
  shiftshareIV = location_industry_sim[, list( z_i = sum(exposure_share_in * industry_shock_n)), location]
  location_sim = merge(location_sim, shiftshareIV, by="location")
  location_sim[, x_i := weight_on_IV * z_i + (1-weight_on_IV) * epsilon_i]
  location_sim[, y_i := effect_of_interest * x_i + epsilon_i]

  # finish
  return(list(
    outcomes_data = location_sim[,list(location, x_i, y_i)],
    exposure_shares = location_industry_sim[,list(location,industry,exposure_share_in)],
    industry_shocks = industry_sim[,list(industry, industry_shock_n)]
  ))

}



