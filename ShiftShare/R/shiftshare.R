
#' Construct the shift-share IV using industry shocks and exposure shares.
#' @param exposure_shares data.table with columns "industry", "location", and "exposure_share_in"
#' @param industry_shocks data.table with columns "industry" and "industry_shock_n"
#' @export
construct_shiftshare_IV = function(exposure_shares, industry_shocks){

  shiftshareIV = merge(exposure_shares,industry_shocks, by="industry")

  return(shiftshareIV[, list(Z_i = sum(industry_shock_n * exposure_share_in)), location])

}


#' Perform Borusyak, Hull & Jaravel (2022) transformation to industry-level regression in shift-share IV.
#' @param outcomes_data data.table with columns "y_i", "x_i", and "location"
#' @param exposure_shares data.table with columns "industry", "location", and "exposure_share_in"
#' @param industry_shocks data.table with columns "industry" and "industry_shock_n"
#' @export
BHJ_IV = function(outcomes_data, exposure_shares, industry_shocks){

  outcomes_with_exposure = merge(outcomes_data, exposure_shares, by="location")

  BHJ_aggregates = outcomes_with_exposure[,list(
    ybar_industry_n = sum(y_i*exposure_share_in)/sum(exposure_share_in),
    xbar_industry_n = sum(x_i*exposure_share_in)/sum(exposure_share_in),
    sbar_n = mean(exposure_share_in)
  ), industry]

  BHJ_aggregates = merge(BHJ_aggregates, industry_shocks, by="industry")

  IVreg_BHJ = feols(ybar_industry_n ~ 1 | 0 | xbar_industry_n ~ industry_shock_n, data = BHJ_aggregates, weights = ~ sbar_n, vcov = "hc1")

  return(IVreg_BHJ)

}


