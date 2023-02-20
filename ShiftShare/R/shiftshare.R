
#' Construct the shift-share IV using industry shocks and exposure shares.
#' @param exposure_shares data.table with columns "industry", "location", and "exposure_share_in"
#' @param industry_shocks data.table with columns "industry" and "industry_shock_n"
#' @export
construct_shiftshare_IV = function(exposure_shares, industry_shocks){

  shiftshareIV = merge(exposure_shares,industry_shocks, by="industry")

  return(shiftshareIV[, list(Z_i = sum(industry_shock_n * exposure_share_in)), location])

}





