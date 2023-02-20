
library(ShiftShare)

overrej = overrejection_check(num_draws = 20)
print(overrej[, mean(pval_location <= 0.05)])
print(overrej[, mean(pval_industry <= 0.05)])

simdata = shiftshare_environment(seed=1, II = 100, NN = 30,
                                 effect_of_interest = 0.2, weight_on_IV = 0.5,
                                 eps_sd = 0.1, regionshock_sd = 0.1)

IVreg_BHJ = BHJ_IV(simdata$outcomes_data, simdata$exposure_shares, simdata$industry_shocks)


simdata2 = shiftshare_environment(seed=2, II = 100, NN = 30,
                                 effect_of_interest = 0.2, weight_on_IV = 0.5,
                                 eps_sd = 0.1, regionshock_sd = 0.1)

IVreg_BHJ2 = BHJ_IV(simdata2$outcomes_data, simdata2$exposure_shares, simdata2$industry_shocks)

# constructed_IV = construct_shiftshare_IV(exposure_shares, industry_shocks)
# thisdata = merge(outcomes_data, constructed_IV , by="location")
# IVreg_location = feols(y_i ~ 1 | x_i ~ Z_i, data=thisdata)

dd = as.data.table(summary(IVreg_BHJ)$coeftable)
dd$rownames = c("intercept", "coefficient")
dd$reg = 1

dd2 = as.data.table(summary(IVreg_BHJ2)$coeftable)
dd2$rownames = c("intercept", "coefficient")
dd2$reg = 2

dd = rbind(dd,dd2)

tab = TR(c("","First Regression", "Second Regression"), c(1,2,2))
tab = tab + midrulep(list(c(2,3),c(4,5)))
tab = tab + TR(c("",rep(c("Estimate","Std. Err."),2)))
tab = tab + midrulep(list(c(2,5)))

tab = tab + TR("This is the intercept") %:%
  TR(as.numeric(dd[reg==1 & rownames=='intercept',c(1,2)]), se=c(F,T), dec=c(3,2)) %:%
  TR(as.numeric(dd[reg==2 & rownames=='intercept',c(1,2)]), se=c(F,T), dec=c(3,2))

tab = tab + vspace(3)

tab = tab + TR("This is the coefficients") %:%
  TR(as.numeric(dd[reg==1 & rownames=='coefficient',c(1,2)]), se=c(F,T), dec=c(3,2)) %:%
  TR(as.numeric(dd[reg==2 & rownames=='coefficient',c(1,2)]), se=c(F,T), dec=c(3,2))

tab = tab + TR("Random") %:% TR("Is this red", 4, surround = "{\\color{red} %s}")

TS(tab,file = "practice_table", header = c("l",rep("r",4)), output_path = "~/Downloads")







