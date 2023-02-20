
library(ShiftShare)
library(fixest)

overrej = overrejection_check(num_draws = 1000)
print(overrej[, mean(pval <= 0.05)])

