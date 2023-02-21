
library(ShiftShare)

# check if we match ssaggregate
library(ssaggregate)
print(ssaggregate_check(seed=1))

# check if we overreject
print(overrejection_check(num_draws = 200, num_locations=2000, num_industries=200, regionshock_sd = 0.1, level_of_significance = 0.05))
