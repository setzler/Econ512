
# simulate some fake data with log-wage and log-wealth
fake_data = data.table(workers = 1:N, logwage = rnorm(N,mean=10), logwealth = rnorm(N, mean=12, sd=2))

# suppose we estimated this log consumption policy
logconsumption = function(logwage, logwealth){
  0.5*logwage + 0.9*logwealth
}

# this will use Monte Carlo to integrate across the joint distribution of (log wage, log wealth)
# it uses the empirical distribution, so no weights are needed in the integral
average_consumption = function(data, num_draws){
  # all the workers in the data
  index_set = data[,workers]
  # randomly draw a set of workers (with replacement)
  actual_draws = sample(index_set, size = num_draws, replace=TRUE)
  # loop over the random workers
  consumption_by_worker = rep(NA, num_draws)
  for(iter in 1:num_draws){
    # get this random worker's predicted consumption
    this_worker = actual_draws[iter]
    this_data = data[workers == this_worker]
    consumption_by_worker[iter] = exp(logconsumption(logwage = this_data$logwage, logwealth = this_data$logwealth))
  }
  # take the mean of the random draws (equally weighted)
  return(mean(consumption_by_worker))
}

# try out the Monte Carlo bivariate integration function
average_consumption(data = fake_data, num_draws = 10000)
