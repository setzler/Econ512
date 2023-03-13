
library(statmod)

## trapezoid rule
trapezoid_rule = function(integrand, num_nodes, lower, upper){
  # define the nodes for the partitions
  set_of_nodes = seq(lower, upper, length.out=num_nodes)
  # container for integrals of partitions
  set_of_integrals = rep(NA, num_nodes - 1)
  # iterate across nodes
  for(iter in 1:(num_nodes - 1)){
    # get the current and next node
    x_current = set_of_nodes[iter]
    x_next = set_of_nodes[iter+1]
    # use the trapezoid rule for the integral of this partition
    set_of_integrals[iter] = 
      (x_next - x_current) * (integrand(x_current) + integrand(x_next))/2
  }
  # combine the integrals of all the partitions
  return(sum(set_of_integrals)) 
}



## simpson's rule
simpsons_rule = function(integrand, num_nodes, lower, upper){
  # define the nodes for the partitions
  set_of_nodes = seq(lower, upper, length.out=num_nodes)
  # container for integrals of partitions
  set_of_integrals = rep(NA, num_nodes - 1)
  # iterate across nodes
  for(iter in 1:(num_nodes - 1)){
    # get the current and next node
    x_current = set_of_nodes[iter]
    x_next = set_of_nodes[iter+1]
    x_mid = (x_current + x_next)/2
    # use the trapezoid rule for the integral of this partition
    set_of_integrals[iter] = 
      (x_next - x_current) * (integrand(x_current) + integrand(x_next) + 4*integrand(x_mid))/6
  }
  # combine the integrals of all the partitions
  return(sum(set_of_integrals)) 
}

legendre_quadrature = function(integrand, num_nodes, lower, upper){
  # define the nodes for the partitions
  set_of_base_points = gauss.quad(num_nodes, kind = "legendre")$nodes
  # convert nodes from [-1,1] to [lower, upper]
  set_of_nodes = (upper-lower)/2*set_of_base_points + (upper + lower)/2
  # get the Gaussian quadrature weights
  set_of_weights = gauss.quad(num_nodes, kind = "legendre")$weights
  # evaluate the integral at each node
  integrand_evaluated = integrand(set_of_nodes)
  # take the weighted sum across nodes
  return(sum(integrand_evaluated * set_of_weights))
}

montecarlo_integration = function(integrand, num_nodes, lower, upper){
  # randomly draw a set of nodes (uniformly)
  set_of_nodes = runif(num_nodes, min = lower, max = upper)
  # evaluate the integral at each node
  integrand_evaluated = integrand(set_of_nodes)
  # take the mean
  return(mean(integrand_evaluated)*(upper - lower))
}

true_integral <- function(lower, upper){
  (upper*log(upper) - lower*log(lower))/(upper - lower) - 1
  
}

logX = function(x){
  log(x)
}


## ------------- working area ------------------

print(sprintf("true value: %s", true_integral(1,3)))
print(sprintf("trapezoid: %s", 
              trapezoid_rule(integrand = logX, num_nodes=5, lower=1, upper=3)/(upper-lower)))
print(sprintf("simpson: %s", 
              simpsons_rule(integrand = logX, num_nodes=5, lower=1, upper=3)/(upper-lower)))
print(sprintf("legendre: %s", 
              legendre_quadrature(integrand = logX, num_nodes=5, lower=1, upper=3)/(upper-lower)))
print(sprintf("monte carlo: %s", 
              montecarlo_integration(integrand = logX, num_nodes=100, lower=1, upper=3)/(upper-lower)))
print(sprintf("integrate() in R: %s", 
              integrate(logX, lower=1, upper=3)$value/(upper-lower)))


