# Numerical Integration by Example
library(data.table)
library(ggplot2)
library(statmod)


# what the solution should be
true_integral_of_logx <- function(lower, upper) {
  return((upper * log(upper) - lower * log(lower))/(upper - lower) - 1)
}

# check
results = data.table()
results = rbind(results, data.table(method = "true", value = true_integral_of_logx(lower=1, upper=3)))

# integrate across uniform distribution using the trapezoidal rule
trapezoid_rule_for_uniform_expectation <- function(objfunc, lower, upper, num_intervals) {
    evaluation_points <- seq(lower, upper, length.out = num_intervals + 1)
    objfunc_evaluations <- objfunc(evaluation_points)
    total_area = 0
    for(ii in 1:num_intervals){
        this_area = (evaluation_points[ii + 1] - evaluation_points[ii]) * (objfunc_evaluations[ii] + objfunc_evaluations[ii + 1]) / 2
        total_area = total_area + this_area
    }
    return(total_area/(upper - lower))
}

# integrate using simpson's rule
simpsons_rule_for_uniform_expectation <- function(objfunc, lower, upper, num_intervals) {
    evaluation_points <- seq(lower, upper, length.out = num_intervals + 1)
    objfunc_evaluations <- objfunc(evaluation_points)
    total_area = 0
    for(ii in 1:num_intervals){
        this_area = (evaluation_points[ii + 1] - evaluation_points[ii]) * (objfunc_evaluations[ii] + 4 * objfunc((evaluation_points[ii] + evaluation_points[ii + 1])/2) + objfunc_evaluations[ii + 1]) / 6
        total_area = total_area + this_area
    }
    return(total_area/(upper - lower))
}


gaussian_legendre = function(func, nodes, lower, upper){
  
  set_of_nodes = gauss.quad(n=nodes, kind='legendre')$nodes
  set_of_weights = gauss.quad(n=nodes, kind='legendre')$weights
  translated_nodes = (upper - lower)/2*set_of_nodes + (upper + lower)/2
  
  sub_integrals = rep(0,nodes)
  for(ii in 1:length(set_of_nodes)){
    sub_integrals[ii] = set_of_weights[ii] * func(translated_nodes[ii])
  }
  
  full_integral = sum(sub_integrals)/(upper-lower)
  return(full_integral)
}


monte_carlo_expectation_of_uniform <- function(objfunc, lower, upper, num_samples){
    samples = runif(num_samples, min=lower, max=upper)
    return(mean(objfunc(samples)))
}


# check convergence
convergence_log = data.table()
for(num_intervals in 1:20){ 
    # run the trapezoid rule
    convergence_log = rbind(convergence_log, data.table(method = "trapezoid", value = trapezoid_rule(log, lower=3, upper=6, num_intervals=num_intervals), num_intervals = num_intervals))
    # run simpson's rule
    convergence_log = rbind(convergence_log, data.table(method = "simpsons", value = simpsons_rule_for_uniform_expectation(log, lower=3, upper=6, num_intervals=num_intervals), num_intervals = num_intervals))
    # run gaussian quadrature
    #convergence_log = rbind(convergence_log, data.table(method = "gaussian", value = gaussian_legendre(log, num_intervals, lower=3, upper=6), num_intervals = num_intervals))
    # run monte carlo
    convergence_log = rbind(convergence_log, data.table(method = "monte_carlo", value = monte_carlo_expectation_of_uniform(log, lower=3, upper=6, num_samples=10000), num_intervals = num_intervals))
    # use integrate() function to evaluate the integral
    convergence_log = rbind(convergence_log, data.table(method = "integrate", value = integrate(log, lower=3, upper=6)$value/(6-3), num_intervals = num_intervals))
}

gg = ggplot(aes(x=num_intervals, y=value, color=method), data=convergence_log) + geom_line() + geom_point() + ggtitle("Convergence of Trapezoid Rule: log(X)") + xlab("Number of Intervals") + ylab("Approximate Integral Value") + geom_line(aes(x=num_intervals, y=true_integral_of_logx(lower=3, upper=6)), color="black")

ggsave(filename="trapezoid_convergence_log.png", gg, width=7, height=5)





true_expectation_of_sin <- function(lower, upper) {
  return((-cos(upper) + cos(lower))/(upper - lower))
}

convergence_sin = data.table()
for(num_intervals in 1:20){
    # run the trapezoid rule 
    convergence_sin = rbind(convergence_sin, data.table(method = "trapezoid", value = trapezoid_rule(sin, lower=3, upper=6, num_intervals=num_intervals), num_intervals = num_intervals))
    # run simpson's rule
    convergence_sin = rbind(convergence_sin, data.table(method = "simpsons", value = simpsons_rule_for_uniform_expectation(sin, lower=3, upper=6, num_intervals=num_intervals), num_intervals = num_intervals))
    # run gaussian quadrature
    #convergence_sin = rbind(convergence_sin, data.table(method = "gaussian", value = gaussian_legendre(sin, num_intervals, lower=3, upper=6), num_intervals = num_intervals))
    # run monte carlo
    convergence_sin = rbind(convergence_sin, data.table(method = "monte_carlo", value = monte_carlo_expectation_of_uniform(sin, lower=3, upper=6, num_samples=10000), num_intervals = num_intervals))
    # use integrate() function to evaluate the integral
    convergence_sin = rbind(convergence_sin, data.table(method = "integrate", value = integrate(sin, lower=3, upper=6)$value/(6-3), num_intervals = num_intervals))
}


gg = ggplot(aes(x=num_intervals, y=value, color=method), data=convergence_sin) + geom_line() + geom_point() + ggtitle("Convergence of Trapezoid Rule: Sin(x)") + xlab("Number of Intervals") + ylab("Approximate Integral Value") + geom_line(aes(x=num_intervals, y=true_expectation_of_sin(lower=3, upper=6)), color="black")

ggsave(filename="trapezoid_convergence_sin.png", gg, width=7, height=5)

