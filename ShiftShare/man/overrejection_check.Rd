% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{overrejection_check}
\alias{overrejection_check}
\title{Demonstrate the over-rejection problem using our DGP.}
\usage{
overrejection_check(
  num_draws = 20,
  num_locations = 750,
  num_industries = 300,
  regionshock_sd = 0.1,
  level_of_significance = 0.1
)
}
\arguments{
\item{num_draws}{Number of random draws from DGP.}

\item{num_locations}{Number of locations to simulate.}

\item{num_industries}{Number of industries to simulate.}

\item{regionshock_sd}{Std Dev of region shocks (set to zero to remove over-rejection problem).}
}
\description{
Demonstrate the over-rejection problem using our DGP.
}
