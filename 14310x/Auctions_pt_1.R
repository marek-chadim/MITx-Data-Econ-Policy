#Preliminaries
rm(list = ls())
setwd("Dropbox/MITx/14310x")

#Uniform Valuations
number_of_bidders <- 2
number_of_simulations <- 1000

set.seed(1)
valuations1 <- matrix(runif(
  number_of_bidders*number_of_simulations, min=0, max=1),
  nrow = number_of_simulations)

#Uniform Valuations
number_of_bidders <- 2
N <- number_of_bidders
V <- 10000

set.seed(5)
valuations <- matrix(runif(
  N*V, min = 0, max = 1),
  nrow = V)

maximum_valuation <- apply(valuations, 1, max)
optimal_price <- 1/((N+1)^(1/N))
expected_revenue <- (N/(N+1)) * 1/((N+1)^(1/N))

revenue <- optimal_price*(maximum_valuation >= optimal_price)
mean(revenue)
expected_revenue
