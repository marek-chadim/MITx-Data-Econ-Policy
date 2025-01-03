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

cartier_data <- read.csv("XXX")
cartier_data$auctionid <- as.character(cartier_data$auctionid)
unique_bids <- unique(cartier_data$XXX)

ratio <- rep(NA, times = XXX)
XXX <- rep(NA, times = length(unique_bids))
number_of_bids <- rep(NA, times = length(unique_bids))

for (i in c(1:length(unique_bids))){
  temp <- subset(XXX, cartier_data$XXX == unique_bids[i])
  bid2 <- XXX[rank(temp$bid, ties.method = 'last') == (length(temp$bid)-1)]
  bid3 <- temp$bid[rank(temp$bid, ties.method = 'last') == (length(XXX)-2)]
  ratio[i] <- bid2 / bid3
  number_of_bidders[XXX] <- length(unique(XXX))
  number_of_bids[i] <- length(temp$bid)
}

data_clean <- data.frame(unique_bids, ratio, number_of_bidders, XXX)