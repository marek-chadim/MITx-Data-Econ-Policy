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