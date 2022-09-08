wine = read.csv("wine.csv")
mod = lm(LogAuctionIndex ~. -Year, data=wine)
summary(mod)
