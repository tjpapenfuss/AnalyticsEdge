library(tidyverse)
library(ggrepel)


### Part 1: Use aggregate data

# Import aggregate wine data
# First ensure that the "wine_agg.csv" file is in the same 
# directory as this R file

wine <- read.csv("wine_agg.csv")

# Summary of the wine data
str(wine)

# Slide 9: Take a look at the data
head(wine,20)
tail(wine,1)

# Slide 11: Split into a training set and a test set, by year
train <- subset(wine, Year <= 1985)
test <- subset(wine, Year > 1985)

# Take a quick look at a few records
head(train,10)
tail(train,2)
head(test,4)
tail(test,2)

### First regression model using the training set
# Fit the model
mod1 <- lm(LogAuctionIndex~WinterRain+HarvestRain+GrowTemp+HarvestTemp+Age+FrancePop+USAlcConsump, data=train)

# Slide 19: Examine the output
summary(mod1)

# We didn't cover confidence intervals in class
# bonus command :-)
confint(mod1, level = .99)

# Slide 30: Let's check for multicollinearity
print(cor(wine[,-1]), digits=2)
# BTW, the '-1' above is to remove the 1st column, which is Year,
# from 'wine' before calculating the correlations

# Since FrancePop and Age have a high correlation, 
# let's do a quick visual check
# Slide 32: Multicollinearity plot
print(ggplot(wine, aes(x=Age, y=FrancePop)) + geom_point() + theme_bw() + xlab("Age of Vintage") + ylab("France Population at Time of Vintage (Millions)") + theme(axis.title=element_text(size=18),axis.text.x=element_text(size=18),axis.text.y=element_text(size=18)))


# Second regression model: 
# let's remove correlated and non-significant variables one by one

# The rule-of-thumb for deciding which variable to keep
# and which to drop is to keep the one with the higher absolute correlation
# to the dependent variable UNLESS 
# there's some business/contextual reason to keep the other
# In our case, FrancePop has a higher absolute correlation (0.077)
# to the dependent variable than Age does (0.011) but our JUDGMENT
# tells us that Age may be a better one to keep
# You can also try both possibilities to see which model has a better OSR2
# For now, we keep Age and remove FrancePop

mod2a <- lm(LogAuctionIndex~WinterRain+HarvestRain+GrowTemp+HarvestTemp+Age+USAlcConsump, data=train)
summary(mod2a)

# We next remove USAlcConsump since it is not significant
mod2b <- lm(LogAuctionIndex~WinterRain+HarvestRain+GrowTemp+HarvestTemp+Age, data=train)
summary(mod2b)

# Finally, we remove HarvestTemp since is not significant
mod2c <- lm(LogAuctionIndex~WinterRain+HarvestRain+GrowTemp+Age, data=train)

# Slide 32: Examine the output of the improved model
summary(mod2c)


# Let's predict using the test set so that we can
# calculate OSR2

# Generate predictions from the first model
pred <- predict(mod1, newdata=test)

# Slide 39: Assessing out-of-sample OSR2
SSR_of_model <- sum((pred-test$LogAuctionIndex)^2)
SSR_of_baseline <- sum((mean(train$LogAuctionIndex)-test$LogAuctionIndex)^2)
OSR2 <- 1-SSR_of_model/SSR_of_baseline 
OSR2

# Generate predictions from the improved model
pred2 <- predict(mod2c, newdata=test)

# Slide 39: Assessing out-of-sample OSR2
SSR_of_model <- sum((pred2-test$LogAuctionIndex)^2)
SSR_of_baseline <- sum((mean(train$LogAuctionIndex)-test$LogAuctionIndex)^2)
OSR2_2c <- 1-SSR_of_model/SSR_of_baseline 
OSR2_2c


# Slide 40: Visualize the quality of the predictions
results <- data.frame(year=test$Year,pred=pred,actual=test$LogAuctionIndex)
ggplot(results, aes(x=pred, y=actual)) + xlab("Predicted values") + ylab ("Actual values") + geom_point() + geom_segment(aes(x=6,y=6,xend=8.5,yend=8.5)) + geom_label_repel(aes(label=year),min.segment.length = 0.1) + theme(text=element_text(size=15),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))

results2 <- data.frame(year=test$Year,pred2=pred2,actual=test$LogAuctionIndex)
ggplot(results2, aes(x=pred2, y=actual)) + xlab("Predicted values") + ylab ("Actual values") + geom_point() + geom_segment(aes(x=6,y=6,xend=8.5,yend=8.5)) + geom_label_repel(aes(label=year),min.segment.length = 0.1) + theme(text=element_text(size=15),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))


### Part 2: Use per-winery data

# Import the disaggregate wine data
wine <- read.csv("wine_cutdown.csv")

# Make Year the row labels for nicer printing
rownames(wine) <- paste(seq_len(nrow(wine)), wine$Year)

# Summary of the wine data
str(wine)

# Slide 47: Display of wine data
head(wine,20)
tail(wine,1)

# Slide 48: Split the data into a training set and a test set, by year
train <- subset(wine, Year <= 1985)
test <- subset(wine, Year > 1985)

# Take a quick look
head(train,10)
tail(train,3)
head(test,3)
tail(test,2)

# First model--- use the variables in the 'winning' model from Part 1
# Fit the mdoel
mod1 <- lm(LogAuction~Age+WinterRain+HarvestRain+GrowTemp, data=train)

# Slide 49: Examine the output
summary(mod1)

### Trends explaining the poor model fit
# Slide 50: Overall scatter plot 
ggplot(wine, aes(x=Year, y=LogAuction)) +
  geom_point(size=4) +
  theme_bw() +
  xlab("Vintage Year") +
  ylab("log(2015 Auction Price)") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=14), legend.title=element_text(size=18), legend.text=element_text(size=14))


# Slide 51: Scatter plot, with diff colors for diff wineries
ggplot(wine, aes(x=Year, y=LogAuction, group=Winery, col=Winery)) +
  geom_point(size=4) +
  theme_bw() +
  xlab("Vintage Year") +
  ylab("log(2015 Auction Price)") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=14), legend.title=element_text(size=18), legend.text=element_text(size=14))

# Slide 52: Line plot, with diff colors for diff wineries
ggplot(wine, aes(x=Year, y=LogAuction, group=Winery, col=Winery)) +
  geom_line(size=4) +
  theme_bw() +
  xlab("Vintage Year") +
  ylab("log(2015 Auction Price)") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=14), legend.title=element_text(size=18), legend.text=element_text(size=14))


### Model with categorical variables for the 5 wineries

# Fit the model 
all.wineries <- lm(LogAuction~Winery+Age+WinterRain+HarvestRain+GrowTemp, data=train)

# Slide 55: Examine the output
summary(all.wineries)

# Generate predictions 
pred <- predict(all.wineries, newdata=test)

# Slide 55: Assessing out-of-sample OSR2
SSR_of_model <- sum((pred-test$LogAuction)^2)
SSR_of_baseline <- sum((mean(train$LogAuction)-test$LogAuction)^2)
OSR2 <- 1-SSR_of_model/SSR_of_baseline
OSR2 


# Slide 56: A few predictions

predict(all.wineries,
        newdata=data.frame(Winery="Cos d'Estournel", Age=1,
                           WinterRain=522.3, HarvestRain=78.9,
                           GrowTemp=18.23))
predict(all.wineries,
        newdata=data.frame(Winery="Giscours", Age=1,
                           WinterRain=522.3, HarvestRain=78.9,
                           GrowTemp=18.23))
predict(all.wineries,
        newdata=data.frame(Winery="Beychevelle", Age=1,
                           WinterRain=522.3, HarvestRain=78.9,
                           GrowTemp=18.23))

### THE END

