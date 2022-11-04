# ----------------------------------------------------------------------------------------
##### Problem 1:
# ----------------------------------------------------------------------------------------
library(caret)
library(rpart)
library(rpart.plot) 
library(caTools)
library(dplyr)
library(randomForest)
library(gdata)
library(ggplot2)
library(tidyr)

# Part (a) code block
# ----------------------------------------------------------------------------------------
data = read.csv("returns.csv")

# Get a subset of the returns data set. This subset includes the Industry (2)
# and the returns from 01/2008 -> 12/2010.
returns = data[c(2,25:60)]

agg_data  = aggregate(. ~ Industry, returns, mean)

agg_data %>% 
  gather(var, val , -Industry) %>% 
  ggplot(aes(x = var, y = val, color = as.factor(Industry), group = as.factor(Industry)))+
  geom_point() + scale_x_discrete(guide = guide_axis(n.dodge = 4))

ggsave("Industry_returns.pdf")

# Generate an aggregate for the each industry for Sept 2008.
aggregate(data$avg200809 ~ data$Industry, data=data, mean)

# ----------------------------------------------------------------------------------------
#Part (b)
# ----------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------
##### Problem 2: eBay.com
# ----------------------------------------------------------------------------------------
library(caret)
library(rpart)
library(rpart.plot) 
library(caTools)
library(dplyr)
library(randomForest)
library(gdata)
library(ggplot2)

ebay.df <- read.xls("eBayAuctions.xls")
table(ebay.df$Competitive.)

ebay.df$category.factor <- as.factor(ebay.df$Category)
ebay.df$currency.factor <- as.factor(ebay.df$currency)
ebay.df$endDay.factor <- as.factor(ebay.df$endDay)
ebay.df$duration.factor <- as.factor(ebay.df$Duration)
ebay.df$competitive.factor <- as.factor(ebay.df$Competitive.)



set.seed(15071)
split = createDataPartition(ebay.df$category.factor, p = 0.6, list = FALSE) 
ebay.train = ebay.df[split,]
ebay.test = ebay.df[-split,]
head(ebay.train)
# str(ebay.train)
# Testing to make sure I have an appropriate distro. 
ggplot(aes(x=category.factor, y = ..count..), data = ebay.train) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ggplot(aes(x=category.factor, y = ..count..), data = ebay.test) +
#  geom_bar(stat = "count") +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#ggplot(aes(x=category.factor, y = ..count..), data = ebay.df) +
#  geom_bar(stat = "count") +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### (a)
## adaboost
library(adabag)
adaboost <- boosting(Competitive.~sellerRating+currency.factor+duration.factor+
                       endDay.factor+category.factor+ClosePrice+OpenPrice, data = ebay.train)
# variable importance
importanceplot(adaboost, cex.names=0.7)

# training
pred_train <- predict(adaboost, newdata=ebay.train, type="Competitive.")
pred_train
pred_train$confusion
#confusion.matrix = table(ebay.train$Competitive., pred_train)
#confusion.matrix
# TN | FN
# FP | TP
#NEED TO FIX THIS BELOW CONDITION.
#TPR <- pred_train$confusion[2,2]/sum(pred_train$confusion[2,])
#TPR
#FPR <- pred_train$confusion[1,2]/sum(pred_train$confusion[1,])
#FPR

# training
# confusionMatrix(, )
# validation
# confusionMatrix(, )

# validation
pred_test <- predict(adaboost, newdata=ebay.test, type="class")
pred_test
pred_test$confusion
1-pred_test$error
# confusion.matrix = table(ebay.train$Competitive., pred_train)
# confusion.matrix
#TPR <- pred_test$confusion[2,2]/sum(pred_test$confusion[2,])
#TPR
#FPR <- pred_test$confusion[1,2]/sum(pred_test$confusion[1,])
#FPR

## xgboost
library(xgboost)
xgb <- xgboost(data = model.matrix(Competitive.~ .-1,
                                   subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.))), label = ebay.train$Competitive., max.depth = 20, eta = 1, nthread = 2, nrounds = 50, objective = "binary:logistic", verbose = 0)
# variable importance
xgb.importance(colnames(model.matrix(Competitive.~ .-1, subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.)))), model = xgb)
# training
confusionMatrix(factor(1*(predict(xgb, model.matrix(Competitive.~ .-1, subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.))))>sum(ebay.df$Competitive.==1)/nrow(ebay.df))), ebay.train$competitive.factor)
# validation
confusionMatrix(factor(1*(predict(xgb, model.matrix(Competitive.~ .-1, subset(ebay.test, select=-c(competitive.factor, ebay.train$Competitive.))))>sum(ebay.df$Competitive.==1)/nrow(ebay.df))), ebay.test$competitive.factor)

## bagging
bag <- bagging(competitive.factor~sellerRating+currency.factor+duration.factor+
                 endDay.factor+category.factor+ClosePrice+OpenPrice, data = ebay.train)
# variable importance
importanceplot(bag, cex.names=0.7)
# training
# confusionMatrix(, )
pred_train <- predict(bag, newdata=ebay.train, type="class")
pred_train
pred_train$confusion
1-pred_train$error

# validation
# confusionMatrix(, )
pred_test <- predict(bag, newdata=ebay.test, type="class")
pred_test
pred_test$confusion
1-pred_test$error


## random forest
library(randomForest)
rf <- randomForest(competitive.factor~sellerRating+currency.factor+duration.factor+
                     endDay.factor+category.factor+ClosePrice+OpenPrice, data = ebay.train, method = "class")
# variable importance
varImp(rf, conditional=TRUE)

#TRAINING
rf_pred_train <- predict(rf, newdata=ebay.train, type="class")
confusionMatrix(ebay.train$competitive.factor, rf_pred_train)
# validation
rf_pred_test <- predict(rf, newdata=ebay.test, type="class")
confusionMatrix(ebay.test$competitive.factor, rf_pred_test)

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
### (b)

## adaboost
adaboost <- boosting(competitive.factor~sellerRating+currency.factor+duration.factor+
                       endDay.factor+category.factor+OpenPrice, data = ebay.train)
# variable importance
importanceplot(adaboost, cex.names=0.7)

# training
ada_pred_train <- predict(adaboost, newdata=ebay.train, type="Competitive.")
ada_pred_train$confusion
1-ada_pred_train$error

# validation
ada_pred_test <- predict(adaboost, newdata=ebay.test, type="class")
ada_pred_test$confusion
1-ada_pred_test$error

## xgboost
xgb <- xgboost(data = model.matrix(Competitive.~sellerRating+currency.factor+duration.factor+
                                     endDay.factor+category.factor+OpenPrice,
                                   subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.))), label = ebay.train$Competitive., max.depth = 20, eta = 1, nthread = 2, nrounds = 50, objective = "binary:logistic", verbose = 0)
# variable importance
xgb.importance(colnames(model.matrix(Competitive.~ sellerRating+currency.factor+duration.factor+
                                       endDay.factor+category.factor+OpenPrice, subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.)))), model = xgb)
# training
confusionMatrix(factor(1*(predict(xgb, model.matrix(Competitive.~sellerRating+currency.factor+duration.factor+
                                                      endDay.factor+category.factor+OpenPrice, subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.))))>sum(ebay.df$Competitive.==1)/nrow(ebay.df))), ebay.train$competitive.factor)
# validation
confusionMatrix(factor(1*(predict(xgb, model.matrix(Competitive.~sellerRating+currency.factor+duration.factor+
                                                      endDay.factor+category.factor+OpenPrice, subset(ebay.test, select=-c(competitive.factor, ebay.train$Competitive.))))>sum(ebay.df$Competitive.==1)/nrow(ebay.df))), ebay.test$competitive.factor)

## bagging
bag <- bagging(competitive.factor~sellerRating+currency.factor+duration.factor+
                 endDay.factor+category.factor+OpenPrice, data = ebay.train)
# variable importance
importanceplot(bag, cex.names=0.7)
# training
bag_pred_train <- predict(bag, newdata=ebay.train, type="class")
1-bag_pred_train$error

# validation
bag_pred_test <- predict(bag, newdata=ebay.test, type="class")
1-bag_pred_test$error

## random forest
rf <- randomForest(competitive.factor~sellerRating+currency.factor+duration.factor+
                     endDay.factor+category.factor+OpenPrice, data = ebay.train, method = "class")
# variable importance
varImp(rf, conditional=TRUE)

#TRAINING
rf_pred_train <- predict(rf, newdata=ebay.train, type="class")
confusionMatrix(ebay.train$competitive.factor, rf_pred_train)
# validation
rf_pred_test <- predict(rf, newdata=ebay.test, type="class")
confusionMatrix(ebay.test$competitive.factor, rf_pred_test)
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------



### (c)
## adaboost
adaboost <- boosting(competitive.factor~sellerRating+currency.factor+duration.factor+
                       endDay.factor+category.factor+ClosePrice, data = ebay.train)
# variable importance
importanceplot(adaboost, cex.names=0.7)

# training
ada_pred_train <- predict(adaboost, newdata=ebay.train, type="Competitive.")
ada_pred_train$confusion
1-ada_pred_train$error

# validation
ada_pred_test <- predict(adaboost, newdata=ebay.test, type="class")
ada_pred_test$confusion
1-ada_pred_test$error

## xgboost
xgb <- xgboost(data = model.matrix(Competitive.~sellerRating+currency.factor+duration.factor+
                                     endDay.factor+category.factor+ClosePrice,
                                   subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.))), label = ebay.train$Competitive., max.depth = 20, eta = 1, nthread = 2, nrounds = 50, objective = "binary:logistic", verbose = 0)
# variable importance
xgb.importance(colnames(model.matrix(Competitive.~ sellerRating+currency.factor+duration.factor+
                                       endDay.factor+category.factor+ClosePrice, subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.)))), model = xgb)
# training
confusionMatrix(factor(1*(predict(xgb, model.matrix(Competitive.~sellerRating+currency.factor+duration.factor+
                                                      endDay.factor+category.factor+ClosePrice, subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.))))>sum(ebay.df$Competitive.==1)/nrow(ebay.df))), ebay.train$competitive.factor)
# validation
confusionMatrix(factor(1*(predict(xgb, model.matrix(Competitive.~sellerRating+currency.factor+duration.factor+
                                                      endDay.factor+category.factor+ClosePrice, subset(ebay.test, select=-c(competitive.factor, ebay.train$Competitive.))))>sum(ebay.df$Competitive.==1)/nrow(ebay.df))), ebay.test$competitive.factor)

## bagging
bag <- bagging(competitive.factor~sellerRating+currency.factor+duration.factor+
                 endDay.factor+category.factor+ClosePrice, data = ebay.train)
# variable importance
importanceplot(bag, cex.names=0.7)
# training
bag_pred_train <- predict(bag, newdata=ebay.train, type="class")
1-bag_pred_train$error

# validation
bag_pred_test <- predict(bag, newdata=ebay.test, type="class")
1-bag_pred_test$error

## random forest
rf <- randomForest(competitive.factor~sellerRating+currency.factor+duration.factor+
                     endDay.factor+category.factor+ClosePrice, data = ebay.train, method = "class")
# variable importance
varImp(rf, conditional=TRUE)

#TRAINING
rf_pred_train <- predict(rf, newdata=ebay.train, type="class")
confusionMatrix(ebay.train$competitive.factor, rf_pred_train)
# validation
rf_pred_test <- predict(rf, newdata=ebay.test, type="class")
confusionMatrix(ebay.test$competitive.factor, rf_pred_test)
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------



### (d)

## adaboost
adaboost <- boosting(competitive.factor~sellerRating+currency.factor+duration.factor+
                       endDay.factor+category.factor, data = ebay.train)
# variable importance
importanceplot(adaboost, cex.names=0.7)

# training
ada_pred_train <- predict(adaboost, newdata=ebay.train, type="Competitive.")
ada_pred_train$confusion
1-ada_pred_train$error

# validation
ada_pred_test <- predict(adaboost, newdata=ebay.test, type="class")
ada_pred_test$confusion
1-ada_pred_test$error

## xgboost
xgb <- xgboost(data = model.matrix(Competitive.~sellerRating+currency.factor+duration.factor+
                                     endDay.factor+category.factor,
                                   subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.))), label = ebay.train$Competitive., max.depth = 20, eta = 1, nthread = 2, nrounds = 50, objective = "binary:logistic", verbose = 0)
# variable importance
xgb.importance(colnames(model.matrix(Competitive.~ sellerRating+currency.factor+duration.factor+
                                       endDay.factor+category.factor, subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.)))), model = xgb)
# training
confusionMatrix(factor(1*(predict(xgb, model.matrix(Competitive.~sellerRating+currency.factor+duration.factor+
                                                      endDay.factor+category.factor, subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.))))>sum(ebay.df$Competitive.==1)/nrow(ebay.df))), ebay.train$competitive.factor)
# validation
confusionMatrix(factor(1*(predict(xgb, model.matrix(Competitive.~sellerRating+currency.factor+duration.factor+
                                                      endDay.factor+category.factor, subset(ebay.test, select=-c(competitive.factor, ebay.train$Competitive.))))>sum(ebay.df$Competitive.==1)/nrow(ebay.df))), ebay.test$competitive.factor)

## bagging
bag <- bagging(competitive.factor~sellerRating+currency.factor+duration.factor+
                 endDay.factor+category.factor, data = ebay.train)
# variable importance
importanceplot(bag, cex.names=0.7)
# training
bag_pred_train <- predict(bag, newdata=ebay.train, type="class")
1-bag_pred_train$error

# validation
bag_pred_test <- predict(bag, newdata=ebay.test, type="class")
1-bag_pred_test$error

## random forest
rf <- randomForest(competitive.factor~sellerRating+currency.factor+duration.factor+
                     endDay.factor+category.factor, data = ebay.train, method = "class")
# variable importance
varImp(rf, conditional=TRUE)

#TRAINING
rf_pred_train <- predict(rf, newdata=ebay.train, type="class")
confusionMatrix(ebay.train$competitive.factor, rf_pred_train)
# validation
rf_pred_test <- predict(rf, newdata=ebay.test, type="class")
confusionMatrix(ebay.test$competitive.factor, rf_pred_test)
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------


### (e)
Music_movie_count = ebay.df %>% count(category.factor, sort = TRUE)

summary(ebay.df)

# ----------------------------------------------------------------------------------------
# END OF Problem 3
# ----------------------------------------------------------------------------------------