library(caret)
library(rpart)
library(rpart.plot) 
library(caTools)
library(dplyr)
library(randomForest)

##### Problem 1: Preventing Hospital Readmissions
readmission = read.csv("readmission.csv")
set.seed(144)
split = createDataPartition(readmission$readmission, p = 0.75, list = FALSE) 
readm.train <- readmission[split,]
readm.test <- readmission[-split,]

### Question a.
ggplot(readm.train, aes(x = insulin, y = readmission, group=insulin)) + geom_point() + 
  geom_count(method = "lm", se = FALSE) + 
  ggtitle("Age vs. Readmission") + xlab("Age") + ylab("Readmission")

regressor = lm(readmission ~ ., data = readm.train)
summary(regressor)

# table(readmission$readmission)

### Question b. See word document for calculations. 
loss.matrix = matrix(0,2,2)
# Then we can specify the two non-zero entries:
loss.matrix[2,1] = 7550
loss.matrix[1,2] = 1200
# We can take a look at the final table:
loss.matrix 

### Question c.
# Fitting the CART model
tree <- rpart(readmission~., parms=list(loss=loss.matrix), data = readm.train, method="class", cp=0.001)

# Plotting the CART tree
pdf('readmission_tree.pdf',12,6)
prp(tree, varlen=0,faclen=0,digits=3)
dev.off()

### Question d.
pred <- predict(tree, newdata=readm.test, type="class")

confusion.matrix = table(readm.test$readmission, pred)
confusion.matrix

TPR <- confusion.matrix[2,2]/sum(confusion.matrix[2,])
TPR

FPR <- confusion.matrix[1,2]/sum(confusion.matrix[1,])
FPR

### Question e.

totalCostNoHelp = 35000*2741
#Below is how you get the rate in which you break even. 
((35000*2741)-(35000*1796)-((35000)*(1-rate)*945))/(4056+945)

(1200*4056)
#This is the rate at which you break even. 
rate = 0.25
rate_value = c()
totalcost = c()
rates = c()
i = 0

for( i in seq(0, 3000, by = 100)){
  newRate = (35000*1796)+((i)*4056)+(35000*(1-rate)*945)+((i)*945)
  rate_value = c(rate_value, newRate)
  rates = c(rates, i)
  totalcost = c(totalcost, totalCostNoHelp)
  newRate
}
rate_value
# rates
ggplot(data.frame(rates, rate_value), aes(x = rates, y = rate_value)) +
  ggtitle("Cost of intervention vs. Total Cost") + xlab("Cost of Intervention") + ylab("Total Cost") + geom_line()+
  geom_line(aes(x = rates, y = totalcost), col="red")

#Below is the formula seen above. This gives you the value at a certain rate.
(35000*1796)+(1200*4056)+(36200*(1-rate)*945)+(1200*(rate)*945)

# ----------------------------------------------------------------------------------------
## BELOW IS TESTING I DID FOR E --------------------------------------------------------
# ----------------------------------------------------------------------------------------


totalCostNoHelp = 35000*2741
#Below is how you get the rate in which you break even. 
(36200-((35000*2741)-(35000*1796)-(1200*4056))/945)/35000

#This is the rate at which you break even. 
rate = 0.18144
rate_value = c()
totalcost = c()
rates = c()
i = 0

for( i in seq(0, 1, by = 0.02)){
  newRate = (35000*1796)+(1200*4056)+(36200*(1-i)*945)+(1200*(i)*945)
  rate_value = c(rate_value, newRate)
  rates = c(rates, i)
  totalcost = c(totalcost, totalCostNoHelp)
  newRate
}
# rate_value
# rates
ggplot(data.frame(rates, rate_value), aes(x = rates, y = rate_value)) +
  ggtitle("Rate vs. Cost") + xlab("Rate") + ylab("Cost") + geom_line()+
  geom_line(aes(x = rates, y = totalcost), col="red")

#Below is the formula seen above. This gives you the value at a certain rate.
(35000*1796)+(1200*4056)+(36200*(1-rate)*945)+(1200*(rate)*945)

# ----------------------------------------------------------------------------------------
# END OF TESTING
# ----------------------------------------------------------------------------------------

### Question f.

##### Problem 2: Housing Prices in Ames, Iowa, Revisited
### This problem should be relatively straightforward, and serves mostly as a review.
ames = read.csv("ames.csv")
set.seed(15071)
split = createDataPartition(ames$SalePrice, p = 0.7, list = FALSE) 
ames.train = ames[split,]
ames.test = ames[-split,]
head(ames.train)

ames.mm.allData <- as.data.frame(model.matrix(SalePrice~., data = ames))  
ames.test.mm = ames.mm.allData[-split,]
ames.train.mm = ames.mm.allData[split,]

SSTTrain = sum((ames.train$SalePrice - mean(ames.train$SalePrice))^2)
SSTTest = sum((ames.test$SalePrice - mean(ames.train$SalePrice))^2)

### Question a.
regressor = lm(SalePrice ~ ., data = ames.train)
summary(regressor)
plot(regressor)

reg.test.pred = predict(regressor, newdata = ames.test)
reg.train.pred = predict(regressor, newdata = ames.train)


reg.SSETrain = sum((reg.train.pred - ames.train$SalePrice)^2)
reg.R2 <- 1 - reg.SSETrain/SSTTrain

reg.SSETest = sum((reg.test.pred - ames.test$SalePrice)^2)
reg.OSR2 <- 1 - reg.SSETest/SSTTest

### Question b.

#Loss <- function(data, lev = NULL, model = NULL, ...) {
#  c(AvgLoss = mean(data$weights * (data$obs != data$pred)), Accuracy = mean(data$obs == data$pred))
#}

cv.trees.cart = train(SalePrice ~ ., method = "rpart", data = ames.train,
                 trControl = trainControl(method = "cv", number = 10), 
                 tuneGrid = data.frame(.cp = seq(.00002,.002,.00002)))
pdf('CART.pdf',12,12)
prp(cv.trees.cart$finalModel,varlen=0,faclen=0,digits=3) 
dev.off()
cv.trees.cart
# printcp(cv.trees)

# par(mar = c(4,4,1,1))
plot(cv.trees.cart$results$cp, cv.trees.cart$results$Rsquared, type = "l", ylab = "RSQ", xlab = "cp") # line plot

best.tree.cart <- cv.trees.cart$finalModel
cv.trees.cart$results$Rsquared
# prp(best.tree.cart,digit=3,tweak=1.2)
# ames.train.mm = as.data.frame(model.matrix(SalePrice ~ ., data = ames.train))
cart.test.pred = predict(best.tree.cart, newdata = ames.test.mm)
cart.train.pred = predict(best.tree.cart, newdata = ames.train.mm)
#The line below is a sanity check to make sure the above yeilds the same results. 
# cart.train.pred.initial = predict(best.tree.cart, newdata = ames.train.mm.initial)

cart.SSETrain = sum((cart.train.pred - ames.train$SalePrice)^2)
R2_CART_treeFinal <- 1 - cart.SSETrain/SSTTrain

cart.SSETest = sum((cart.test.pred - ames.test$SalePrice)^2)
OSR2_CART_treeFinal <- 1 - cart.SSETest/SSTTest


### Question c.

rf.mtry = train(SalePrice~., data = ames.train, method="rf", ntree=80, nodesize=25,
              trControl=trainControl(method="cv", number=5),
              tuneGrid=data.frame(mtry=seq(2,30,2)))
#best.rf = rf.cv$finalModel
#rf.cv
plot(rf.mtry$results$mtry, rf.cv$results$Rsquared, type = "l", ylab = "RSQ", xlab = "MTRY") # line plot
best.rf = rf.mtry$finalModel
importance(best.rf)

rf.mod.mtry = rf.mtry$finalModel
rf.mod.mtry
# Finally, we can make predictions
rf.pred.train = predict(rf.mod.mtry, newdata=ames.train.mm)
rf.pred.test = predict(rf.mod.mtry, newdata=ames.test.mm)

rf.SSETrain = sum((rf.pred.train - ames.train$SalePrice)^2)
R2_RF_treeFinal <- 1 - rf.SSETrain/SSTTrain

rf.SSETest = sum((rf.pred.test - ames.test$SalePrice)^2)
OSR2_RF_treeFinal <- 1 - rf.SSETest/SSTTest

### Question d.

lin.reg.R2.train <- reg.R2
lin.reg.MAE.train <- MAE(reg.train.pred, ames.train$SalePrice)
lin.reg.RMSE.train <- RMSE(reg.train.pred, ames.train$SalePrice)

lin.reg.R2.test <- reg.OSR2
lin.reg.MAE.test <- MAE(reg.test.pred, ames.test$SalePrice)
lin.reg.RMSE.test <- RMSE(reg.test.pred, ames.test$SalePrice)

CART.R2.train <- R2_CART_treeFinal
CART.MAE.train <- MAE(cart.train.pred, ames.train$SalePrice)
CART.RMSE.train <- RMSE(cart.train.pred, ames.train$SalePrice)

CART.R2.test <- OSR2_CART_treeFinal
CART.MAE.test <- MAE(cart.test.pred, ames.test$SalePrice)
CART.RMSE.test <- RMSE(cart.test.pred, ames.test$SalePrice)

RF.R2.train <- R2_RF_treeFinal
RF.MAE.train <- MAE(rf.pred.train, ames.train$SalePrice)
RF.RMSE.train <- RMSE(rf.pred.train, ames.train$SalePrice)

RF.R2.test <- OSR2_RF_treeFinal
RF.MAE.test <- MAE(rf.pred.test, ames.test$SalePrice)
RF.RMSE.test <- RMSE(rf.pred.test, ames.test$SalePrice)

# Summary
summary_statistics <- data.frame(
  IS.R2 = c(lin.reg.R2.train,CART.R2.train,RF.R2.train),
  IS.MAE = c(lin.reg.MAE.train,CART.MAE.train,RF.MAE.train),
  IS.RMSE = c(lin.reg.RMSE.train,CART.RMSE.train,RF.RMSE.train),
  OOS.R2 = c(lin.reg.R2.test,CART.R2.test,RF.R2.test),
  OOS.MAE = c(lin.reg.MAE.test,CART.MAE.test,RF.MAE.test),
  OOS.RMSE = c(lin.reg.RMSE.test,CART.RMSE.test,RF.RMSE.test)
)

### Question e.

### Question f.

# ----------------------------------------------------------------------------------------
# END OF Problem 2
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
##### Problem 3: eBay.com
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


# ----------------------------------------------------------------------------------------
# END OF Problem 3
# ----------------------------------------------------------------------------------------