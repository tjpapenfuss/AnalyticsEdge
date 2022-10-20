library(caret)
##### Problem 1: Preventing Hospital Readmissions

### Question a.

### Question b.

### Question c.
# Fitting the CART model
tree <- rpart(, parms=list(loss=loss.matrix), method="class", cp=0.001)

# Plotting the CART tree
pdf('readmission_tree.pdf',12,6)
prp(tree, varlen=0,faclen=0,digits=3)
dev.off()

### Question d.
pred <- predict(, newdata=readm.test, type="class")
confusionMatrix()

### Question e.

### Question f.

##### Problem 2: Housing Prices in Ames, Iowa, Revisited
### This problem should be realtively straightforward, and serves mostly as a review.

### Question a.

### Question b.

cv.trees = train(y = , x = , method = "rpart", 
                 trControl = trainControl(method = "cv", number = 10), 
                 tuneGrid = data.frame(.cp = seq(.00002,.002,.00002)))
pdf('CART.pdf',12,12)
prp(cv.trees$finalModel,varlen=0,faclen=0,digits=3) 
dev.off()

best.tree <- cv.trees$finalModel

cart.train.pred = predict(best.tree, newdata = )
cart.R2 = ...

### Question c.

rf.cv = train(y = , x = , method="rf", ntree=80, nodesize=25,
              trControl=trainControl(method="cv", number=5),
              tuneGrid=data.frame(mtry=seq(2,30,2)))
best.rf = rf.cv$finalModel

importance(best.rf)

### Question d.

lin.reg.R2.train <-
lin.reg.MAE.train <-
lin.reg.RMSE.train <-

lin.reg.R2.test <-
lin.reg.MAE.test <-
lin.reg.RMSE.test <-

CART.R2.train <-
CART.MAE.train <-
CART.RMSE.train <-

CART.R2.test <-
CART.MAE.test <-
CART.RMSE.test <-

RF.R2.train <-
RF.MAE.train <-
RF.RMSE.train <-

RF.R2.test <-
RF.MAE.test <-
RF.RMSE.test <-

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

##### Problem 3: eBay.com
library(gdata)
ebay.df <- read.xls("eBayAuctions.xls")
table(ebay.df$Competitive.)

ebay.df$competitive.factor <- as.factor(ebay.df$Competitive.)

### (a)
## adaboost
library(adabag)
adaboost <- boosting(, data = )
# variable importance
importanceplot(adaboost, cex.names=0.7)
# training
confusionMatrix(, )
# validation
confusionMatrix(, )

## xgboost
library(xgboost)
xgb <- xgboost(data = model.matrix(~ .-1, subset(train.df, select=-c(competitive.factor, Competitive.))), label = train.df$Competitive., max.depth = 20, eta = 1, nthread = 2, nrounds = 50, objective = "binary:logistic", verbose = 0)
# variable importance
xgb.importance(colnames(model.matrix(~ .-1, subset(train.df, select=-c(competitive.factor, Competitive.)))), model = xgb)
# training
confusionMatrix(factor(1*(predict(xgb, model.matrix(~ .-1, subset(train.df, select=-c(competitive.factor, Competitive.))))>sum(ebay.df$Competitive.==1)/nrow(ebay.df))), train.df$competitive.factor)
# validation
confusionMatrix(factor(1*(predict(xgb, model.matrix(~ .-1, subset(valid.df, select=-c(competitive.factor, Competitive.))))>sum(ebay.df$Competitive.==1)/nrow(ebay.df))), valid.df$competitive.factor)

## bagging
bag <- bagging(, data = )
# variable importance
importanceplot(bag, cex.names=0.7)
# training
confusionMatrix(, )
# validation
confusionMatrix(, )

## random forest
library(randomForest)
rf <- randomForest(, data, method = "class")
# variable importance
varImp(rf, conditional=TRUE)
# training
confusionMatrix(, )
# validation
confusionMatrix(, )

### (b)

### (c)

### (d)

### (e)
