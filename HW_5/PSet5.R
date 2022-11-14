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
full_returns = data[,3:122]

agg_data  = aggregate(. ~ Industry, returns, mean)

agg_data %>% 
  gather(var, val , -Industry) %>% 
  ggplot(aes(x = var, y = val, color = as.factor(Industry), group = as.factor(Industry)))+
  geom_point() + scale_x_discrete(guide = guide_axis(n.dodge = 4))

ggsave("Industry_returns.pdf")

# Generate an aggregate for the each industry for Sept 2008.
aggregate(data$avg200809 ~ data$Industry, data=data, mean)

# ----------------------------------------------------------------------------------------
# Part (b) - Hierarchical clustering. "Ward D2"
# The following code is referenced to the Rec7.R file provided in Recitation 7.
# ----------------------------------------------------------------------------------------

d <- dist(full_returns, method="euclidean")    # method = "euclidean"
class(d)

# Creates the Hierarchical clustering
hclust.mod <- hclust(d, method="ward.D2")

# Plot the hierarchy structure (dendrogram)
pdf('dendrogram.pdf',8,5)
plot(hclust.mod, labels=F, ylab="Dissimilarity", xlab = "", sub = "")
dev.off()

# To choose a good value for k, we need to create the scree plot: dissimilarity for each k
# the next line puts this data in the right form to b?mixtoolse plotted
hc.dissim <- data.frame(k = seq_along(hclust.mod$height),   # index: 1,2,...,length(hclust.mod$height)
                        dissimilarity = rev(hclust.mod$height)) # reverse elements
(hc.dissim)

# Scree plot
pdf('scree2.pdf',8,5)
plot(hc.dissim$k, hc.dissim$dissimilarity, type="l")
dev.off()
# Let's zoom on the smallest k values:
pdf('scree2_zoom.pdf',8,5)
plot(hc.dissim$k, hc.dissim$dissimilarity, type="l", xlim=c(0,40))
axis(side = 1, at = 1:10)
dev.off()

# ----------------------------------------------------------------------------------------
# Part c.
# ----------------------------------------------------------------------------------------

# Improvement in dissimilarity for increasing number of clusters
hc.dissim.dif = head(hc.dissim,-1)-tail(hc.dissim,-1)  # all but the last of hc.dissim - all but the first, basically a shifted difference
head(hc.dissim.dif,10)

# now that we have k (we chose k=7 in the lecture), we can construct the clusters
h.clusters <- cutree(hclust.mod, 8)
h.clusters

# The *centroid* for a cluster is the mean value of all points in the cluster: 
aggregate(full_returns, by=list(h.clusters), mean) # Compute centroids

# Number of companies in each cluster
table(h.clusters)

# Number of companies per industry in each cluster
table(data$Industry, h.clusters)

# Average returns by cluster for Oct 2008
aggregate(full_returns$avg200810, by=list(h.clusters), mean) 
# Average returns by cluster for March 2009
aggregate(full_returns$avg200903, by=list(h.clusters), mean) 

# ----------------------------------------------------------------------------------------
# Part d.
# ----------------------------------------------------------------------------------------

#Start here. 
set.seed(2407)

# The kmeans function creates the clusters
# we can set an upper bound to the number of iterations
# of the algorithm 
# here we set k=7
km_returns <- kmeans(full_returns, centers = 8, iter.max=100) # centers randomly selected from rows of airline.scaled

class(km_returns) # class: kmeans
names(km_returns) # Take a look at what's inside this object.
# Let's explore the results!
# cluster centroids. Store this result
km_returns.centroids <- km_returns$centers
km_returns.centroids
# cluster for each point. Store the assignment of each point to its corresponding cluster.
km_returns.clusters <- km_returns$cluster
km_returns.clusters
# the sum of the squared distances of each observation from its cluster centroid.
# we use it the measure cluster dissimilarity
km_returns$tot.withinss  # cluster dissimilarity
# the number of observations in each cluster -- table(km$cluster) also works. Store this resul
km_returns.size <- km_returns$size
km_returns.size

# Average returns by cluster for Oct 2008
aggregate(full_returns$avg200810, by=list(km_returns.clusters), mean) 
# Average returns by cluster for March 2009
aggregate(full_returns$avg200903, by=list(km_returns.clusters), mean) 

# Scree plot for k-means
# For k means, we literally try many value of k and look at their dissimilarity.
# here we test all k from 1 to 100
k_ret.data <- data.frame(k_ret = 1:100)
k_ret.data$SS <- sapply(k_ret.data$k_ret, function(k_ret) {
  kmeans(full_returns, iter.max=100, k_ret)$tot.withinss
})

# Plot the scree plot.
pdf('kmeans.pdf',8,5)
plot(k_ret.data$k_ret, k_ret.data$SS, type="l")
dev.off()

# Compare the clusters from kmeans and Hierarchical. Do some clusters "match up"? (Yes, because there are alot of zeros.)
table(h.clusters, km_returns.clusters)

# ----------------------------------------------------------------------------------------
# Part e.
# ----------------------------------------------------------------------------------------
# Clustering
library(cluster) 

?silhouette
k_means_ss = silhouette(km_returns.clusters,dist(full_returns))
mean(k_means_ss[, 3])
aggregate(k_means_ss, by=list(km_returns.clusters), mean) 

hier_ss = silhouette(h.clusters,dist(full_returns))
aggregate(hier_ss, by=list(h.clusters), mean) 
mean(hier_ss[, 3])

plot(k_means_ss, col=1:8, border=NA)
plot(hier_ss, col=1:8, border=NA)

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

# validation
pred_test <- predict(adaboost, newdata=ebay.test, type="class")
pred_test
pred_test$confusion
1-pred_test$error

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
                      subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.))), 
                      label = ebay.train$Competitive., max.depth = 20, eta = 1, nthread = 2, nrounds = 50, 
                      objective = "binary:logistic", verbose = 0)
# variable importance
xgb.importance(colnames(model.matrix(Competitive.~ sellerRating+currency.factor+duration.factor+
                      endDay.factor+category.factor+ClosePrice, subset(ebay.train, 
                      select=-c(competitive.factor, ebay.train$Competitive.)))), model = xgb)
# training
confusionMatrix(factor(1*(predict(xgb, model.matrix(Competitive.~sellerRating+currency.factor+ 
                      duration.factor+endDay.factor+category.factor+ClosePrice, 
                      subset(ebay.train, select=-c(competitive.factor, ebay.train$Competitive.)))) > 
                      sum(ebay.df$Competitive.==1)/nrow(ebay.df))), ebay.train$competitive.factor)
# validation
confusionMatrix(factor(1*(predict(xgb, model.matrix(Competitive.~sellerRating+currency.factor+
                      duration.factor+endDay.factor+category.factor+ClosePrice, subset(ebay.test, 
                      select=-c(competitive.factor, ebay.train$Competitive.)))) > 
                      sum(ebay.df$Competitive.==1)/nrow(ebay.df))), ebay.test$competitive.factor)

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