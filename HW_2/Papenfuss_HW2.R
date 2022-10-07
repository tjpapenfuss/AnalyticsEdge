
library(tidyverse)
library(ggrepel)
library(caTools)
library(caret)
library(ROCR)
library(ggplot2)
library(splines)
library(ISLR)
library(caret)
library(leaps)
library(gridExtra)
library(dplyr)
library(tidyr)
library(lars)
library(gbm)
library(glmnet)
library(ggcorrplot)
library(Hmisc)


# --------------------------------------------------------------------------------- #
#### Problem 1
# --------------------------------------------------------------------------------- #
# Import the dataset
###### TRY AND CONVERT nonretail to a factor!!!!!!!

boston = read.csv('boston.csv')
boston$nonretail = as_factor(boston$nonretail)
# Split the data set into training and testing data sets
set.seed(123)
split = createDataPartition(boston$nox, p = 0.7, list = FALSE) 
boston.train = boston[split,]
boston.test = boston[-split,]

head(boston.test)
str(boston.train)
# --------------------------------------------------------------------------------- #
#### Problem 1a
# Run the linear regression. Nox is the dependent var. DIS is the independent
regressor = lm(formula = nox ~ dis, data = boston.train)
summary(regressor)

#Run the prediction on the test data set
prediction_Test_Set = predict(regressor, newdata = boston.test)

#Obtain the R^2 value of the test data set. 
SSR_of_model <- sum((prediction_Test_Set-boston.test$nox)^2)
SSR_of_baseline <- sum((mean(boston.train$nox)-boston.test$nox)^2)
OSR2 <- 1-(SSR_of_model/SSR_of_baseline )
OSR2
# plot a scatter plot
plot(boston.train$dis,boston.train$nox,
     main='Regression for NOX and DIS',
     xlab='DIS',ylab='NOX')
# plot a regression line
abline(regressor,col='red')


# --------------------------------------------------------------------------------- #
#### Problem 1b
# Run the linear regression. Nox is the dependent var. DIS is the independent
regressor_poly = lm(formula = nox ~ poly(dis, degree=3), data = boston.train)
summary(regressor_poly)

#Run the prediction on the test data set
prediction_Test_Set_poly = predict(regressor_poly, newdata = boston.test)

#Obtain the R^2 value of the test data set. 
SSR_of_model <- sum((prediction_Test_Set_poly-boston.test$nox)^2)
SSR_of_baseline <- sum((mean(boston.train$nox)-boston.test$nox)^2)
OSR2 <- 1-(SSR_of_model/SSR_of_baseline )
OSR2
# plot a scatter plot
ggplot(data=boston.train,aes(x=dis, y=nox)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, degree=3),se = FALSE) +
  theme_bw() +
  xlab('DIS') +
  ylab("NOX") +
  ggtitle("Polynomial regression of degree 3") +
  theme(axis.title=element_text(size=12), 
       axis.text=element_text(size=12), 
       legend.text=element_text(size=12),
       plot.title = element_text(hjust = 0.5))


# --------------------------------------------------------------------------------- #
#### Question 4c


## Run regressor with the nonretail factor. This generates really good R^2
regressor_splines = lm(formula = nox ~ poly(dis,df=3)*(nonretail), data = boston.train)
summary(regressor_splines)

#Run the prediction on the test data set
prediction_Test_Set_spline = predict(regressor_splines, newdata = boston.test)

#Obtain the R^2 value of the test data set. 
SSR_of_model <- sum((prediction_Test_Set_spline-boston.test$nox)^2)
SSR_of_baseline <- sum((mean(boston.train$nox)-boston.test$nox)^2)
OSR2 <- 1-(SSR_of_model/SSR_of_baseline )
OSR2

#Generate a subset with nonretail = 1
boston.train_1 = subset(boston.train, boston.train$nonretail == 1)
boston.train_1
#Generate a subset with nonretail = 0
boston.train_0 = subset(boston.train, boston.train$nonretail == 0)
boston.train_0

#Create two new regressors. These are for nonretail =0 & 1. These regressors will be used
#below in the ggplot
# regressor_splines_1 = lm(formula = nox ~ poly(dis,df=3), data = boston.train_1)
# regressor_splines_0 = lm(formula = nox ~ poly(dis,df=3), data = boston.train_0)
# 
# #First plot the old regressor, then plot the two new regressors. 
# ggplot(data=boston.train,aes(x=dis, y=nox, color=nonretail)) +
#   geom_point() +
#   stat_smooth(method = "lm", formula = y ~ poly(x, degree=3),se = FALSE, 
#               color = "red") +
#   geom_smooth(data = boston.train_0,aes(x=dis, y=nox), 
#               method = "lm", formula = y ~ poly(x, degree=3), se=FALSE) +
#   geom_smooth(data = boston.train_1,aes(x=dis, y=nox), 
#               method = "lm", formula = y ~ poly(x, degree=3), se=FALSE, color = "blue") +
#   theme_bw() +
#   xlab('DIS') +
#   ylab("NOX") +
#   ggtitle("Polynomial regression of degree 3") +
#   theme(axis.title=element_text(size=12), 
#         axis.text=element_text(size=12), 
#         legend.text=element_text(size=12),
#         plot.title = element_text(hjust = 0.5))

ggplot(data=boston.train,aes(x=dis, y=nox, color=nonretail)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, degree=3),se = FALSE, 
              color = "black") +
  geom_smooth(method = "lm", formula = y ~ poly(x,df=3), se=FALSE) +
  theme_bw() +
  xlab('DIS') +
  ylab("NOX") +
  ggtitle("Polynomial regression of degree 3") +
  theme(axis.title=element_text(size=12), 
        axis.text=element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5))

# --------------------------------------------------------------------------------- #
#### Question 4d


knots_bos <- quantile(boston.train$dis, p = c(0.2, 0.4, 0.6, 0.8))

regressor_splines_2 = lm(formula = nox ~ bs(dis,knots=c(knots_bos[1], 
                       knots_bos[2], knots_bos[3], knots_bos[4]))*nonretail,
                       data = boston.train)
summary(regressor_splines_2)

ggplot(data=boston.train,aes(x=dis, y=nox, color=nonretail)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ bs(x,knots=c(knots_bos[1], 
              knots_bos[2], knots_bos[3], knots_bos[4])),se = FALSE) +
  theme_bw() +
  xlab('DIS') +
  ylab("NOX") +
  ggtitle("Spline graph") +
  theme(axis.title=element_text(size=12), 
        axis.text=element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5))

#Run the prediction on the test data set
prediction_Test_Set_spline_2 = predict(regressor_splines_2, newdata = boston.test)

#Obtain the R^2 value of the test data set. 
SSR_of_model <- sum((prediction_Test_Set_spline_2-boston.test$nox)^2)
SSR_of_baseline <- sum((mean(boston.train$nox)-boston.test$nox)^2)
OSR2 <- 1-(SSR_of_model/SSR_of_baseline )
OSR2



# # ------ The below code for exercise 1d will plot the splines on the data set.
# #Generate a subset with nonretail = 1
# boston.train_1 = subset(boston.train, boston.train$nonretail == 1)
# boston.train_1
# #Generate a subset with nonretail = 0
# boston.train_0 = subset(boston.train, boston.train$nonretail == 0)
# boston.train_0
# 
# #Create two new regressors. These are for nonretail =0 & 1. These regressors will be used
# #below in the ggplot
# regressor_splines_1 = lm(formula = nox ~ bs(dis,knots=c(knots_bos[1],
#                          knots_bos[2], knots_bos[3], knots_bos[4])),
#                          data = boston.train_1)
# regressor_splines_0 = lm(formula = nox ~ bs(dis,knots=c(knots_bos[1],
#                          knots_bos[2], knots_bos[3], knots_bos[4])),
#                          data = boston.train_0)
# 
# ggplot(data=boston.train,aes(x=dis, y=nox, color=nonretail)) +
#   geom_point() +
#   geom_smooth(data = boston.train_0,aes(x=dis, y=nox),
#               method = "lm", formula = y ~ bs(x,knots=c(knots_bos[1],
#               knots_bos[2], knots_bos[3], knots_bos[4])), se=FALSE) +
#   geom_smooth(data = boston.train_1,aes(x=dis, y=nox),
#               method = "lm", formula = y ~ bs(x,knots=c(knots_bos[1],
#               knots_bos[2], knots_bos[3], knots_bos[4])), se=FALSE, color = "blue") +
#   theme_bw() +
#   xlab('DIS') +
#   ylab("NOX") +
#   ggtitle("Spline graph Question 1 part d") +
#   theme(axis.title=element_text(size=12),
#         axis.text=element_text(size=12),
#         legend.text=element_text(size=12),
#         plot.title = element_text(hjust = 0.5))

# --------------------------------------------------------------------------------- #
#### Problem 2

kc_raw = read.csv("kc_house.csv")
str(kc_raw)
kc_raw$waterfront = as_factor(kc_raw$waterfront)
kc_raw$grade = as_factor(kc_raw$grade)



# --------------------------------------------------------------------------------- #
#### Problem 2a

# Find the correlation
corMatrix = cor(kc_raw)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res2<-rcorr(as.matrix(kc_raw))
matrix = flattenCorrMatrix(res2$r, res2$P)

# Plot based on variables I think are important 
# Variables: sqft_living, grade, and waterfront
regressor = lm(formula = price ~., data = kc_raw)
summary(regressor)

ggplot(data=kc_raw,aes(x=sqft_living, y=price)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  xlab('sqft') +
  ylab("price") +
  ggtitle("price vs. sqft") +
  theme(axis.title=element_text(size=12), 
        axis.text=element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5))

library(scales)

ggplot(data=kc_raw,aes(x=grade, y=price, color=grade)) +
  geom_boxplot() +
  #geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  xlab('grade') +
  ylab("price") +
  ggtitle("price vs. grade") +
  scale_y_continuous(labels = dollar)

ggplot(data=kc_raw,aes(x=waterfront, y=price, color = waterfront)) +
  geom_boxplot() +
  #geom_smooth(method = lm, se = FALSE) +
  xlab('waterfront') +
  ylab("price") +
  ggtitle("price vs. waterfront") +
  scale_y_continuous(labels = dollar)


# --------------------------------------------------------------------------------- #
#### Problem 2b

pp <- preProcess(kc_raw, method=c("center", "scale"))
kc <- predict(pp, kc_raw)
set.seed(123)
train.obs <- sort(sample(seq_len(nrow(kc)), 0.7*nrow(kc))) 
train <- kc[train.obs,]
test <- kc[-train.obs,]

regressor = lm(formula = price ~ sqft_living + waterfront + view +
                 grade + yr_built + lat + long, data = train)
summary(regressor)

#Run the prediction on the test data set
prediction = predict(regressor, newdata = test)

#Obtain the R^2 value of the test data set. 
SSR_of_model <- sum((prediction-test$price)^2)
SSR_of_baseline <- sum((mean(train$price)-test$price)^2)
OSR2 <- 1-(SSR_of_model/SSR_of_baseline )
OSR2

# --------------------------------------------------------------------------------- #
#### Problem 2c

### Ridge regression

# Preparation of the train and test matrices
x.train=model.matrix(price~.,data=train)
y.train=train$price
x.test=model.matrix(price~.,data=test) 
y.test=test$price

# TA work
# Alpha is the control for whether you use ridge or LASSO
# No need to worry about the lamda parameter
#

# Run Ridge Regression on the train Set
# The first is to find the right Lambda. 
#lambdas.ridge <- c(exp(seq(15, -10, -.1)))
# N.lambda.ridge <- length(lambdas.ridge)
ridge.tr=cv.glmnet(x.train,y.train,alpha=0, nfolds=5)
plot(ridge.tr)

best.ridge.lambda = ridge.tr$lambda.min

# ridge.new=glmnet(x=x.full,y=y.full,alpha=0,lambda = min_lambda)
# predict(ridge.new,type="coef")
# summary(ridge.new)

# THIS IS WHAT I NEED TO DO NEXT
# In sample and out of sample performance
ridge.final <- glmnet(x.train,y.train,alpha=0,lambda=best.ridge.lambda)
pred.train.final <- predict(ridge.final,x.train)
R2.ridge.final <- 1-sum((pred.train.final-train$price)^2)/sum((mean(train$price)-train$price)^2)
pred.test.final <- predict(ridge.final,x.test)
OSR2.ridge.final <- 1-sum((pred.test.final-test$price)^2)/sum((mean(train$price)-test$price)^2)
#R2 = 0.688; OSR2 =  0.692
coef(ridge.final)

# ------ LASSO ----------------------------------- 
### LASSO

lasso.tr=cv.glmnet(x.train,y.train,alpha=1, nfolds=5)
plot(lasso.tr)

best.lasso.lambda = lasso.tr$lambda.min

# In sample and out of sample performance
lasso.final <- glmnet(x.train,y.train,alpha=1,lambda=best.lasso.lambda)
pred.train.final <- predict(lasso.final,x.train)
R2.lasso.final <- 1-sum((pred.train.final-train$price)^2)/sum((mean(train$price)-train$price)^2)
pred.test.final <- predict(lasso.final,x.test)
OSR2.lasso.final <- 1-sum((pred.test.final-test$price)^2)/sum((mean(train$price)-test$price)^2)
#R2 = 0.688; OSR2 =  0.692
coef(lasso.final)



# END  OF CODE. The code below is all testing. DO NOT USE!!
# --------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------- #





# Derive ridge path
# ridge_coef <- c()

# k <- ncol(kc_raw)-1
# k = 5
# for (j in 1:k){
#   ridge_coef <- c(ridge_coef,ridge.tr$beta[j,])
# }
# ridge <- data.frame(lambda=rep(ridge.tr$lambda,k),coefficients=ridge_coef,var=as.factor(sort(rep(1:k,length(ridge.tr$lambda)))))

# Plot ridge path
#pdf('ridge_path.pdf',12,6)
# ggplot(ridge %>% filter(log(lambda)>=-5) %>% filter(log(lambda)<=10),aes(x=log(lambda),y=coefficients,group=var,color=var)) +
#   geom_line() + 
#   theme_bw() +
#   ylim(-0.2,0.5)+
#   xlab(expression(paste("log(",lambda,")"))) +
#   ylab("Coefficients") +
#   theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.position='none')
# #dev.off()

# Prediction on the train and test sets
ridge.pred.test=predict(ridge.tr,x.test)
ridge.pred.train=predict(ridge.tr,x.train)

# Performance assessment on the train and test sets
N.lambda <- ncol(ridge.pred.test)
ridge.R2.train <- c()
ridge.R2.test <- c()
for (i in 1:N.lambda){
  ridge.R2.train[i] <- 1-sum((ridge.pred.train[,i]-train$price)^2)/sum((mean(train$price)-train$price)^2)
  ridge.R2.test[i] <- 1-sum((ridge.pred.test[,i]-test$price)^2)/sum((mean(train$price)-test$price)^2)
}

# Plot of in-sample and out-of-sample performance assessment
R2.ridge <- data.frame(lambda=ridge.tr$lambda,R2=ridge.R2.train,OSR2=ridge.R2.test)
# setwd('./../plots')
# pdf('ridge_R2.pdf',8,6)
ggplot() +
  geom_line(data=R2.ridge %>% filter(log(lambda)>=-5) %>% filter(log(lambda)<=10),aes(x=log(lambda),y=R2,colour="1"),lwd=2) + 
  geom_line(data=R2.ridge %>% filter(log(lambda)>=-5) %>% filter(log(lambda)<=10),aes(x=log(lambda),y=OSR2,colour="2"),lwd=2) + 
  theme_bw() +
  ylim(0,1)+
  xlab(expression(paste("log(",lambda,")"))) +
  ylab("R2") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.text=element_text(size=18)) +
  scale_color_discrete(name='',labels=c('Train','Test'))
#dev.off()

# Best lambda
best.ridge.lambda <- R2.ridge$lambda[which.max(R2.ridge$OSR2)]

# In sample and out of sample performance
ridge.final <- glmnet(x.train,y.train,alpha=0,lambda=best.ridge.lambda)
pred.train.final <- predict(ridge.final,x.train)
R2.ridge.final <- 1-sum((pred.train.final-train$price)^2)/sum((mean(train$price)-train$price)^2)
pred.test.final <- predict(ridge.final,x.test)
OSR2.ridge.final <- 1-sum((pred.test.final-test$price)^2)/sum((mean(train$price)-test$price)^2)

# ------ LASSO ----------------------------------- 
### LASSO

# Run LASSO on the train Set
lambdas.lasso <- c(exp(seq(10, -4, -.1)),0)
N.lambda.lasso <- length(lambdas.lasso)
lasso.tr=glmnet(x.train,y.train,alpha=1,lambda=lambdas.lasso)

# Obtain LASSO path
lasso_coef <- c()
for (j in 1:k){
  lasso_coef <- c(lasso_coef,lasso.tr$beta[j,])
}
lasso <- data.frame(lambda=rep(lasso.tr$lambda,k),coefficients=lasso_coef,var=as.factor(sort(rep(1:k,length(lasso.tr$lambda)))))

# Plot LASSO path
#setwd('./../plots')
#pdf('lasso_path.pdf',12,6)
ggplot(lasso %>% filter(log(lambda)>=-2) %>% filter(log(lambda)<=6),aes(x=log(lambda),y=coefficients,group=var,color=var)) +
  geom_line() + 
  theme_bw() +
  ylim(-1,1)+
  xlab(expression(paste("log(",lambda,")"))) +
  ylab("Coefficients") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.position='none')
#dev.off()

# Prediction on the train and test sets
lasso.pred.test=predict(lasso.tr,x.test)
lasso.pred.train=predict(lasso.tr,x.train)

# Performance assessment on the train and test sets
N.lambda <- ncol(lasso.pred.test)
lasso.R2.train <- c()
lasso.R2.test <- c()
for (i in 1:N.lambda){
  lasso.R2.train[i] <- 1-sum((lasso.pred.train[,i]-train$price)^2)/sum((mean(train$price)-train$price)^2)
  lasso.R2.test[i] <- 1-sum((lasso.pred.test[,i]-test$price)^2)/sum((mean(train$price)-test$price)^2)
}

# Plot of in-sample and out-of-sample performance assessment
R2.lasso <- data.frame(lambda=lasso.tr$lambda,R2=lasso.R2.train,OSR2=lasso.R2.test)
#setwd('./../plots')
#pdf('lasso_R2.pdf',8,6)
ggplot() +
  geom_line(data=R2.lasso %>% filter(log(lambda)>=-2) %>% filter(log(lambda)<=6),aes(x=log(lambda),y=R2,colour="1"),lwd=2) + 
  geom_line(data=R2.lasso %>% filter(log(lambda)>=-2) %>% filter(log(lambda)<=6),aes(x=log(lambda),y=OSR2,colour="2"),lwd=2) + 
  theme_bw() +
  ylim(0,1)+
  xlab(expression(paste("log(",lambda,")"))) +
  ylab("R2") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.text=element_text(size=18)) +
  scale_color_discrete(name='',labels=c('Train','Test'))
dev.off()

# Best lambda
best.lasso.lambda <- R2.lasso$lambda[which.max(R2.lasso$OSR2)]

# In sample and out of sample performance
lasso.final <- glmnet(x.train,y.train,alpha=1,lambda=best.lasso.lambda)
pred.train.final <- predict(lasso.final,x.train)
R2.lasso.final <- 1-sum((pred.train.final-train$price)^2)/sum((mean(train$price)-train$price)^2)
pred.test.final <- predict(lasso.final,x.test)
OSR2.lasso.final <- 1-sum((pred.test.final-test$price)^2)/sum((mean(train$price)-test$price)^2)

### Summary of all methods

summary <- data.frame(
  R2=c(R2.full,R2.restricted,R2.forward.subset.final,R2.backward.subset.final,R2.ridge.final,R2.lasso.final),
  OSR2=c(OSR2.full,OSR2.restricted,OSR2.forward.subset.final,OSR2.backward.subset.final,OSR2.ridge.final,OSR2.lasso.final)
)
 # ------------------ End of LASSO ----------------------------------- 






##TESTING Below this line is the dark abyss. Do not venture. 


knots_bos <- quantile(boston.train$dis, p = c(0.2, 0.4, 0.6, 0.8))


## Run regressor with the nonretail factor. This generates really good R^2
regressor_splines = lm(formula = nox ~ bs(dis,knots=c(2.5, 5.0, 7.5, 10.0))*factor(nonretail),
                       data = boston.train)
summary(regressor_splines)

ggplot(data=boston.train,aes(x=dis, y=nox)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ bs(x,knots=c(2.5, 5.0, 7.5, 10.0)),se = FALSE) +
  theme_bw() +
  xlab('DIS') +
  ylab("NOX") +
  ggtitle("Polynomial regression of degree 3") +
  theme(axis.title=element_text(size=12), 
        axis.text=element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5))



models = boston.train %>% 
  split(boston.train$nonretail) %>% 
  map(~lm(formula = nox ~ poly(dis,df=3), data = boston.train))
models

predict(regressor_splines_0, newdata=x_values)

newdat = data.frame(x_values) 
y_data = data.frame(predict(regressor_splines_0,x_values))
appended 

model.predictions = map_df(models, ~cbind(newdat, Petal.Width=predict(.x, newdat)),
                          .id="Species")



x_values = c(2,2.5,3,3.5,4,4.5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,11,12)
grid <- data.frame(x_values)
grid %>% add_predictions(regressor_splines_0)

#Keep data only if nonretail is 1
boston.train_1 = subset(boston.train, boston.train$nonretail == 1)
boston.train_1
boston.train_0 = subset(boston.train, boston.train$nonretail == 0)
boston.train_0 
plot(boston.train$nox,boston.train$dis)
lines(sort(boston.train_0$dis),
      fitted(regressor_splines_0)[order(boston.train_0$dis)],
      col = "red")
lines(sort(boston.train_1$dis),
      fitted(regressor_splines_1)[order(boston.train_1$dis)],
      col = "blue")

regressor_splines_1 = lm(formula = nox ~ poly(dis,df=3), data = boston.train_1)
regressor_splines_0 = lm(formula = nox ~ poly(dis,df=3), data = boston.train_0)

total_data = data.frame(x = boston.train$dis,
                        y1 = )

ggplot() +
  geom_line(aes(y = boston.train_1$nox))



d = data.frame(actual=boston.train$nox, predicted = predict(regressor_splines,data=boston.train))
plot.actual.vs.predicted(d)
gam.2.for.nice.plots = gam(count~ns(pickup_hour_qtr,6) + 
                             factor(pickup_wday) + ns(previous,6), data=train.new)
plot(gam.2.for.nice.plots, se=TRUE,col="red")

models = boston.train %>% 
  split(boston.train$nonretail) %>% 
  map(~lm(formula = nox ~ poly(dis,df=3), data = boston.train))
models
# 
newdat = data.frame(Sepal.Width=seq(min(iris$Sepal.Width), max(iris$Sepal.Width), length=20)) 
# 
# model.predictions = map_df(models, ~cbind(newdat, Petal.Width=predict(.x, newdat)),
#                            .id="Species")
# 
# newdat = data.frame(Bos.dis=seq(min(boston.train$dis), max(boston.train$dis) , length=20))
# model.predictions = map_df(models, ~cbind(newdat, Bos.nox=predict(.x, newdat)),
#                            .id="nonretail") 
#newdat = data.frame(boston.train$dis) 

predict(models[1], boston.train)
predict(models[2], boston.train)
model.predictions = data.frame(predict(models[2], boston.train), boston.train$dis)
model.predictions
#model.predictions = map_df(models, ~cbind(newdat, bos.nox=predict(.x, boston.train)),
#                          .id="nonretail") 

ggplot(data=boston.train,aes(x=dis, y=nox), color=nonretail) +
  geom_point() +
  geom_smooth(size=1.5, se=FALSE, formula=y ~ bs(x,3), method="lm") +
  theme_bw() +
  xlab('DIS') +
  ylab("NOX") +
  geom_line(data=data.frame(boston.train$x, predict(models[2], boston.train))) +
  ggtitle("Polynomial regression of degree 3") +
  theme(axis.title=element_text(size=12), 
        axis.text=element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5))



# Run the linear regression. Nox is the dependent var. DIS is the independent
regressor_poly_3 = lm(formula = nox ~ (poly(dis, degree=3)), data = boston.train)
summary(regressor_poly)

plot(nox~dis, boston.train)
lines(boston.train$dis, predict(regressor_poly_3), col = 'blue')

boston.train$retail1_dis = boston.train$nonretail*boston.train$dis
boston.train$retail0_dis = (boston.train$nonretail-1)*boston.train$dis*-1

regressor_poly3_retail1 = lm(formula = nox ~ (poly(retail1_dis, degree=3)), data = boston.train)
regressor_poly3_retail0 = lm(formula = nox ~ (poly(retail0_dis, degree=3)), data = boston.train)



ggplot(data=boston.train,aes(x=dis, y=nox, color=nonretail)) +
  geom_point() +
  theme_bw() +
  xlab('DIS') +
  ylab("NOX") +
  ggtitle("Polynomial regression of degree 3") +
  theme(axis.title=element_text(size=12), 
        axis.text=element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5))

lines(boston.train$dis, predict(regressor_poly3_retail0), col = 'green')


#Run the prediction on the test data set
prediction_Test_Set_poly = predict(regressor_poly, newdata = boston.test)

#Obtain the R^2 value of the test data set. 
SSR_of_model <- sum((prediction_Test_Set_poly-boston.test$nox)^2)
SSR_of_baseline <- sum((mean(boston.train$nox)-boston.test$nox)^2)
OSR2 <- 1-(SSR_of_model/SSR_of_baseline )
OSR2

  