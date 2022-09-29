
library(tidyverse)
library(ggrepel)
library(caTools)
library(caret)
library(ROCR)
library(ggplot2)
library(splines)

# Import the dataset
boston = read.csv('boston.csv')

# Split the data set into training and testing data sets
set.seed(123)
split = createDataPartition(boston$nox, p = 0.7, list = FALSE) 
boston.train = boston[split,]
boston.test = boston[-split,]

head(boston.test)

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
regressor_splines = lm(formula = nox ~ poly(dis,df=3)*factor(nonretail), data = boston.train)
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
regressor_splines_1 = lm(formula = nox ~ poly(dis,df=3), data = boston.train_1)
regressor_splines_0 = lm(formula = nox ~ poly(dis,df=3), data = boston.train_0)

#First plot the old regressor, then plot the two new regressors. 
ggplot(data=boston.train,aes(x=dis, y=nox, color=nonretail)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, degree=3),se = FALSE, 
              color = "red") +
  geom_smooth(data = boston.train_0,aes(x=dis, y=nox), 
              method = "lm", formula = y ~ poly(x, degree=3), se=FALSE) +
  geom_smooth(data = boston.train_1,aes(x=dis, y=nox), 
              method = "lm", formula = y ~ poly(x, degree=3), se=FALSE, color = "blue") +
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
                       knots_bos[2], knots_bos[3], knots_bos[4]))*factor(nonretail),
                       data = boston.train)
summary(regressor_splines_2)

ggplot(data=boston.train,aes(x=dis, y=nox)) +
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



# ------ The below code for exercise 1d will plot the splines on the data set. 
#Generate a subset with nonretail = 1
boston.train_1 = subset(boston.train, boston.train$nonretail == 1)
boston.train_1
#Generate a subset with nonretail = 0
boston.train_0 = subset(boston.train, boston.train$nonretail == 0)
boston.train_0

#Create two new regressors. These are for nonretail =0 & 1. These regressors will be used
#below in the ggplot
regressor_splines_1 = lm(formula = nox ~ bs(dis,knots=c(knots_bos[1], 
                         knots_bos[2], knots_bos[3], knots_bos[4])),
                         data = boston.train_1)
regressor_splines_0 = lm(formula = nox ~ bs(dis,knots=c(knots_bos[1], 
                         knots_bos[2], knots_bos[3], knots_bos[4])),
                         data = boston.train_0)

ggplot(data=boston.train,aes(x=dis, y=nox, color=nonretail)) +
  geom_point() +
  geom_smooth(data = boston.train_0,aes(x=dis, y=nox), 
              method = "lm", formula = y ~ bs(x,knots=c(knots_bos[1], 
              knots_bos[2], knots_bos[3], knots_bos[4])), se=FALSE) +
  geom_smooth(data = boston.train_1,aes(x=dis, y=nox), 
              method = "lm", formula = y ~ bs(x,knots=c(knots_bos[1], 
              knots_bos[2], knots_bos[3], knots_bos[4])), se=FALSE, color = "blue") +
  theme_bw() +
  xlab('DIS') +
  ylab("NOX") +
  ggtitle("Spline graph Question 1 part d") +
  theme(axis.title=element_text(size=12), 
        axis.text=element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5))






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

  