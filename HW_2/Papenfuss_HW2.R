
library(tidyverse)
library(ggrepel)
library(caTools)
library(caret)
library(ROCR)
library(ggplot2)


# Import the dataset
boston = read.csv('boston.csv')

# Split the data set into training and testing data sets
set.seed(123)
split = createDataPartition(boston$nox, p = 0.7, list = FALSE) 
boston.train = boston[split,]
boston.test = boston[-split,]

head(boston.train)

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
  
  
  
  
  