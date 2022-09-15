library(tidyverse)
library(ggrepel)
library(caTools)

# Import the dataset
dataset = read.csv('climate_change.csv')
# dataset = dataset[, 2:3]

# Split data set into test and training set

#set.seed(123) 
#split = sample.split(dataset$Temp, 0.7)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

training_set = dataset[dataset$Year <= 2002, ]
test_set = dataset[dataset$Year > 2002, ]


regressor = lm(formula = Temp ~ CO2 + CH4 + N2O + CFC_11 + CFC_12 + TSI + Aerosols + MEI, data = training_set)
regressor_1 = lm(formula = Temp ~ N2O + TSI + Aerosols + MEI, data = training_set)
summary(regressor_1)
summary(regressor)$r.squared 

#Displays the confidence variables. 
confint(regressor_1, level = .99)

#Run the prediction on the test data set
test_predict = predict(regressor, newdata = test_set)
test_predict_1 = predict(regressor_1, newdata = test_set)

test_predict
#Obtain the R^2 value of the test data set. 
SSR_of_model <- sum((test_predict-test_set$Temp)^2)
SSR_of_model_1 <- sum((test_predict_1-test_set$Temp)^2)
SSR_of_baseline <- sum((mean(test_set$Temp)-test_set$Temp)^2)
OSR2 <- 1-SSR_of_model/SSR_of_baseline 
OSR2
OSR2_1 <- 1-SSR_of_model_1/SSR_of_baseline 
OSR2_1

# Correlation with N2O
print(cor(dataset[,-1]), digits=2)
round(cor(training_set), digits = 2)

summary(regressor)

summary(test_predict)
test_predict
cor(test_set$Temp, test_predict)^2

RSS <- sum((test_set$Temp - test_predict)^2)
TSS <- sum((test_set$Temp - mean(test_set$Temp))^2)
rsquared <-1 - (RSS/TSS)
rsquared
