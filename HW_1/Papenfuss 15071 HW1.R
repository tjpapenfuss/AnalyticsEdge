# Import the dataset
dataset = read.csv('climate_change.csv')
# dataset = dataset[, 2:3]

# Split data set into test and training set
library(caTools)
#set.seed(123) 
#split = sample.split(dataset$Temp, 0.7)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

training_set = dataset[dataset$Year <= 2003, ]
test_set = dataset[dataset$Year > 2003, ]


#regressor = lm(formula = Temp ~ CO2 + CH4 + N2O + CFC_11 + CFC_12 + TSI + Aerosols + MEI, data = training_set)
regressor = lm(formula = Temp ~ N2O + TSI + Aerosols + MEI, data = training_set)
summary(regressor)
summary(regressor)$r.squared 

test_predict = predict(regressor, newdata = test_set)
summary(test_predict)
test_predict
cor(test_set$Temp, test_predict)^2

#round(cor(training_set), digits = 2) # rounded to 2 decimals


#1. MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
#MAPE = function(y_actual,y_predict){
#  mean(abs((y_actual-y_predict)/y_actual))*100
#}


#2. R SQUARED error metric -- Coefficient of Determination
#RSQUARE = function(y_actual,y_predict){
#  cor(y_actual,y_predict)^2
#}
