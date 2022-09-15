library(tidyverse)
library(ggrepel)
library(ggplot2)
library(caTools)

# Import the dataset
dataset = read.csv('WranglerElantra2019.csv')

#Split out the data sets into training and testing
training_set = dataset[dataset$Year <= 2018, ]
test_set = dataset[dataset$Year > 2018, ]

# Building out the linear regression for the Jeep Wrangler. Question 2 part i. 
regressor = lm(formula = Wrangler.Sales ~ Year + Unemployment.Rate + 
               Wrangler.Queries + CPI.Energy, data = training_set)
summary(regressor)
summary(regressor)$r.squared 

# Building out my linear regressor model based on variables that I think are significant
# I am choosing the variables: Elantra.Sales + Unemployment.Rate + Wrangler.Queries + CPI.Energy + CPI.All
regressor_Tanner = lm(formula = Wrangler.Sales ~ Month.Numeric + Elantra.Sales + 
                 Wrangler.Queries + CPI.Energy + Elantra.Queries, data = training_set)
summary(regressor_Tanner)
summary(regressor_Tanner)$r.squared 

#Run the prediction on the test data set
test_predict = predict(regressor_Tanner, newdata = test_set)

#Obtain the R^2 value of the test data set. 
SSR_of_model <- sum((test_predict-test_set$Wrangler.Sales)^2)
#SSR_of_model_1 <- sum((test_predict_1-test_set$Wrangler.Sales)^2)
SSR_of_baseline <- sum((mean(training_set$Wrangler.Sales)-test_set$Wrangler.Sales)^2)
OSR2 <- 1-(SSR_of_model/SSR_of_baseline )
OSR2


#Get a subset of the data. Then run a correlation on that subset to test. 
subset_Data = subset(training_set, select = -c(date,Month.Factor, Unemployment.Rate, CPI.All))
cor(subset_Data)

#Plot out the Wrangler queries by month.
ggplot(dataset, aes(x=Month.Numeric, y=Wrangler.Queries, group=Year)) + geom_line()

#Plot out the Wrangler sales by month.
ggplot(dataset, aes(x=Month.Numeric, y=Wrangler.Queries, group=Year)) + geom_line()
