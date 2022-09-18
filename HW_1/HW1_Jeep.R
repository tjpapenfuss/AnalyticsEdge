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
regressor = lm(formula = Wrangler.Sales ~ Year + Unemployment.Rate + CPI.All +
               Wrangler.Queries + CPI.Energy, data = training_set)
summary(regressor)
summary(regressor)$r.squared 

# Building out my linear regressor model based on variables that I think are significant
# I am choosing the variables: Elantra.Sales + Unemployment.Rate + Wrangler.Queries + CPI.Energy + CPI.All
regressor_Tanner = lm(formula = Wrangler.Sales ~ Elantra.Sales + 
                 Wrangler.Queries + CPI.Energy, data = training_set)
summary(regressor_Tanner)
summary(regressor_Tanner)$r.squared 

ggplot(data=regressor, aes(x=Year, y=Wrangler.Sales)) + 
  geom_line() +
  theme_bw()

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

#Plot out the Elantra queries by month.
ggplot(dataset, aes(x=Month.Numeric, y=Elantra.Queries, group=Year)) + geom_line()

#Plot out the Wrangler sales by month.
ggplot(dataset, aes(x=Month.Numeric, y=Wrangler.Queries, group=Year)) + geom_line()


# moving on to question 2 part b. Seasonality
# ---------------------------------------------------------------------------------- #
regressor_include_month = lm(formula = Wrangler.Sales ~ Year + Elantra.Sales + Month.Factor +
                        Wrangler.Queries + CPI.Energy, data = training_set)
summary(regressor_include_month)
summary(regressor_include_month)$r.squared 

ggplot(data=regressor_include_month, aes(x=Year, y=Wrangler.Sales)) + 
  geom_line() +
  theme_bw()

#Run the prediction on the test data set
test_predict = predict(regressor_include_month, newdata = test_set)

#Obtain the R^2 value of the test data set. 
SSR_of_model <- sum((test_predict-test_set$Wrangler.Sales)^2)
#SSR_of_model_1 <- sum((test_predict_1-test_set$Wrangler.Sales)^2)
SSR_of_baseline <- sum((mean(training_set$Wrangler.Sales)-test_set$Wrangler.Sales)^2)
OSR2 <- 1-(SSR_of_model/SSR_of_baseline )
OSR2


# moving on to question 2 part c. Elantra.Sales
# ---------------------------------------------------------------------------------- #
# Year, Unemployment.Rate, Elantra.Queries, CPI.Energy, and CPI.All
# Building out the linear regression for the Elantra 
regressor_Elantra = lm(formula = Elantra.Sales ~ Year + Unemployment.Rate + CPI.All +
                 Elantra.Queries + CPI.Energy, data = training_set)
summary(regressor_Elantra)

#Run the prediction on the test data set
test_predict = predict(regressor_Elantra, newdata = test_set)

#Obtain the R^2 value of the test data set. 
SSR_of_model <- sum((test_predict-test_set$Elantra.Sales)^2)
#SSR_of_model_1 <- sum((test_predict_1-test_set$Elantra.Sales)^2)
SSR_of_baseline <- sum((mean(training_set$Elantra.Sales)-test_set$Elantra.Sales)^2)
OSR2 <- 1-(SSR_of_model/SSR_of_baseline )
OSR2

####
# Plot and compare the Sales of “Jeep Wrangler” and “Hyundai Elantra” from the time 
# period January 1, 2010 to December 31, 2018. What do you observe? Do you think that 
# adding seasonality would improve your model to predict Elantra sales? Why or why not?
library(ggplot2)
ggplot() + 
  geom_line(data=dataset, aes(x=Year,y=Elantra.Sales,col='Elan')) +
  geom_line(data=dataset, aes(x=Year,y=Wrangler.Sales,col='Wran')) +
  # theme_bw() +
  xlab('Year') +
  ylab('Sales') +
  scale_color_manual(name="Sales", values = c('Elan'='darkgreen','Wran'='blue'), labels = c('Elan'="Elantra",'Wran'="Wrangler")) +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=14), legend.text=element_text(size=14), legend.title=element_text(size=14))

#Show graphically the sales per year
lo <- loess(dataset$Year~dataset$Elantra.Sales)
plot(dataset$Year, dataset$Elantra.Sales)
lines(predict(lo), col='red', lwd=2)




