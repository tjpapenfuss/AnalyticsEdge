
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

data = read.csv("titanic.csv") 
data$Pclass <- factor(data$Pclass) 
data$Sex <- factor(data$Sex)

# Split the data set into training and testing data sets
set.seed(123)
split = createDataPartition(data$Survived, p = 0.7, list = FALSE) 
titanic.train = data[split,]
titanic.test = data[-split,]

head(titanic.test)
str(titanic.train)
# --------------------------------------------------------------------------------- #
#### Problem 1a
# Run the linear regression. Survived is the dependent var. Pclass, Sex, and SibSp is the independent
model <- glm(Survived ~ Pclass + Sex + SibSp,family=binomial,data=titanic.train)
summary(model)
regressor = lm(formula = Survived ~ Pclass + Sex + SibSp, data = titanic.train)
summary(regressor)



# --------------------------------------------------------------------------------- #
#### Problem 2
# --------------------------------------------------------------------------------- #
# Import the dataset

library(caTools)
data = read.csv("framingham.csv") 


# Set CHD as a factor variable. This is the dependent variable (y).
data$TenYearCHD <- factor(data$TenYearCHD)

# Set your additional factor variables.
data$male <- factor(data$male)
data$currentSmoker <- factor(data$currentSmoker) 
data$BPMeds <- factor(data$BPMeds) 
data$prevalentStroke <- factor(data$prevalentStroke) 
data$prevalentHyp <- factor(data$prevalentHyp) 
data$diabetes <- factor(data$diabetes)


set.seed(31)
N <- nrow(data)
idx = sample.split(data$TenYearCHD, 0.75)
train <- data[idx,]
test = data[!idx,]

str(train)

# --------------------------------------------------------------------------------- #
#### Problem 2b
# Run the logistic regression. 
model <- glm(TenYearCHD ~.,family=binomial, data)
summary(model)

# --------------------------------------------------------------------------------- #
#### Problem 2c
# predict based on a real life example
# newdata = read.csv("~/AnalyticsEdge/AnalyticsEdge/HW_3/ex_2c.csv") 
newdata_patient0 = with(data, data.frame(male = factor(1),age = 55,education = "College",currentSmoker=factor(1),
                                cigsPerDay=11,BPMeds=factor(0),prevalentStroke=factor(0),
                                prevalentHyp=factor(1),diabetes=factor(0),totChol=220,sysBP=140,
                                diaBP=100,BMI=30,heartRate=60,glucose=80))
# Set your additional factor variables.
predict(model, newdata = newdata_patient0, type = "response")

# --------------------------------------------------------------------------------- #
#### Problem 2d
# predict based on a real life example
# newdata = read.csv("~/AnalyticsEdge/AnalyticsEdge/HW_3/ex_2c.csv") 
newdata_patient0_updates = with(data, data.frame(male = factor(1),age = 55,education = "College",currentSmoker=factor(0),
                                         cigsPerDay=0,BPMeds=factor(0),prevalentStroke=factor(0),
                                         prevalentHyp=factor(0),diabetes=factor(0),totChol=180,sysBP=115,
                                         diaBP=100,BMI=20,heartRate=60,glucose=80))
# Set your additional factor variables.
predict(model, newdata = newdata_patient0_updates, type = "response")


# --------------------------------------------------------------------------------- #
#### Problem 2f
library(caret)
library(InformationValue)
library(ISLR)
model_train <- glm(TenYearCHD ~.,family=binomial, train)
summary(model_train)
predicted = predict(model_train, newdata = test, type="response")
optimal = 0.08
predicted <- ifelse(predicted>=optimal, 1, 0)
predicted
test$TenYearCHD
confusionMatrix(test$TenYearCHD, predicted)
summarise(test)
table(test, predicted)
# just some extra testing to see a count of all people that get CHD.
library("dplyr")   
dplyr::count(test, TenYearCHD)

#calculate TPR
sensitivity(test$TenYearCHD, predicted)

#calculate TNR
specificity(test$TenYearCHD, predicted)

#calculate total misclassification error rate
misClassError(test$TenYearCHD, predicted, threshold=optimal)




# --------------------------------------------------------------------------------- #
#### Problem 2h


library(pROC)
roc_score=roc(test$TenYearCHD, predicted) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")
roc_score

# --------------------------------------------------------------------------------- #
#### Problem 2i
# Testing with a model that has only three risk factors.

library(caret)
library(InformationValue)
library(ISLR)
model_train_three <- glm(TenYearCHD ~age+male+sysBP,family=binomial, train)
summary(model_train_three)
predicted = predict(model_train_three, newdata = test, type="response")
optimal = 0.08
predicted <- ifelse(predicted>=optimal, 1, 0)
predicted
train$TenYearCHD
confusionMatrix(test$TenYearCHD, (predicted))
summarise(train)






