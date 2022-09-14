library(dplyr)
library(splines)
library(gam)


# a little function to calc R2 and OSR2
# since we have to do it often

calc.OSR2 <- function(actuals, model.preds, baseline.preds) {
  return( 1 - sum((actuals - model.preds)^2) / sum((actuals - baseline.preds)^2) )
}

# Slide 2
# Read in the data 
taxi <- read.csv("taxi.time.all.csv")

# Slide 3
# Split into training and test 
train <- taxi %>% filter(pickup_month == 2) 
test <- taxi %>% filter(pickup_month==3) 

# Let's do some visualizations

# Slide 4
# Scatter plot
ggplot(data=train,aes(x=pickup_hour_qtr)) +
  geom_point(aes(y=count),size=.5) +
  theme_bw() +
  xlab('Hour of day') +
  ylab("Taxi count") +
  theme(axis.title=element_text(size=18), 
        axis.text=element_text(size=18), 
        legend.position='none')

# Scatter plot by day of week
ggplot(data=train,aes(x=pickup_hour_qtr)) +
  geom_point(aes(y=count,color=factor(pickup_wday)),size=.5) +
  theme_bw() +
  xlab('Hour of day') +
  ylab("Taxi count") +
  theme(axis.title=element_text(size=18), 
        axis.text=element_text(size=18), 
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18)) + 
  scale_color_brewer(name='Weekday',
                     labels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'), 
                     palette="Set1")

# Slide 5: Let's try a simple regression for starters

model <- lm(count~pickup_hour_qtr, data = train)
summary(model)$r.squared
# In sample R2 0.48
preds <- predict(model, newdata=test)
calc.OSR2(test$count, preds, mean(train$count))
# OSR2 0.47

# Slide 7: Add the day variable to the regression

model.basic <- lm(count~pickup_hour_qtr + factor(pickup_wday), data = train)
summary(model.basic)$r.squared 
# In sample R2 0.51
preds <- predict(model.basic, newdata=test)
calc.OSR2(test$count, preds, mean(train$count))
# OSR2 0.48

# Slide 10: Interact day and hour

model.interact <- lm(count~pickup_hour_qtr*factor(pickup_wday), data = train)
summary(model.interact)$r.squared 
# In sample R2 0.56
preds <- predict(model.interact, newdata=test)
calc.OSR2(test$count, preds, mean(train$count))
# OSR2 0.52

# Slide 12
#  Let's fit polynomials from degree 1 through 10 and calculate their R2 and OSR2
degrees <- 1:10
n <- length(degrees)
summary.model.fit <- train
R2.all <- rep(NA,n)
OSR2.all <- rep(NA,n)
for (i in 1:n){
  degree <- degrees[i]
  model=lm(count~poly(pickup_hour_qtr, degree),data=train)
  summary.model.fit[[paste0('poly', degree)]] <- predict(model,train)
  R2.all[i] <- calc.OSR2(train$count, predict(model,train), mean(train$count))
  OSR2.all[i] <- calc.OSR2(test$count, predict(model,test), mean(train$count))
}


# Slide 13: Let's plot the resulting curves

ggplot(data=summary.model.fit,aes(x=pickup_hour_qtr)) +
  geom_point(aes(y=count),size=.5) +
  geom_line(aes(y=poly1,col='1'),lwd=2) +
  geom_line(aes(y=poly2,col='2'),lwd=2) +
  geom_line(aes(y=poly3,col='3'),lwd=2) +
  geom_line(aes(y=poly4,col='4'),lwd=2) +
  geom_line(aes(y=poly5,col='5'),lwd=2) +
  geom_line(aes(y=poly6,col='6'),lwd=2) +
  theme_bw() +
  xlab('Hour of day') +
  ylab("Taxi count") +
  theme(axis.title=element_text(size=18), 
        axis.text=element_text(size=18), 
        legend.text=element_text(size=18)) + 
  scale_color_brewer(name='',
                     labels=paste0('degree ',c(1,2,3,4,5,6)), 
                     palette="Set1")


# Slide 14
# We will go with the degree 6 fit
R2.all[6]
OSR2.all[6]



# Piecewise polynomials

# Let's fit piecewise polynomials manually (ugh)

cuts <- c(6,12,18)

preds <- rep(NA,nrow(train))

poly.fit1=lm(count~poly(pickup_hour_qtr,2),data=train %>% filter(pickup_hour_qtr<=cuts[1]))
poly.fit1.pred <- predict(poly.fit1,data=train %>% filter(pickup_hour_qtr<=cuts[1]))
preds[which(train$pickup_hour_qtr<=cuts[1])] <- poly.fit1.pred

poly.fit2=lm(count~poly(pickup_hour_qtr,2),data=train %>% filter(pickup_hour_qtr>cuts[1] & pickup_hour_qtr<=cuts[2]))
poly.fit2.pred <- predict(poly.fit2,data=train %>% filter(pickup_hour_qtr>cuts[1] & pickup_hour_qtr<=cuts[2]))
preds[which(train$pickup_hour_qtr>cuts[1] & train$pickup_hour_qtr<=cuts[2])] <- poly.fit2.pred

poly.fit3=lm(count~poly(pickup_hour_qtr,2),data=train %>% filter(pickup_hour_qtr>cuts[2] & pickup_hour_qtr<=cuts[3]))
poly.fit3.pred <- predict(poly.fit3,data=train %>% filter(pickup_hour_qtr>cuts[2] & pickup_hour_qtr<=cuts[3]))
preds[which(train$pickup_hour_qtr>cuts[2] & train$pickup_hour_qtr<=cuts[3])] <- poly.fit3.pred

poly.fit4=lm(count~poly(pickup_hour_qtr,2),data=train %>% filter(pickup_hour_qtr>cuts[3]))
poly.fit4.pred <- predict(poly.fit4,data=train %>% filter(pickup_hour_qtr>cuts[3]))
preds[which(train$pickup_hour_qtr>cuts[3])] <- poly.fit4.pred

train$discontinous <- preds

# Slide 17: Plot them

ggplot() +
  geom_point(data=train,aes(x=pickup_hour_qtr,y=count),size=.5) +
  geom_line(data=train %>% filter(pickup_hour_qtr<=cuts[1]),aes(x=pickup_hour_qtr,y=discontinous,col='1'),lwd=2) +
  geom_line(data=train %>% filter(pickup_hour_qtr>cuts[1] & pickup_hour_qtr<=cuts[2]),aes(x=pickup_hour_qtr,y=discontinous,col='1'),lwd=2) +
  geom_line(data=train %>% filter(pickup_hour_qtr>cuts[2] & pickup_hour_qtr<=cuts[3]),aes(x=pickup_hour_qtr,y=discontinous,col='1'),lwd=2) +
  geom_line(data=train %>% filter(pickup_hour_qtr>cuts[3]),aes(x=pickup_hour_qtr,y=discontinous,col='1'),lwd=2) +
  geom_vline (aes(xintercept=cuts[1],col='1'),lty=2,show.legend=FALSE) +
  geom_vline (aes(xintercept=cuts[2],col='1'),lty=2,show.legend=FALSE) +
  geom_vline (aes(xintercept=cuts[3],col='1'),lty=2,show.legend=FALSE) +
  theme_bw() +
  xlab('Hour of day') +
  ylab("Taxi count") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.position='none') +
  scale_color_brewer(palette="Set1")

train$discontinous <- NULL

# Regression Splines

# We introduce jitter in the x data to make the regressions more stable. This is minor
set.seed(121)
train$pickup_hour_qtr <- jitter(train$pickup_hour_qtr)
test$pickup_hour_qtr <- jitter(test$pickup_hour_qtr)


# Slide 19: Regular splines: fit with degrees from 1 through 5 but
# with degrees of freedom fixed at 6

df <- 6
degrees <- 1:5
n <- length(degrees)
summary.model.fit <- train
R2.all <- rep(NA,n)
OSR2.all <- rep(NA,n)
for (i in 1:n) {
  degree <- degrees[i]
  model=lm(count~bs(pickup_hour_qtr,df=df,degree=degree),data=train)
  summary.model.fit[[paste0('degree', degree)]] <- predict(model,train)
  R2.all[i] <- calc.OSR2(train$count, predict(model,train), mean(train$count))
  OSR2.all[i] <- calc.OSR2(test$count, predict(model,test), mean(train$count))
}

  
ggplot(data=summary.model.fit,aes(x=pickup_hour_qtr)) +
  geom_point(aes(y=count),size=.5) +
  geom_line(aes(y=degree1,col='1'),lwd=2) +
  geom_line(aes(y=degree2,col='2'),lwd=2) +
  geom_line(aes(y=degree3,col='3'),lwd=2) +
  geom_line(aes(y=degree4,col='4'),lwd=2) +
  geom_line(aes(y=degree5,col='5'),lwd=2) +
  theme_bw() +
  xlab('Hour of day') +
  ylab("Taxi count") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.text=element_text(size=18)) + 
  scale_color_brewer(name='',labels=paste0('degree ',1:5), palette="Set1")


# Slide 20
R2.all
OSR2.all

# Slide 21
# Cubic splines: fit with degrees of freedom from 4 through 10

df.all <- c(4,5,6,7,8,9,10)
n <- length(df.all)
summary.model.fit <- train
R2.all <- rep(NA,n)
OSR2.all <- rep(NA,n)
for (i in 1:n) {
  df <- df.all[i]
  model=lm(count~bs(pickup_hour_qtr,df=df),data=train)
  summary.model.fit[[paste0('df', df)]] <- predict(model,train)
  R2.all[i] <- calc.OSR2(train$count, predict(model,train), mean(train$count))
  OSR2.all[i] <- calc.OSR2(test$count, predict(model,test), mean(train$count))
}

ggplot(data=summary.model.fit,aes(x=pickup_hour_qtr)) +
  geom_point(aes(y=count),size=.5) +
  geom_line(aes(y=df4,col='1'),lwd=2) +
  geom_line(aes(y=df5,col='2'),lwd=2) +
  geom_line(aes(y=df6,col='3'),lwd=2) +
  geom_line(aes(y=df7,col='4'),lwd=2) +
  geom_line(aes(y=df8,col='5'),lwd=2) +
  geom_line(aes(y=df9,col='6'),lwd=2) +
  geom_line(aes(y=df10,col='7'),lwd=2) +
  theme_bw() +
  xlab('Hour of day') +
  ylab("Taxi count") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.text=element_text(size=18)) + 
  scale_color_brewer(name='',labels=paste0(4:10,' df'), palette="Set1")


# Slide 22
R2.all[3]
OSR2.all[3]


# Slide 27
# Natural splines: fit with different degrees of freedom

df.all <- c(4,5,6,7,8,9,10)
n <- length(df.all)
summary.model.fit <- train
R2.all <- rep(NA,n)
OSR2.all <- rep(NA,n)
for (i in 1:n) {
  df <- df.all[i]
  model=lm(count~ns(pickup_hour_qtr,df=df),data=train)
  summary.model.fit[[paste0('df', df)]] <- predict(model,train)
  R2.all[i] <- calc.OSR2(train$count, predict(model,train), mean(train$count))
  OSR2.all[i] <- calc.OSR2(test$count, predict(model,test), mean(train$count))
}


ggplot(data=summary.model.fit,aes(x=pickup_hour_qtr)) +
  geom_point(aes(y=count),size=.5) +
  geom_line(aes(y=df4,col='1'),lwd=2) +
  geom_line(aes(y=df5,col='2'),lwd=2) +
  geom_line(aes(y=df6,col='3'),lwd=2) +
  geom_line(aes(y=df7,col='4'),lwd=2) +
  geom_line(aes(y=df8,col='5'),lwd=2) +
  geom_line(aes(y=df9,col='6'),lwd=2) +
  geom_line(aes(y=df10,col='7'),lwd=2) +
  theme_bw() +
  xlab('Hour of day') +
  ylab("Taxi count") +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.text=element_text(size=18)) + 
  scale_color_brewer(name='',labels=paste0(4:10,' df'), palette="Set1")


# Slide 28
R2.all[3]
OSR2.all[3]


##### GAMs

# Slide 39
# splines + weekday factor
gam.1 <- lm(count~ns(pickup_hour_qtr, df = 6) + factor(pickup_wday), data = train)
summary(gam.1)$r.squared 
# In sample R2 0.78

preds <- predict(gam.1, newdata=test)
calc.OSR2(test$count, preds, mean(train$count))
# OSR2 0.72


# Add previous-day traffic as a new predictor
train$previous <- lag(train$count,96)
test$previous <- lag(test$count, 96)

# Now the first 24*4 = 96 rows will have NAs in the 'previous' column
# so remove those 96 rows
train.new <- train[-(1:96),]
test.new <- test[-(1:96),]

# Slide 41
gam.2=lm(count~ns(pickup_hour_qtr,6) + ns(previous,6) + factor(pickup_wday),data=train.new)

summary(gam.2)$r.squared 
# In sample R2 0.82
preds <- predict(gam.2, newdata=test.new)
calc.OSR2(test.new$count, preds, mean(train.new$count))
# OSR2 0.74

# Slide 42
# While we fitted the gam above using 'lm', 
# to get the nice plots on slide 41, we need to use 
# the 'gam' function as below. Note that literally the 
# only difference here is replacing 'lm' with 'gam'


gam.2.for.nice.plots = gam(count~ns(pickup_hour_qtr,6) + 
                             factor(pickup_wday) + ns(previous,6), data=train.new)
par(mfrow=c(1,3))
plot(gam.2.for.nice.plots, se=TRUE,col="red")


# Slide 43

# let's write a little function to plot
# actual vs predicted values since we want to do it a bunch of times
# this is a handy thing to have for any prediction problem
# assumes you send in a dataframe with a column called 'actual'
# and a column called 'predicted'

plot.actual.vs.predicted <- function(d) {
  ggplot(data=d,aes(x=actual,y=predicted)) +
    geom_point(size=.5) +
    theme_bw() +
    xlab("Actual") +
    ylab("Predicted") +
    geom_abline(slope=1, intercept = 0, col = "red")+
    theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.position='none')
}


# let's do it for our chosen natural spline from before
model = lm(count~ns(pickup_hour_qtr, df=6), data=train)
d = data.frame(actual = test$count, predicted = predict(model,newdata = test))
plot.actual.vs.predicted(d)

# let's do it for the GAM
model = gam.2
d = data.frame(actual = test$count, predicted = predict(model,newdata = test))
plot.actual.vs.predicted(d)

# Slide 46
# The grand finale: Add an interaction term to the GAM

gam.3=lm(count~ns(pickup_hour_qtr,6)*factor(pickup_wday) + ns(previous,6),data=train.new)

summary(gam.3)$r.squared 
# In sample R2 0.95
preds <- predict(gam.3, newdata=test.new)
calc.OSR2(test.new$count, preds, mean(train.new$count))
# OSR2 0.87

# The following isn't in the slides but what the heck ...

# Since we have the handy-dandy function, let's 
# plot actual vs predicted values for the 'grand finale' GAM
model = gam.3
d = data.frame(actual = test$count, predicted = predict(model,newdata = test))
plot.actual.vs.predicted(d)

# Also, we see that since the previous day's traffic helped only a little,
# let's remove it and see what happens to the OSR2

gam.4 <- lm(count~ns(pickup_hour_qtr, df = 6)*factor(pickup_wday), data = train)
summary(gam.4)$r.squared 
# In sample R2 0.93

preds <- predict(gam.4, newdata=test)
calc.OSR2(test$count, preds, mean(train$count))
# OSR2 0.87

# Nice! We removed a predictor and the OSR2 didn't change.
# In general, if the OSR2 doesn't drop when you try removing a predictor, 
# remove the predictor permanently. Smaller models are better, as long as 
# the OSR2 isn't worse

# Again,let's plot actual vs predicted values 
model = gam.4
d = data.frame(actual = test$count, predicted = predict(model,newdata = test))
plot.actual.vs.predicted(d)


#------------------THE END--------------------------------------
