library(tm)
library(SnowballC)
library(caret)
library(rpart)
library(rpart.plot)
library(softImpute)
library(dplyr)

rm(list = ls())

##### Problem 1: Predicting AirBnB review scores

# data importation
reviews.small = read.csv("airbnb-small.csv", stringsAsFactors = FALSE)

### Question a.


### Question b.

### Question c.



### Question d.


### Question e.



# PROBLEM 2 - Songs
##### Problem 2: Recommending Songs to Music Listeners

# reading the function file
source("functionsCF.R")

# data importation
songs <- read.csv("Songs.csv")
users <- read.csv("Users.csv")
music <- read.csv("MusicRatings.csv")

# look at the files
head(songs)
head(users)
head(music)

# splitting into a training set and a test set
set.seed(144)
training.rows <- cf.training.set(music$userID, music$songID, prop=0.92)
music.train <- music[training.rows,]
music.test <- music[-training.rows,]
mat.train <- Incomplete(music.train[,1], music.train[,2], music.train[,3])

# scaling the matrix
set.seed(15071)
mat.scaled <- biScale(mat.train, maxit=1000, row.scale = FALSE, col.scale = FALSE)

### Question a.

set.seed(123)

### Question b.

set.seed(15071)


### Question c.



### Question d.


# STEPS
# - Make predictions for all songs Daisy has not rated. ------> 
#       all.songs <- 1:nrow(songs)
#       impute(fit,rep(1584,nrow(songs)),all.songs)
#   Equivalent to: impute(fit,1584,1), impute(fit,1584,2), impute(fit,1584,3), ...
# - Create a dataframe for Daisy with the following structure:
#     (song id, actual or predicted rating, is this an actual or a predicted rating?)
#   Example for song 1: [ 1   impute(fit,1584,1)    0 ]
#        sum(music$songID[music$userID==1584]==1)
# ----> how to fill the entire dataframe in a non-manual way? that is, compute the 2nd and 3rd field of the dataframe iteratively
#     maybe use a for loop?
#     alternatively, do it in a vectorized way: 
#        data.frame(
#                    songs=all.songs,
#                    ...... )       <------- for third column, maybe look into sapply function
# - From this dataframe, you should be able to extract the information you need 
#     (5 top songs among those Daisy has not rated, i.e., the rating in your dataframe is a predicted rating).


