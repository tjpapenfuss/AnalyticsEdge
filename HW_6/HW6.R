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
reviews = read.csv("airbnb-small.csv", stringsAsFactors = FALSE)

### Question a.

table(reviews$review_scores_rating)

reviews %>%
  group_by(review_scores_rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(review_scores_rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:5) 

reviews$comment_len = nchar(reviews$comments)
str(reviews)
summary(reviews)

### Question b.
corpus = Corpus(VectorSource(reviews$comments)) # An array of document
corpus

corpus[[1]]   # individual doc
strwrap(corpus[[1]])
strwrap(corpus[[3]])
# Let us start processing the text in the corpus! Here is a 
# summary of how we shall process the text.  
# 1. Change all the text to lower case.  
# 2. Remove all punctuation.
# 3. Remove stop words 
# 4. Remove the word "airbnb"
# 5. "Stem" the documents. 

# 1. Let's change all the text to lower case. 
corpus = tm_map(corpus, tolower)
strwrap(corpus[[1]])

# 2. We remove punctuation from the document
corpus <- tm_map(corpus, removePunctuation)
strwrap(corpus[[1]])

# 3. Let us remove some words. First, we remove stop words:  
corpus = tm_map(corpus, removeWords, stopwords("english"))  # removeWords(corpus,stopwords("english"))
# stopwords("english") is a dataframe that constains a list of 
# stop words. Let us look at the first ten stop words. 
stopwords("english")[1:10]

# Checking again:  
strwrap(corpus[[1]])

# 4. Next, we remove the word "airbnb"
strwrap(corpus[[4]])
corpus = tm_map(corpus, removeWords, c("airbnb"))
strwrap(corpus[[4]])

# 5. Now we stem our documents. Recall that this corresponds to removing the parts of words
# that are in some sense not necessary (e.g. 'ing' and 'ed'). 
corpus = tm_map(corpus, stemDocument)

# We have: 
strwrap(corpus[[1]])

### Question c.

# Let us "sparsify" the corpus and remove infrequent words. 
# First, we calculate the frequency of each words over all tweets. 
frequencies = DocumentTermMatrix(corpus)
frequencies                              # documents as the rows, terms as the columns
# Let us get a feel for what words occur the most. Words that appear at least 900 times: 
findFreqTerms(frequencies, lowfreq=900)
# Let us only keep terms that appear in at least 1% of the tweets. We create a list of these words as follows. 
sparse = removeSparseTerms(frequencies, 0.99)  # 0.99: maximal allowed sparsity 
sparse # We now have 172 terms instead of 12,093

### Question d.
document_terms = as.data.frame(as.matrix(sparse))
str(document_terms)
head(document_terms)
# Lastly, we create a new column for the dependent variable: 
document_terms$positive_review = reviews$review_scores_rating >= 80

# Training and test set.
split1 = (reviews$date < "2018-01-01")
split2 = (reviews$date >= "2018-01-01")
train = document_terms[split1,]
test = document_terms[split2,]

### Question e.

# i. Constructing and plotting the CART model.
cart = rpart(positive_review ~ ., data=train, method="class", cp = .003)  # classification
prp(cart)

# reviews[grepl("stay", reviews$comments), "comments"]


# ii. Assessing the out-of-sample performance of the CART model
predictions.cart <- predict(cart, newdata=test, type="class")
matrix.cart = table(test$positive_review, predictions.cart) # confusion matrix
accuracy.cart = (matrix.cart[1,1]+matrix.cart[2,2])/nrow(test)
TPR.cart = (matrix.cart[2,2])/sum(matrix.cart[2,])
FPR.cart = (matrix.cart[1,2])/sum(matrix.cart[1,])
accuracy.cart
TPR.cart
FPR.cart

true.predict = predictions.cart
levels(true.predict) = c("True", "True")

matrix.true.cart = table(test$positive_review, true.predict)
accuracy.true.cart = (matrix.true.cart[2,1])/nrow(test)
FPR.true.cart = (matrix.cart[1,1])/sum(matrix.cart[1,])
accuracy.true.cart

# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# PROBLEM 2 - Songs
##### Problem 2: Recommending Songs to Music Listeners
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------

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
out_eval = cf.evaluate.ranks(music.train, 1:10, prop.validate=0.05)
ggplot(data = out_eval) +
  geom_point(aes(rank, r2))
  

### Question b.

set.seed(15071)
# We are now ready to apply our collaborative filtering algorithm.
# This is done with the softImpute function, as follows:
fit <- softImpute(mat.train, rank.max=4, lambda=0, maxit=1000)


pred.insample <- impute(fit, music.train[, 1], music.train[, 2])
pred.outsample <- impute(fit, music.test[, 1], music.test[, 2])

R2.insample <- 1 - sum((pred.insample-music.train$rating)^2)/sum((mean(music.train$rating) - music.train$rating)^2)
R2.outsample <- 1 - sum((pred.outsample-music.test$rating)^2)/sum((mean(music.train$rating) - music.test$rating)^2)
R2.insample
R2.outsample
?impute
outTable = fit$u
Daisy = (fit$u)[1584,]

hist(pred.insample)
hist(pred.outsample)

# complete(mat.train,fit)
# impute(music.train,fit)
### Question c.

sum(fit$u[music.train[1584,1],] * fit$v[131,] * fit$d)
sum(fit$u[music.train[1584,1],] * fit$v[156,] * fit$d)

### Question d.
daisy_rated = music[which(music$userID ==1584),2]
daisy_full = music[which(music$userID ==1584),]

daisy_df = data.frame("songID"=c(0), "Estimated_Rating"=c(0))
for (x in 1:807) {
  if (!(x %in% daisy_rated)) {
    pred.Daisy = sum(fit$u[music.train[1584,1],] * fit$v[x,] * fit$d)
    daisy_df[x,] = c(x,pred.Daisy)    
  }
}
sort(daisy_df$Estimated_Rating)
top_n(daisy_df,5)

sort(daisy_rated)
top_n(daisy_full,5)

daisy_final_rated = merge(top_n(daisy_full,5), songs, by=c("songID"))
daisy_final_recommend = merge(top_n(daisy_df,5), songs, by=c("songID"))
write.csv(daisy_final_rated, "finalrated.csv")
write.csv(daisy_final_recommend, "daisyRecommended.csv")

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


