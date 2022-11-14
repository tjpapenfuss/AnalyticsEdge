# Select the row indices of a random training set of "prop" proportion of
# the ratings that guarantees each rater appears at least once and each item
# appears at least once.

cf.training.set <- function(rater, item, prop) {
  # Draw a  sample from a passed vector, including a length-1 vector
  # (see, eg, http://stackoverflow.com/a/13990144/3093387)
  set.seed(4)
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  
  # Select one rating from each rater
  rater.samples <- sapply(split(seq_along(rater), rater), resample, size=1)
  
  # Select one rating for each item
  item.samples <- sapply(split(seq_along(item), item), resample, size=1)
  
  # Determine the samples currently drawn and not drawn for the training set
  selected <- unique(c(rater.samples, item.samples))
  unselected <- seq_along(rater)[-selected]
  
  # Draw the remaining elements for the training set randomly, and return
  num.needed <- max(0, round(prop*length(rater)) - length(selected))
  return(c(selected, resample(unselected, size=num.needed)))
}

# Select the desired rank for the model
# dat: data frame of ratings, with row indices in the first
#      column, column indices in the second, and ratings in
#      the third column
# ranks: all the ranks to be tested
# prop.validate: the proportion to be set aside in a validation set
cf.evaluate.ranks <- function(dat, ranks, prop.validate=0.05) {
  # Draw a training and validation set from the original training set
  train.rows <- cf.training.set(dat[,1], dat[,2], 1-prop.validate)
  dat.train <- dat[train.rows,]
  dat.validate <- dat[-train.rows,]
  
  # Compute a scaled version of the training set
  mat.train <- Incomplete(dat.train[,1], dat.train[,2], dat.train[,3])
  minimum <- min(dat.train[,3])
  maximum <- max(dat.train[,3])
  mat.scaled <- biScale(mat.train, maxit=1000, row.scale = FALSE, col.scale = FALSE)
  
  # For each rank, compute validation-set predictions
  pred <- lapply(ranks, function(r) {
    if (r == 0) {
      # Rank-0 fit is just sum of row and column indices
      imputed <- attr(mat.scaled, "biScale:row")$center[dat.validate[,1]] +
        attr(mat.scaled, "biScale:column")$center[dat.validate[,2]]
    } else {
      fit <- softImpute(mat.scaled, rank.max=r, lambda=0, maxit=1000)
      imputed <- impute(fit, dat.validate[,1], dat.validate[,2])
    }
    pmin(pmax(imputed, minimum), maximum)
  })
  data.frame(rank = ranks,
             r2 = sapply(pred, function(x) 1-sum((x-dat.validate[,3])^2) / sum((mean(dat.train[,3])-dat.validate[,3])^2)),
             MAE = sapply(pred, function(x) mean(abs(x-dat.validate[,3]))),
             RMSE = sapply(pred, function(x) sqrt(mean((x-dat.validate[,3])^2))))
}