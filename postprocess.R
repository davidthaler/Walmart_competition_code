require(plyr)
require(reshape)

# NB: this is implemented this way because there were originally other 
# postprocessing transformations used (or tried), some of which used
# the training data.

postprocess <- function(train, test, ...){
  # Iterates over the departments and calls shift() on each.
  #
  # args:
  #  train - the training set as returned from raw.train() in util 
  #  test - a reloaded submission or a data frame similar to test,
  #         from raw.test() in util, but with predictions in the 
  #         Weekly_Sales field
  # ... - additional arguments passed to shift()
  #
  # returns:
  #  the data frame input as test, after calling shift on it department-wise
  if('Id' %in% names(test)){
    #This is a saved submission
    sales <- test$Weekly_Sales
    test <- raw.test()
    test$Weekly_Sales <- sales
  }
  test.dates <- unique(test$Date)
  num.test.dates <- length(test.dates)
  all.stores <- unique(test$Store)
  num.stores <- length(all.stores)
  test.depts <- unique(test$Dept)
  forecast.frame <- data.frame(Date=rep(test.dates, num.stores),
                               Store=rep(all.stores, each=num.test.dates))
  pred <- test
  pred$Weekly_Sales <- 0
  
  train.dates <- unique(train$Date)
  num.train.dates <- length(train.dates)
  train.frame <- data.frame(Date=rep(train.dates, num.stores),
                            Store=rep(all.stores, each=num.train.dates))
  for(d in test.depts){
    print(paste('dept:', d))
    tr.d <- join(train.frame,
                 train[train$Dept==d, c('Store','Date','Weekly_Sales')])
    tr.d <- cast(tr.d, Date ~ Store) 
    fc.d <- join(forecast.frame,
                 test[test$Dept==d, c('Store', 'Date', 'Weekly_Sales')])
    fc.d <- cast(fc.d, Date ~ Store)
    result <- shift(tr.d, fc.d, ...)
    result <- melt(result)
    pred.d.idx <- pred$Dept==d
    pred.d <- pred[pred.d.idx, c('Store', 'Date')]
    pred.d <- join(pred.d, result)
    pred$Weekly_Sales[pred.d.idx] <- pred.d$value
  }
  pred
}

shift <- function(train, test, threshold=1.1, shift=2){
  # This function executes a shift of the sales forecasts in the Christmas
  # period to reflect that the models are weekly, and that the day of the week
  # that Christmas occurs on shifts later into the week containing the holiday.
  #
  # NB: Train is actually not used here. Previously, there were other post-
  #     adjustments which did use it, and it is taken in here to preserve a 
  #     calling signature.
  #
  # args:
  # train - this is an n_weeks x n_stores matrix of values of Weekly_Sales
  #         for the training set within department, across all the stores
  # test - this is a (forecast horizon) x n_stores matrix of Weekly_Sales
  #        for the training set within department, across all the stores
  # threshold - the shift is executed if the mean of Weekly_Sales for weeks
  #          49-51 is greater than that for weeks 48 and 52 by at least
  #          a ratio of threshold
  # shift - The number of days to shift sales around Christmas.
  #         Should be 2 if the model is based on the last year only,
  #         or 2.5 if it uses both years
  #
  # returns:
  #  the test data 
  s <- ts(rep(0,39), frequency=52, start=c(2012,44))
  idx <- cycle(s) %in% 48:52
  holiday <- test[idx, 2:46]
  baseline <- mean(rowMeans(holiday[c(1, 5), ], na.rm=TRUE))
  surge <- mean(rowMeans(holiday[2:4, ], na.rm=TRUE))
  holiday[is.na(holiday)] <- 0
  if(is.finite(surge/baseline) & surge/baseline > threshold){
      shifted.sales <- ((7-shift)/7) * holiday
      shifted.sales[2:5, ] <- shifted.sales[2:5, ] + (shift/7) * holiday[1:4, ]
      shifted.sales[1, ] <- holiday[1, ]
      test[idx, 2:46] <- shifted.sales
  }
  test
}
