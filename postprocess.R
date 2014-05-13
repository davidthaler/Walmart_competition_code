require(plyr)
require(reshape)

PNAMES <- c('shift', 'super.bowl', 'thanksgiving')

postprocess <- function(train, test, pname, ...){
  if(pname %in% PNAMES){
    f <- get(pname)
  }else{
    stop(pname,' not legal forecast option')
  }
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
    result <- f(tr.d, fc.d, ...)
    result <- melt(result)
    pred.d.idx <- pred$Dept==d
    pred.d <- pred[pred.d.idx, c('Store', 'Date')]
    pred.d <- join(pred.d, result)
    pred$Weekly_Sales[pred.d.idx] <- pred.d$value
  }
  pred
}

shift <- function(train, test, threshold=1.1, shift=2){
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

super.bowl <- function(train, test, wts=c(1,1), threshold=1.1){
  wts <- wts/sum(wts)
  dates <- as.Date(c('2010-02-12', '2011-02-11', '2012-02-10', '2013-02-08'))
  before <- which(train$Date %in% dates) - 1
  after <- which(train$Date %in% dates) + 1
  around <- union(before, after)
  train[is.na(train)] <- 0
  baseline <- colMeans(train[around, 2:46])
  sales <- colMeans(train[train$Date %in% dates, 2:46])
  fc <- as.numeric(test[test$Date %in% dates, 2:46])
  ratio <- mean(sales)/mean(baseline)
  if(is.finite(ratio) & (ratio > threshold)){
    test[test$Date %in% dates, 2:46] <- wts[1] * fc + wts[2] * sales
    print('adjusting')
  }
  test
}

thanksgiving <- function(train, test, wts=c(2,1), threshold=1.1){
  wts <- wts/sum(wts)
  dates <- as.Date(c('2010-11-26', '2011-11-25', '2012-11-23'))
  before <- which(train$Date %in% dates) - 1
  after <- which(train$Date %in% dates) + 1
  around <- union(before, after)
  train[is.na(train)] <- 0
  baseline <- colMeans(train[around, 2:46])
  sales <- colMeans(train[train$Date %in% dates, 2:46])
  fc <- as.numeric(test[test$Date %in% dates, 2:46])
  ratio <- mean(sales)/mean(baseline)
  if(is.finite(ratio) & (ratio > threshold)){
    test[test$Date %in% dates, 2:46] <- wts[1] * fc + wts[2] * sales
    print('adjusting')
  }
  test
}
