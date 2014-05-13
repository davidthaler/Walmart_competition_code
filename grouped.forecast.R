require(plyr)
require(forecast)
require(reshape)

grouped.forecast <- function(train, test, fname, ...){
  # Iterates over the departments and calls a model function to make forecasts
  # on each of them.
  #
  # args:
  #  train - a data frame containing the data from train.csv, or part of it.
  #  test - a data frame like that returned by raw.test()
  #  fname - a string specifying which model function to call
  #
  # returns:
  #  a data frame corresponding to the test parameter, but with all of the 
  #  predictions in the Weekly_Sales field
  FNAMES <- c('seasonal.naive',
              'product',
              'stlf.svd',
              'fourier.arima',
              'stlf.nn',
              'seasonal.arima.svd',
              'tslm.basic')
  
  if(fname %in% FNAMES){
    f <- get(fname)
  }else{
    stop(fname,' not legal forecast option')
  }
  if('Weekly_Sales' %in% names(test)){
    test <- subset(test, select=-Weekly_Sales)
  }
  
  test.dates <- unique(test$Date)
  num.test.dates <- length(test.dates)
  all.stores <- unique(test$Store)
  num.stores <- length(all.stores)
  test.depts <- unique(test$Dept)
  #reverse the depts so the grungiest data comes first
  test.depts <- test.depts[length(test.depts):1]
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
    tr.d <- train.frame
    # This joins in Weekly_Sales but generates NA's. Resolve NA's 
    # in the model because they are resolved differently in different models.
    tr.d <- join(tr.d, train[train$Dept==d, c('Store','Date','Weekly_Sales')])
    tr.d <- cast(tr.d, Date ~ Store)    
    fc.d <- forecast.frame
    fc.d$Weekly_Sales <- 0
    fc.d <- cast(fc.d, Date ~ Store)
    result <- f(tr.d, fc.d, ...)
    # This has all Stores/Dates for this dept, but may have some that
    # don't go into the submission.
    result <- melt(result)
    pred.d.idx <- pred$Dept==d
    #These are the Store-Date pairs in the submission for this dept
    pred.d <- pred[pred.d.idx, c('Store', 'Date')]
    pred.d <- join(pred.d, result)
    pred$Weekly_Sales[pred.d.idx] <- pred.d$value
  }
  pred
}

seasonal.naive <- function(train, test){
  # Computes seasonal naive forecasts
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  #
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  h <- nrow(test)
  tr <- train[nrow(train) - (52:1) + 1,]
  tr[is.na(tr)] <- 0
  test[,2:ncol(test)]  <- tr[1:h,2:ncol(test)]
  test
}

product <- function(train, test){
  # Computes forecasts with the product model. This model predicts the mean
  # value by store times the mean value by week divided by the mean value
  # over the department.
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  #
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  h <- nrow(test)
  tr <- train[nrow(train) - (52:1) + 1,]
  tr[is.na(tr)] <- 0
  levels <- colMeans(tr[,2:ncol(tr)])
  profile <- rowMeans(tr[,2:ncol(tr)])
  overall <- mean(levels)
  pred <- matrix(profile, ncol=1) %*% matrix(levels, nrow=1)
  pred <- pred / overall
  test[,2:ncol(test)] <- pred[1:h,]
  test
}

tslm.basic <- function(train, test){
  # Computes a forecast using linear regression and seasonal dummy variables
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  #
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  horizon <- nrow(test)
  train[is.na(train)] <- 0
  for(j in 2:ncol(train)){
    s <- ts(train[, j], frequency=52)
    model <- tslm(s ~ trend + season)
    fc <- forecast(model, h=horizon)
    test[, j] <- as.numeric(fc$mean)
  }
  test
}

stlf.svd <- function(train, test, model.type, n.comp){
  # Replaces the training data with a rank-reduced approximation of itself,
  # then forecasts each store using stlf() from the forecast package.
  # That function performs an STL decomposition on each series, seasonally
  # adjusts the data, non-seasonally forecasts the seasonally adjusted data,
  # and then adds in the naively extended seasonal component to get the
  # final forecast.
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  # model.type - one of 'ets' or 'arima', specifies which type of model to
  #        use for the non-seasonal forecast
  # n.comp - the number of components to keep in the singular value
  #         decomposition that is performed for preprocessing
  #
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  horizon <- nrow(test)
  train <- preprocess.svd(train, n.comp) 
  for(j in 2:ncol(train)){
    s <- ts(train[, j], frequency=52)
    if(model.type == 'ets'){
      fc <- stlf(s, 
                 h=horizon, 
                 s.window=3, 
                 method='ets',
                 ic='bic', 
                 opt.crit='mae')
    }else if(model.type == 'arima'){
      fc <- stlf(s, 
                 h=horizon, 
                 s.window=3, 
                 method='arima',
                 ic='bic')
    }else{
      stop('Model type must be one of ets or arima.')
    }
    pred <- as.numeric(fc$mean)
    test[, j] <- pred
  }
  test
}

stlf.nn <- function(train, test, method='ets', k, level1, level2){
  # Function standard scales the series and computes a correlation matrix.
  # Then it forecasts each store using stlf() from the forecast package.
  # That function performs an STL decomposition on each series, seasonally
  # adjusts the data, non-seasonally forecasts the seasonally adjusted data,
  # and then adds in the naively extended seasonal component to get the
  # final forecast.
  # Finally, it averages together some of the most correlated series before
  # restoring the original scale.
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  # method - one of 'ets' or 'arima', specifies which type of model to
  #        use for the non-seasonal forecast
  # level1 - all series correlated to this level are used in the average
  # level2 - no series are used if they are correlated to less than this level
  # k - up to k series that are above level2 will be selected
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  horizon <- nrow(test)
  tr <- train[, 2:ncol(train)]
  tr[is.na(tr)] <- 0
  crl <- cor(tr)
  tr.scale <- scale(tr)
  tr.scale[is.na(tr.scale)] <- 0
  raw.pred <- test[, 2:ncol(test)]
  for(j in 1:ncol(tr)){
    s <- ts(tr.scale[, j], frequency=52)
    if(method == 'ets'){
      fc <- stlf(s, 
                 h=horizon, 
                 s.window=3, 
                 method='ets',
                 ic='bic', 
                 opt.crit='mae')
    }else if(method == 'arima'){
      fc <- stlf(s, 
                 h=horizon, 
                 s.window=3, 
                 method='arima',
                 ic='bic')
    }
    raw.pred[, j] <- fc$mean
  }
  for(j in 1:ncol(tr)){
    o <- order(crl[j, ], decreasing=TRUE)
    score <- sort(crl[j, ], decreasing=TRUE)
    if(length(o[score >= level1]) > k){
      top.idx <- o[score >= level1]
    }else{
      top.idx <- o[score >= level2]
      top.idx <- top.idx[1:min(length(top.idx),k)]
    }
    top <- raw.pred[, top.idx]
    if (length(top.idx) > 1){
      pred <- rowMeans(top)
    }else{
      pred <- as.numeric(top)
    }
    pred <- pred * attr(tr.scale, 'scaled:scale')[j]
    pred <- pred + attr(tr.scale, 'scaled:center')[j]
    test[, j + 1] <- pred
  }
  test
}

fourier.arima <- function(train, test, k){
  # This model is a regression on k sin/cos pairs of Fourier series terms
  # with non-seasonal arima errors. The call to auto.arima() crashes on data
  # with too many missing values, or too many identical values, so this 
  # function falls back to another, more stable method in that case.
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  # k - number of sin/cos pair to use
  #
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  horizon <- nrow(test)
  for(j in 2:ncol(train)){
    if(sum(is.na(train[, j])) > nrow(train)/3){
      test[, j] <- fallback(train[,j], horizon)
      print(paste('Fallback on store:', names(train)[j]))
    }else{
      # fit arima model
      s <- ts(train[, j], frequency=365/7)
      model <- auto.arima(s, xreg=fourier(s, k), ic='bic', seasonal=FALSE)
      fc <- forecast(model, h=horizon, xreg=fourierf(s, k, horizon))
      test[, j] <- as.numeric(fc$mean)
    }
  }
  test
}

seasonal.arima.svd <- function(train, test, n.comp){
  # Replaces the training data with a rank-reduced approximation of itself
  # and then produces seasonal arima forecasts for each store.
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  # n.comp - the number of components to keep in the singular value
  #         decomposition that is performed for preprocessing
  #
  # returns:
  #  the test(forecast) data frame with the forecasts filled in 
  horizon <- nrow(test)
  tr <- preprocess.svd(train, n.comp)
  for(j in 2:ncol(tr)){
    if(sum(is.na(train[, j])) > nrow(train)/3){
      # Use DE model as fallback
      test[, j] <- fallback(tr[,j], horizon)
      store.num <- names(train)[j]
      print(paste('Fallback on store:', store.num))
    }else{
      # fit arima model
      s <- ts(tr[, j], frequency=52)
      model <- auto.arima(s, ic='bic', seasonal.test='ch')
      fc <- forecast(model, h=horizon)
      test[, j] <- as.numeric(fc$mean)
    }
  }
  test
}

fallback <- function(train, horizon){
  # This method is a fallback forecasting method in the case that there are
  # enough NA's to possibly crash arima models. It takes one seasonal 
  # difference, forecasts with a level-only exponential model, and then
  # inverts the seasonal difference.
  # 
  # args:
  # train - a vector of training data for one store
  # horizon - the forecast horizon in weeks
  #
  # returns:
  #  a vector of forecast values
  s <- ts(train, frequency=52)
  s[is.na(s)] <- 0
  fc <- ses(diff(s, 52), h=horizon)
  result <- diffinv(fc$mean, lag=52, xi=s[length(s) - 51:0])
  result[length(result) - horizon:1 + 1]
}

preprocess.svd <- function(train, n.comp){
  # Replaces the training data with a rank-reduced approximation of itself.
  # This is for noise reduction. The intuition is that characteristics
  # that are common across stores (within the same department) are probably
  # signal, while those that are unique to one store may be noise.
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # n.comp - the number of components to keep in the singular value
  #         decomposition
  #
  # returns:
  #  the rank-reduced approximation of the training data
  train[is.na(train)] <- 0
  z <- svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
  s <- diag(z$d[1:n.comp])
  train[, 2:ncol(train)] <- z$u %*% s %*% t(z$v)
  train
}