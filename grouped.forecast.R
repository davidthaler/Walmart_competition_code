require(plyr)
require(forecast)
require(reshape)

grouped.forecast <- function(train, test, fname, ...){
  
  FNAMES <- c('hook',
              'seasonal.naive',
              'naive2',
              'product',
              'svd.model',
              'de',
              'stlf.svd',
              'fourier.arima',
              'fourier.detrend',
              'stlf.nn',
              'seasonal.arima.svd',
              'tslm.svd',
              'stl.svd',
              'naive.loess')
  
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
#     result <- tryCatch(f(tr.d, fc.d, ...), 
#                        error=function(e) {
#                          print(paste('exception on dept:',d))
#                          de(tr.d, fc.d)
#                        })
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

hook <- function(train, test){
  browser()
  test
}

stl.svd <- function(train, test, n.comp, s.deg=0, s.win=3){
  horizon <- nrow(test)
  tr <- preprocess.svd(train, n.comp)
  for(j in 2:ncol(tr)){
    s <- ts(tr[, j], frequency=52)
    dc <- stl(s, s.window=s.win, s.degree=s.deg, l.window=9)
    fc <- forecast(dc, h=horizon, ic='bic', opt.crit='mae')
    test[,j] <- as.numeric(fc$mean)
  }
  test
}

seasonal.naive <- function(train, test){
  all <- rbind(train, test)
  pred.idx <- (nrow(train) + 1):nrow(all)
  all[is.na(all)] <- 0
  for(k in pred.idx){
    all[k, 2:ncol(all)] <- all[k-52, 2:ncol(all)]
  }
  all[pred.idx,]
}

naive2 <- function(train, test){
  h <- nrow(test)
  tr <- train[nrow(train) - (52:1) + 1,]
  tr[is.na(tr)] <- 0
  test[,2:ncol(test)]  <- tr[1:h,2:ncol(test)]
  test
}

naive.loess <- function(train, test, tgt=30){
  h <- nrow(test)
  train[is.na(train)] <- 0
  for(j in 2:ncol(train)){
    d <- data.frame('y'=train[,j], 't'=(2.75/nrow(train))*(1:nrow(train)))
    l <- loess(y ~ t, data=d, enp.target=tgt)
    train[, j] <- fitted(l)
  }
  tr <- train[nrow(train) - (52:1) + 1,]
  test[,2:ncol(test)]  <- tr[1:h,2:ncol(test)]
  test
}

product <- function(train, test){
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

svd.model <- function(train, test, n.comp){
  h <- nrow(test)
  tr <- train[nrow(train) - (52:1) + 1, 2:ncol(train)]
  tr2 <- preprocess.svd(tr, n.comp)
  test[, 2:ncol(test)] <- tr2[1:h, ]
  test
}

preprocess.svd <- function(train, n.comp){
  train[is.na(train)] <- 0
  z <- svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
  s <- diag(z$d[1:n.comp])
  train[, 2:ncol(train)] <- z$u %*% s %*% t(z$v)
  train
}

stlf.svd <- function(train, test, model.type, n.comp){
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
  # level1 - a correlation cutoff above this level, take everything
  # -or-
  # k - take up to k components...
  # level2 - that are above level2
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

fourier.detrend <- function(train, test, s.win, k=12){
  horizon <- nrow(test)
  # not doing svd for now
  for(j in 2:ncol(train)){
    if(sum(is.na(train[,j])) > nrow(train)/3){
      test[, j] <- fallback.linear(train[,j], horizon, k)
      print(paste('Fallback to linear model on store:', names(train)[j]))
    }else{
      s <- ts(train[, j], frequency=52)
      s[is.na(s)] <- 0
      dc <- stl(s, s.window=s.win)
      s.comp <- dc$time.series[,1]
      model1 <- auto.arima(s.comp, 
                           xreg=fourier(s.comp, k), 
                           ic='bic', 
                           seasonal=FALSE)
      fc1 <- forecast(model1, h=horizon, xreg=fourierf(s.comp, k, horizon))
      model2 <- ets(seasadj(dc), model='ZZN', opt.crit='mae', ic='bic')
      fc2 <- forecast(model2, h=horizon)
      pred <- fc1$mean + fc2$mean
      test[,j] <- as.numeric(pred)
    }
  }
  test
}

fourier.arima <- function(train, test, n.comp, k){
  horizon <- nrow(test)
  tr <- preprocess.svd(train, n.comp)
  for(j in 2:ncol(tr)){
    #train still has its NA's
    if(sum(is.na(train[, j])) > nrow(train)/3){
      test[, j] <- fallback.linear(tr[,j], horizon, 12)
      print(paste('Fallback to linear model on store:', names(train)[j]))
    }else{
      # fit arima model
      s <- ts(tr[, j], frequency=365/7)
      model <- auto.arima(s, xreg=fourier(s, k), ic='bic', seasonal=FALSE)
      fc <- forecast(model, h=horizon, xreg=fourierf(s, k, horizon))
      test[, j] <- as.numeric(fc$mean)
    }
  }
  test
}

seasonal.arima.svd <- function(train, test, n.comp){
  horizon <- nrow(test)
  tr <- preprocess.svd(train, n.comp)
  for(j in 2:ncol(tr)){
    if(sum(is.na(train[, j])) > nrow(train)/3){
      # Use DE model as fallback
      test[, j] <- fallback(tr[,j], horizon)
      store.num <- names(train)[j]
      print(paste('Fallback to DE model on store:', store.num))
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
  #NB: train should be a column
  s <- ts(train, frequency=52)
  s[is.na(s)] <- 0
  fc <- ses(diff(s, 52), h=horizon)
  result <- diffinv(fc$mean, lag=52, xi=s[length(s) - 51:0])
  result[length(result) - horizon:1 + 1]
}

fallback.linear <- function(train, horizon, k){
  s <- ts(train, frequency=52)
  s[is.na(s)] <- 0
  f <- fourier(s, k)
  model <- tslm(s ~ trend + f)
  f <- fourierf(s, k, horizon)
  fc <- forecast(model, h=horizon)
  as.numeric(fc$mean)
}

de <- function(train, test, n.comp){
  horizon <- nrow(test)
  train <- preprocess.svd(train, n.comp)
  for(j in 2:ncol(train)){
    s <- ts(train[, j], frequency=52)
    fc <- ses(diff(s, 52), h=horizon)
    result <- diffinv(fc$mean, lag=52, xi=s[length(s) - 51:0])
    test[, j] <- result[length(result) - horizon:1 + 1]
  }
  test
}

tslm.svd <- function(train, test, n.comp){
  horizon <- nrow(test)
  train <- preprocess.svd(train, n.comp)
  for(j in 2:ncol(train)){
    s <- ts(train[, j], frequency=52)
    model <- tslm(s ~ trend + season)
    fc <- forecast(model, h=horizon)
    test[, j] <- as.numeric(fc$mean)
  }
  test
}
