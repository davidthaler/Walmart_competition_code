require(plyr)
source('util.R')

paths = list(data='~/Documents/Kaggle/Walmart/data/',
             submit='~/Documents/Kaggle/Walmart/submissions/',
             r='~/Documents/Kaggle/Walmart/R/',
             valid='~/Documents/Kaggle/Walmart/validation/')

sample.submission <- function(){
  ss <- read.csv(paste0(paths$data, 'sampleSubmission.csv'))
}

write.submission <- function(pred){
  ss <- sample.submission()
  subs <- dir(paths$submit)
  subs <- grep('submission[0-9]+(.csv)(.zip|.gz)?', subs, value=TRUE)
  nums <- gsub('submission','', gsub('(.csv)(.zip|.gz)?','', subs))
  submission.number <- max(as.numeric(nums))
  if(is.na(submission.number)){
    submission.number <- 0
  }
  submission.number <- submission.number + 1
  ss$Weekly_Sales <- pred$Weekly_Sales
  submit.path = paste0(paths$submit, 
                       'submission', 
                       submission.number,
                       '.csv')
  print(paste('Writing to:', submit.path))
  write.csv(ss, file = submit.path, quote=FALSE, row.names=FALSE)
}

write.validation <- function(submit.name, pred){
  submit.path = paste0(paths$valid, submit.name,'.csv')
  write.csv(pred, file = submit.path, quote=FALSE, row.names=FALSE)
}

raw.train <- function(){
  cls <- c('factor', 'factor', 'Date', 'numeric', 'logical')
  train <- read.csv(paste0(paths$data, 'train.csv'), 
                    colClasses=cls)
}

raw.test <- function(){
  cls <- c('factor', 'factor', 'Date', 'logical')
  test <- read.csv(paste0(paths$data, 'test.csv'), 
                    colClasses=cls)
}

raw.stores <- function(){
  cls <- c('factor', 'factor', 'numeric')
  test <- read.csv(paste0(paths$data, 'stores.csv'), 
                   colClasses=cls) 
}

raw.features <- function(){
  cls <- c('factor', 'Date', 'numeric', 'numeric',
           'numeric','numeric','numeric','numeric',
           'numeric','numeric', 'numeric', 'logical')
  test <- read.csv(paste0(paths$data, 'features.csv'), 
                   colClasses=cls)
}

get.train <- function(){
  x <- join(raw.train(), raw.stores())
  train <- join(x, raw.features())
}

get.test <- function(){
  x <- join(raw.test(), raw.stores())
  test <- join(x, raw.features())
}

short.split <- function(stores=c(5,10,15,20), 
                        split = '2011-10-26', 
                        raw=FALSE){
  split <- get.split(split, raw)
  tr.idx <- split$train$Store %in% stores
  val.idx <- split$val$Store %in% stores
  split$train <- split$train[tr.idx,]
  split$val <- split$val[val.idx,]
  split
}

get.split <- function(split = '2011-11-04', raw=FALSE){
  if(raw){
    data <- raw.train()
  }else{
    data <- get.train()
  }
  tr <- data[data$Date < as.Date(split),]
  val <- data[data$Date >= as.Date(split),]
  list(train=tr, val=val)
}

reload.validation <- function(submit.name){
  read.csv(paste0(paths$valid, submit.name, '.csv'))
}

reload.submission <- function(submit.num){
  submit.path <- paste0(paths$submit, 'submission', submit.num, '.csv')
  read.csv(submit.path)
}

make.average <- function(submissions, wts=NULL){
  if(is.null(wts)){
    wts <- rep(1, length(submissions))
  }
  pred <- sample.submission()
  for(k in 1:length(submissions)){
    sub.k <- reload.submission(submissions[k])
    pred.k <- wts[k] * sub.k$Weekly_Sales
    pred$Weekly_Sales <- pred$Weekly_Sales + pred.k
  }
  pred$Weekly_Sales <- pred$Weekly_Sales/sum(wts)
  pred
}




