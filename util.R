require(plyr)

paths = list(data='~/Documents/Kaggle/Kaggle_Walmart/data/',
             submit='~/Documents/Kaggle/Kaggle_Walmart/submissions/',
             r='~/Documents/Kaggle/Kaggle_Walmart/R/')

sample.submission <- function(){
  ss <- read.csv(paste0(paths$data, 'sampleSubmission.csv'))
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


wmae <- function(pred, test){
  w <- 4*test$IsHoliday + 1
  sum(w*abs(pred$Weekly_Sales - test$Weekly_Sales))/sum(w)
}