require(plyr)

#TODO: set data, submit and r paths to correct directories, if needed
paths = list(data='../data/',
             submit='../submissions/',
             r='../R/')

sample.submission <- function(){
  # Loads the sample submission, which is used in writing predictions
  ss <- read.csv(paste0(paths$data, 'sampleSubmission.csv'))
}

raw.train <- function(){
  # Loads the training data with correct classes
  cls <- c('factor', 'factor', 'Date', 'numeric', 'logical')
  train <- read.csv(paste0(paths$data, 'train.csv'), 
                    colClasses=cls)
}

raw.test <- function(){
  # Loads the test data with correct column types
  cls <- c('factor', 'factor', 'Date', 'logical')
  test <- read.csv(paste0(paths$data, 'test.csv'), 
                   colClasses=cls)
}

reload.submission <- function(submit.num){
  # Reloads a previously saved submission
  #
  # args:
  #  submit.num - the number of the submission
  #
  # returns:
  #  the saved submission as a data frame (with Id $ Weekly_Sales fields)
  submit.path <- paste0(paths$submit, 'submission', submit.num, '.csv')
  read.csv(submit.path)
}

make.average <- function(submissions, wts=NULL){
  # Averages previously saved submissions.
  #
  # args:
  #  submissions - a vector of submission numbers
  #  wts - optional vector of weights for submissions
  #
  # returns:
  #  a data frame with the weighted average of the Weekly_Sales fields
  #  from the submissions as its Weekly_Sales field
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
  # Writes a valid submission to paths$submit.
  #
  # args:
  #  pred - a data frame with predictions in the Weekly_Sales field
  #
  # returns:
  #  the submission number used
  ss <- sample.submission()
  subs <- dir(paths$submit)
  subs <- grep('submission[0-9]+(.csv)(.zip|.gz)?', subs, value=TRUE)
  nums <- gsub('submission','', gsub('(.csv)(.zip|.gz)?','', subs))
  if(length(nums) == 0){
    submission.number <- 1
  }else{
    submission.number <- max(as.numeric(nums)) + 1
  }
  ss$Weekly_Sales <- pred$Weekly_Sales
  submit.path = paste0(paths$submit, 
                       'submission', 
                       submission.number,
                       '.csv')
  print(paste('Writing to:', submit.path))
  write.csv(ss, file = submit.path, quote=FALSE, row.names=FALSE)
  submission.number
}

wmae <- function(pred, test){
  # Computes the evaluation metric for Kaggle/Walmart.
  #
  # args:
  #  pred - a data frame with predictions in the Weekly_Sales field
  #  test - a data frame with an IsHoliday field and with the ground truth
  #         in the Weekly_Sales field
  # returns:
  #  wmae - the weighted mean absolute error
  w <- 4*test$IsHoliday + 1
  sum(w*abs(pred$Weekly_Sales - test$Weekly_Sales))/sum(w)
}