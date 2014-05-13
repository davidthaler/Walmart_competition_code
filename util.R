require(plyr)

extract.year<- function(dates){
  # returns a vector of the years, given a list of Dates
  sapply(strsplit(as.character(dates),'-'), as.numeric)[1,]
}

extract.month <- function(dates){
  # returns a vector of the month numbers, given a list of Dates
  sapply(strsplit(as.character(dates),'-'), as.numeric)[2,]
}

extract.day <- function(dates){
  # returns a vector day numbers in month, given a list of Dates
  sapply(strsplit(as.character(dates),'-'), as.numeric)[3,]
}

first.week <- function(dates){
  # returns a logical vector which is TRUE if the date is from the
  # first week of its month
  extract.day(dates) <= 7
}

extract.series <- function(train, store, dept){
  train.dates <- data.frame(Date=unique(train$Date))
  train.sd <- train[train$Store==store & train$Dept==dept, 
                    c('Date','Weekly_Sales')]
  train.dates <- join(train.dates, train.sd)
  ts(train.dates$Weekly_Sales, start=c(2010, 5), frequency=52)
}

wmae <- function(pred, test){
  w <- 4*test$IsHoliday + 1
  sum(w*abs(pred$Weekly_Sales - test$Weekly_Sales))/sum(w)
}

convert.pred <- function(test.data, pred){
  #Converts a prediction vector into a form that can be submitted
  #using write.submission, which requires a data frame.
  #Params:
  # test.data - the test data frame; this must have the same
  #             ordering as what was given to the predict routine
  # pred - the prediction vector to convert to a data frame
  # Returns:
  # A data frame with the index fields (Store, Dept, Date)
  # of the test set and the predictions joined on as Weekly_Sales.
  out <- test.data[c('Store', 'Dept', 'Date')]
  out$Weekly_Sales <- pred
  out
}

unique.loc <- function(data){
  # Params:
  # data - either train or test df
  # Returns:
  # list of tuples of unique store-dept pairs
  out <- list()
  stores <- unique(data$Store)
  for (s in stores){
    df <- data[ data$Store == s, ]
    depts <- unique(df$Dept)
    for (d in depts){
      tag <- paste0(s,'-',d)
      out[[tag]] <- c(s,d)
    }
  }
  out
}

