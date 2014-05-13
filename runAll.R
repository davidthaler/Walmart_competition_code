source('util.R')
source('grouped.forecast.R')
source('postprocess.R')

train <- raw.train()
test <- raw.test()

# Make the 3 simple models, shift their values and average them.
# The shifted average gets 2503 on the final board.
# This first part runs in about 5 minutes.
simple.names <- c('tslm.basic', 'seasonal.naive', 'product')
shifts <- c(2.5, 2, 2)
simple.nums <- 0 * shifts
for(k in 1:3){
  print(paste('Predicting on model:', simple.names[k]))
  pred <- grouped.forecast(train, test, simple.names[k])
  print(paste('Shifting predictions for model:', simple.names[k]))
  pred <- postprocess(train, pred, shift=shifts[k])
  simple.nums[k] <- write.submission(pred)
}
pred <- make.average(simple.nums)
print('This is the shifted average of simple models.')
# keep the number, because this goes into the final model
sub.nums <- write.submission(pred) 

# This is model 5 from the post, regression on Fourier series terms with 
# non-seasonal arima errors. This model scores poorly on its own, but 
# improves the average anyway. This model is shifted by 1 because its
# period is 365/7, not 52. It is also very smooth, so the shift actually
# makes no difference here anyway.
#
# NB: This model may take a couple of hours to run
pred <- grouped.forecast(train, test, 'fourier.arima', k=12)
pred <- postprocess(train, pred, shift=1)
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# This is model 1 from the post. It gets 2348 on the final board.
pred <- grouped.forecast(train, test, 'stlf.svd', model.type='ets', n.comp=12)
pred <- postprocess(train, pred, shift=2.5)
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# This is model 2 from the post.
pred <- grouped.forecast(train, test, 'stlf.svd', model.type='arima', n.comp=12)
pred <- postprocess(train, pred, shift=2.5)
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# This is model 3 from the post, the one that averages predictions.
pred <- grouped.forecast(train, test, 'stlf.nn', k=5, level1=0.95, level2=0.8)
pred <- postprocess(train, pred, shift=2.5)
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# This is model 4, the seasonal arima model.
# It gets shifted 2 days, because all of the models are (p, d, q)(0, 1, 0)[52],
# they are like arima errors on a seasonal naive model.
# n.comp=15 is what I actually used.
pred <- grouped.forecast(train, test, 'seasonal.arima.svd', n.comp=15)
pred <- postprocess(train, pred, shift=2)
s.num <- write.submission(pred)
sub.nums <- c(sub.nums, s.num)

# This is the final result.
pred <- make.average(sub.nums)
final.num <- write.submission(pred)
print(paste0('The final average is submission', final.num, '.csv'))