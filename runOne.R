source('util.R')
source('grouped.forecast.R')
source('postprocess.R')

train <- raw.train()
test <- raw.test()

# This is the best single model from the competition.
pred <- grouped.forecast(train, test, 'stlf.svd', model.type='ets', n.comp=12)
pred <- postprocess(train, pred, shift=2.5)
snum <- write.submission(pred)
