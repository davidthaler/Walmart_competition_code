#Kaggle-Walmart Sales Forecasting  
This repository hosts R code for the winning entry in Kaggle's [Walmart sales forecasting competition](http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting). 

##Description  
There is a [Kaggle forum post](http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/forums/t/8023/thank-you-and-2-rank-model?page=2) explaining the winning entry. Briefly, it is an unweighted average of 6 component models, all of them weekly time-series models, followed by a transformation around Christmas to reflect that the day of the week that Christmas lands on shifts from year to year. That shift is explained [here](http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/forums/t/8028/a-key-adjustment).

##Requirements
* [R](http://www.r-project.org/)
* R packages forecast, plyr, and reshape - use install.packages('forecast'), etc. from within R
* [Walmart data](http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/data) - You will need train.csv, test.csv, and sampleSubmission.csv. You may have to accept a set of competition rules to get the data.

##Setup
The code assumes a directory structure under the project level of:  
* data/ - with the data from Kaggle
* R/ - with the code from here
* submissions/ - the code will write submissions here

##Usage:
You can regenerate the winning entry with source('runAll.R') from inside R. Without the fourier.arima model, the code runs in 30-40 minutes and gets about 2320 on the final leader board. With that model included, it generates the winning score of around 2300, but takes an extra 2 hours or so.  

To run the best performing single model from scratch:  

   source('util.R')  
   source('grouped.forecast')  
   source('util')  
   train <- raw.train()  
   test <- raw.test()  
   pred <- grouped.forecast(train, test, 'stlf.svd', model.type='ets', n.comp=12)  
   pred <- postprocess(train, test, shift=2.5)  
   write.submission(pred)  

If you are in the competition (or have accepted the rules), you should be able to submit that for 2348 on the final leaderboard by going to: [submit](http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/submissions/attach) and uploading the .csv file.
