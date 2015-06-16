#Kaggle-Walmart Sales Forecasting  
This repository hosts R code for the winning entry in Kaggle's [Walmart sales forecasting competition](http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting). 

##Description  
There is a [Kaggle forum post](http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/forums/t/8125/first-place-entry) explaining the winning entry. Briefly, it is an unweighted average of 6 component models, all of them weekly time-series models, followed by a transformation around Christmas to reflect that the day of the week that Christmas lands on shifts from year to year. That shift is explained [here](http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/forums/t/8028/a-key-adjustment).   
For each model, the function grouped.forecast() is called with a parameter specifying the model type. That function iterates over the possible departments and for each one, it builds a data matrix of size (number of weeks x number of stores). That is, predictions are made per-department, across stores. This is because the pattern of seasonal variation is mainly within-department.

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
To regenerate the best performing single model, use the script runOne.R. If you are in the competition (or have accepted the rules), you should be able to submit that for 2366 on the final leaderboard by going to: [submit](http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/submissions/attach) and uploading the .csv file. It should run in ~ 5 minutes.

You can regenerate the winning entry with the script runAll.R. Without the fourier.arima model, the code runs in 30-40 minutes and gets about 2320 on the final leader board. With that model included, it generates the winning score of around 2300, but takes an extra 2 hours or so.  
