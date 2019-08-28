# Quantile Regression Real Data

Real data analysis using distributed quantile regression. The dataset is obtained from the [China Stock Market Accounting Research (CSMAR) Database](http://www.gtarsc.com/). We collect a total 25,326 observations from the period of 1999 to 2015, and each observation corresponds to one particular ﬁrm’s yearly ﬁnancial information. 

Required `R` version: `3.5.1`

Files:
- `estimator.R`: one-shot estimation and one-step estimation for distributed quantile regression
- `data_cleaning.R`: generate cleaned version of ROE data
- `uilts.R`: other functions used
- `main.R`: conduct estimation and generate plot

Please run `main.R` to see how to use the functions.
