
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wateRtemp

<!-- badges: start -->

<!-- badges: end -->

A machine learning toolbox for daily mean river water temperature
prediction.

# Overview

watRtemp includes 7 machine learning models with already implemented
bayesian hyperparameter optimization. The main functions are:

  - `wt_preprocessing()` for preprocessing data for the machine learning
    models,

  - `wt_lm()` for muliple regression and step-wise linear regression,

  - `wt_randomforest()` for random forests,

  - `wt_xgboost()` for XGBoost (Extreme Gradient Boosting),

  - `wt_fnn()` for feedforward neural networks,

  - `wt_rnn()` for recurrent neural networks: LSTMs and GRUs

Additionally, prepared data sets of 10 central european catchments are
included. An overview of available data sets can be obtained by:

``` r
as.data.frame(data(package = "wateRtemp")$results)$Item
#>  [1] "Aschach"      "Donau"        "Enns"         "Erlauf"       "Inn"         
#>  [6] "Kleine_Muehl" "Saalach"      "Salzach"      "Traisen"      "Ybbs"
```

For further details on these catchment data sets refer to Feigl, M.,
Lebidzinski, K., Herrnegger, M. and Schulz, K.: Machine learning methods
for stream water temperature prediction.

## Installation

You can install the released version of wateRtemp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MoritzFeigl/wateRtemp")
```

## Example

This is a basic example of how to preprocess data and how to apply
different machine learning algorithms using wateRtemp.

For using watRtemp preprocessing and modelling functions, the necessary
data should be available as a data frame. Example data frames are
included in wateRtemp and can be called by `data("Aschach")`.

``` r
# Provide the catchment data as a data frame
data("Aschach")
# it should look like this:
summary(Aschach)
#>       year          month             day              Q          
#>  Min.   :2004   Min.   : 1.000   Min.   : 1.00   Min.   :  0.399  
#>  1st Qu.:2007   1st Qu.: 4.000   1st Qu.: 8.00   1st Qu.:  1.380  
#>  Median :2010   Median : 7.000   Median :16.00   Median :  2.040  
#>  Mean   :2010   Mean   : 6.567   Mean   :15.74   Mean   :  3.803  
#>  3rd Qu.:2013   3rd Qu.:10.000   3rd Qu.:23.00   3rd Qu.:  3.415  
#>  Max.   :2015   Max.   :12.000   Max.   :31.00   Max.   :127.000  
#>                                                                   
#>        P              Ta_min            Ta_max             Ta         
#>  Min.   : 0.000   Min.   :-19.800   Min.   :-9.250   Min.   :-13.730  
#>  1st Qu.: 0.000   1st Qu.: -0.050   1st Qu.: 6.065   1st Qu.:  3.100  
#>  Median : 0.150   Median :  5.450   Median :14.520   Median : 10.130  
#>  Mean   : 2.504   Mean   :  5.211   Mean   :13.923   Mean   :  9.569  
#>  3rd Qu.: 2.740   3rd Qu.: 11.040   3rd Qu.:21.540   3rd Qu.: 16.080  
#>  Max.   :54.540   Max.   : 20.020   Max.   :35.570   Max.   : 27.190  
#>                                                                       
#>        wt              GL         
#>  Min.   :-0.50   Min.   :  7.766  
#>  1st Qu.: 4.63   1st Qu.: 51.404  
#>  Median :10.85   Median :113.890  
#>  Mean   :10.78   Mean   :135.984  
#>  3rd Qu.:16.45   3rd Qu.:213.720  
#>  Max.   :26.08   Max.   :351.461  
#>                  NA's   :1074
```

After loading the necessecery data, we can use the watRtemp
preprocessing function to apply feature engineering and data splits. The
preprocessed data will be saved in the Catchment folder automatically.

``` r
# Preprocess the data
wt_preprocess(Aschach)
#> *** Preprocessing data of catchment Aschach ***
#> Split data into 80% training/validation and 20% testing...
#> Preprocessed data sets are stored in folder Aschach :
#> input_data.feather: full preprocessed data set in feather format
#> train_data.feather: first 80% of the preprocessed data set in feather format
#> test_data.feather: last 20% of the preprocessed data set in feather format
#> 
#> Preparing 2nd dataset with radiation for the whole time series
#> Split data into 80% training/validation and 20% testing...
#> Preprocessed data sets are stored in folder Aschach :
#> train_radiation_data.feather: first 80% of the preprocessed data set in feather format
#> test_radiation_data.feather: last 20% of the preprocessed data set in feather format
```

After preprocessing, the corresponding training and test datasets are
stored in the catchment folder and can be loaded for using them in the
models.

``` r
# Preprocess the data
train_data <- feather::read_feather("Aschach/train_data.feather")
test_data <- feather::read_feather("Aschach/test_data.feather")
```

Now we are ready to apply our machine learning models. For this example
we run the most simple model available in wateRtemp: a multiple
regression model using the function wt\_lm().

``` r
wt_lm(train_data, test_data, "Aschach", "LM", "repCV", "standard_LM")
#> *** Starting LM computation for catchment Aschach ***
#> Applying multiple linear regression
#> Saving prediction for train_data in Aschach/LM/LM/standard_LM/train_data_prediction.csv 
#> Saving prediction for test_data in Aschach/LM/LM/standard_LM/test_data_prediction.csv 
#> 
#> Model training performance: RMSE = 1.041, MAE = 0.814
#> Model testing performance: RMSE = 0.865, MAE = 0.703 
#> 
#> Model results are saved in /Aschach/LM/model_scores.csv
#> The trained model is saved in /Aschach/LM/LM/standard_LM/model.rds
```

Similar to multiple regression, we can easily apply other type of
machine learning models on the preprocessed data. All models in include
Bayesian hyperparemeter optimization and automatical storing of results
and trained models.

  - `wt_lm(type = "stepLM"`) for step-wise linear regression,

  - `wt_randomforest` for random forests,

  - `wt_xgboost` for XGBoost (Extreme Gradient Boosting),

  - `wt_ann` for feed forward neural networks,

  - `wt_rnn` for recurrent neural networks: LSTMs and GRUs
