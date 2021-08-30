
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wateRtemp

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/186992552.svg)](https://zenodo.org/badge/latestdoi/186992552)
[![R-CMD-check](https://github.com/MoritzFeigl/wateRtemp/workflows/R-CMD-check/badge.svg)](https://github.com/MoritzFeigl/wateRtemp/actions)
<!-- badges: end -->

A machine learning toolbox for daily mean river water temperature
prediction.

# Overview

watRtemp includes 6 machine learning models with already implemented
bayesian hyperparameter optimization. The main functions are:

-   `wt_preprocessing()` for preprocessing data for the machine learning
    models,

-   `wt_lm()` for muliple regression and step-wise linear regression,

-   `wt_randomforest()` for random forests,

-   `wt_xgboost()` for XGBoost (Extreme Gradient Boosting),

-   `wt_fnn()` for feedforward neural networks,

-   `wt_rnn()` for recurrent neural networks: LSTMs and GRUs

Additionally, a prepared synthetic data set for testing wateRtemp
functionalities is included and can be used by running:

``` r
data("test_catchment")
```

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
included in wateRtemp and can be called by `data("test_catchment")`.

``` r
# Provide the catchment data as a data frame
data("test_catchment")
# it should look like this:
summary(test_catchment)
#>       year          month             day              Q         
#>  Min.   :1997   Min.   : 1.000   Min.   : 1.00   Min.   :  4.94  
#>  1st Qu.:2001   1st Qu.: 4.000   1st Qu.: 8.00   1st Qu.: 28.65  
#>  Median :2006   Median : 7.000   Median :16.00   Median : 47.00  
#>  Mean   :2006   Mean   : 6.582   Mean   :15.73   Mean   : 59.26  
#>  3rd Qu.:2011   3rd Qu.:10.000   3rd Qu.:23.00   3rd Qu.: 76.60  
#>  Max.   :2015   Max.   :12.000   Max.   :31.00   Max.   :351.00  
#>                                                                  
#>        P              Ta_min           Ta_max              Ta          
#>  Min.   : 0.000   Min.   :-26.84   Min.   :-20.450   Min.   :-23.5900  
#>  1st Qu.: 0.000   1st Qu.: -9.09   1st Qu.: -1.575   1st Qu.: -5.2900  
#>  Median : 0.140   Median : -3.46   Median :  4.710   Median :  0.6100  
#>  Mean   : 2.558   Mean   : -4.15   Mean   :  4.391   Mean   :  0.1205  
#>  3rd Qu.: 2.075   3rd Qu.:  1.50   3rd Qu.: 10.380   3rd Qu.:  5.8300  
#>  Max.   :79.310   Max.   : 10.22   Max.   : 21.980   Max.   : 15.6700  
#>                                                                        
#>        wt               GL        
#>  Min.   : 0.000   Min.   : 17.20  
#>  1st Qu.: 2.714   1st Qu.: 74.48  
#>  Median : 6.061   Median :145.49  
#>  Mean   : 6.005   Mean   :152.84  
#>  3rd Qu.: 9.046   3rd Qu.:221.68  
#>  Max.   :13.645   Max.   :360.91  
#>                   NA's   :3566
```

After loading the necessary data, we can use the watRtemp preprocessing
function to apply feature engineering and data splits. The preprocessed
data will be saved in the Catchment folder automatically.

``` r
# Preprocess the data
wt_preprocess(test_catchment, nlags = 4, training_test_fractions = c(0.8, 0.2))
#> *** Preprocessing data of catchment test_catchment ***
#> Preprocessed data sets are stored in folder test_catchment :
#> input_data.feather: full preprocessed data set in feather format
#> train_data.feather: first 80 % of the preprocessed data set in feather format
#> test_data.feather: last 20 % of the preprocessed data set in feather format
#> 
#> Preparing 2nd dataset with radiation for the whole time series
#> Preprocessed data sets are stored in folder test_catchment :
#> train_radiation_data.feather: first 80 % of the preprocessed data set in feather format
#> test_radiation_data.feather: last 20 % of the preprocessed data set in feather format
```

After preprocessing, the corresponding training and test datasets are
stored in the catchment folder and can be loaded for using them in the
models.

``` r
# Preprocess the data
train_data <- feather::read_feather("test_catchment/train_data.feather")
test_data <- feather::read_feather("test_catchment/test_data.feather")
```

Now we are ready to apply our machine learning models. For this example
we run the most simple model available in wateRtemp: a multiple
regression model using the function wt\_lm().

``` r
wt_lm(train_data, test_data, "test_catchment", "LM", "repCV", "standard_LM")
#> *** Starting LM computation for catchment test_catchment ***
#> Applying multiple linear regression
#> Saving prediction for train_data in test_catchment/LM/LM/standard_LM/train_data_prediction.csv 
#> Saving prediction for test_data in test_catchment/LM/LM/standard_LM/test_data_prediction.csv 
#> 
#> Model training performance: RMSE = 0.61, MAE = 0.484
#> Model testing performance: RMSE = 0.718, MAE = 0.564 
#> 
#> Model results are saved in /test_catchment/LM/model_scores.csv
#> The trained model is saved in /test_catchment/LM/LM/standard_LM/model.rds
```

Similar to multiple regression, we can easily apply other type of
machine learning models on the preprocessed data. All models in include
Bayesian hyperparemeter optimization and automatically storing of
results and trained models.

-   `wt_lm(type = "stepLM")` for step-wise linear regression,

-   `wt_randomforest()` for random forests,

-   `wt_xgboost()` for XGBoost (Extreme Gradient Boosting),

-   `wt_ann()` for feed forward neural networks,

-   `wt_rnn()` for recurrent neural networks: LSTMs and GRUs
