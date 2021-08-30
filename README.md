
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wateRtemp - A toolbox for stream water temperature prediction

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/186992552.svg)](https://zenodo.org/badge/latestdoi/186992552)
[![R-CMD-check](https://github.com/MoritzFeigl/wateRtemp/workflows/R-CMD-check/badge.svg)](https://github.com/MoritzFeigl/wateRtemp/actions)
<!-- badges: end -->

wateRtemp is a machine learning toolbox for mean daily stream water
temperature prediction, which was used to produce all results of the
publication [Machine-learning methods for stream water temperature
prediction (Feigl et al.,
2021)](https://doi.org/10.5194/hess-25-2951-2021). Please refer to this
publication for detailed descriptions of the applied models and
preprocessing steps. The code used to create the figures and tables of
this publication is available
[here](github.com/MoritzFeigl/ML_methods_for_stream_water_temperature_prediction)
and might be interesting for further analyzing wateRtemp results.

If you have any questions regarding or want to report issues with the
code, please do not hesitate to create an issue under
[wateRtemp/issues](https://github.com/MoritzFeigl/wateRtemp/issues). I
will update this library whenever new issues are posted.

Last update: August 30, 2021

# Overview

wateRtemp includes 6 machine learning models with bayesian
hyperparameter optimization:

-   Multiple and step-wise linear regression
-   Random forest
-   XGBoost (Extreme Gradient Boosting)
-   Feedforward neural networks
-   Recurrent neural networks:
    -   Long short-term memory networks (LSTMs)
    -   Gated recurrent units networks(GRUs)

The main functions are:

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
```

The provided data should be a data frame which includes the columns:

-   `year`, `month`, `day`: The data given as three columns of integers.
-   `wt`: The mean daily stream water temperature data as numeric.
-   additional columns containing covariates, e.g. precipitation,
    discharge, etc. The column names of these variables can be arbitrary
    chosen and will not affect the wateRtemp functions.

For example, the test catchment has following structure:

``` r
str(test_catchment)
#> 'data.frame':    6851 obs. of  10 variables:
#>  $ year  : int  1997 1997 1997 1997 1997 1997 1997 1997 1997 1997 ...
#>  $ month : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ day   : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ Q     : num  14.3 51.3 56.8 27.6 12.1 41.4 62 64.6 58.3 49.5 ...
#>  $ P     : num  3.88 5.96 4.16 1.71 0.06 3.19 0.66 0.11 0.51 0 ...
#>  $ Ta_min: num  -14.67 -13.33 -6.71 -12.79 -14.84 ...
#>  $ Ta_max: num  -5.57 -5.25 -2.29 -4.25 -7.11 -7.22 -5.44 -5.6 -6 -6.22 ...
#>  $ Ta    : num  -10.12 -9.29 -4.5 -8.52 -10.98 ...
#>  $ wt    : num  0.9 1.5 2.2 1.9 1.6 1.8 2.3 2.4 2.5 2.4 ...
#>  $ GL    : num  NA NA NA NA NA NA NA NA NA NA ...
```

After loading the necessary data, we can use the watRtemp preprocessing
function to apply feature engineering and data splits. The preprocessed
data will be saved in the Catchment folder automatically. This function
generates training and test data sets with the given fraction for the
training data and automatically computes lagged version of all variables
(except wt and time variables) with the given number of lags (nlags)

``` r
# Preprocess the data
wt_preprocess(test_catchment, nlags = 4, training_fraction = 0.8)
#> *** Preprocessing data of catchment test_catchment ***
#> Preprocessed data sets are stored in folder test_catchment :
#> input_data.feather: full preprocessed data set in feather format
#> train_data.feather: first 80 % of the preprocessed data set in feather format
#> test_data.feather: last 20 % of the preprocessed data set in feather format
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
regression model using the function wt\_lm(). All results and
hyperparameter optimization scores are stored in the `test_catchment`
folder, which is automatically created in the current working directory.

``` r
wt_lm(train_data = train_data, 
      test_data = test_data, 
      catchment = "test_catchment", 
      type = "LM", 
      cv_mode = "repCV", 
      model_name = "standard_LM")
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

## Citation

If you use any of this code in your experiments, please make sure to
cite the following publication

    @article{Feigl2021,
    author = {Feigl, Moritz and Lebiedzinski, Katharina and Herrnegger, Mathew and Schulz, Karsten},
    doi = {10.5194/HESS-25-2951-2021},
    journal = {Hydrology and Earth System Sciences},
    month = {may},
    number = {5},
    pages = {2951--2977},
    publisher = {Copernicus GmbH},
    title = {{Machine-learning methods for stream water temperature prediction}},
    volume = {25},
    year = {2021}
    }

## License of the code

[Apache License
2.0](https://github.com/MoritzFeigl/wateRtemp/blob/master/LICENSE.md)
