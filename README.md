
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wateRtemp

<!-- badges: start -->

<!-- badges: end -->

A machine learning toolbox for river water temperature prediction.

## Installation

You can install the released version of wateRtemp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MoritzFeigl/wateRtemp")
```

## Example

This is a basic example of how to preprocess data and how to apply
different machine learning algorithms using wateRtemp

At first it is necessary to create a folder with the name of the
catchment or study area in which the preprocessed data and results will
be stored. For that we create here a dummy folder called “Catchment”.

``` r
dir.create("Catchment")
#> Warning in dir.create("Catchment"): 'Catchment' existiert bereits
```

For using watRtemp preprocessing and modelling functions, the necessary
data should be available as a data frame. An example data frame is
included in wateRtemp and can be called by `data("wt_data")`.

``` r
# Provide the catchment data as a data frame
data("wt_data")
# it should look like this:
str(wt_data)
#> 'data.frame':    3223 obs. of  11 variables:
#>  $ year : int  2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...
#>  $ mon  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ day  : int  10 11 13 14 15 16 17 18 19 20 ...
#>  $ Q    : num  6.46 25 38.9 21.7 16 14.2 12.8 11.5 10.4 9.67 ...
#>  $ RR   : num  13.42 39.48 0 0.7 6.74 ...
#>  $ Tmin : num  1.36 2.65 6.91 3.54 -0.01 -3.08 -5.44 -7.2 -11.6 -9.37 ...
#>  $ Tmax : num  11.04 14.95 12.98 10.97 4.04 ...
#>  $ Tmean: num  6.2 8.8 9.95 7.26 2.02 -0.72 -3.38 -6.08 -8.28 -5.95 ...
#>  $ wt   : num  6.12 7.16 7.78 7.57 6.22 ...
#>  $ date : POSIXct, format: "2007-01-17 16:00:00" "2007-01-18 16:00:00" ...
#>  $ GL   : num  37.87 26.51 9.82 47.4 35.77 ...
```

After loading the necessecery data, we can use the watRtemp
preprocessing function to apply feature engineering and data splits. The
preprocessed data will be saved in the Catchment folder automatically.

``` r
# Preprocess the data
wt_preprocess("Catchment", data = wt_data,
              year_range = c(2007, 2015))
#> Split data into 80% training/validation and 20% testing...
#> Done!
```

Now we are ready to apply our machine learning models. For this example
we run a stepwise linear model using the function `wt_lm` with the
specification `type = "step"`.

``` r
wt_lm(catchment = "Catchment",
      data_inputs = "simple", 
      type = "step", 
      user_name = "Example")
#> *** Starting computation for catchment Catchment ***
#> Create LM folder for catchment Catchment.
#> 
#> Model quality criteria: 
#> RMSE: 0.774
#> NSE: 0.962
#> Resulting model and model diagnostics are saved under /Catchment/LM/
```

Similar to linear models, we can easily apply other type of machine
learning models on the preprocessed data (including hyperparemeter
optimization):

  - `wt_randomforest` for random forests,

  - `wt_xgboost` for XGBoost (Extreme Gradient Boosting),

  - `wt_ann` for feed forward neural networks,

  - `wt_rnn` for recurrent neural networks: LSTMs and GRUs
