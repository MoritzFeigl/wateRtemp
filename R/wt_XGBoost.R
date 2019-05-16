#' wt_XGBoost
#'
#' @param catchment
#' @param model_or_optim
#' @param cv_mode
#' @param no.cores
#' @param n_iter
#' @param plot_ts
#' @param save_importance_plot
#'
#' @return
#' @export
#'
#' @examples
wt_XGBoost <- function(catchment, model_or_optim, cv_mode, no.cores, n_iter = 30,
                            plot_ts = FALSE, save_importance_plot = FALSE){
  old_wd <- getwd()
  setwd(paste0("/media/cfgrammar/data/Dropbox/WT_Project/Modelling/data/", catchment))

  # 1. Data --------------------------------------------------------------------------------
  #setwd("D:/WT/z_Sicherung_Dropbox/WT_Project")
  #setwd("C:/Users/h0740567/Dropbox/WT_Project")
  cat("Loading catchment data.\n")
  library(feather)
  data <- read_feather("input_data_V2.feather")
  train <- read_feather("train_data_V2.feather")
  val <- read_feather("val_data_V2.feather")

  library(dygraphs)
  library(xts)
  library(tidyverse)
  library(caret)
  library(doParallel)
  library(MASS)
  library(ranger)
  library(rBayesianOptimization)
  library(pander)
  set.seed(42)


  xgb_train <- train[, -c(1, 3, 4, 11, 14:56)] # fuzzy
  xgb_val <- val[, -c(1, 3, 4, 11, 14:56)] # fuzzy
  # remove NA rows resulting from Qdiff, Tmean_diff
  na_train <- which(is.na(xgb_train), arr.ind = TRUE)
  xgb_train <- xgb_train[-na_train[,1],]

  cat(paste0("Create XGBoost folder for catchment ", catchment, ".\n"))
  if (file.exists("XGBoost")){
    setwd(file.path("XGBoost"))
  } else {
    dir.create(file.path("XGBoost"))
    setwd(file.path("XGBoost"))

  }

  # caret Model --------------------------------------------------------------------------
  cat("Chosen cross validation mode:", cv_mode, "\n")
  if(!(cv_mode %in% c("timeslice", "repCV"))) stop("cv_model can be either timeslice or repCV!")
  if(cv_mode == "timeslice"){
    tc <- trainControl(method = "timeslice",
                       initialWindow = 730,
                       horizon = 90,
                       fixedWindow = FALSE,
                       allowParallel = FALSE,
                       verboseIter = TRUE,
                       skip = 60)
  }
  if(cv_mode == "repCV"){
    tc <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       allowParallel = FALSE,
                       verboseIter = TRUE)

  }

  if(model_or_optim == "optim"){
    cat("Starting hyperparameter optimization\n -----------------------------------\n")

    # Some notes to XGBoost parameter and optimization:
    # nrounds: It controls the maximum number of iterations.
    #          For classification, it is similar to the number of trees to grow.
    # eta: learning rate, Step size shrinkage used in update to prevents overfitting.
    # gamma: Minimum loss reduction required to make a further partition on a leaf node of the tree.
    #       The larger gamma is, the more conservative the algorithm will be.
    # max_depth: Maximum depth of a tree. larger -> more complex (could start overfitting!)
    # min_child_weight: Minimum sum of instance weight (hessian) needed in a child.
    #                    The larger min_child_weight is, the more conservative the algorithm will be.
    # subsample: Subsample ratio of the training instances. Setting it to 0.5 means that XGBoost would
    #            randomly sample half of the training data prior to growing trees.
    #            and this will prevent overfitting.
    # colsample_bytree: specify the fraction of columns to be subsampled.

# Parameter optimization will be done in 4 steps:
    # 1. Choose a relatively high learning rate and optimze the number of trees for this learning rate
    # 2. Tune tree-specific parameters ( max_depth, min_child_weight, gamma, subsample, colsample_bytree)
    # 3. Tune regularization parameters (lambda, alpha)
    # 4. Lower the learning rate and decide the optimal parameters


    # 1. Choos eta = 0.05, and take two nrounds as initial values for optim.
    #    All other parameters are on their default value
    tg <- expand.grid(nrounds = c(50, 200, 500, 1000, 2000, 3000),
                      max_depth = 5,
                      eta = 0.05,
                      gamma = c(0),
                      colsample_bytree = c(0.8),
                      subsample = 1,
                      min_child_weight = 1)

    #registerDoParallel(cores = no.cores)
    cat("1. Optimize number of iterations for model with eta = 0.05:\n")
    set.seed(42)
    capture.output(xgb_fit <- train(wt ~ .,
                                       data = xgb_train,
                                       method = "xgbTree",
                                       trControl = tc,
                                       tuneGrid = tg,
                                       num.threads = 1))
    # find out where the difference in RMSE is smaller than 0.005 -> choosen nrounds
    optim_ind <- which(abs(xgb_fit$results$RMSE - c(0, xgb_fit$results$RMSE[-length(xgb_fit$results$RMSE)])) < 0.005)[1]
    # Choose surrounding nrounds to further pin it down
    current_opt_nrounds <- xgb_fit$results[optim_ind,]
    nrounds_search <- current_opt_nrounds[, "nrounds"] + c(100, 200, 300, 400)
    tg <- expand.grid(nrounds = nrounds_search,
                      max_depth = 5,
                      eta = 0.05,
                      gamma = c(0),
                      colsample_bytree = c(0.8),
                      subsample = 1,
                      min_child_weight = 1)
    set.seed(42)
    capture.output(xgb_fit <- train(wt ~ .,
                                    data = xgb_train,
                                    method = "xgbTree",
                                    trControl = tc,
                                    tuneGrid = tg,
                                    num.threads = 1))
    optim_ind <- which(xgb_fit$results$RMSE[1] - xgb_fit$results$RMSE  > 0.001)
    if(length(optim_ind) == 0){
      nrounds_optim <- current_opt_nrounds[, "nrounds"]
    } else {
      nrounds_optim <- xgb_fit$results[optim_ind, "nrounds"]
    }
    cat("   optimal number of iterations:", nrounds_optim, "\n")
    cat("2. Optimize tree specific parameters\n")

    tg <- expand.grid(nrounds = nrounds_optim,
                      max_depth = seq(3,20, 2),
                      eta = 0.05,
                      gamma = c(0),
                      colsample_bytree = c(0.8),
                      subsample = 1,
                      min_child_weight = c(1, 3, 5, 7, 9))
    set.seed(42)
    capture.output(xgb_fit <- train(wt ~ .,
                                    data = xgb_train,
                                    method = "xgbTree",
                                    trControl = tc,
                                    tuneGrid = tg,
                                    num.threads = 1))
    # up to here: nrounds = 1000, max_depth = 17, min_child_weight = 5 in Greimpersdorf

    cat("3. Bayesian optimization of gamma, subsample, colsample_bytree\n")
    # make initial points for gamma optim
    tg <- expand.grid(nrounds = nrounds_optim,
                      max_depth = xgb_fit$bestTune$max_depth,
                      eta = 0.05,
                      gamma = c(0, 5),
                      colsample_bytree = c(0.5, 0.8),
                      subsample = c(0.8, 1),
                      min_child_weight = xgb_fit$bestTune$min_child_weight)
    set.seed(42)
    txt <- capture.output(gamma_init <- train(wt ~ .,
                                    data = xgb_train,
                                    method = "xgbTree",
                                    trControl = tc,
                                    tuneGrid = tg,
                                    num.threads = 1))
    initial_grid <- gamma_init$results[, c("gamma", "subsample", "colsample_bytree", "RMSE")]
    names(initial_grid) <- c("gamma", "subsample", "colsample_bytree", "Value")
    initial_grid$Value <- initial_grid$Value * -1
    # Bayesian optimization of gamma, subsample and colsample_by_tree
    xgb_optim_gamma <- function(gamma, subsample, colsample_bytree) {
      ## Use the same model code but for a single (C, sigma) pair.
      tune_grid <- expand.grid(nrounds = nrounds_optim,
                               max_depth = xgb_fit$bestTune$max_depth,
                               eta = 0.05,
                               gamma = gamma,
                               colsample_bytree = colsample_bytree,
                               subsample = subsample,
                               min_child_weight = xgb_fit$bestTune$min_child_weight)
      txt <- capture.output(
        mod <- train(wt ~ .,
                     data = xgb_train,
                     method = "xgbTree",
                     trControl = tc,
                     tuneGrid = tune_grid,
                     num.threads = 1)
      )
      ## The function wants to _maximize_ the outcome so we return
      ## the negative of the resampled RMSE value. `Pred` can be used
      ## to return predicted values but we'll avoid that and use zero
      list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0)
    }
    bounds <- list(gamma = c(0, 5),
                   subsample = c(0.25, 1),
                   colsample_bytree = c(0.25, 1))

    set.seed(42)
    txt <- capture.output(search <- BayesianOptimization(xgb_optim_gamma,
                                   bounds = bounds,
                                   init_grid_dt = initial_grid,
                                   init_points = 0,
                                   n_iter = n_iter,
                                   acq = "ucb",
                                   kappa = 2.576,
                                   eps = 0.0,
                                   verbose = TRUE))


    # Reduce eta and increase nrounds
    cat("4. reduce learning rate and increase number of iterations\n")
    grid <- expand.grid(nrounds = c(2000, 3000, 4000, 5000),
                        max_depth = xgb_fit$bestTune$max_depth,
                        eta = c(0.001, 0.005),
                        gamma = search$Best_Par["gamma"],
                        colsample_bytree = search$Best_Par["colsample_bytree"],
                        subsample = search$Best_Par["subsample"],
                        min_child_weight = xgb_fit$bestTune$min_child_weight)
    set.seed(42)
    txt <- capture.output(eta_nrounds_init <- train(wt ~ .,
                                              data = xgb_train,
                                              method = "xgbTree",
                                              trControl = tc,
                                              tuneGrid = grid,
                                              num.threads = 1))
    initial_grid <- eta_nrounds_init$results[, c("eta", "nrounds", "RMSE")]
    names(initial_grid) <- c("eta", "nrounds", "Value")
    initial_grid$Value <- initial_grid$Value * -1

    # Bayesian optimization of gamma
    xgb_optim_eta_nrounds <- function(eta, nrounds) {
      tune_grid <- expand.grid(nrounds = nrounds,
                               max_depth = xgb_fit$bestTune$max_depth,
                               eta = eta,
                               gamma = search$Best_Par["gamma"],
                               colsample_bytree = search$Best_Par["colsample_bytree"],
                               subsample = search$Best_Par["subsample"],
                               min_child_weight = xgb_fit$bestTune$min_child_weight)
      txt <- capture.output(
        mod <- train(wt ~ .,
                     data = xgb_train,
                     method = "xgbTree",
                     trControl = tc,
                     tuneGrid = tune_grid,
                     num.threads = 1)
      )

      list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0)
    }
    bounds <- list(eta = c(0.0001, 0.01),
                   nrounds = c(1000L, 6000L))

    set.seed(42)
    txt <- capture.output(search2 <- BayesianOptimization(xgb_optim_eta_nrounds,
                                                         bounds = bounds,
                                                         init_grid_dt = initial_grid,
                                                         init_points = 0,
                                                         n_iter = n_iter,
                                                         acq = "ucb",
                                                         kappa = 2.576,
                                                         eps = 0.0,
                                                         verbose = TRUE))

    best_par <- data.frame(nrounds = search2$Best_Par["nrounds"],
                           max_depth = xgb_fit$bestTune$max_depth,
                           eta = search2$Best_Par["eta"],
                           gamma = search$Best_Par["gamma"],
                           colsample_bytree = search$Best_Par["colsample_bytree"],
                           subsample = search$Best_Par["subsample"],
                           min_child_weight = xgb_fit$bestTune$min_child_weight)
    write_feather(best_par, paste0("xgb_optimization_results_", cv_mode, ".feather"))
    cat("Finished hyperparameter optimization, best parameters:\n")
        for(i in 1:ncol(best_par)) cat(names(best_par)[i], ":", as.numeric(best_par[1, i]), "\n")
  }

  # Train best model
  cat("Train best model.\n")
  if(model_or_optim == "model"){
    best_par <- read_feather(paste0("xgb_optimization_results_", cv_mode, ".feather"))
  }

  grid <- expand.grid(nrounds = as.integer(best_par["nrounds"]),
                      max_depth = as.integer(best_par["max_depth"]),
                      eta = as.numeric(best_par["eta"]),
                      gamma = format(as.numeric(best_par["gamma"]), scientific = FALSE),
                      colsample_bytree = as.numeric(best_par["colsample_bytree"]),
                      subsample = as.numeric(best_par["subsample"]),
                      min_child_weight = as.numeric(best_par["min_child_weight"]))

  txt <- capture.output(xgb_fit <- train(wt ~ .,
                      data = xgb_train,
                      method = "xgbTree",
                      trControl = tc,
                      tuneGrid = grid,
                      num.threads = 1,
                      importance = "impurity"))
  saveRDS(xgb_fit, paste0("optimizedXGBoost_model_", cv_mode,  ".rds"))
  cat("Saved best model as", paste0("optimizedXGBoost_model_", cv_mode,  ".rds"), "\n")


  # Prediction and model diagnostics -------------------------------------------------------
  cat("Start prediction and model diagnostics\n")

  source("../../../functions/rmse_nse.R")
  model_diagnostic <- rmse_nse(model = xgb_fit, val = xgb_val)
  model_diagnostic <- cbind(model_diagnostic, grid,
                            stringsAsFactors = FALSE)
  cat("\nModel quality criteria: \nRMSE:", model_diagnostic$RMSE, "\n", "NSE:", model_diagnostic$NSE)
  write.table(model_diagnostic, paste0("XGB_results_", cv_mode, ".txt"),
              append = FALSE, sep = " ", dec = ".",
              row.names = FALSE, col.names = TRUE)

  if(plot_ts){
    predict_xgb <- predict(xgb_fit, xgb_val)
    pred_xts_xgb <- xts(cbind(xgb_val, "predictions" = predict_xgb),
                           order.by = as.POSIXct(paste0(val$year, "-", val$mon, "-", val$day)))
    print(dygraph(pred_xts_xgb[, c("wt", "predictions")]) %>%  dyRangeSelector())
  }

  if(save_importance_plot){
    cat("\nSaving importance plot in", getwd(), "\n")
    importance <- varImp(xgb_fit)$importance
    v <- as.numeric(importance[,1])
    w <- rownames(importance)
    DF <- data.frame(w,v, stringsAsFactors = FALSE)
    p <- ggplot(DF, aes(x=reorder(w,v), y=v, fill = v))+
      geom_bar(stat="identity", position="dodge") + coord_flip()+
      ylab("Variable Importance")+
      xlab("")+
      ggtitle("Information Value Summary - XGBoost")+
      guides(fill=F) +
      scale_fill_gradient(low="red", high="blue")
    ggsave(filename = paste0("XGB_importance_plot_", cv_mode, ".png"), plot = p, device = "png",
           dpi = "retina")
  }

  setwd(old_wd)
}
