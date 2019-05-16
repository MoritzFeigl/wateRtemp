#' wt_RandomForest
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
wt_RandomForest <- function(catchment, model_or_optim, cv_mode, no.cores, n_iter = 30,
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
  set.seed(42)


  ranger_train <- train[, -c(1, 3, 4, 11, 14:56)] # fuzzy
  ranger_val <- val[, -c(1, 3, 4, 11, 14:56)] # fuzzy
  # remove NA rows resulting from Qdiff, Tmean_diff
  na_train <- which(is.na(ranger_train), arr.ind = TRUE)
  ranger_train <- ranger_train[-na_train[,1],]

  cat(paste0("Create random forest folder for catchment ", catchment, ".\n"))
  if (file.exists("RF")){
    setwd(file.path("RF"))
  } else {
    dir.create(file.path("RF"))
    setwd(file.path("RF"))
  }

  # caret Model --------------------------------------------------------------------------
  if(!(cv_mode %in% c("timeslice", "repCV"))) stop("cv_model can be either timeslice or repCV!")
  if(cv_mode == "timeslice"){
    tc <- trainControl(method = "timeslice",
                       initialWindow = 730,
                       horizon = 90,
                       fixedWindow = FALSE,
                       allowParallel = TRUE,
                       verboseIter = TRUE,
                       skip = 60)
  }
  if(cv_mode == "repCV"){
    tc <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       allowParallel = TRUE,
                       verboseIter = TRUE)
  }

  if(model_or_optim == "optim"){
    cat("Grid search optimization.\n")
    #cat("Calculate initial points for bayesian hyperparameter optimization.\n")

    tg <- expand.grid(.mtry = c(3:(ncol(ranger_train) - 1)),
                      .splitrule = "extratrees", #c("variance", "extratrees"),
                      .min.node.size = c(1:10))
    registerDoParallel(cores = no.cores)
    set.seed(42)
    ranger_fit <- train(wt ~ .,
                        data = ranger_train,
                        method = "ranger",
                        trControl = tc,
                        tuneGrid = tg,
                        num.threads = 1,
                        importance = "impurity")
    saveRDS(ranger_fit, paste0("optimized_RF_model_", cv_mode, ".rds"))
    best_par <- data.frame(mtry = ranger_fit$bestTune["mtry"],
                           "min.node.size" = ranger_fit$bestTune["min.node.size"],
                           "splitrule" = ranger_fit$bestTune["splitrule"])

    cat("Finished hyperparameter optimization, best parameters:\n")
    for(i in 1:ncol(best_par)) cat(names(best_par)[i], ": ", as.numeric(best_par[1, i]), "\n", sep = "")

  }
    # NOT USED AT THE MOMENT
    # Bayesian optimization ----------------------------------------------------------------
  #   cat("Starting bayesian hyperparameter optimization.\n")
  #   ranger_fit_bayes <- function(mtry, min.node.size) {
  #     rf_grid <- expand.grid(.mtry = mtry,
  #                            .splitrule = "extratrees",
  #                            # results have shown that for wt extratrees always performed best
  #                            .min.node.size = round(min.node.size, 0))
  #     registerDoParallel(cores = 17)
  #     txt <- capture.output(
  #       mod <- train(wt ~ .,
  #                    data = ranger_train,
  #                    method = "ranger",
  #                    trControl = tc,
  #                    tuneGrid = rf_grid,
  #                    num.threads = 1)
  #     )
  #     list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0)
  #   }
  #   ## Define the bounds of the search.
  #   lower_bounds <- c("mtry" = 3L, "min.node.size" = 1L)
  #   upper_bounds <- c("mtry" = 20L, "min.node.size" = 10L)
  #   bounds <- list("mtry" = c(lower_bounds[1], upper_bounds[1]),
  #                  "min.node.size" = c(lower_bounds[2], upper_bounds[2]))
  #   ## Create a grid of values as the input into the BO code
  #   initial_grid <- ranger_fit$results[ranger_fit$results$splitrule == "extratrees",
  #                                      c("mtry", "min.node.size", "RMSE")]
  #   names(initial_grid) <- c("mtry", "min.node.size", "Value")
  #   initial_grid$Value <- initial_grid$Value * -1
  #
  #   set.seed(42)
  #   rf_search <- BayesianOptimization(ranger_fit_bayes,
  #                                     bounds = bounds,
  #                                     init_grid_dt = initial_grid,
  #                                     init_points = 0,
  #                                     n_iter = n_iter,
  #                                     acq = "ucb",
  #                                     kappa = 2.576,
  #                                     eps = 0.0,
  #                                     verbose = TRUE)
  #   saveRDS(rf_search, paste0("RF_optimization_results_", cv_mode, ".rds"))
  #
  #   best_par <- data.frame(mtry = rf_search$Best_Par["mtry"],
  #                          "min.node.size" = rf_search$Best_Par["min.node.size"])
  #
  #   cat("Finished hyperparameter optimization, best parameters:\n")
  #   for(i in 1:ncol(best_par)) cat(names(best_par)[i], ":", as.numeric(best_par[1, i]), "\n")
  # }
  #

  # NOT USED AS GRID SEARCH HAS OPTIM MODEL ALREADY!
  # Train best model --------------------------------------------------------------------
  # cat("Train best model.\n")
  # if(model_or_optim == "model"){
  #   rf_search <- readRDS(paste0("RF_optimization_results_", cv_mode, ".rds"))
  # }
  #
  # rf_grid <- expand.grid(.mtry = rf_search$Best_Par["mtry"],
  #                        .splitrule = "extratrees",
  #                        # results have shown that for wt extratrees always performed best
  #                        .min.node.size = rf_search$Best_Par["min.node.size"])
  # registerDoParallel(cores = no.cores)
  # txt <- capture.output(ranger_fit <- train(wt ~ .,
  #                                           data = ranger_train,
  #                                           method = "ranger",
  #                                           trControl = tc,
  #                                           tuneGrid = rf_grid,
  #                                           num.threads = 1,
  #                                           importance = "impurity"))
  # saveRDS(ranger_fit, paste0("optimizedRF_model_", cv_mode, ".rds"))
  # cat("Saved best model as", paste0("optimizedRF_model_", cv_mode,  ".rds"), "\n")

  # Load optimized model
  if(model_or_optim == "model"){
    cat("Loading optimized model.\n")
    ranger_fit <- readRDS(paste0("optimized_RF_model_", cv_mode, ".rds"))
  }

  # Prediction and model diagnostics -------------------------------------------------------
  cat("Start prediction and model diagnostics")

  source("../../../functions/rmse_nse.R")
  model_diagnostic <- rmse_nse(model = ranger_fit, val = ranger_val)
  model_diagnostic <- cbind(model_diagnostic,
                            mtry = ranger_fit$bestTune["mtry"],
                            splitrule = ranger_fit$bestTune["splitrule"],
                            min.node.size = ranger_fit$bestTune["min.node.size"],
                            stringsAsFactors = FALSE)
  cat("\nModel quality criteria: \nRMSE: ", model_diagnostic$RMSE, "\n", " NSE: ", model_diagnostic$NSE, sep = "")
  write.table(model_diagnostic, paste0("RF_results_", cv_mode, ".txt"), append = FALSE, sep = " ", dec = ".",
              row.names = FALSE, col.names = TRUE)

  if(plot_ts){
    predict_ranger <- predict(ranger_fit, ranger_val)
    pred_xts_ranger <- xts(cbind(ranger_val, "predictions" = predict_ranger),
                           order.by = as.POSIXct(paste0(val$year, "-", val$mon, "-", val$day)))
    print(dygraph(pred_xts_ranger[, c("wt", "predictions")]) %>%  dyRangeSelector())
  }

  if(save_importance_plot){
    cat("\nSaving importance plot in", getwd(), "\n")
    importance <- varImp(ranger_fit)$importance
    v <- as.numeric(importance[,1])
    w <- rownames(importance)
    DF <- data.frame(w,v, stringsAsFactors = FALSE)
    p <- ggplot(DF, aes(x=reorder(w,v), y=v, fill = v))+
      geom_bar(stat="identity", position="dodge") + coord_flip()+
      ylab("Variable Importance")+
      xlab("")+
      ggtitle("Information Value Summary - Random Forest")+
      guides(fill=F) +
      scale_fill_gradient(low="red", high="blue")
    ggsave(filename = paste0("RF_importance_plot_", cv_mode, ".png"), plot = p, device = "png",
           dpi = "retina")
  }

  setwd(old_wd)
}
