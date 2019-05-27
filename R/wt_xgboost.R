#' wt_XGBoost
#'
#' XGBoost modelling for water temperature data prepared with wt_preprocessing. Can either be used for loading pre-trained models or train a new model. The training is done in parallel and can take a while. yperparameter optimization is done using grid search and the rBayesianOptimization packages.
#' @param catchment Name of the folder with the data as a string.
#' @param data_inputs The kind of data to be used for the model run: "simple", "precip", "radiation", "all". See Details
#' @param model_or_optim "model" if a pretrained model should be load, or "optim" if a new model should be trained
#' @param cv_mode The crossvalidation scheme to be used. Can be either: "timeslice", or "repCV"
#' @param n_iter Number of iterations for the bayesian hyperparameter optimization as a integer.
#' @param plot_ts Should a dygraphy plot of the prediction and observation be plotted? TRUE/FALSE
#' @param save_importance_plot Should the importance plot for XGBoost be saved in the folder. TRUE/FALSE
#'
#' @return None
#' @export
#' @details This function is able to train or load a pre-trained model for water temperature prediction. The features used for the prediction are defined by the "data_inputs" variable. "simple" means only discharge Q and mean air temperature Tmean are used. "precip" means that additional to "simple", precipitation data "RR" is used. "radiation" means that additional to "simple", longwave radiation data "GL" is used. "all" means that "simple" + precipitation + radiation data is used.
#'
#' @examples
wt_xgboost <- function(catchment, data_inputs = NULL, model_or_optim, cv_mode, n_iter = 30,
                       plot_ts = FALSE, save_importance_plot = FALSE){


  # 1. Data --------------------------------------------------------------------------------
  if(sum(list.files() %in% catchment) < 1){
    stop(paste0("ERROR: Cannot find catchment folder(s) in your current working directory."))
  }
  if(sum(model_or_optim %in% c("model", "optim")) == 0){
    stop('\nChoose a valid model_or_optim option:
            "model"    = pretrained model created with wt_xgboost will be loaded
            "optim"    = a new model with hyperparameter optimization will be trained')
  }
  if(is.null(data_inputs)){
    stop('\nChoose a valid data_input:
            "simple"    = Q, Tmean and water temperatur observations
            "precip"    = "simple" + additional precipitation observations
            "radiation" = "simple" + additional longwave radiation observations
            "all"       = all the above mentioned observations')
  }
  if(sum(data_inputs %in% c("simple", "precip", "radiation", "all")) == 0){
    stop('\nChoose a valid data_input:
            "simple"    = Q, Tmean and water temperatur observations
            "precip"    = "simple" + additional precipitation observations
            "radiation" = "simple" + additional longwave radiation observations
            "all"       = all the above mentioned observations')
  }
  # loop variables
  data_inputs_meta <- data_inputs
  cv_mode_meta <- cv_mode

  for(catchment in catchment){
    cat("*** Starting computation for catchment", catchment, "***\n")
    # Catch wrong catchment name
    if(sum(list.files() %in% catchment) != 1){
      message(paste0("ERROR: There is no folder named ", catchment, " in your current working directory."))
      next
    }
    for(cv_mode in cv_mode_meta){
      for(data_inputs in data_inputs_meta){
        start_time <- Sys.time()
        model_name <- paste0(data_inputs, "Model_", cv_mode)
        cat("Loading catchment data.\n")
        # check if there is seperate radiation data
        rad_data <- length(list.files(catchment, pattern = "radiation_")) > 0
        # in case of radiation or all data_input, load radiation data
        if(data_inputs == "radiation" & rad_data | data_inputs == "all" & rad_data){
          data_prefix <- "radiation_"
        } else {
          data_prefix <- ""
        }

        #data <- read_feather(paste0("input_", data_prefix, "data.feather"))
        train <- read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
        val <- read_feather(paste0(catchment, "/val_", data_prefix, "data.feather"))

        if(data_inputs == "simple"){
          relevant_data <- c("year", "Q", "Tmean", "wt", "Qdiff", "Tmean_diff",
                             "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5", "Fmon.6",
                             "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11",
                             "Tmean_lag1", "Tmean_lag2", "Tmean_lag3", "Tmean_lag4",
                             "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
        }
        if(data_inputs == "precip"){
          relevant_data <- c("year", "Q", "RR", "Tmean", "wt", "Qdiff", "Tmean_diff",
                             "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5", "Fmon.6",
                             "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11",
                             "Tmean_lag1", "Tmean_lag2", "Tmean_lag3", "Tmean_lag4",
                             "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
        }
        if(data_inputs == "radiation"){
          relevant_data <- c("year", "Q", "GL", "Tmean", "wt", "Qdiff", "Tmean_diff",
                             "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5", "Fmon.6",
                             "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11",
                             "Tmean_lag1", "Tmean_lag2", "Tmean_lag3", "Tmean_lag4",
                             "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
        }
        if(data_inputs == "all"){
          relevant_data <- c("year", "Q", "RR", "GL", "Tmean", "wt", "Qdiff", "Tmean_diff",
                             "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5", "Fmon.6",
                             "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11",
                             "Tmean_lag1", "Tmean_lag2", "Tmean_lag3", "Tmean_lag4",
                             "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
        }

        xgb_train <- train[, relevant_data]
        xgb_val <- val[, relevant_data]
        # remove NA rows resulting from Qdiff, Tmean_diff
        na_train <- which(is.na(xgb_train), arr.ind = TRUE)
        if(nrow(na_train) > 0) {
          xgb_train <- xgb_train[-unique(na_train[,1]),]
          train <- train[-unique(na_train[,1]),]
        }
        na_val <- which(is.na(xgb_val), arr.ind = TRUE)
        if(nrow(na_val) > 0) {
          xgb_val <- xgb_val[-unique(na_val[,1]),]
          val <- val[-unique(na_val[,1]),]
        }

        cat(paste0("Create XGBoost folder for catchment ", catchment, ".\n"))
        if (!file.exists(paste0(catchment, "/XGBoost"))){
          dir.create(file.path(paste0(catchment, "/XGBoost")))
        }
        if (!file.exists(paste0(catchment, "/XGBoost/", model_name))){
          dir.create(file.path(paste0(catchment, "/XGBoost/", model_name)))
        }

        # caret Model --------------------------------------------------------------------------
        cat("Chosen cross validation mode:", cv_mode, "\n")
        if(!(cv_mode %in% c("timeslice", "repCV"))) stop('cv_model can be either "timeslice" or "repCV"!')
        if(cv_mode == "timeslice"){
          n_seeds <- ceiling((nrow(ranger_train) - 730)/60)
          seeds <- vector(mode = "list", length = n_seeds)
          set.seed(1234)
          for(i in 1:n_seeds) seeds[[i]] <- sample(10000, 65)
          tc <- trainControl(method = "timeslice",
                             initialWindow = 730,
                             horizon = 90,
                             fixedWindow = FALSE,
                             allowParallel = FALSE,
                             verboseIter = TRUE,
                             skip = 60,
                             seeds = seeds)
        }
        if(cv_mode == "repCV"){
          seeds <- vector(mode = "list", length = 51)
          set.seed(1234)
          for(i in 1:51) seeds[[i]] <- sample(10000, 65)
          tc <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             allowParallel = FALSE,
                             verboseIter = TRUE,
                             seeds = seeds)

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
          cat("1. Optimize number of trees for model with eta = 0.05:\n")

          txt <- capture.output(xgb_fit <- caret::train(wt ~ .,
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

          txt <- capture.output(xgb_fit <- caret::train(wt ~ .,
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
          cat("   optimal number of trees:", nrounds_optim, "\n")
          cat("2. Optimize tree specific parameters\n")

          tg <- expand.grid(nrounds = nrounds_optim,
                            max_depth = c(3, 4, 5, 6),
                            eta = 0.05,
                            gamma = c(0),
                            colsample_bytree = c(0.8),
                            subsample = 1,
                            min_child_weight = c(1, 3, 5, 7, 9))

          txt <- capture.output(xgb_fit <- caret::train(wt ~ .,
                                                        data = xgb_train,
                                                        method = "xgbTree",
                                                        trControl = tc,
                                                        tuneGrid = tg,
                                                        num.threads = 1))

          cat("3. Bayesian optimization of gamma, subsample, colsample_bytree\n")
          # make initial points for gamma optim
          tg <- expand.grid(nrounds = nrounds_optim,
                            max_depth = xgb_fit$bestTune$max_depth,
                            eta = 0.05,
                            gamma = c(0, 5),
                            colsample_bytree = c(0.5, 0.8),
                            subsample = c(0.8, 1),
                            min_child_weight = xgb_fit$bestTune$min_child_weight)

          txt <- capture.output(gamma_init <- caret::train(wt ~ .,
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
              mod <- caret::train(wt ~ .,
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
          cat("4. reduce learning rate and increase number of trees\n")
          grid <- expand.grid(nrounds = c(2000, 3000, 4000, 5000),
                              max_depth = xgb_fit$bestTune$max_depth,
                              eta = c(0.001, 0.005),
                              gamma = search$Best_Par["gamma"],
                              colsample_bytree = search$Best_Par["colsample_bytree"],
                              subsample = search$Best_Par["subsample"],
                              min_child_weight = xgb_fit$bestTune$min_child_weight)

          txt <- capture.output(eta_nrounds_init <- caret::train(wt ~ .,
                                                                 data = xgb_train,
                                                                 method = "xgbTree",
                                                                 trControl = tc,
                                                                 tuneGrid = grid,
                                                                 num.threads = 1))
          initial_grid <- eta_nrounds_init$results[, c("eta", "nrounds", "RMSE")]
          names(initial_grid) <- c("eta", "nrounds", "Value")
          initial_grid$Value <- initial_grid$Value * -1
          #initial_grid$nrounds <- as.integer(initial_grid$nrounds)
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
              mod <- caret::train(wt ~ .,
                                  data = xgb_train,
                                  method = "xgbTree",
                                  trControl = tc,
                                  tuneGrid = tune_grid,
                                  num.threads = 1)
            )

            list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0)
          }
          bounds <- list(eta = c(0.001, 0.1),
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

          best_par <- data.frame(model = model_name,
                                 nrounds = search2$Best_Par["nrounds"],
                                 max_depth = xgb_fit$bestTune$max_depth,
                                 eta = search2$Best_Par["eta"],
                                 gamma = search$Best_Par["gamma"],
                                 colsample_bytree = search$Best_Par["colsample_bytree"],
                                 subsample = search$Best_Par["subsample"],
                                 min_child_weight = xgb_fit$bestTune$min_child_weight)

          feather::write_feather(best_par, paste0(catchment, "/XGBoost/", model_name,
                                                  "/xgb_optimized_parameters.feather"))
          cat("Finished hyperparameter optimization, best parameters:\n")
          for(i in 2:ncol(best_par)) cat(names(best_par)[i], ":", as.numeric(best_par[1, i]), "\n")
        }

        # Train best model
        cat("Train best model.\n")
        if(model_or_optim == "model"){
          best_par <- feather::read_feather(paste0(catchment, "/XGBoost/", model_name,
                                                   "/xgb_optimized_parameters.feather"))
        }

        grid <- expand.grid(nrounds = as.integer(best_par["nrounds"]),
                            max_depth = as.integer(best_par["max_depth"]),
                            eta = as.numeric(best_par["eta"]),
                            gamma = format(as.numeric(best_par["gamma"]), scientific = FALSE),
                            colsample_bytree = as.numeric(best_par["colsample_bytree"]),
                            subsample = as.numeric(best_par["subsample"]),
                            min_child_weight = as.numeric(best_par["min_child_weight"]))

        txt <- capture.output(xgb_fit <- caret::train(wt ~ .,
                                                      data = xgb_train,
                                                      method = "xgbTree",
                                                      trControl = tc,
                                                      tuneGrid = grid,
                                                      num.threads = 1,
                                                      importance = "impurity"))
        saveRDS(xgb_fit, paste0(catchment, "/XGBoost/", model_name,
                                "/optimizedXGBoost_model.rds"))
        cat("Saved best model as", paste0(catchment, "/XGBoost/",
                                          model_name, "/optimizedXGBoost_model.rds"), "\n")


        # Prediction and model diagnostics -------------------------------------------------------
        cat("Start prediction and model diagnostics\n")
        run_time <- paste0(round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
                           " minutes")
        # save predicted values
        predict_xgb <- predict(xgb_fit, xgb_val)
        feather::write_feather(data.frame("predicted_values" = predict_xgb),
                               paste0(catchment, "/XGBoost/", model_name, "/predicted_values.feather"))
        # scores
        model_diagnostic <- rmse_nse(model = xgb_fit, val = xgb_val)
        model_diagnostic <- cbind(model = model_name,
                                  start_time = start_time,
                                  run_time = run_time,
                                  model_diagnostic,
                                  grid,
                                  stringsAsFactors = FALSE)
        cat("\nModel quality criteria: \nRMSE:", model_diagnostic$RMSE, "\n", "NSE:", model_diagnostic$NSE)

        if("model_scores.csv" %in% list.files(paste0(catchment, "/XGBoost"))){
          model_scores <- read.csv(paste0(catchment, "/XGBoost/", "model_scores.csv"),
                                   stringsAsFactors = FALSE)
          write.csv(rbind(model_scores, model_diagnostic, stringsAsFactors = FALSE),
                    paste0(catchment, "/XGBoost/", "model_scores.csv"), row.names = FALSE)
        } else {
          write.csv(model_diagnostic, paste0(catchment, "/XGBoost/", "model_scores.csv"), row.names = FALSE)
        }


        if(plot_ts){
          predict_xgb <- predict(xgb_fit, xgb_val)
          pred_xts_xgb <- xts::xts(cbind(xgb_val, "predictions" = predict_xgb),
                                   order.by = as.POSIXct(paste0(val$year, "-", val$mon, "-", val$day)))
          print(dygraphs::dygraph(pred_xts_xgb[, c("wt", "predictions")], main = "XGBoost prediction") %>%
                  dygraphs::dyRangeSelector())
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
          ggsave(filename = paste0(catchment, "/XGBoost/", model_name, "/XGB_importance_plot.png"), plot = p, device = "png",
                 dpi = "retina")
        }
      }
    }

  }
}
