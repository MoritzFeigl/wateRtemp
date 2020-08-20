#' wt_xgboost
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
wt_xgboost <- function(catchment,
                       data_inputs = NULL,
                       cv_mode,
                       n_iter = 20,
                       n_random_initial_points = 20,
                       no_cores = detectCores() - 1,
                       user_name = "R2D2",
                       plot_ts = FALSE,
                       save_importance_plot = FALSE,
                       random_seed = NULL){

  if(user_name == "R2D2") cat('No user_name was chosen! Default user "R2D2" is running the model.\n')

  # 1. Data --------------------------------------------------------------------------------
  if(sum(list.files() %in% catchment) < 1){
    stop(paste0("ERROR: Cannot find catchment folder(s) in your current working directory."))
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

        train <- feather::read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
        test <- feather::read_feather(paste0(catchment, "/test_", data_prefix, "data.feather"))

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
        xgb_test <- test[, relevant_data]

        # remove NA rows resulting from Qdiff, Tmean_diff
        na_train <- which(is.na(xgb_train), arr.ind = TRUE)
        if(nrow(na_train) > 0) xgb_train <- xgb_train[-unique(na_train[,1]),]
        na_test <- which(is.na(xgb_test), arr.ind = TRUE)
        if(nrow(na_test) > 0) xgb_test <- xgb_test[-unique(na_test[,1]),]

        cat(paste0("Create XGBoost folder for catchment ", catchment, ".\n"))
        if (!file.exists(paste0(catchment, "/XGBoost_new"))){
          dir.create(file.path(paste0(catchment, "/XGBoost_new")))
        }
        if (!file.exists(paste0(catchment, "/XGBoost_new/", model_name))){
          dir.create(file.path(paste0(catchment, "/XGBoost_new/", model_name)))
        }

        # caret Model --------------------------------------------------------------------------
        cat("Chosen cross validation mode:", cv_mode, "\n")
        if(!(cv_mode %in% c("timeslice", "repCV"))) {
          stop('cv_model can be either "timeslice" or "repCV"!')
        }
        if(cv_mode == "timeslice"){
          n_seeds <- ceiling((nrow(xgb_train) - 730)/60)
          seeds <- vector(mode = "list", length = n_seeds)
          set.seed(random_seed)
          for(i in 1:n_seeds) seeds[[i]] <- sample(10000, 65)
          tc <- caret::trainControl(method = "timeslice",
                                    initialWindow = 730,
                                    horizon = 90,
                                    fixedWindow = FALSE,
                                    allowParallel = TRUE,
                                    verboseIter = TRUE,
                                    skip = 60,
                                    seeds = seeds)
        }
        if(cv_mode == "repCV"){
          seeds <- vector(mode = "list", length = 51)
          set.seed(random_seed)
          for(i in 1:51) seeds[[i]] <- sample(10000, 65)
          tc <- caret::trainControl(method = "repeatedcv",
                                    number = 10,
                                    repeats = 5,
                                    allowParallel = TRUE,
                                    verboseIter = TRUE,
                                    seeds = seeds)
        }

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

        parameter_bounds <- list(
          "nrounds" = c(300L, 3000L),
          "eta" = c(0.001, 0.3),
          "max_depth" = c(3L, 12L),
          "min_child_weight" = c(1L, 10L),
          "subsample" = c(0.7, 1),
          "colsample_bytree" = c(0.7, 1),
          "gamma" = c(0L, 5L)
        )
        initial_grid <- parameter_bounds
        for(i in 1:length(parameter_bounds)){
          range <- parameter_bounds[[i]]
          if(is.integer(range)){
            initial_grid[[i]] <- sample(range[1]:range[2], n_random_initial_points, replace = TRUE)
          } else {
            initial_grid[[i]] <- round(sample(seq(range[1], range[2], length.out = 10),
                                              n_random_initial_points, replace = TRUE), 3)
          }
        }
        initial_grid <- as.data.frame(do.call(cbind, initial_grid))

        # Compute initial grid
        intial_grid_model <- caret::train(wt ~ .,
                                          data = xgb_train,
                                          method = "xgbTree",
                                          trControl = tc,
                                          tuneGrid = initial_grid,
                                          nthread = 1)
        optimization_results <- intial_grid_model$results[, c(names(initial_grid), "RMSE", "MAE")]
        write.csv(optimization_results,
                  file = paste0(catchment, "/XGBoost_new/", model_name, "/optimization_scores.csv"),
                  row.names = FALSE)


        initial_grid <- optimization_results[, 1:7]
        initial_grid$Value <- optimization_results$RMSE * -1
        # Bayesian optimization of gamma, subsample and colsample_by_tree
        xgb_optim_gamma <- function(nrounds, eta, max_depth, min_child_weight,
                                    subsample, colsample_bytree, gamma) {
          ## Use the same model code but for a single (C, sigma) pair.
          tune_grid <- expand.grid(nrounds = nrounds,
                                   max_depth = max_depth,
                                   eta = eta,
                                   gamma = gamma,
                                   colsample_bytree = colsample_bytree,
                                   subsample = subsample,
                                   min_child_weight = min_child_weight)
          cl <- parallel::makePSOCKcluster(no_cores)
          doParallel::registerDoParallel(cl)
          mod <- caret::train(wt ~ .,
                              data = xgb_train,
                              method = "xgbTree",
                              trControl = tc,
                              tuneGrid = tune_grid,
                              num.threads = 1)
          parallel::stopCluster(cl)
          # save results of given parameter set
          optimization_results <- read.csv(paste0(catchment, "/XGBoost_new/",
                                                  model_name, "/optimization_scores.csv"))
          optimization_results <- rbind(optimization_results,
                                        mod$results[, names(optimization_results)])
          write.csv(optimization_results,
                    file = paste0(catchment, "/XGBoost_new/",
                                  model_name, "/optimization_scores.csv"),
                    row.names = FALSE)
          return(list(Score = -getTrainPerf(mod)[, "TrainRMSE"], Pred = 0))
        }

        set.seed(random_seed)
        Bopt_xgboost <- rBayesianOptimization::BayesianOptimization(xgb_optim_gamma,
                                                                    bounds = parameter_bounds,
                                                                    init_grid_dt = initial_grid,
                                                                    init_points = 0,
                                                                    n_iter = n_iter,
                                                                    acq = "ucb",
                                                                    kappa = 2.576,
                                                                    eps = 0.0,
                                                                    verbose = TRUE)

        best_par <- data.frame(nrounds = Bopt_xgboost$History$nrounds,
                               max_depth =  Bopt_xgboost$History$max_depth,
                               eta = Bopt_xgboost$History$eta,
                               gamma = Bopt_xgboost$History$gamma,
                               colsample_bytree = Bopt_xgboost$History$colsample_bytree,
                               subsample = Bopt_xgboost$History$subsample,
                               min_child_weight = Bopt_xgboost$History$min_child_weight)

        model_diagnostic <- data.frame(user_name = user_name,
                                       start_time = as.character(start_time),
                                       catchment = catchment,
                                       data_inputs = data_inputs,
                                       cv_mode = cv_mode,
                                       best_par,
                                       test_RMSE = NA,
                                       test_MAE = NA,
                                       stringsAsFactors = FALSE)

        cat("Finished hyperparameter optimization, best parameters:\n")
        for(i in 2:ncol(best_par)) cat(names(best_par)[i], ":", as.numeric(best_par[1, i]), "\n")
        cl <- parallel::makePSOCKcluster(no_cores)
        doParallel::registerDoParallel(cl)
        final_model <- caret::train(wt ~ .,
                                    data = xgb_train,
                                    method = "xgbTree",
                                    trControl = tc,
                                    tuneGrid = best_par,
                                    num.threads = 1,
                                    importance = "impurity")
        parallel::stopCluster(cl)
        saveRDS(xgb_fit, paste0(catchment, "/XGBoost_new/", model_name,
                                "/XGBoost_model.rds"))
        cat("Saved best model as", paste0(catchment, "/XGBoost_new/",
                                          model_name, "/XGBoost_model.rds"), "\n")

        # Prediction and model diagnostics -------------------------------------------------------
        cat("Start prediction and model diagnostics\n")
        run_time <- paste0(round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
                           " minutes")
        # save predicted values
        predict_xgb <- predict(final_model, xgb_test)
        prediction <- data.frame(test$date, observed_wt = test$wt, predicted_wt = NA)
        if(nrow(na_test) > 0){
          prediction$predicted_wt[-na_test[, 1]] <- predict_xgb
        } else {
          prediction$predicted_wt <- predict_xgb
        }
        write.csv(prediction, paste0(catchment, "/XGBoost_new/", model_name, "/predicted_values.csv"))

        # scores
        test_rmse <- round(RMSE(prediction = predict_xgb, observation = xgb_test$wt), 4)
        test_mae <- round(MAE(y_obs = xgb_test$wt, y_pred = predict_xgb), 4)

        optimization_results <- read.csv(paste0(catchment, "/XGBoost_new/",
                                                model_name, "/optimization_scores.csv"))

        optimization_results[, 4:5] <- round(optimization_results[, c("RMSE", "MAE")], 4)
        names(optimization_results)[4:5] <- c("validation_RMSE", "validation_MAE")
        model_diagnostic <- data.frame(user_name = user_name,
                                       start_time = as.character(start_time),
                                       catchment = catchment,
                                       data_inputs = data_inputs,
                                       cv_mode = cv_mode,
                                       optimization_results[, -1],
                                       test_RMSE = NA,
                                       test_MAE = NA,
                                       stringsAsFactors = FALSE)
        # best parameter line
        best_par_line <- which.min(optimization_results[, "validation_RMSE"])
        model_diagnostic[best_par_line, c("test_RMSE", "test_MAE")] <- c(test_rmse, test_mae)

        cat("\nModel quality criteria \nRMSE: ", round(test_rmse, 3), "\n",
            " MAE: ", round(test_mae, 3), sep = "")

        if("model_scores.csv" %in% list.files(paste0(catchment, "/XGBoost_new"))){
          model_scores <- read.csv(paste0(catchment, "/XGBoost_new/", "model_scores.csv"),
                                   stringsAsFactors = FALSE)
          write.csv(rbind(model_scores, model_diagnostic, stringsAsFactors = FALSE),
                    paste0(catchment, "/XGBoost_new/", "model_scores.csv"), row.names = FALSE)
        } else {
          write.csv(model_diagnostic, paste0(catchment, "/XGBoost_new/", "model_scores.csv"), row.names = FALSE)
        }

        if(plot_ts){
          pred_xts <- xts::xts(prediction[, -1],
                               order.by = as.POSIXct(prediction$test.date))
          print(dygraphs::dygraph(pred_xts, main = "Random forest prediction") %>%
                  dygraphs::dyRangeSelector())
        }
        if(save_importance_plot){
          cat("\nSaving importance plot in",
              paste0(getwd(), "/", catchment, "/RF_new/", model_name, "/"),
              "\n")
          importance <- caret::varImp(final_model)$importance
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
          ggsave(filename = paste0(catchment, "/XGBoost_new/", model_name, "/importance_plot.png"),
                 plot = p, device = "png",
                 dpi = "retina")
        }
      }
    }
  }
}
