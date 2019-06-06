#' wt_xgboost_simple
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
wt_xgboost_simple <- function(catchment, data_inputs = NULL, model_or_optim, cv_mode, parameter_list,
                              plot_ts = FALSE, save_importance_plot = FALSE, user_name = "R2D2"){

  if(user_name == "R2D2") cat('No user_name was chosen! Default user "R2D2" is running the model.\n')

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
         "simple"    = Qsetwd("/media/cfgrammar/Data, Tmean and water temperatur observations
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
        if(nrow(na_train) > 0) {
          xgb_train <- xgb_train[-unique(na_train[,1]),]
          train <- train[-unique(na_train[,1]),]
        }
        na_test <- which(is.na(xgb_test), arr.ind = TRUE)
        if(nrow(na_test) > 0) {
          xgb_test <- xgb_test[-unique(na_test[,1]),]
          test <- test[-unique(na_test[,1]),]
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
          n_seeds <- ceiling((nrow(xgb_train) - 730)/60)
          seeds <- vector(mode = "list", length = n_seeds)
          set.seed(1234)
          for(i in 1:n_seeds) seeds[[i]] <- sample(10000, 500)
          tc <- caret::trainControl(method = "timeslice",
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
          for(i in 1:51) seeds[[i]] <- sample(10000, 500)
          tc <- caret::trainControl(method = "repeatedcv",
                                    number = 10,
                                    repeats = 5,
                                    allowParallel = FALSE,
                                    verboseIter = TRUE,
                                    seeds = seeds)

        }


        cat("Starting hyperparameter optimization\n -----------------------------------\n")
        # 1. Choos eta = 0.05, and take two nrounds as initial values for optim.
        #    All other parameters are on their default value
        tg <- expand.grid(nrounds = 300,
                          max_depth = parameter_list[["max_depth"]],
                          eta = 0.15,
                          gamma = parameter_list[["gamma"]],
                          colsample_bytree = parameter_list[["colsample_bytree"]],
                          subsample = parameter_list[["colsample_bytree"]],
                          min_child_weight = parameter_list[["min_child_weight"]])

        cat("1. Find optimal parameter set with eta = 0.15 and nrounds = 300")

        txt <- capture.output(xgb_fit <- caret::train(wt ~ .,
                                                      data = xgb_train,
                                                      method = "xgbTree",
                                                      trControl = tc,
                                                      tuneGrid = tg,
                                                      num.threads = 1))
        # get best model parameters
        opt_parameter <- xgb_fit$bestTune
        best_par <- data.frame(model = model_name,
                               nrounds = parameter_list[["nrounds"]],
                               max_depth = opt_parameter$max_depth,
                               eta = parameter_list[["eta"]],
                               gamma = opt_parameter$gamma,
                               colsample_bytree = opt_parameter$colsample_bytree,
                               subsample = opt_parameter$subsample,
                               min_child_weight = opt_parameter$min_child_weight)
        cat("Finished model optimization, best parameters:\n")
        for(i in 2:ncol(best_par)) cat(names(best_par)[i], ":", as.numeric(best_par[1, i]), "\n")
        cat("2. Optimize best parameter set with eta =", best_par$eta, "and nrounds", best_par$nrounds)
        # Optimize best_par with nrounds and eta
        tg <- expand.grid(nrounds = parameter_list[["nrounds"]],
                          max_depth = opt_parameter$max_depth,
                          eta = parameter_list[["eta"]],
                          gamma = opt_parameter$gamma,
                          colsample_bytree = opt_parameter$colsample_bytree,
                          subsample = opt_parameter$subsample,
                          min_child_weight = opt_parameter$min_child_weight)

        txt <- capture.output(xgb_fit <- caret::train(wt ~ .,
                                                      data = xgb_train,
                                                      method = "xgbTree",
                                                      trControl = tc,
                                                      tuneGrid = tg,
                                                      num.threads = 1))
        feather::write_feather(best_par, paste0(catchment, "/XGBoost/", model_name,
                                                "/xgb_optimized_parameters.feather"))
        # save model
        saveRDS(xgb_fit, paste0(catchment, "/XGBoost/", model_name,
                                "/optimizedXGBoost_model.rds"))
        cat("Saved best model as", paste0(catchment, "/XGBoost/",
                                          model_name, "/optimizedXGBoost_model.rds"), "\n")
        # Prediction and model diagnostics -------------------------------------------------------
        cat("Start prediction and model diagnostics\n")
        run_time <- paste0(round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
                           " minutes")
        # save predicted values
        predict_xgb <- predict(xgb_fit, xgb_test)
        feather::write_feather(data.frame("predicted_values" = predict_xgb),
                               paste0(catchment, "/XGBoost/", model_name, "/predicted_values.feather"))
        # scores
        model_diagnostic <- rmse_nse(model = xgb_fit, val = xgb_test)
        model_diagnostic <- cbind("user_name" = user_name,
                                  "model" = model_name,
                                  "start_time" = as.character(start_time),
                                  "run_time" = run_time,
                                  best_par[,-1],
                                  model_diagnostic,
                                  stringsAsFactors = FALSE)
        cat("\nModel quality criteria: \nRMSE:", model_diagnostic$RMSE, "\n", "NSE:", model_diagnostic$NSE)

        if("model_scores.csv" %in% list.files(paste0(catchment, "/XGBoost"))){
          model_scores <- read.csv(paste0(catchment, "/XGBoost/", "model_scores.csv"),
                                   stringsAsFactors = FALSE)
          model_scores$start_time <- as.character(model_scores$start_time)
          write.csv(rbind(model_scores, model_diagnostic, stringsAsFactors = FALSE),
                    paste0(catchment, "/XGBoost/", "model_scores.csv"), row.names = FALSE)
        } else {
          write.csv(model_diagnostic, paste0(catchment, "/XGBoost/", "model_scores.csv"), row.names = FALSE)
        }


        if(plot_ts){
          predict_xgb <- predict(xgb_fit, xgb_test)
          pred_xts_xgb <- xts::xts(cbind(xgb_test, "predictions" = predict_xgb),
                                   order.by = as.POSIXct(paste0(test$year, "-", test$mon, "-", test$day)))
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
            guides(fill = FALSE) +
            scale_fill_gradient(low="red", high="blue")
          ggsave(filename = paste0(catchment, "/XGBoost/", model_name, "/XGB_importance_plot.png"), plot = p, device = "png",
                 dpi = "retina")
        }
      }
    }
  }
}
