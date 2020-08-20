#' wt_RandomForest
#'
#' Random Forest modelling for water temperature data prepared with wt_preprocessing. Can either be used for loading pre-trained models or train a new model. The training is done in parallel and can take a while. Hyperparameter optimization is done by grid search.
#' @param catchment Name of the folder with the data given as a string.
#' @param data_inputs The kind of data to be used for the model run: "simple", "precip", "radiation", "all". See Details
#' @param model_or_optim "model" if a pretrained model should be load, or "optim" if a new model should be trained
#' @param cv_mode The crossvalidation scheme to be used. Can be either: "timeslice", or "repCV"
#' @param no_cores Number of cores used for training as integer.
#' @param plot_ts Should a dygraphy plot of the prediction and observation be plotted? TRUE/FALSE
#' @param save_importance_plot Should the importance plot for the random forest be saved in the folder. TRUE/FALSE
#'
#' @return None, results are saved in RF folder which is created inside the catchment folder.
#' @export
#'
#' @examples
#' wt_preprocess("Ybbs", data = data, year_range = c(1981, 2015))
#' wt_RandomForest(catchment = "Ybbs",
#'                 model_or_optim = "optim",
#'                 cv_mode = "timeslice",
#'                 no.cores = 17,
#'                 plot_ts = FALSE,
#'                 save_importance_plot = TRUE)
#'
wt_randomforest <- function(catchment,
                            data_inputs = NULL,
                            cv_mode,
                            no_cores = detectCores() - 1,
                            user_name = "R2D2",
                            plot_ts = FALSE,
                            save_importance_plot = TRUE,
                            random_seed = NULL
                            ){

  if(user_name == "R2D2") cat('No user_name was chosen! Default user "R2D2" is running the model.\n')

if(is.null(random_seed)) random_seed <- sample(1000:100000, 1)
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
        # load data
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

        # subset relevant features
        ranger_train <- train[, relevant_data]
        ranger_test <- test[, relevant_data]
        # remove NA rows resulting from Qdiff, Tmean_diff
        na_train <- which(is.na(ranger_train), arr.ind = TRUE)
        if(nrow(na_train) > 0){
          ranger_train <- ranger_train[-unique(na_train[,1]),]
        }
        na_test <- which(is.na(ranger_test), arr.ind = TRUE)
        if(nrow(na_test) > 0){
          ranger_test <- ranger_test[-unique(na_test[,1]),]
        }

        # create folder
        if(!file.exists(paste0(catchment, "/RF_new"))){
        cat(paste0("Create random forest folder for catchment ", catchment, ".\n"))
          dir.create(file.path(paste0(catchment, "/RF_new")))
        }

        if(!file.exists(paste0(catchment, "/RF_new/", model_name))){
          dir.create(file.path(paste0(catchment, "/RF_new/", model_name)))
        }

        # caret Model --------------------------------------------------------------------
        # train control
        if(!(cv_mode %in% c("timeslice", "repCV"))) {
          stop("cv_model can be either timeslice or repCV!")
        }
        if(cv_mode == "timeslice"){
          n_seeds <- ceiling((nrow(ranger_train) - 730)/60)
          seeds <- vector(mode = "list", length = n_seeds)
          set.seed(random_seed)
          for(i in 1:n_seeds) seeds[[i]] <- sample(10000, 200)
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
          for(i in 1:51) seeds[[i]] <- sample(10000, 200)
          tc <- caret::trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             allowParallel = TRUE,
                             verboseIter = TRUE,
                             seeds = seeds)
        }

          cat("Grid search parameters.\n")
          # random initial grid
          all_mtries <- 3:(ncol(ranger_train) - 1)
          # search grid with every 2nd mtry
          search_grid <- expand.grid(mtry = all_mtries[all_mtries %% 2 == 1],
                                    min.node.size = 2:10,
                                    splitrule = "extratrees")
          # train in parallel
          cl <- parallel::makePSOCKcluster(no_cores)
          doParallel::registerDoParallel(cl)
          ranger_fit <- caret::train(wt ~ .,
                                     data = ranger_train,
                                     method = "ranger",
                                     trControl = tc,
                                     tuneGrid = search_grid,
                                     num.threads = 1)
          parallel::stopCluster(cl)
          optimization_results <- ranger_fit$results
          optimization_results <- merge(search_grid,
                                        optimization_results,
                                        by = c("min.node.size", "splitrule", "mtry"),
                                        all.x = TRUE, sort = FALSE)
          write.csv(optimization_results,
                    file = paste0(catchment, "/RF_new/", model_name, "/optimization_scores.csv"),
                    row.names = FALSE)

          # try the best grid model and the two adjacent mtry values
          best_par <- ranger_fit$bestTune
          tg <- data.frame(mtry = best_par$mtry + c(-1, 0, 1),
                            splitrule = "extratrees",
                            min.node.size = best_par$min.node.size,
                           stringsAsFactors = FALSE)
          cl <- parallel::makePSOCKcluster(no_cores)
          doParallel::registerDoParallel(cl)
          suppressWarnings(final_model <- caret::train(wt ~ .,
                                                       data = ranger_train,
                                                       method = "ranger",
                                                       trControl = tc,
                                                       tuneGrid = tg,
                                                       num.threads = 1,
                                                       importance = "impurity"))
          parallel::stopCluster(cl)
          saveRDS(final_model, paste0(catchment, "/RF_new/", model_name, "/RF_model.rds"))
          cat("Finished hyperparameter optimization, best parameters:\n")
          for(i in 1:ncol(tg)) cat(names(tg)[i], ": ", tg[1, as.integer(i)], "\n", sep = "")

        # Prediction and model diagnostics -------------------------------------------------------
        cat("Start prediction and model diagnostics")
        run_time <- paste0(round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
                           " minutes")
        # save predicted values
        predict_RF <- predict(final_model, ranger_test)
        prediction <- data.frame(test$date, observed_wt = test$wt, predicted_wt = NA)
        if(nrow(na_test) > 0){
          prediction$predicted_wt[-na_test[, 1]] <- predict_RF
        } else {
          prediction$predicted_wt <- predict_RF
        }
        write.csv(prediction, paste0(catchment, "/RF_new/", model_name, "/predicted_values.csv"))

        # scores
        test_rmse <- round(RMSE(prediction = predict_RF, observation = ranger_test$wt), 4)
        test_mae <- round(MAE(y_obs = ranger_test$wt, y_pred = predict_RF), 4)

        optimization_results <- read.csv(paste0(catchment, "/RF_new/",
                                                model_name, "/optimization_scores.csv"))

        optimization_results <- optimization_results[, c(1:4, 6)]
        optimization_results[, 4:5] <- round(optimization_results[, 4:5], 4)
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

        if("model_scores.csv" %in% list.files(paste0(catchment, "/RF_new"))){
          model_scores <- read.csv(paste0(catchment, "/RF_new/", "model_scores.csv"),
                                   stringsAsFactors = FALSE)
          write.csv(rbind(model_scores, model_diagnostic, stringsAsFactors = FALSE),
                    paste0(catchment, "/RF_new/", "model_scores.csv"), row.names = FALSE)
        } else {
          write.csv(model_diagnostic, paste0(catchment, "/RF_new/", "model_scores.csv"), row.names = FALSE)
        }

        if(plot_ts){
          pred_xts <- xts::xts(prediction[, -1],
                                      order.by = as.POSIXct(prediction$test.date))
          print(dygraphs::dygraph(pred_xts_ranger, main = "Random forest prediction") %>%
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
            ggtitle("Information Value Summary - Random Forest")+
            guides(fill=F) +
            scale_fill_gradient(low="red", high="blue")
          ggsave(filename = paste0(catchment, "/RF_new/", model_name, "/importance_plot.png"), plot = p, device = "png",
                 dpi = "retina")
        }
      }
    }
  }
}
