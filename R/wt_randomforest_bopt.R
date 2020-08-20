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
wt_randomforest_opt <- function(catchment,
                            data_inputs = NULL,
                            model_or_optim,
                            cv_mode,
                            no_cores = detectCores() - 1,
                            plot_ts = FALSE,
                            save_importance_plot = FALSE,
                            user_name = "R2D2",
                            n_iter = 20,
                            n_random_initial_points = 20){

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
  if(sum(model_or_optim %in% c("model", "optim")) == 0){
    stop('\nChoose a valid model_or_optim option:
            "model"    = pretrained model created with wt_randomforest will be loaded
            "optim"    = a new model with hyperparameter optimization will be trained')
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

        # caret Model --------------------------------------------------------------------------
        # train control
        if(!(cv_mode %in% c("timeslice", "repCV"))) stop("cv_model can be either timeslice or repCV!")
        if(cv_mode == "timeslice"){
          n_seeds <- ceiling((nrow(ranger_train) - 730)/60)
          seeds <- vector(mode = "list", length = n_seeds)
          set.seed(1234)
          for(i in 1:n_seeds) seeds[[i]] <- sample(10000, 100)
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
          # set.seed(1242)
          # seeds <- as.list(sample(10000, 51))
          seeds <- vector(mode = "list", length = 51)
          set.seed(1234)
          for(i in 1:51) seeds[[i]] <- sample(10000, 100)
          tc <- caret::trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             allowParallel = TRUE,
                             verboseIter = TRUE,
                             seeds = seeds)
        }

        if(model_or_optim == "optim"){



          cat("Computing initial grid.\n")
          # random initial grid
          all_mtries <- 3:(ncol(ranger_train) - 1)
          initial_grid <- data.frame(
            min.node.size = sample(1:10, n_random_initial_points, replace = TRUE),
            splitrule = sample(c("variance", "extratrees"),
                               n_random_initial_points, replace = TRUE),
            mtry = sample(all_mtries, n_random_initial_points, replace = TRUE)
          )
          cl <- parallel::makePSOCKcluster(no_cores)
          doParallel::registerDoParallel(cl)
          ranger_fit <- caret::train(wt ~ .,
                                     data = ranger_train,
                                     method = "ranger",
                                     trControl = tc,
                                     tuneGrid = initial_grid,
                                     num.threads = 1,
                                     importance = "impurity")
          optimization_results <- ranger_fit$results
          optimization_results <- merge(initial_grid,
                                        optimization_results,
                                        by = c("min.node.size", "splitrule", "mtry"),
                                        all.x = TRUE, sort = FALSE)

          parallel::stopCluster(cl)

          write.csv(optimization_results,
                    file = paste0(catchment, "/RF_new/", model_name, "/optimization_scores.csv"),
                    row.names = FALSE)
          initial_grid$Value <- optimization_results$RMSE * -1
          for (i in 1:n_random_initial_points){
            initial_grid$splitrule[i] <- which(c("variance", "extratrees") == initial_grid$splitrule[i])
          }
          initial_grid$splitrule <- as.integer(initial_grid$splitrule)
          # Bayesian hyperparameter optimization
          Bopt_rf <- function(min.node.size, splitrule, mtry) {
            tg <- data.frame("min.node.size" = min.node.size,
                             "mtry" = mtry,
                             "splitrule" = c("variance", "extratrees")[splitrule])
            cl <- parallel::makePSOCKcluster(no_cores)
            doParallel::registerDoParallel(cl)
            results <- caret::train(wt ~ .,
                                    data = ranger_train,
                                    method = "ranger",
                                    trControl = tc,
                                    tuneGrid = tg,
                                    num.threads = 1)
            parallel::stopCluster(cl)
            optimization_results <- read.csv(paste0(catchment, "/RF_new/",
                                                    model_name, "/optimization_scores.csv"))
            optimization_results <- rbind(optimization_results, results$results)
            write.csv(optimization_results,
                      file = paste0(catchment, "/RF_new/", model_name, "/optimization_scores.csv"),
                      row.names = FALSE)


            return(list("Score" = results$results$RMSE*-1, "Pred" = 0))
          }
          Bopt_rnn <- suppressWarnings(
            rBayesianOptimization::BayesianOptimization(Bopt_rf,
                                                                  bounds = list(
                                                                    min.node.size = c(1L, 10L),
                                                                    splitrule = c(1L, 2L),
                                                                    mtry = c(3L, as.integer(ncol(ranger_train) - 1))
                                                                    ),
                                                                  n_iter = n_iter,
                                                                  init_grid_dt = initial_grid,
                                                                  acq = "ucb", kappa = 10, eps = 0.0,
                                                                  verbose = TRUE)
          )

          # train optimized parameter model
          best_par <- Bopt_rnn$History[which.max(Bopt_rnn$History$Value), ]
          tg <- data.frame(mtry = best_par$mtry,
                            splitrule = c("variance", "extratrees", "maxstat", "beta")[best_par$splitrule],
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
        }

        # Load optimized model
        if(model_or_optim == "model"){
          cat("Loading optimized model.\n")
          ranger_fit <- readRDS(paste0(catchment, "/RF_new/", model_name, "/optimized_RF_model_", cv_mode, ".rds"))
        }

        # Prediction and model diagnostics -------------------------------------------------------
        cat("Start prediction and model diagnostics")
        run_time <- paste0(round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
                           " minutes")
        # save predicted values
        predict_RF <- predict(final_model, ranger_test)
        prediction <- data.frame(test$date, observed_wt = test$wt, predicted_wt = NA)
        prediction$predicted_wt[-na_test[, 1]] <- predict_RF
        write.csv(prediction, paste0(catchment, "/RF_new/", model_name, "/predicted_values.csv"))

        # scores
        test_rmse <- RMSE(prediction = predict_RF, observation = ranger_test$wt)
        test_mae <- MAE(y_obs = ranger_test$wt, y_pred = predict_RF)

        optimization_results <- read.csv(paste0(catchment, "/RF_new/",
                                                model_name, "/optimization_scores.csv"))

        optimization_results <- optimization_results[, c(1:4, 6)]
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
          predict_ranger <- predict(final_model, ranger_test)
          pred_xts_ranger <- xts::xts(cbind(ranger_test, "predictions" = predict_ranger),
                                      order.by = as.POSIXct(paste0(ranger_test$year, "-", ranger_test$mon, "-", ranger_test$day)))

          pred_xts_ranger <- xts::xts(prediction[, -1],
                                      order.by = as.POSIXct(prediction$test.date))

          print(dygraphs::dygraph(pred_xts_ranger, main = "Random forest prediction") %>%
                  dygraphs::dyRangeSelector())
        }

        if(save_importance_plot){
          cat("\nSaving importance plot in", getwd(), "\n")
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
          ggsave(filename = paste0(catchment, "/RF_new/", model_name, "/RF_importance_plot_", cv_mode, ".png"), plot = p, device = "png",
                 dpi = "retina")
        }
      }
    }
  }
}
