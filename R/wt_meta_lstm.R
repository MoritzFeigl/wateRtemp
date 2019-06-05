#' lstm_metaf
#'
#' LSTM modelling function. Not available to the user. Use wt_lstm.
#' @param catchment Name of the folder with the data given as a string.
#' @param x_train
#' @param y_train
#' @param x_val
#' @param y_val
#' @param ts
#' @param u
#' @param bs
#' @param epochs
#' @param LSTM_type
#' @param n_predictions
#' @param n_features
#'
#' @return
#'
#' @examples
lstm_metaf <- function(catchment,
                       x_train, y_train, x_val, y_val, x_test, y_test,
                       data_inputs, train_mean, train_sd,
                       ts, u, bs, epochs, LSTM_type, n_predictions, n_features,
                       user_name, ensemble_runs
){

  # Parameters ---------------------------------------------------------------------------
  start_time <- Sys.time()
  n_timesteps <- ts           # number of timesteps trained together
  batch_size <- bs            # number of points used for optimization -> backpropagation
  features <- ncol(x_train)

  model_name <- ifelse(n_predictions == 1,
                       paste0(data_inputs, "Model_", u, "units_", ts,
                              "ts_", LSTM_type, "_single"),
                       paste0(data_inputs, "Model_", u, "units_", ts,
                              "ts_", LSTM_type, "_", n_predictions, "multiple"))
  folder_name <- paste0(epochs, "epochs_", bs, "batchsize")
  # Reshaping ----------------------------------------------------------------------------
  # remove all sub time series with length < ts + n_predictions
  train_long_ts <- sapply(x_train, nrow) >= (ts + n_predictions)
  val_long_ts <- sapply(x_val, nrow) >= (ts + n_predictions)
  test_long_ts <- sapply(x_test, nrow) >= (ts + n_predictions)
  x_train <- x_train[train_long_ts]
  y_train <- y_train[train_long_ts]
  x_val <- x_val[val_long_ts]
  y_val <- y_val[val_long_ts]
  x_test <- x_test[test_long_ts]
  y_test <- y_test[test_long_ts]


  # train data: 3D array with dimesions(sample, n_timesteps, features)
  #             therefore the n_timesteps of observations before our prediction point
  # val data: 2D array with dimensions (sample, 1) -> 1 because we only predict 1 day
  # X arrays
  x_train_arr <- lapply(x_train, x_reshaper,
                        n_timesteps = n_timesteps, n_predictions = n_predictions)
  x_val_arr <- lapply(x_val, x_reshaper,
                      n_timesteps = n_timesteps, n_predictions = n_predictions)
  x_test_arr <- lapply(x_test, x_reshaper,
                       n_timesteps = n_timesteps, n_predictions = n_predictions)
  x_train_arr <- abind::abind(x_train_arr, along = 1)
  x_val_arr <- abind::abind(x_val_arr, along = 1)
  x_test_arr <- abind::abind(x_test_arr, along = 1)

  # Y arrays
  # single step prediction
  y_train_arr <- lapply(y_train, y_reshaper,
                        n_timesteps = n_timesteps, n_predictions = n_predictions)
  y_val_arr <- lapply(y_val, y_reshaper,
                      n_timesteps = n_timesteps, n_predictions = n_predictions)
  y_test_arr <- lapply(y_test, y_reshaper,
                       n_timesteps = n_timesteps, n_predictions = n_predictions)
  y_train_arr <- abind::abind(y_train_arr, along = 1)
  y_val_arr <- abind::abind(y_val_arr, along = 1)
  y_test_arr <- abind::abind(y_test_arr, along = 1)

  # # Custom metric
  # scaled_loss <- custom_metric(name = "scaled_loss", metric_fn = function(y_true, y_pred) {
  #   k_mean(k_mean(k_mean(k_sqrt((y_pred * train_sd + train_mean) - (y_true * train_sd + train_mean)))))
  # })

  # Optimizer
  #optimizer <- tf$train$AdamOptimizer()
  optimizer <- optimizer_adam()

  # Model definition ---------------------------------------------------------------------
  # 1 Layer LSTM
  if(LSTM_type == "lstm1"){
    create_model <- function(){
      model <- keras_model_sequential() %>%
        layer_lstm(units = u, input_shape = c(n_timesteps, n_features))  %>%
        layer_dense(units = n_predictions) %>%
        compile(loss = "mse",
                optimizer = optimizer)
      return(model)
    }
  }

  # 2 Layer LSTM
  if(LSTM_type == "lstm2"){
    create_model <- function(){
      model <- keras_model_sequential() %>%
        layer_lstm(units = u, input_shape = c(n_timesteps, n_features), return_sequences = TRUE)  %>%
        layer_lstm(units = u)  %>%
        layer_dense(units = n_predictions) %>%
        compile(loss = "mse",
                optimizer = optimizer)
      return(model)
    }

  }
  # 3 Layer LSTM
  if(LSTM_type == "lstm3"){
    create_model <- function(){
      model <- keras_model_sequential() %>%
        layer_lstm(units = u, input_shape = c(n_timesteps, n_features), return_sequences = TRUE)  %>%
        layer_lstm(units = u, return_sequences = TRUE)  %>%
        layer_lstm(units = u)  %>%
        layer_dense(units = n_predictions) %>%
        compile(loss = "mse",
                optimizer = optimizer)
      return(model)
    }
  }

  # Training ------------------------------------------------------------------------------
  if(!dir.exists(paste0(catchment, "/LSTM"))){
    dir.create(paste0(catchment, "/LSTM"))
  }
  if(!dir.exists(paste0(catchment, "/LSTM/", model_name))){
    dir.create(paste0(catchment, "/LSTM/", model_name))
  }
  if(!dir.exists(paste0(catchment, "/LSTM/", model_name, "/", folder_name))){
    dir.create(paste0(catchment, "/LSTM/", model_name, "/", folder_name))
  }
  if(!dir.exists(paste0(catchment, "/LSTM/", model_name, "/", folder_name, "/checkpoints"))){
    dir.create(paste0(catchment, "/LSTM/", model_name, "/", folder_name, "/checkpoints"))
  }
  if(!dir.exists(paste0(catchment, "/LSTM/", model_name, "/", folder_name, "/training_metrics"))){
    dir.create(paste0(catchment, "/LSTM/", model_name, "/", folder_name, "/training_metrics"))
  }

  # Ensemble runs
  for(run in 1:ensemble_runs){
    model <- create_model()
    model_checkpoint <- callback_model_checkpoint(
      filepath = paste0(catchment, "/LSTM/", model_name, "/", folder_name,
                        "/checkpoints/model_checkpoint_run", run,
                        "_weights.{epoch:02d}.hdf5"),
      save_best_only = TRUE, save_weights_only = TRUE, period = epochs)
    history <- model %>% fit(
      x_train_arr, y_train_arr,
      epochs = epochs,
      batch_size = batch_size,
      callbacks = list(model_checkpoint),# early_stopping),
      validation_data = list(x_val_arr, y_val_arr))
    #save_model_weights_hdf5(model, paste0(catchment, "/LSTM/", model_name, "/", folder_name,
    #                                      "/model_checkpoint_run", run, ".hdf5"))
    if(run == 1){
      predict_LSTM_val <- predict(model, x_val_arr)
      predict_LSTM_test <- predict(model, x_test_arr)
    } else {
      predict_LSTM_val <- cbind(predict_LSTM_val, predict(model, x_val_arr))
      predict_LSTM_test <- cbind(predict_LSTM_test, predict(model, x_test_arr))
    }

    png(paste0(catchment, "/LSTM/", model_name, "/", folder_name,
               "/training_metrics/plot_ensemble_member_", run, ".png"),
        width = 800, heigh = 600)
    plot(history$metrics$loss, xlab = "epochs", main = "Model loss", ylim = c(0, max(history$metrics$loss)),
         ylab = "model loss", type="l", col="blue")
    lines(history$metrics$val_loss, col = "darkgreen")
    legend("topright", c("training","validation"), col=c("blue", "darkgreen"), lty=c(1,1), bty = "n")
    dev.off()
    feather::write_feather(
      data.frame(epoch = 1:epochs,
                 train_loss = history$metrics$loss,
                 val_loss = history$metrics$val_loss),
      paste0(catchment, "/LSTM/", model_name, "/", folder_name,
             "/training_metrics/loss_ensemble_member_", run, ".feather")
    )



  }

  # choose best ensemble runs -> delete others, take redictions only from the best

  # Calculating RMSE for all members of the ensemble
  model_rmse <- function(model_pred, y){
    residuals_model <- model_pred - y
    RMSE <- sqrt(mean(residuals_model^2))
    return(RMSE)
  }
  all_rmse_val <- apply(predict_LSTM_val, 2, model_rmse, y = y_val_arr)

  # define model_subset depending on the number of ensembles
  if(ensemble_runs < 100){
    model_subset <- ifelse(ensemble_runs <= 10, ensemble_runs, 10)
  } else {
    model_subset <- ceiling(ensemble_runs*0.1)
  }
  # choose best models
  best_model_preds_val <- predict_LSTM_val[, order(all_rmse_val) <= model_subset]

  # Create vector with avg prediction of best 10% of the members (models) of the ensemble
  if(model_subset == ensemble_runs & model_subset > 1){
    message("All ensemble runs were used for the prediction.")
  }
  if(model_subset < ensemble_runs & model_subset >= 1){
    if(model_subset > 10){
      message("The best 10% of ensemble runs were used for the prediction.")
    } else {
      message("The best 10 of ensemble runs were used for the prediction.")
    }
    # Delete all loss data frames and loss plots from the ensembles not used for prediction
    # all checkpoints
    model_to_delete <- which(order(all_rmse_val) > model_subset)
    cp_files <- list.files(paste0(catchment, "/LSTM/", model_name, "/",
                                  folder_name, "/checkpoints"))
    cp_numbers <- unlist(lapply(strsplit(cp_files, "_"),
                                function(x) as.integer(sub("run", "", x[grep("run", x)]))))
    txt <- capture.output(file.remove(paste0(catchment, "/LSTM/", model_name, "/", folder_name,
                                             "/checkpoints/", cp_files[cp_numbers %in% model_to_delete])))

    # all training metrics
    txt <- capture.output(
      file.remove(
        paste0(catchment, "/LSTM/", model_name, "/", folder_name,
               "/training_metrics/plot_ensemble_member_",
               model_to_delete, ".png")))
    txt <- capture.output(
      file.remove(
        paste0(catchment, "/LSTM/", model_name, "/", folder_name,
               "/training_metrics/loss_ensemble_member_",
               model_to_delete, ".feather")))
  }


  if(model_subset != 1){
    mean_pred_results_val <- apply(best_model_preds_val, 1, mean)
    mean_pred_results_test <- apply(best_model_preds_test, 1, mean)
  } else {
    mean_pred_results_val <- best_model_preds_val
    mean_pred_results_test <- best_model_preds_test
    message("Only one model was used and therefore no ensemble prediction was done.")
  }


  # Model Scores -------------------------------------------------------------------------
  # save predicted values
  feather::write_feather(data.frame("predicted_values" = mean_pred_results_test*train_sd + train_mean),
                         paste0(catchment, "/LSTM/", model_name, "/",
                                folder_name, "/predicted_values.feather"))
  # scores
  residuals_LSTM_val <- (mean_pred_results_val*train_sd + train_mean) - (y_val_arr*train_sd + train_mean)
  RMSE_LSTM_val <- round(sqrt(mean(residuals_LSTM_val^2)), 3)
  NSE_val <- round(
    1 - (sum((mean_pred_results_val- y_val_arr)^2, na.rm = TRUE) /
           sum( (y_val_arr - mean(y_val_arr, na.rm = TRUE))^2, na.rm = TRUE ) ),
    3)
  residuals_LSTM_test <- (mean_pred_results_test*train_sd + train_mean) - (y_test_arr*train_sd + train_mean)
  RMSE_LSTM_test <- round(sqrt(mean(residuals_LSTM_test^2)), 3)
  NSE_test <- round(
    1 - (sum((mean_pred_results_test- y_test_arr)^2, na.rm = TRUE) /
           sum( (y_test_arr - mean(y_test_arr, na.rm = TRUE))^2, na.rm = TRUE ) ),
    3)
  run_time <- paste0(
    round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
    " minutes")

  # scores timestep wise for multiple step prediction
  if(n_predictions != 1){
    residuals_LSTM_m_val <- (mean_pred_results_val*train_sd + train_mean) - (y_val_arr*train_sd + train_mean)
    MSE_LSTM_m_val <- apply(residuals_LSTM_m_val, 2, function(x) mean(x^2))
    RMSE_LSTM_m_val <- round(sqrt(MSE_LSTM_m_val), 3)
    NSE_m_val <- numeric(n_predictions)
    for(j in 1:ncol(mean_pred_results_val)){
      NSE_m_val[j] <- round(
        1 - (sum((mean_pred_results_val[, j]- y_val_arr[, j])^2, na.rm = TRUE) /
               sum( (y_val_arr[, j] - mean(y_val_arr[, j], na.rm = TRUE))^2,
                    na.rm = TRUE ) ), 3)
    }

    residuals_LSTM_m_test <- (mean_pred_results_test*train_sd + train_mean) - (y_test_arr*train_sd + train_mean)
    MSE_LSTM_m_test <- apply(residuals_LSTM_m_test, 2, function(x) mean(x^2))
    RMSE_LSTM_m_test <- round(sqrt(MSE_LSTM_m_test), 3)
    NSE_m_test <- numeric(n_predictions)
    for(j in 1:ncol(mean_pred_results_test)){
      NSE_m_test[j] <- round(
        1 - (sum((mean_pred_results_test[, j]- y_test_arr[, j])^2, na.rm = TRUE) /
               sum( (y_test_arr[, j] - mean(y_test_arr[, j], na.rm = TRUE))^2,
                    na.rm = TRUE ) ), 3)
    }

    multiple_score <- data.frame("user_name" = user_name,
                                 "model" = model_name,
                                 "start_time" = as.character(start_time),
                                 "run_time" = run_time,
                                 "time_step" = 1:n_predictions,
                                 "RMSE_val" = RMSE_LSTM_m_val,
                                 "NSE_val" = NSE_m_val,
                                 "RMSE_test" = RMSE_LSTM_m_test,
                                 "NSE_test" = NSE_m_test)


    if("multiple_score.csv" %in% list.files(paste0(catchment, "/LSTM"))){
      multiple_score_all <- read.csv(paste0(catchment, "/LSTM/multiple_score.csv"))
      write.csv(rbind(multiple_score_all, multiple_score),
                paste0(catchment, "/LSTM/multiple_score.csv"), row.names = FALSE)
    } else {
      write.csv(multiple_score, paste0(catchment, "/LSTM/multiple_score.csv"), row.names = FALSE)
    }
  }

  if("model_scores.csv" %in% list.files(paste0(catchment, "/LSTM"))){
    model_scores <- read.csv(paste0(catchment, "/LSTM/model_scores.csv"))

    model_scores <- rbind(model_scores,
                          data.frame("user_name" = user_name,
                                     "model" = model_name,
                                     "data_inputs" = data_inputs,
                                     "start_time" = as.character(start_time),
                                     "run_time" = run_time,
                                     "LSTM_type" = LSTM_type,
                                     "n_timesteps" = ts,
                                     "units" = u,
                                     "n_predictions" = n_predictions,
                                     "epochs" = epochs,
                                     "batch_size" = bs,
                                     "ensemble_runs" = ensemble_runs,
                                     "model_val_loss" = round(min(history$metrics$val_loss), 3),
                                     "RMSE_val" = RMSE_LSTM_val,
                                     "NSE_val" = NSE_val,
                                     "RMSE_test" = RMSE_LSTM_test,
                                     "NSE_test" = NSE_test))
    write.csv(model_scores, paste0(catchment, "/LSTM/model_scores.csv"), row.names = FALSE)

  } else {
    model_scores <- data.frame("user_name" = user_name,
                               "model" = model_name,
                               "data_inputs" = data_inputs,
                               "start_time" = as.character(start_time),
                               "run_time" = run_time,
                               "LSTM_type" = LSTM_type,
                               "n_timesteps" = ts,
                               "units" = u,
                               "n_predictions" = n_predictions,
                               "epochs" = epochs,
                               "batch_size" = bs,
                               "ensemble_runs" = ensemble_runs,
                               "model_val_loss" = round(min(history$metrics$val_loss), 3),
                               "RMSE_val" = RMSE_LSTM_val,
                               "NSE_val" = NSE_val,
                               "RMSE_test" = RMSE_LSTM_test,
                               "NSE_test" = NSE_test)
    write.csv(model_scores, paste0(catchment, "/LSTM/model_scores.csv"), row.names = FALSE)

  }
}
