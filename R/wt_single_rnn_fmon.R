#' single_rnn
#'
#' RNN modelling function. Not available to the user. Use wt_rnn.
#' @param catchment Name of the folder with the data given as a string.
#' @param x_train
#' @param y_train
#' @param x_val
#' @param y_val
#' @param ts
#' @param u
#' @param bs
#' @param epochs
#' @param layers
#' @param n_predictions
#' @param n_features
#'
#' @return
#'
#' @examples
wt_single_rnn_fmon <- function(catchment,
                               x_train, y_train, x_val, y_val, x_test, y_test, test, rnn_test,
                               user_name, data_inputs, rnn_type,
                               timesteps, units, dropout, layers,
                               batch_size, epochs, early_stopping_patience,
                               n_predictions, n_features, ensemble_runs, return_flag
){
  cat("\nRunning", as.character(rnn_type), "with:",
      "\n    layers =", layers,
      "\n    units =", units,
      "\n    timesteps =", timesteps,
      "\n    dropout =", dropout,
      "\n    batch_size =", batch_size,
      "\n    ensemble_runs =", ensemble_runs, "\n\n")
  # time and folders ---------------------------------------------------------------------
  start_time <- Sys.time()
  model_name <- paste0(rnn_type, "_", data_inputs)
  folder_name <- ifelse(n_predictions == 1,
                        paste0(batch_size, "batchsize_", units, "units_", timesteps,
                               "ts_", layers, "layers_", round(dropout, 2), "dropout_single"),
                        paste0(batch_size, "batchsize_", units, "units_", timesteps,
                               "ts_", layers, "layers_", round(dropout, 2), "dropout_multiple"))
  # Reshaping ----------------------------------------------------------------------------
  # remove all sub time series with length < ts + n_predictions
  train_long_ts <- sapply(x_train, nrow) >= (timesteps + n_predictions)
  val_long_ts <- sapply(x_val, nrow) >= (timesteps + n_predictions)
  test_long_ts <- sapply(x_test, nrow) >= (timesteps + n_predictions)
  x_train <- x_train[train_long_ts]
  y_train <- y_train[train_long_ts]
  x_val <- x_val[val_long_ts]
  y_val <- y_val[val_long_ts]
  x_test <- x_test[test_long_ts]
  y_test <- y_test[test_long_ts]
  # train data: 3D array with dimesions(sample, timesteps, n_features)
  #             therefore the n_timesteps of observations before our prediction point
  # val data: 2D array with dimensions (sample, n_predictions)
  # X arrays
  x_train_arr <- lapply(x_train, x_reshaper,
                        n_timesteps = timesteps, n_predictions = n_predictions)
  x_val_arr <- lapply(x_val, x_reshaper,
                      n_timesteps = timesteps, n_predictions = n_predictions)
  x_test_arr <- lapply(x_test, x_reshaper,
                       n_timesteps = timesteps, n_predictions = n_predictions)
  x_train_arr <- abind::abind(x_train_arr, along = 1)
  x_val_arr <- abind::abind(x_val_arr, along = 1)
  x_test_arr <- abind::abind(x_test_arr, along = 1)
  # Y arrays
  # single step prediction
  y_train_arr <- lapply(y_train, y_reshaper,
                        n_timesteps = timesteps, n_predictions = n_predictions)
  y_val_arr <- lapply(y_val, y_reshaper,
                      n_timesteps = timesteps, n_predictions = n_predictions)
  y_test_arr <- lapply(y_test, y_reshaper,
                       n_timesteps = timesteps, n_predictions = n_predictions)
  y_train_arr <- abind::abind(y_train_arr, along = 1)
  y_val_arr <- abind::abind(y_val_arr, along = 1)
  y_test_arr <- abind::abind(y_test_arr, along = 1)

  # Model definition ---------------------------------------------------------------------
  layer_rnn <- function(...) {
    if(rnn_type == "lstm") return(layer_lstm(...))
    if(rnn_type == "gru") return(layer_gru(...))
  }
  create_model <- function(){
    input <- layer_input(shape = c(timesteps, n_features))
    for(lay in 1:layers){
      # return sequances on for all except for last layer
      if(lay < layers) {
        return_sequences_flag <- TRUE
      } else {
        return_sequences_flag <- FALSE
      }
      # add lstm layer
      if(lay == 1) {
        output <- input %>% layer_rnn(units = units,
                                      return_sequences = return_sequences_flag,
                                      dropout = dropout)
      } else {
        output <- output %>% layer_rnn(units = units,
                                       return_sequences = return_sequences_flag,
                                       dropout = dropout)
      }
    }
    output <- output %>% layer_dense(n_predictions)
    model <- keras_model(input, output) %>%
      compile(loss = "mse",
              optimizer = "adam")
    return(model)
  }
  # Training ------------------------------------------------------------------------------
  if(!dir.exists(paste0(catchment, "/RNN_fmon"))){
    dir.create(paste0(catchment, "/RNN_fmon"))
  }
  if(!dir.exists(paste0(catchment, "/RNN_fmon/", model_name))){
    dir.create(paste0(catchment, "/RNN_fmon/", model_name))
  }
  if(!dir.exists(paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name))){
    dir.create(paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name))
  } else {
    version_numbers <- sum(grepl(folder_name,
                                 list.files(paste0(catchment, "/RNN_fmon/", model_name))))
    folder_name <- paste0(folder_name, "_version", version_numbers + 1)
    dir.create(paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name))
  }
  if(!dir.exists(paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name,
                        "/checkpoints"))){
    dir.create(paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name,
                      "/checkpoints"))
  }
  if(!dir.exists(paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name,
                        "/training_metrics"))){
    dir.create(paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name,
                      "/training_metrics"))
  }

  # Ensemble runs
  for(run in 1:ensemble_runs){

    model <- create_model()

    model_checkpoint <- callback_model_checkpoint(
      filepath = paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name,
                        "/checkpoints/model_checkpoint_run", run,
                        "_weights.hdf5"),
      save_best_only = TRUE)
    history <- model %>% fit(
      x_train_arr, y_train_arr,
      epochs = epochs,
      batch_size = batch_size,
      callbacks = list(
        callback_early_stopping(patience = early_stopping_patience,
                                restore_best_weights = TRUE),
        model_checkpoint),
      validation_data = list(x_val_arr, y_val_arr))

    if(run == 1){
      predict_rnn_val <- list(predict(model, x_val_arr))
      predict_rnn_test <- list(predict(model, x_test_arr))
    } else {
      predict_rnn_val[[run]] <- predict(model, x_val_arr)
      predict_rnn_test[[run]] <- predict(model, x_test_arr)
    }
    # Plot training losses
    png(paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name,
               "/training_metrics/plot_ensemble_member_", run, ".png"),
        width = 800, heigh = 600)
    plot(history$metrics$loss, xlab = "epochs", main = "Model loss",
         ylim = c(0, max(history$metrics$loss, na.rm = TRUE)),
         ylab = "model loss", type="l", col="blue")
    lines(history$metrics$val_loss, col = "darkgreen")
    legend("topright", c("training","validation"),
           col = c("blue", "darkgreen"), lty = c(1,1), bty = "n")
    dev.off()
    # save training metrics
    feather::write_feather(
      data.frame(epoch = 1:length(history$metrics$loss),
                 train_loss = history$metrics$loss,
                 val_loss = history$metrics$val_loss),
      paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name,
             "/training_metrics/loss_ensemble_member_", run, ".feather")
    )
  }
  # define model_subset depending on the number of ensembles
  if(ensemble_runs < 100){
    model_subset <- ifelse(ensemble_runs <= 10, ensemble_runs, 10)
  } else {
    model_subset <- ceiling(ensemble_runs*0.1)
  }

  # for a small ensemble number the subset = all runs
  if(model_subset == ensemble_runs & model_subset >= 1){
    # choose best models
    best_model_preds_val <- predict_rnn_val
    best_model_preds_test <- predict_rnn_test
    message("All ensemble runs were used for the prediction.")
  }

  # Create vector with avg prediction of best members of the ensemble
  if(model_subset < ensemble_runs & model_subset > 1){
    if(model_subset > 10){
      message("The best 10% of ensemble runs were used for the prediction.")
    } else {
      message("All ensemble runs were used for the prediction.")
    }

    # RMSE for all model runs, mean timestep RMSE for n_predictions > 1
    all_rmse_val <- sapply(predict_rnn_val,
                           function(x) mean(apply(x, 2, RMSE, observation = y_val_arr)))
    # choose best models
    best_model_preds_val <- predict_rnn_val[order(all_rmse_val) <= model_subset]
    best_model_preds_test <- predict_rnn_test[order(all_rmse_val) <= model_subset]
    # Delete all loss data frames and loss plots from the ensembles not used for prediction
    model_to_delete <- which(order(all_rmse_val) > model_subset)
    cp_files <- list.files(paste0(catchment, "/RNN_fmon/", model_name, "/",
                                  folder_name, "/checkpoints"))
    cp_numbers <- unlist(lapply(strsplit(cp_files, "_"),
                                function(x) as.integer(
                                  sub("run", "", x[grep("run", x)])
                                )))
    txt <- capture.output(file.remove(
      paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name,
             "/checkpoints/", cp_files[cp_numbers %in% model_to_delete])))
    txt <- capture.output(
      file.remove(
        paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name,
               "/training_metrics/plot_ensemble_member_",
               model_to_delete, ".png")))
    txt <- capture.output(
      file.remove(
        paste0(catchment, "/RNN_fmon/", model_name, "/", folder_name,
               "/training_metrics/loss_ensemble_member_",
               model_to_delete, ".feather")))
  }
  # get mean prediction
  if(model_subset != 1){
    # reshape list to length = n_predictions with mean predictions
    mean_pred_results_val <- best_model_preds_val %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      as.list() %>%
      lapply(matrix, ncol = model_subset) %>%
      lapply(function(x) apply(x, 1, mean)) %>%
      do.call(cbind, .)

    mean_pred_results_test <- best_model_preds_test %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      as.list() %>%
      lapply(matrix, ncol = model_subset) %>%
      lapply(function(x) apply(x, 1, mean)) %>%
      do.call(cbind, .)
    colnames(mean_pred_results_test) <- paste0(1:n_predictions)
  } else {
    mean_pred_results_val <- best_model_preds_val[[1]]
    mean_pred_results_test <- best_model_preds_test[[1]]
    message("Only one model was used and therefore no ensemble prediction was done.")
  }
  # Model Scores -------------------------------------------------------------------------
  if(n_predictions == 1){
    # scores
    RMSE_rnn_val <- RMSE(prediction = mean_pred_results_val,
                         observation = y_val_arr)
    NSE_val <- NSE(prediction = mean_pred_results_val,
                   observation = y_val_arr)

    RMSE_rnn_test <- RMSE(prediction = mean_pred_results_test,
                          observation = y_test_arr)
    NSE_test <- NSE(prediction = mean_pred_results_test,
                    observation = y_test_arr)
  } else {
    # scores timestep wise for multiple step prediction
    # Validation RMSE and NSE
    RMSE_rnn_val <- (mean_pred_results_val - y_val_arr) %>%
      apply(2, function(x) round(sqrt(mean(x^2)), 3))
    NSE_val <- numeric(n_predictions)
    for(j in 1:ncol(mean_pred_results_val)){
      NSE_val[j] <- NSE(prediction = mean_pred_results_val[, j],
                        observation = y_val_arr[, j])
    }
    #test RMSE and NSE
    RMSE_rnn_test <- (mean_pred_results_test - y_test_arr) %>%
      apply(2, function(x) round(sqrt(mean(x^2)), 3))
    NSE_test <- numeric(n_predictions)
    for(j in 1:ncol(mean_pred_results_test)){
      NSE_test[j] <- NSE(prediction = mean_pred_results_test[, j],
                         observation = y_val_arr[, j])
    }
  }
  # run time
  run_time <- paste0(round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
                     " minutes")
  if("model_scores.csv" %in% list.files(paste0(catchment, "/RNN_fmon"))){
    model_scores <- read.csv(paste0(catchment, "/RNN_fmon/model_scores.csv"))
    model_scores <- rbind(model_scores,
                          data.frame("user_name" = user_name,
                                     "rnn_type" = rnn_type,
                                     "data_inputs" = data_inputs,
                                     "start_time" = as.character(start_time),
                                     "run_time" = run_time,
                                     "layers" = layers,
                                     "n_predictions" = n_predictions,
                                     "pred_time_step" = 1:n_predictions,
                                     "units" = units,
                                     "timesteps" = timesteps,
                                     "max_epochs" = epochs,
                                     "early_stopping_patience" = early_stopping_patience,
                                     "batch_size" = batch_size,
                                     "dropout" = dropout,
                                     "ensemble_runs" = ensemble_runs,
                                     "RMSE_val" = round(RMSE_rnn_val, 4),
                                     "NSE_val" = NSE_val,
                                     "RMSE_test" = round(RMSE_rnn_test, 4),
                                     "NSE_test" = NSE_test,
                                     stringsAsFactors = FALSE))
    write.csv(model_scores, paste0(catchment, "/RNN_fmon/model_scores.csv"), row.names = FALSE)

  } else {
    model_scores <- data.frame("user_name" = user_name,
                               "rnn_type" = rnn_type,
                               "data_inputs" = data_inputs,
                               "start_time" = as.character(start_time),
                               "run_time" = run_time,
                               "layers" = layers,
                               "n_predictions" = n_predictions,
                               "pred_time_step" = 1:n_predictions,
                               "units" = units,
                               "timesteps" = timesteps,
                               "max_epochs" = epochs,
                               "early_stopping_patience" = early_stopping_patience,
                               "batch_size" = batch_size,
                               "dropout" = dropout,
                               "ensemble_runs" = ensemble_runs,
                               "RMSE_val" = round(RMSE_rnn_val, 4),
                               "NSE_val" = NSE_val,
                               "RMSE_test" = round(RMSE_rnn_test, 4),
                               "NSE_test" = NSE_test,
                               stringsAsFactors = FALSE)
    write.csv(model_scores, paste0(catchment, "/RNN_fmon/model_scores.csv"), row.names = FALSE)
  }

  # get predicted values in the same dim/format as the input data
  test_prediction_full <- test[test_long_ts] %>%
    lapply(function(x) x[-c(1:timesteps), ]) %>%
    do.call(rbind, .) %>%
    select(date) %>%
    slice(1:(n()-n_predictions+1)) %>%
    pull(date) %>%
    as.POSIXct() %>%
    data.frame(date = .,
               prediction = mean_pred_results_test[, 1], stringsAsFactors = FALSE) %>%
    merge(rnn_test, ., by = "date", all.x = TRUE) %>%
    select("date", starts_with("prediction"))
  # save predicted values
  feather::write_feather(test_prediction_full,
                         paste0(catchment, "/RNN_fmon/", model_name, "/",
                                folder_name, "/test_prediction.feather"))
  cat("Finished run with validation rmse =", RMSE_rnn_val, "\n\n")
  if(return_flag) return(mean(RMSE_rnn_val))
}
