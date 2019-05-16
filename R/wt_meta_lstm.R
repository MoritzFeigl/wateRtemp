#' lstm_metaf
#'
#' LSTM modelling function. Not available to the user. Use wt_lstm.
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
lstm_metaf <- function(x_train, y_train, x_val, y_val, data_inputs, train_mean, train_sd,
                       ts, u, bs, epochs, LSTM_type, n_predictions, n_features
                       ){

  # Parameters ---------------------------------------------------------------------------
  n_timesteps <- ts           # number of timesteps trained together
  batch_size <- bs            # number of points used for optimization -> backpropagation
  features <- ncol(x_train)

  model_name <- ifelse(n_predictions == 1,
                       paste0(data_inputs, "Model_", u, "units_", ts,
                              "ts_", bs, "bs_",epochs, "epochs_",
                              LSTM_type, "_single"),
                       paste0(data_inputs, "Model_", u, "units_", ts,
                              "ts_", bs, "bs_",epochs, "epochs_",
                              LSTM_type, "_", n_predictions, "multiple"))
  # Reshaping ----------------------------------------------------------------------------
  # train data: 3D array with dimesions(sample, n_timesteps, features)
  #             therefore the n_timesteps of observations before our prediction point
  # val data: 2D array with dimensions (sample, 1) -> 1 because we only predict 1 day
  reshaper <- function(X){
    if(is.null(nrow(X))) X <- as.matrix(X, ncol = 1)
    X_list <- vector(mode = "list", length = (nrow(X)-n_timesteps - n_predictions + 1))
    for(i in 1:(nrow(X) - n_timesteps - n_predictions + 1)){
      X_list[[i]] <- t(X[i:(i + (n_timesteps - 1)), ])
    }
    x_arr <- simplify2array(X_list)
    x_arr <- aperm(x_arr)
    return(x_arr)
  }
  # X arrays
  x_train_arr <- reshaper(x_train)
  x_val_arr <- reshaper(x_val)

  # Y arrays
  # single step prediction
  if(n_predictions == 1){
    # training
    y_train_arr <- numeric()
    for(i in 1:dim(x_train_arr)[1]){
      y_train_arr[i] <- y_train[i+n_timesteps]
    }
    # validation
    y_val_arr <- numeric()
    for(i in 1:dim(x_val_arr)[1]){
      y_val_arr[i] <- y_val[i+n_timesteps]
    }
  }
  # multiple step prediction
  if(n_predictions != 1){
    # training
    y_train_arr <- matrix(NA, nrow = dim(x_train_arr)[1], ncol = n_predictions)
    for(i in 1:dim(x_train_arr)[1]){
      y_train_arr[i,] <- y_train[(i+n_timesteps):(i+n_timesteps+n_predictions-1)]
    }
    # validation
    y_val_arr <- matrix(NA, nrow = dim(x_val_arr)[1], ncol = n_predictions)
    for(i in 1:dim(x_val_arr)[1]){
      y_val_arr[i,] <- y_val[(i+n_timesteps):(i+n_timesteps+n_predictions-1)]
    }
  }
  y_train_arr <- array(y_train_arr, dim = c(dim(x_train_arr)[1], n_predictions))
  y_val_arr <- array(y_val_arr, dim= c(dim(x_val_arr)[1], n_predictions))

  # Custom metric
  scaled_loss <- custom_metric(name = "scaled_loss", metric_fn = function(y_true, y_pred) {
    k_mean(k_mean(k_mean(k_sqrt((y_pred * train_sd + train_mean) - (y_true * train_sd + train_mean)))))
  })

  # Optimizer
  optimizer <- tf$train$AdamOptimizer()

  # Model definition ---------------------------------------------------------------------
  # 1 Layer LSTM
  if(LSTM_type == "lstm1"){
    model <- keras_model_sequential() %>%
      layer_lstm(units = u, input_shape = c(n_timesteps, n_features))  %>%
      layer_dense(units = n_predictions) %>%
      compile(loss = "mse",
              optimizer = optimizer,
              metric = scaled_loss)
  }

  # 2 Layer LSTM
  if(LSTM_type == "lstm2"){
    model <- keras_model_sequential() %>%
      layer_lstm(units = u, input_shape = c(n_timesteps, n_features), return_sequences = TRUE)  %>%
      layer_lstm(units = u)  %>%
      layer_dense(units = n_predictions) %>%
      compile(loss = "mse",
              optimizer = optimizer,
              metric = scaled_loss)
  }
  # 3 Layer LSTM
  if(LSTM_type == "lstm3"){
    model <- keras_model_sequential() %>%
      layer_lstm(units = u, input_shape = c(n_timesteps, n_features), return_sequences = TRUE)  %>%
      layer_lstm(units = u, return_sequences = TRUE)  %>%
      layer_lstm(units = u)  %>%
      layer_dense(units = n_predictions) %>%
      compile(loss = "mse",
              optimizer = optimizer,
              metric = scaled_loss)
  }

  # Training ------------------------------------------------------------------------------
  if(!dir.exists("LSTM")){
    dir.create("LSTM")
  }
  if(!dir.exists(paste0("LSTM/", model_name))){
    dir.create(paste0("LSTM/", model_name))
  }
  model_checkpoint <- callback_model_checkpoint(
    filepath = paste0("LSTM/", model_name, "/", model_name, ".hdf5"),
    save_best_only = TRUE, save_weights_only = TRUE)
  early_stopping <- callback_early_stopping(monitor = "val_loss",
                                            patience = 5,
                                            min_delta = 0.001,
                                            restore_best_weights = TRUE)

  history <- model %>% fit(
    x_train_arr, y_train_arr,
    epochs = epochs,
    batch_size = batch_size,
    callbacks = list(model_checkpoint, early_stopping),
    validation_data = list(x_val_arr, y_val_arr)
  )
  #save_model_hdf5(model, paste0("LSTM/", model_name, ".hdf5"))
  #model <- load_model_hdf5(paste0("LSTM/Model_30units_30ts_10bs_2epochs_lstm3_single.loss_0.13.hdf5"))

  # Model Scores -------------------------------------------------------------------------
  predict_LSTM <- predict(model, x_val_arr)
  residuals_LSTM <- (predict_LSTM*train_sd + train_mean) - (y_val_arr*train_sd + train_mean)
  MSE_LSTM <- mean(residuals_LSTM^2)
  RMSE_LSTM <- sqrt(mean(residuals_LSTM^2))
  NSE <- 1 - (sum((predict_LSTM- y_val_arr)^2, na.rm = TRUE) /
                sum( (y_val_arr - mean(y_val_arr, na.rm = TRUE))^2, na.rm = TRUE ) )

  # scores timestep wise for multiple step prediction
  if(n_predictions != 1){
    residuals_LSTM_m <- (predict_LSTM*train_sd + train_mean) - (y_val_arr*train_sd + train_mean)
    MSE_LSTM_m <- apply(residuals_LSTM_m, 2, function(x) mean(x^2))
    RMSE_LSTM_m <- sqrt(MSE_LSTM_m)
    NSE_m <- numeric(n_predictions)
    for(j in 1:ncol(predict_LSTM)){
      NSE_m[j] <- 1 - (sum((predict_LSTM[, j]- y_val_arr[, j])^2, na.rm = TRUE) /
                         sum( (y_val_arr[, j] - mean(y_val_arr[, j], na.rm = TRUE))^2, na.rm = TRUE ) )
    }

    multiple_score <- data.frame(model = model_name,
                                 timestep = 1:n_predictions,
                                 MSE = MSE_LSTM_m,
                                 RMSE = RMSE_LSTM_m,
                                 NSE = NSE_m)

    if("multiple_score.csv" %in% list.files("LSTM")){
      multiple_score_all <- read.csv("LSTM/multiple_score.csv")
      write.csv(rbind(multiple_score_all, multiple_score), "LSTM/multiple_score.csv")
    } else {
      write.csv(multiple_score, "LSTM/multiple_score.csv")
    }
  }

  if("model_scores.csv" %in% list.files("LSTM")){
    model_scores <- read.csv("LSTM/model_scores.csv")

  model_scores <- rbind(model_scores,
                        data.frame(model = model_name,
                          "n_timesteps" = ts,
                                   "units" = u,
                                   "batch_size" = bs,
                                   "validation_loss" = min(history$metrics$val_loss),
                                   "validation_scaled_loss" = min(history$metrics$val_scaled_loss),
                                   "mse" = MSE_LSTM,
                                   "RMSE" = RMSE_LSTM,
                                   "NSE" = NSE))
  write.csv(model_scores, "LSTM/model_scores.csv")

  } else {
    model_scores <- data.frame(model = model_name,
                                     "n_timesteps" = ts,
                                     "units" = u,
                                     "batch_size" = bs,
                                     "validation_loss" = min(history$metrics$val_loss),
                                     "validation_scaled_loss" = min(history$metrics$val_scaled_loss),
                                     "mse" = MSE_LSTM,
                                     "RMSE" = RMSE_LSTM,
                                     "NSE" = NSE)
    write.csv(model_scores, "LSTM/model_scores.csv")

  }



  # Training plot ------------------------------------------------------------------------
  png(paste0("LSTM/", model_name, "/", model_name, ".png"),
      width = 1200, heigh = 1000)
  par(mfrow = c(2, 1), mar = c(2, 4, 5, 2))
  plot(history$metrics$loss, xlab = "", main = "Model losses", ylim = c(0, max(history$metrics$loss)),
       ylab = "loss", type="l", col="blue")
  lines(history$metrics$val_loss, col = "darkgreen")
  legend("topright", c("training","validation"), col=c("blue", "darkgreen"), lty=c(1,1), bty = "n")
  par(mar = c(5, 4, 2, 2))
  plot(history$metrics$scaled_loss, ylim = c(0, max(history$metrics$scaled_loss)),
       xlab = "Epoch", ylab = "scaled loss", type="l", col="blue")
  lines(history$metrics$val_scaled_loss, col = "darkgreen")
  legend("topright", c("training","validation"), col=c("blue", "darkgreen"), lty=c(1,1), bty = "n")
  dev.off()
}
