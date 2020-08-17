#' wt_rnn
#'
#' @param catchment Name of the folder with the data given as a string.
#' @param data_inputs The kind of data to be used for the model run: "simple", "precip", "radiation", "all". See Details
#' @param ts Number of timesteps used for prediction, integer
#' @param u Number of units of the LSTM layers, integer
#' @param bs batch size for training, integer
#' @param epochs Number of epochs, integer
#' @param layers Number RNN LSTM layers to use, integer
#' @param n_predictions Number of timesteps to be predicted by LSTM, integer
#'
#' @return None
#' @export
#'
#' @examples
wt_rnn_fmon <- function(catchment,
                   data_inputs = NULL,
                   rnn_type = NULL,
                   layers = NULL,
                   n_predictions = c(1, 3, 7),
                   timesteps = c(60, 90),
                   units = c(30, 60),
                   dropout = 0.0,
                   batch_size = c(10, 50, 100),
                   epochs = 100,
                   early_stopping_patience = 5,
                   ensemble_runs = 5,
                   user_name = "R2D2"){

  if(user_name == "R2D2") cat('No user_name was chosen! Default user "R2D2" is running the model.\n')

  if(sum(list.files() %in% catchment) < 1){
    stop("Cannot find catchment folder(s) in your current working directory.")
  }
  if(is.null(data_inputs)){
    stop('\nChoose a valid data_input:
            "simple"    = Q, Tmean and water temperatur observations
            "precip"    = "simple" + additional precipitation observations
            "radiation" = "simple" + additional longwave radiation observations
            "all"       = all the above mentioned observations')
  }
  if(is.null(layers)){
    stop('\nChoose a number of layers. Has to be an integer.')
  }
  if(is.null(rnn_type)){
    stop('\nChoose a valid rnn type. Can either be "lstm", "gru" or both.')
  }

  # variables for loop
  data_inputs_meta <- data_inputs

  for(catchment in catchment){
    cat("*** Starting computation for catchment", catchment, "***\n")
    if(sum(list.files() %in% catchment) != 1){
      message(paste0("ERROR: There is no folder named ", catchment, " in your current working directory."))
      next
    }
    for(data_inputs in data_inputs_meta){
      # check if there is seperate radiation data
      rad_data <- length(list.files(catchment, pattern = "radiation_")) > 0
      # in case of radiation or all data_input, load radiation data
      if(data_inputs == "radiation" & rad_data | data_inputs == "all" & rad_data){
        data_prefix <- "radiation_"
      } else {
        data_prefix <- ""
      }

      train_ini <- read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
      test_ini <- read_feather(paste0(catchment, "/test_", data_prefix, "data.feather"))
      part_training <- nrow(train_ini)/4 * 3
      train_length <- floor(part_training)
      val_ini <- train_ini[(train_length + 1):nrow(train_ini), ]
      train_ini <- train_ini[1:train_length, ]
      relevant_data <- c("date", "Q", "Tmean", "wt",
                         "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5", "Fmon.6",
                         "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11")
      if(data_inputs == "precip") relevant_data <- c(relevant_data, "RR")
      if(data_inputs == "radiation") relevant_data <- c(relevant_data, "GL")
      if(data_inputs == "all") relevant_data <- c(relevant_data, "RR", "GL")

      n_features <- length(relevant_data) - 2
      rnn_train <- train_ini[, relevant_data]
      rnn_val <- val_ini[, relevant_data]
      rnn_test <- test_ini[, relevant_data]
      # Feature scaling
      train_means <- apply(rnn_train[, -1], 2, mean, na.rm = TRUE)
      train_sds <- apply(rnn_train[, -1], 2, sd, na.rm = TRUE)
      features_to_scale <- colnames(rnn_train)[!(colnames(rnn_train) %in% c("date", "wt"))]
      for(col in features_to_scale){
        rnn_train[, col] <- (rnn_train[, col] - train_means[col]) / train_sds[col]
        rnn_val[, col] <- (rnn_val[, col] - train_means[col]) / train_sds[col]
        rnn_test[, col] <- (rnn_test[, col] - train_means[col]) / train_sds[col]
      }

      # check if there are time gaps in the data -> split
      train <- data_splitter_for_rnn(rnn_train, data_name = "train", catchment = catchment)
      val <- data_splitter_for_rnn(rnn_val, data_name = "validation", catchment = catchment)
      test <- data_splitter_for_rnn(rnn_test, data_name = "test", catchment = catchment)
      # split in x & y in
      x_train <- lapply(train, function(x) x %>% select(-wt, -date) %>% as.matrix())
      y_train <- lapply(train, function(x) x %>% select(wt) %>% as.matrix())
      x_val <- lapply(val, function(x) x %>% select(-wt, -date) %>% as.matrix())
      y_val <- lapply(val, function(x) x %>% select(wt) %>% as.matrix())
      x_test <- lapply(test, function(x) x %>% select(-wt, -date) %>% as.matrix())
      y_test <- lapply(test, function(x) x %>% select(wt) %>% as.matrix())
      # Define grid and apply function
      grid <- expand.grid("rnn_type" = rnn_type,
                          "timestepsts" = timesteps,
                          "units" = units,
                          "dropout" = dropout,
                          "batch_size" = batch_size,
                          "layers" = layers,
                          "n_predictions" = n_predictions)

      # check if its a single run -> return flag RMSE output
      return_flag <- ifelse(
        length(catchment) == 1 & length(data_inputs) == 1 & nrow(grid) == 1,
        TRUE, FALSE)

      val_rmse <- mapply(wt_single_rnn_fmon,
                         rnn_type = grid$rnn_type,
                         timesteps = grid$timesteps,
                         units = grid$units,
                         batch_size = grid$batch_size,
                         dropout = grid$dropout,
                         layers = grid$layers,
                         n_predictions = grid$n_predictions,
                         MoreArgs = list(catchment = catchment,
                                         x_train = x_train,
                                         y_train = y_train,
                                         x_val = x_val,
                                         y_val = y_val,
                                         x_test = x_test,
                                         y_test = y_test,
                                         test = test,
                                         epochs = epochs,
                                         rnn_test = rnn_test,
                                         user_name = user_name,
                                         n_features = n_features,
                                         data_inputs = data_inputs,
                                         ensemble_runs = ensemble_runs,
                                         early_stopping_patience = early_stopping_patience,
                                         return_flag = return_flag))
      if(return_flag) return(val_rmse)
    }
  }
}
