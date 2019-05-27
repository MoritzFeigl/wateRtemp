
#' wt_lstm
#'
#' @param catchment Name of the folder with the data given as a string.
#' @param data_inputs The kind of data to be used for the model run: "simple", "precip", "radiation", "all". See Details
#' @param ts Number of timesteps used for prediction, integer
#' @param u Number of units of the LSTM layers, integer
#' @param bs batch size for training, integer
#' @param epochs Number of epochs, integer
#' @param lstm_layers Number of LSTM layers to use, integer
#' @param n_predictions Number of timesteps to be predicted by LSTM, integer
#'
#' @return None
#' @export
#'
#' @examples
wt_lstm <- function(catchment,
                    data_inputs = NULL,
                    ts = c(30, 90, 180, 365),
                    u = c(30, 60, 90),
                    bs = c(10, 50, 100),
                    epochs = c(100),
                    lstm_layers = NULL,
                    n_predictions = c(1, 3, 7)){

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
  if(is.null(lstm_layers)){
    stop('\nChoose a valid lstm_layers or a vector of lstm_layers:
            1 = 1 LSTM layer
            2 = 2 LSTM layer
            3 = 3 LSTM layer')
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
      LSTM_type <- paste0("lstm", lstm_layers)

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
        relevant_data <- c("date", "Q", "Tmean", "wt")
        n_features <- 2
      }
      if(data_inputs == "precip"){
        relevant_data <- c("date", "Q", "Tmean", "wt", "RR")
        n_features <- 3
      }
      if(data_inputs == "radiation"){
        relevant_data <- c("date", "Q", "Tmean", "wt", "GL")
        n_features <- 3
      }
      if(data_inputs == "all"){
        relevant_data <- c("date", "Q", "Tmean", "wt", "RR", "GL")
        n_features <- 4
      }

      LSTM_train <- train[, relevant_data]
      LSTM_val <- val[, relevant_data]
      # Scaling
      train_mean <- mean(LSTM_train$wt)
      train_sd <- sd(LSTM_train$wt)
      LSTM_train[, relevant_data[-1]] <- scale(LSTM_train[, relevant_data[-1]])
      LSTM_val[, relevant_data[-c(1, 4)]] <- scale(LSTM_val[, relevant_data[-c(1, 4)]])
      LSTM_val$wt <- (LSTM_val$wt - train_mean)/train_sd


      # check if there are time gaps in the data -> split
      train <- data_splitter_for_lstm(LSTM_train)
      val <- data_splitter_for_lstm(LSTM_val)

      x_train <- lapply(train, function(x) as.matrix(x[, relevant_data[-c(1, 4)]]))
      y_train <- lapply(train, function(x) as.matrix(x[, c("wt")]))
      x_val <- lapply(val, function(x) as.matrix(x[, relevant_data[-c(1, 4)]]))
      y_val <- lapply(val, function(x) as.matrix(x[, c("wt")]))

      # Define grid and apply function
      grid <- expand.grid("ts" = ts,
                          "u" = u,
                          "bs" = bs,
                          "epochs" = epochs,
                          "LSTM_type" = LSTM_type,
                          "n_predictions" = n_predictions)

      mapply(lstm_metaf,
             ts = grid$ts,
             u = grid$u,
             bs = grid$bs,
             epochs = grid$epochs,
             LSTM_type = grid$LSTM_type,
             n_predictions = grid$n_predictions,
             MoreArgs = list(catchment = catchment,
                             x_train = x_train, y_train = y_train,
                             x_val = x_val, y_val = y_val,
                             n_features = n_features, data_inputs = data_inputs,
                             train_mean = train_mean, train_sd = train_sd))
    }
  }
}
