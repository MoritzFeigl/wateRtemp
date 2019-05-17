
#' wt_lstm
#'
#' @param catchment
#' @param data_inputs
#' @param ts
#' @param u
#' @param bs
#' @param epochs
#' @param LSTM_type
#' @param n_predictions
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
                    LSTM_type = NULL,
                    n_predictions = c(1, 3, 7)){

  if(is.null(data_inputs)){
    warning('\nChoose a valid data_input:
            "simple"    = Q, Tmean and water temperatur observations
            "precip"    = "simple" + additional precipitation observations
            "radiation" = "simple" + additional longwave radiation observations
            "all"       = all the above mentioned observations')
  }
  if(is.null(LSTM_type)){
    warning('\nChoose a valid LSTM_type or a vector of LSTM_types:
            "lstm1" = 1 layer LSTM
            "lstm2" = 2 layer LSTM
            "lstm3" = 3 layer LSTM')
  }

  old_wd <- getwd()
  setwd(paste0("../data/", catchment))

  # check if there is seperate radiation data
  rad_data <- length(list.files(pattern = "radiation_")) > 0
  # in case of radiation or all data_input, load radiation data
  if(data_inputs == "radiation" | data_inputs == "all" & rad_data){
    data_prefix <- "radiation_"
  } else {
    data_prefix <- ""
  }

  #data <- read_feather(paste0("input_", data_prefix, "data.feather"))
  train <- read_feather(paste0("train_", data_prefix, "data.feather"))
  val <- read_feather(paste0("val_", data_prefix, "data.feather"))

  if(data_inputs == "simple"){
    relevant_data <- c("Q", "Tmean", "wt")
    n_features <- 2
  }
  if(data_inputs == "precip"){
    relevant_data <- c("Q", "Tmean", "wt", "RR")
    n_features <- 3
  }
  if(data_inputs == "radiation"){
    relevant_data <- c("Q", "Tmean", "wt", "GL")
    n_features <- 3
  }
  if(data_inputs == "all"){
    relevant_data <- c("Q", "Tmean", "wt", "RR", "GL")
    n_features <- 4
  }


  LSTM_train <- train[, relevant_data]
  LSTM_val <- val[, relevant_data]
  # Scaling
  train_mean <- mean(LSTM_train$wt)
  train_sd <- sd(LSTM_train$wt)
  LSTM_train[, relevant_data] <- scale(LSTM_train[, relevant_data])
  LSTM_val[, relevant_data[-3]] <- scale(LSTM_val[, relevant_data[-3]])
  LSTM_val$wt <- (LSTM_val$wt - train_mean)/train_sd

  # As matrix
  LSTM_train <- as.matrix(LSTM_train)
  LSTM_val <- as.matrix(LSTM_val)
  # x: Q and Tmean, y: wt
  x_train <- LSTM_train[, relevant_data[-3]]
  y_train <- LSTM_train[, c("wt")]
  x_val <- LSTM_val[, relevant_data[-3]]
  y_val <- LSTM_val[, c("wt")]


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
          MoreArgs = list(x_train = x_train, y_train = y_train,
                          x_val = x_val, y_val = y_val,
                          n_features = n_features, data_inputs = data_inputs,
                          train_mean = train_mean, train_sd = train_sd))
  setwd(old_wd)
}
# Varying units (u) and n_timesteps (ts) and batch_size (bs)
#u in c(30, 60, 90))
#ts in c(30, 60, 90, 180, 365))
#bs in c(10, 50, 100, 150))
