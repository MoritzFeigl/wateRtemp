
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
#' @return
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
  library(keras, quietly = TRUE)

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

  data <- feather::read_feather("input_data_V2.feather")
  train <- feather::read_feather("train_data_V2.feather")
  val <- feather::read_feather("val_data_V2.feather")

  if(data_inputs == "simple"){
    LSTM_train <- train[, c("Q", "Tmean", "wt")]
    LSTM_val <- val[, c("Q", "Tmean", "wt")]
    # Scaling
    train_mean <- mean(LSTM_train$wt)
    train_sd <- sd(LSTM_train$wt)
    LSTM_train[, c("Q", "Tmean", "wt")] <- scale(LSTM_train[, c("Q", "Tmean", "wt")])
    LSTM_val[, c("Q", "Tmean")] <- scale(LSTM_val[, c("Q", "Tmean")])
    LSTM_val$wt <- (LSTM_val$wt - train_mean)/train_sd

    # As matrix
    LSTM_train <- as.matrix(LSTM_train)
    LSTM_val <- as.matrix(LSTM_val)
    # x: Q and Tmean, y: wt
    x_train <- LSTM_train[, -3]
    y_train <- LSTM_train[, 3]
    x_val <- LSTM_val[, -3]
    y_val <- LSTM_val[, 3]
    n_features <- 2
  }
  if(data_inputs == "precip"){
    LSTM_train <- train[, c("Q", "Tmean", "wt", "RR")]
    LSTM_val <- val[, c("Q", "Tmean", "wt", "RR")]
    # Scaling
    train_mean <- mean(LSTM_train$wt)
    train_sd <- sd(LSTM_train$wt)
    LSTM_train[, c("Q", "Tmean", "wt", "RR")] <- scale(LSTM_train[, c("Q", "Tmean", "wt", "RR")])
    LSTM_val[, c("Q", "Tmean", "RR")] <- scale(LSTM_val[, c("Q", "Tmean", "RR")])
    LSTM_val$wt <- (LSTM_val$wt - train_mean)/train_sd

    # As matrix
    LSTM_train <- as.matrix(LSTM_train)
    LSTM_val <- as.matrix(LSTM_val)
    # x: Q and Tmean, y: wt
    x_train <- LSTM_train[, c("Q", "Tmean", "RR")]
    y_train <- LSTM_train[, "wt"]
    x_val <- LSTM_val[, c("Q", "Tmean", "RR")]
    y_val <- LSTM_val[, "wt"]
    n_features <- 3
  }

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
                          n_features = n_features))
  setwd(old_wd)
}
# Varying units (u) and n_timesteps (ts) and batch_size (bs)
#u in c(30, 60, 90))
#ts in c(30, 60, 90, 180, 365))
#bs in c(10, 50, 100, 150))
