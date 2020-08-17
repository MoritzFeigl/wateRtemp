#' wt_fnn
#'
#' @param catchment
#' @param data_inputs
#' @param batch_size
#' @param layers
#' @param units
#' @param dropout
#' @param epochs
#' @param early_stopping_patience
#' @param ensemble_runs
#' @param user_name
#'
#' @return
#' @export
#'
#' @examples
wt_fnn <- function(catchment,
                   data_inputs = NULL,
                   batch_size = c(10, 50, 100),
                   layers = c(2, 4, 8, 15, 20),
                   units = c(10, 30, 50, 100),
                   dropout = 0,
                   epochs = c(100),
                   early_stopping_patience = 5,
                   ensemble_runs = 3,
                   user_name = "R2D2"){

  if(user_name == "R2D2") cat('No user_name was chosen! Default user "R2D2" is running the model.\n')

  if(is.null(data_inputs)){
    stop('\nChoose a valid data_input:
            "simple"    = Q, Tmean and water temperatur observations
            "precip"    = "simple" + additional precipitation observations
            "radiation" = "simple" + additional longwave radiation observations
            "all"       = all the above mentioned observations')
  }

  if(sum(list.files() %in% catchment) < 1){
    stop(paste0("ERROR: Cannot find catchment folder(s) in your current working directory."))
  }

  data_inputs_meta <- data_inputs
  for(catchment in catchment){
    cat("*** Starting computation for catchment", catchment, "***\n")
    if(sum(list.files() %in% catchment) != 1){
      message(paste0("ERROR: There is no folder named ", catchment, " in your current working directory."))
      next
    }
    for(data_inputs in data_inputs_meta){
      # check if there is seperate radiation data
      rad_data <- length(list.files(path = catchment, pattern = "radiation_")) > 0
      # in case of radiation or all data_input, load radiation data
      if(data_inputs == "radiation" & rad_data | data_inputs == "all" & rad_data){
        data_prefix <- "radiation_"
      } else {
        data_prefix <- ""
      }
      # train, val, test definition
      train <- feather::read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
      test <- feather::read_feather(paste0(catchment, "/test_", data_prefix, "data.feather"))
      part_training <- nrow(train)/4 * 3
      train_length <- floor(part_training)
      val <- train[(train_length + 1):nrow(train), ]
      train <- train[1:train_length, ]

      # feature selection
      relevant_data <- c("Q", "Tmean", "wt", "Qdiff", "Tmean_diff",
        "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5", "Fmon.6",
        "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11",
        "Tmean_lag1", "Tmean_lag2", "Tmean_lag3", "Tmean_lag4",
        "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
      if(data_inputs == "precip") relevant_data <- c("RR", relevant_data)
      if(data_inputs == "radiation") relevant_data <- c( "GL", relevant_data)
      if(data_inputs == "all") relevant_data <- c("RR", "GL", relevant_data)
      fnn_train <- train %>% select(relevant_data)
      fnn_val <- val %>% select(relevant_data)
      fnn_test <- test %>% select(relevant_data)

      # remove NA rows resulting from Qdiff, Tmean_diff and shuffle the train data
      fnn_train <- fnn_train %>% tidyr::drop_na()
      fnn_val <- fnn_val %>% tidyr::drop_na()
      fnn_test <- fnn_test %>% tidyr::drop_na()

      # define y and x
      y_train <- fnn_train %>% pull(wt)
      y_val <- fnn_val %>% pull(wt)
      y_test <- fnn_test %>% pull(wt)

      x_train <- fnn_train %>% select(-wt)
      x_val <- fnn_val %>% select(-wt)
      x_test <- fnn_test %>% select(-wt)

      # scale features with mean and sd from training data
      train_means <- apply(x_train, 2, mean)
      train_sd <- apply(x_train, 2, sd)

      # apply it on al data sets
      for (i in 1:ncol(x_train)){
        x_train[, i] <- (x_train[, i] - train_means[i]) / train_sd[i]
        x_val[, i] <- (x_val[, i] - train_means[i]) / train_sd[i]
        x_test[, i] <- (x_test[, i] - train_means[i]) / train_sd[i]
      }

      x_train <- x_train %>% as.matrix()
      x_val <- x_val %>% as.matrix()
      x_test <- x_test %>% as.matrix()

      if(!file.exists(paste0(catchment, "/FNN"))){
        dir.create(file.path(paste0(catchment, "/FNN")))
      }

      # Define grid and apply function
      grid <- expand.grid("batch_size" = batch_size,
                          "layers" = layers,
                          "units" = units,
                          "dropout" = dropout,
                          "ensemble_runs" = ensemble_runs)
      # check if its a single run -> return flag RMSE output
      return_flag <- ifelse(
        length(catchment) == 1 & length(data_inputs) == 1 & nrow(grid) == 1,
        TRUE, FALSE)

      val_rmse <- mapply(wt_fnn_single,
                         batch_size = grid$batch_size,
                         layers = grid$layers,
                         units = grid$units,
                         dropout = grid$dropout,
                         ensemble_runs = grid$ensemble_runs,
                         MoreArgs = list(catchment = catchment,
                                         x_train = x_train, y_train = y_train,
                                         x_val = x_val, y_val = y_val,
                                         x_test = x_test, y_test = y_test,
                                         data_inputs = data_inputs,
                                         epochs = epochs,
                                         early_stopping_patience = early_stopping_patience,
                                         user_name = user_name,
                                         test = test,
                                         return_flag = return_flag))
      if(return_flag) return(val_rmse)
    }
  }
}
