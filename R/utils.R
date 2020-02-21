data_splitter_for_rnn <- function(data, data_name, catchment){
  # inputs: data: data frame with the first column containing a POSIXct data and is named "date".
  # create full time series with no gaps
  full_ts <- data.frame(
    date = as.character(format.POSIXct(
      seq(data$date[1], data$date[nrow(data)], by = "day"),
      format = "%Y-%m-%d"), stringsAsFactors = FALSE))
  data$date <- as.character(format.POSIXct(data$date, format = "%Y-%m-%d"))
  # merge with data
  full_train <- merge(full_ts, data, by = "date", all = TRUE)
  full_train$date <- as.character(full_train$date)
  # split data in sub time series without gaps
  missing_days <- unique(which(is.na(full_train), arr.ind = TRUE)[, 1])
  missing_days <- sort(missing_days)
  # if no split is necessary
  if(length(missing_days) != 0){

    # if only one split is necessary
    if(length(missing_days) == 1){
      cut_points <- missing_days
      data_list <- vector(mode = "list")
      data_list[[1]] <- full_train[1:(cut_points[1] - 1), ]
      data_list[[length(data_list) + 1]] <- full_train[(cut_points[length(cut_points)] + 1):nrow(full_train), ]
    } else {
      data_list <- vector(mode = "list")
      for(i in seq_along(missing_days)){
        if(i == 1){
          data_list[[i]] <- full_train[1:(missing_days[i] - 1), ]
        } else {
          if(missing_days[i] - missing_days[i-1] <= 1) next
          data_list[[i]] <- full_train[(missing_days[i-1]+1):(missing_days[i]-1), ]
        }
      }
      # for the last cut point also add the time from the last cutpoint to the end of the data
      data_list[[length(data_list) + 1]] <- full_train[(missing_days[length(missing_days)] + 1):nrow(full_train), ]
      data_list <- data_list [!sapply(data_list, is.null)]
    }

    # Check if all split where done correctly
    for(i in 2:length(data_list)){
      check_if_date_should_be_there <- data_list[[i]][1,1] %in% full_train$date[missing_days + 1]
      if(!check_if_date_should_be_there){
        stop("ERROR: Data splitting procedure for LSTM data is producing an Error. Check data or source code! Or contact moritz.feigl@boku.ac.at for further help.")
      }
    }

  } else {
    data_list <- vector(mode = "list")
    data_list[[1]] <- data
  }

  # Sanity check
  if(sum(sapply(data_list, function(x) sum(is.na(x)))) != 0){
    stop("ERROR: Data splitting procedure for LSTM data is producing an Error. Check data or source code! Or contact moritz.feigl@boku.ac.at for further help.")
  }
  cat(catchment, data_name, "data was sucessfully splitted!\n")
  cat("number of missing days:", length(missing_days), "\n")
  return(data_list)
}

rmse_nse <- function(model, val){
  m_predict <- predict(model, val)
  m_res <- val$wt - m_predict
  RMSE_m <- sqrt(mean(m_res^2, na.rm = TRUE))
  NSE <- 1 - (sum((m_predict- val$wt)^2, na.rm = TRUE) / sum( (val$wt - mean(val$wt, na.rm = TRUE))^2, na.rm = TRUE ) )
  df <- data.frame("RMSE" = round(RMSE_m, 3), "NSE" = round(NSE, 3))
  rownames(df) <- model$method
  return(df)
}

RMSE <- function(prediction, observation){
  return(sqrt(mean((prediction - observation)^2, na.rm = TRUE)))
}

NSE <- function(prediction, observation){
  round(
    1 - (sum((prediction- observation)^2, na.rm = TRUE) /
           sum( (observation - mean(observation, na.rm = TRUE))^2, na.rm = TRUE ) ),
    3)
}

x_reshaper <- function(x, n_timesteps, n_predictions){
  # Function for reshaping data for LSTM applications.
  # x_reshaper reshapes the feature data to a 3D array of features with
  # (number observations, timesteps, number of features).

  # train data: 3D array with dimesions(sample, n_timesteps, features)
  #             therefore the n_timesteps of observations before our prediction point
  # val data: 2D array with dimensions (sample, n_predictions)
  if(is.null(nrow(x))) x <- as.matrix(x, ncol = 1)
  x_list <- vector(mode = "list", length = (nrow(x) - n_timesteps - n_predictions + 1))
  for(i in 1:(nrow(x) - n_timesteps - n_predictions + 1)){
    x_list[[i]] <- t(x[i:(i + (n_timesteps - 1)), ])
  }
  x_arr <- simplify2array(x_list)
  x_arr <- aperm(x_arr)
  return(x_arr)
}

y_reshaper <- function(y, n_timesteps, n_predictions){
  # Function for reshaping data for LSTM applications.
  # y_reshaper reshapes the the data to a 1D array with labels
  # corresponding to the 3D feature array.

  if(n_predictions == 1){
    timestep_length <- dim(y)[1] - n_timesteps
    y_arr <- numeric(timestep_length)
    for(i in 1:timestep_length){
      y_arr[i] <- y[i+n_timesteps]
    }
    y_arr <- array(y_arr, dim = c(timestep_length, n_predictions))
  }
  # multiple step prediction
  if(n_predictions != 1){
    timestep_length <- dim(y)[1] - n_timesteps - n_predictions + 1
    y_arr <- matrix(NA, nrow = timestep_length, ncol = n_predictions)
    for(i in 1:timestep_length){
      y_arr[i,] <- y[(i+n_timesteps):(i+n_timesteps+n_predictions-1)]
    }
  }
  return(y_arr)
}
