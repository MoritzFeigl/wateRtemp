# function to check model inuts
general_checks <- function(catchment,
                           train_data,
                           test_data,
                           model_name,
                           type,
                           cv_mode,
                           model_short){
  if(class(catchment) != "character" | class(model_name) != "character"){
    stop('\n"Catchment" and "model_name" have to be given as character.')

  }
  if(is.null(catchment)){
    stop('\nChoose a catchment name. This will be used to save model results in your current working directory.')
  }
  if(is.null(model_name)){
    stop('\nChoose a valid model_name. This will be used to save model results in the catchment folder in your current working directory.')
  }
  if(!("wt" %in% names(train_data))){
    stop('\nCould not find water temperature column named "wt" in train_data.')
  }
  if(!("date" %in% names(train_data))){
    stop('\nCould not find date column named "date" in train_data.')
  }
  if(!("POSIXct" %in% class(train_data$date))){
    stop('\n"date" column is not given as POSIXct in train_data. Please use as.POISXct or similar function to change the class of "date" to POSIXct.')
  }
  if(!is.null(test_data)){
    if(!("date" %in% names(test_data))){
      stop('\nCould not find date column named "date" in test_data')
    }
    if(!("POSIXct" %in% class(test_data$date))){
      stop('\n"date" column is not given as POSIXct in test_data Please use as.POISXct or similar function to change the class of "date" to POSIXct.')
    }
    if(sum(names(test_data) %in% names(train_data)) < ncol(train_data)){
      stop('\ntest_data does not include all train_data columns. Add variables or check column names of test_data.')
    }
  }
  if(!is.null(type)){
    if(model_short == "LM"){
      if(sum(type %in% c("LM", "stepLM")) == 0){
        stop('\nChoose a valid "type" option:
          "LM"    = simple multiple linear model
          "stepLM"  = linear model with stepwise model selection')
      }
    }
    if(model_short == "RNN"){
      if(sum(type %in% c("LSTM", "GRU")) == 0){
        stop('\nChoose a valid "type" option:
          "LSTM"    = Long Short-Term Memory
          "GRU"  = Gated Recurrent Unit')
      }
    }
  }
  if(!is.null(cv_mode)){
    if(!(cv_mode %in% c("timeseriesCV", "repCV"))) {
      stop('"cv_mode" can be either timeseriesCV or repCV')
    }
  }
}

# create model folder structure
folder_structure <- function(catchment, model_short, model_name, type){
  # catchment folder
  if(!file.exists(paste0(catchment))){
    dir.create(file.path(paste0(catchment)))
  }
  # model folder
  if(!file.exists(paste0(catchment, "/", model_short))){
    dir.create(file.path(paste0(catchment, "/", model_short)))
  }
  # for models with different types create type subfolder
  if(!is.null(type)){
    if(!file.exists(paste0(catchment, "/", model_short, "/", type))){
      dir.create(paste0(catchment, "/", model_short, "/", type))
    }
    # model name folder
    if(!file.exists(paste0(catchment, "/", model_short, "/", type, "/", model_name))){
      dir.create(paste0(catchment, "/", model_short, "/", type, "/", model_name))
    }
  } else {
    # model name folder
    if(!file.exists(paste0(catchment, "/", model_short, "/", model_name))){
      dir.create(paste0(catchment, "/", model_short, "/", model_name))
    }
  }
}

# start message
start_message <- function(catchment, model_short){
  cat("*** Starting", model_short, "computation for catchment", catchment, "***\n")
}
# Loss functions
RMSE <- function(prediction, observation){
  round(sqrt(mean((prediction - observation)^2, na.rm = TRUE)), 3)
}

MAE <- function(prediction, observation){
  round(mean(abs(observation - prediction)), 3)
}

# Model diagnostic
model_diagnostic <- function(train_prediction, train_data,
                             test_prediction = NULL, test_data,
                             cv_mode, cv_or_val_results, start_time,
<<<<<<< HEAD
                             catchment, type, model_name, model_short, model = NULL){
=======
                             catchment, type, model_name, model_short){
>>>>>>> 665bd01ae0a173f48e7914981360a5a4bd0f02ae
  if(is.null(type)) {
    type <- NA
    type_path <- ""
  } else {
    type_path <- paste0(type, "/")
  }
<<<<<<< HEAD
  if(type == "LM") type_path <- ""
=======
>>>>>>> 665bd01ae0a173f48e7914981360a5a4bd0f02ae
  # training performance
  train_rmse <- RMSE(train_prediction, train_data$wt)
  train_mae <- MAE(train_prediction, train_data$wt)
  # CV or validation set performance
  cv_or_validation_rmse <- round(as.numeric(cv_or_val_results["RMSE"]), 3)
  cv_or_validation_mae <- round(as.numeric(cv_or_val_results["MAE"]), 3)
  # test performance (optional)
  if(!is.null(test_prediction)){
    test_rmse <- RMSE(test_prediction, test_data$wt)
    test_mae <- MAE(test_prediction, test_data$wt)
  } else {
    test_rmse <- NA
    test_mae <- NA
  }
  # run time
  run_time <-  paste0(
    round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
    " minutes")
  # output data frame and save as csv
  model_scores <- data.frame(start_time = as.character(start_time),
                             run_time = run_time,
                             catchment = catchment,
                             model = model_short,
                             model_type = type,
                             model_name = model_name,
                             cv_mode = cv_mode,
                             train_RMSE = train_rmse,
                             train_MAE = train_mae,
                             cv_or_validation_RMSE = cv_or_validation_rmse,
                             cv_or_validation_MAE = cv_or_validation_mae,
                             test_RMSE = test_rmse,
                             test_MAE = test_mae,
                             stringsAsFactors = FALSE)

  if("model_scores.csv" %in% list.files(paste0(catchment, "/", model_short))){
    model_scores_old <- read.csv(paste0(catchment, "/", model_short, "/model_scores.csv"),
                                 stringsAsFactors = FALSE)
    write.csv(rbind(model_scores_old, model_scores, stringsAsFactors = FALSE),
              paste0(catchment, "/", model_short, "/model_scores.csv"),
              row.names = FALSE)
  } else {
    write.csv(model_scores,
              paste0(catchment, "/", model_short, "/model_scores.csv"),
              row.names = FALSE)
  }
  cat("\nModel training performance:", paste0("RMSE = ", model_scores$train_RMSE, ","),
      paste0("MAE = ", model_scores$train_MAE))
  if(!is.null(test_prediction)){
    cat("\nModel testing performance:", paste0("RMSE = ", model_scores$test_RMSE, ","),
        paste0("MAE = ", model_scores$test_MAE), "\n")
  }
  cat("\nModel results are saved in",
      paste0("/",catchment, "/", model_short, "/model_scores.csv\n"))
  cat("The trained model is saved in",
      paste0("/", catchment, "/", model_short, "/", type_path, model_name, "/model.rds\n"))
<<<<<<< HEAD
  if(!is.null(model)){
    saveRDS(model, paste0(catchment, "/", model_short, "/", type_path, model_name, "/model.rds"))
  }
=======
  saveRDS(model, paste0(catchment, "/", model_short, "/", type_path, model_name, "/model.rds"))

>>>>>>> 665bd01ae0a173f48e7914981360a5a4bd0f02ae
}

# save prediction results
save_prediction_results <- function(prediction, data, na_data, model_short,
                                    model_name, data_name, type = NULL){
<<<<<<< HEAD
  type_path <- ifelse(is.null(type) | type == "LM", "", paste0(type, "/"))

=======
  type_path <- ifelse(is.null(type), "", paste0(type, "/"))
>>>>>>> 665bd01ae0a173f48e7914981360a5a4bd0f02ae
  results <- data.frame(data$date, observed_wt = data$wt, predicted_wt = NA)
  if(nrow(na_data) > 0){
    results$predicted_wt[-na_data[, 1]] <- prediction
  } else {
    results$predicted_wt <- prediction
  }
  cat("Saving prediction for", data_name, "in",
      paste0(catchment, "/", model_short, "/", type_path, model_name, "/",
             data_name, "_prediction.csv"),
      "\n")
  write.csv(results,
            paste0(catchment, "/", model_short, "/", type_path, model_name, "/",
                   data_name, "_prediction.csv"), row.names = FALSE)
}


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

save_variable_importance <- function(model, model_short, model_name){
  cat("\nSaving variable importance plot in",
      paste0(catchment, "/", model_short, "/", model_name, "/"),"\n")
  importance <- caret::varImp(model)$importance
  v <- as.numeric(importance[,1])
  w <- rownames(importance)
  DF <- data.frame(w,v, stringsAsFactors = FALSE)
  p <- ggplot(DF, aes(x = stats::reorder(w,v), y = v, fill = v))+
    geom_bar(stat="identity", position="dodge") + coord_flip() +
    ylab("Variable Importance") +
    xlab("") +
    ggtitle("Information Value Summary - Random Forest") +
    guides(fill = FALSE) +
    scale_fill_gradient(low="red", high="blue") +
    ggsave(paste0(catchment, "/", model_short, "/", model_name,
                  "/importance_plot.png"), dpi = "retina")
}

# create lags function
create_lags <- function(data, variable){
  lags <- data.frame("lag1" = c(NA, data[-nrow(data), variable]))
  lags$lag2 <- c(NA, lags$lag1[-nrow(data)])
  lags$lag3 <- c(NA, lags$lag2[-nrow(data)])
  lags$lag4 <- c(NA, lags$lag3[-nrow(data)])
  names(lags) <- paste0(variable, "_lag", 1:4)
  return(cbind(data, lags))
}




# define model
create_model <- function(nn_type, x_train, layers, units, dropout, seed, type = NULL,
                         timesteps = NULL, n_features = NULL){
  if(nn_type == "FNN"){
    input <- keras::layer_input(shape = ncol(x_train))
    dropout_layers <- ifelse(dropout != 0, TRUE, FALSE)
    for(lay in 1:layers){
      # first time step
      if(lay == 1) {
        if(dropout_layers){
          output <- input %>%
            keras::layer_alpha_dropout(rate = dropout) %>%
            keras::layer_dense(units = units,
                               activation = 'selu',
                               kernel_initializer = "lecun_normal")
        } else {
          output <- input %>%
            keras::layer_dense(units = units,
                               activation = 'selu',
                               kernel_initializer = "lecun_normal")
        }
      }
      # all other time steps
      if(dropout_layers){
        output <- output %>% keras::layer_alpha_dropout(rate = dropout)
      }
      # add dense layers
      output <- output %>%
        keras::layer_dense(units = units,
                           activation = 'selu',
                           kernel_initializer = "lecun_normal")
    }
    # add last layer
    output <- output %>% keras::layer_dense(1)
    tensorflow::tf$random$set_seed(seed)
    model <- keras::keras_model(input, output)
    # compile
    model %>% compile(
      loss = "mse",
      optimizer = "adam"
    )
  }

  if(nn_type == "RNN"){
    layer_rnn <- function(...) {
      if(type == "LSTM") return(keras::layer_lstm(..., recurrent_activation = "sigmoid"))
      if(type == "GRU") return(keras::layer_gru(..., recurrent_activation = "sigmoid"))
    }
    input <- keras::layer_input(shape = c(timesteps, n_features))
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
    output <- output %>% keras::layer_dense(1)
    model <- keras::keras_model(input, output) %>%
      compile(loss = "mse",
              optimizer = "adam")
  }
  return(model)
}
