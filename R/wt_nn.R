wt_nn <- function(catchment, x_train, x_val, x_test = NULL, x_full_train = NULL,
                  y_train, y_full_train = NULL, y_val, y_test = NULL, batch_size,
                  data_inputs, layers, units, dropout, ensemble_runs,
                  epochs, user_name, early_stopping_patience,
                  model_short, model_name,
                  save_model_and_prediction = FALSE,
                  na_test = NULL,
                  na_train = NULL,
                  start_time = NULL,
                  seed,
                  nn_type,
                  type = NULL,
                  timesteps = NULL,
                  n_features = NULL,
                  train = NULL,
                  test = NULL,
                  full_train_split = NULL,
                  test_split = NULL){

  if(!save_model_and_prediction){
    if(nn_type == "FNN"){
      cat("layers = ", layers,
          ", units = ", units,
          ", dropout = ", dropout,
          ", batch_size = ", batch_size,
          ", ensemble_runs = ", ensemble_runs, ", ", sep = "")
    }
    if(nn_type == "RNN"){
      cat("RNN type = ", type,
          ", layers = ", layers,
          ", units = ", units,
          ", dropout = ", dropout,
          ", batch_size = ", batch_size,
          ", timesteps = ", timesteps,
          ", ensemble_runs = ", ensemble_runs, ", ", sep = "")
    }
  }
  # path depending on type
  if(!(is.null(type))){
    model_short_type_path <- paste0(model_short, "/", type)
  } else {
    model_short_type_path <- model_short
  }
  # reshape RNN data
  if(nn_type == "RNN"){
    # remove all sub time series with length < ts + n_predictions
    train_long_ts <- sapply(x_train, nrow) >= (timesteps + 1)
    full_train_long_ts <- sapply(x_full_train, nrow) >= (timesteps + 1)
    val_long_ts <- sapply(x_val, nrow) >= (timesteps + 1)
    test_long_ts <- sapply(x_test, nrow) >= (timesteps + 1)
    x_train <- x_train[train_long_ts]
    y_train <- y_train[train_long_ts]
    x_full_train <- x_full_train[full_train_long_ts]
    y_full_train <- y_full_train[full_train_long_ts]
    x_val <- x_val[val_long_ts]
    y_val <- y_val[val_long_ts]
    x_test <- x_test[test_long_ts]
    y_test <- y_test[test_long_ts]
    # x data: 3D array with dimesions(sample, timesteps, n_features)
    #             therefore the n_timesteps of observations before our prediction point
    # y data: 2D array with dimensions (sample, n_predictions)
    # X arrays
    x_train_arr <- lapply(x_train, x_reshaper, n_timesteps = timesteps, n_predictions = 1)
    x_full_train_arr <- lapply(x_full_train, x_reshaper,
                               n_timesteps = timesteps, n_predictions = 1)
    x_val_arr <- lapply(x_val, x_reshaper, n_timesteps = timesteps, n_predictions = 1)
    x_test_arr <- lapply(x_test, x_reshaper, n_timesteps = timesteps, n_predictions = 1)
    x_train <- abind::abind(x_train_arr, along = 1)
    x_full_train <- abind::abind(x_full_train_arr, along = 1)
    x_val <- abind::abind(x_val_arr, along = 1)
    x_test <- abind::abind(x_test_arr, along = 1)
    # Y arrays
    # single step prediction
    y_train_arr <- lapply(y_train, y_reshaper,
                          n_timesteps = timesteps, n_predictions = 1)
    y_full_train_arr <- lapply(y_full_train, y_reshaper,
                               n_timesteps = timesteps, n_predictions = 1)
    y_val_arr <- lapply(y_val, y_reshaper,
                        n_timesteps = timesteps, n_predictions = 1)
    y_test_arr <- lapply(y_test, y_reshaper,
                         n_timesteps = timesteps, n_predictions = 1)
    y_train <- abind::abind(y_train_arr, along = 1)
    y_full_train <- abind::abind(y_full_train_arr, along = 1)
    y_val <- abind::abind(y_val_arr, along = 1)
    y_test <- abind::abind(y_test_arr, along = 1)
  }

  # Ensemble runs
    set.seed(seed)
  for(run in 1:ensemble_runs){
    # define model object depending on nn_type and type
    if(nn_type == "FNN"){
      model <- create_model(nn_type, x_train, layers, units, dropout, seed = seed + run)
    }
    if(nn_type == "RNN"){
      model <- create_model(nn_type, x_train, layers, units, dropout, seed = seed + run,
                            type = type, timesteps = timesteps, n_features = n_features)
    }
    # training
    tensorflow::tf$random$set_seed((seed + run))
    history <- model %>% fit(
      x_train, y_train,
      epochs = epochs,
      batch_size = batch_size,
      callbacks = list(
        callback_early_stopping(patience = early_stopping_patience,
                                restore_best_weights = TRUE)),
      validation_data = list(x_val, y_val),
      verbose = 0)
    # store prediction results in lists
    if(run == 1){
      predict_val <- list(predict(model, x_val))
      if(save_model_and_prediction){
        predict_full_train <- list(predict(model, x_full_train))
        if(!is.null(x_test)) predict_test <- list(predict(model, x_test))
      }
    } else {
      predict_val[[run]] <- predict(model, x_val)
      if(save_model_and_prediction){
        predict_full_train[[run]] <- predict(model, x_full_train)
        if(!is.null(x_test)) predict_test[[run]] <- predict(model, x_test)
      }
    }
    # save model as rds
    if(save_model_and_prediction){
      saveRDS(model, paste0(catchment, "/", model_short_type_path, "/",
                            model_name, "/model", run, ".rds"))
    }
  }
  # get mean prediction
  if(ensemble_runs != 1){
    # reshape list to length = 1 with mean predictions
    mean_pred_results_val <- predict_val %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      as.list() %>%
      lapply(matrix, ncol = ensemble_runs) %>%
      lapply(function(x) apply(x, 1, mean)) %>%
      do.call(cbind, .)
    if(save_model_and_prediction){
      if(!is.null(x_test)){
        mean_pred_results_test <- predict_test %>%
          do.call(rbind, .) %>%
          as.data.frame() %>%
          as.list() %>%
          lapply(matrix, ncol = ensemble_runs) %>%
          lapply(function(x) apply(x, 1, mean)) %>%
          do.call(cbind, .)
      }
      mean_pred_results_full_train <- predict_full_train %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        as.list() %>%
        lapply(matrix, ncol = ensemble_runs) %>%
        lapply(function(x) apply(x, 1, mean)) %>%
        do.call(cbind, .)
    }

  } else {
    mean_pred_results_val <- predict_val[[1]]
    if(save_model_and_prediction){
      mean_pred_results_full_train <- predict_full_train[[1]]
      if(!is.null(x_test)) mean_pred_results_test <- predict_test[[1]]
    }
  }
  # validation scores
  RMSE_val <- RMSE(prediction = mean_pred_results_val,
                   observation = y_val) %>% round(3)
  MAE_val <- MAE(prediction = mean_pred_results_val,
                 observation = y_val) %>% round(3)

  # save hyperparameter results
  if(is.null(type)){
    model_scores <- data.frame("layers" = layers,
                               "units" = units,
                               "max_epochs" = epochs,
                               "early_stopping_patience" = early_stopping_patience,
                               "batch_size" = batch_size,
                               "dropout" = dropout,
                               "ensemble_runs" = ensemble_runs,
                               "cv_or_validation_RMSE" = RMSE_val,
                               "cv_or_validation_MAE" = MAE_val,
                               stringsAsFactors = FALSE)
  } else {
    model_scores <- data.frame("layers" = layers,
                               "units" = units,
                               "max_epochs" = epochs,
                               "early_stopping_patience" = early_stopping_patience,
                               "batch_size" = batch_size,
                               "dropout" = dropout,
                               "timesteps" = timesteps,
                               "ensemble_runs" = ensemble_runs,
                               "cv_or_validation_RMSE" = RMSE_val,
                               "cv_or_validation_MAE" = MAE_val,
                               stringsAsFactors = FALSE)
  }

  # either combine with old results or write new
  if("hyperpar_opt_scores.csv" %in% list.files(paste0(catchment, "/",
                                                      model_short_type_path, "/", model_name))){
    existing_model_scores <- read.csv(paste0(catchment, "/", model_short_type_path, "/",
                                             model_name, "/hyperpar_opt_scores.csv"))
    write.csv(rbind(existing_model_scores,model_scores),
              paste0(catchment, "/", model_short_type_path, "/", model_name,
                     "/", "hyperpar_opt_scores.csv"), row.names = FALSE)
  } else {
    write.csv(model_scores,
              paste0(catchment, "/", model_short_type_path, "/", model_name,
                     "/", "hyperpar_opt_scores.csv"), row.names = FALSE)
  }
  # save train and test prediction
  if(save_model_and_prediction){
    if(nn_type == "RNN"){
      # training prediction
      if(sum(full_train_long_ts) > 1) full_train_split <- full_train_split[full_train_long_ts]
      full_train_prediction_full <- full_train_split %>%
        lapply(function(x) x[-c(1:timesteps), ]) %>%
        do.call(rbind, .) %>%
        select(date) %>%
        slice(1:(n()-1+1)) %>%
        pull(date) %>%
        as.POSIXct() %>%
        data.frame(date = .,
                   predicted_wt = mean_pred_results_full_train[, 1], stringsAsFactors = FALSE) %>%
        merge(train, ., by = "date", all.x = TRUE)
      cat("Saving prediction for test_data in",
          paste0(catchment, "/", model_short_type_path, "/", model_name, "/",
                 "train_data_prediction.csv"),"\n")
      write.csv(full_train_prediction_full,
                paste0(catchment, "/", model_short_type_path, "/", model_name, "/",
                       "train_data_prediction.csv"), row.names = FALSE)
      # test prediction
      if(sum(test_long_ts) > 1) test_split <- test_split[test_long_ts]
      test_prediction_full <- test_split %>%
        lapply(function(x) x[-c(1:timesteps), ]) %>%
        do.call(rbind, .) %>%
        select(date) %>%
        slice(1:(n()-1+1)) %>%
        pull(date) %>%
        as.POSIXct() %>%
        data.frame(date = .,
                   predicted_wt = mean_pred_results_test[, 1], stringsAsFactors = FALSE) %>%
        merge(test, ., by = "date", all.x = TRUE)
      cat("Saving prediction for test_data in",
          paste0(catchment, "/", model_short_type_path, "/", model_name, "/",
                 "test_data_prediction.csv"),"\n")
      write.csv(test_prediction_full,
                paste0(catchment, "/", model_short_type_path, "/", model_name, "/",
                       "test_data_prediction.csv"), row.names = FALSE)
    }
    if(nn_type == "FNN"){
      save_prediction_results(mean_pred_results_full_train, train, na_train,
                              model_short, model_name, "train_data", type)
      save_prediction_results(mean_pred_results_test, test, na_test, model_short, model_name,
                              "test_data", type = type)
    }
    if(nn_type == "RNN"){}
    # write model diagnostics
    cv_or_val_results <- data.frame("RMSE" = RMSE_val, "MAE" = MAE_val)
    model_diagnostic(train_prediction = mean_pred_results_full_train,
                     train_data = data.frame(wt = y_full_train),
                     test_prediction = mean_pred_results_test,
                     test_data = data.frame(wt = y_test),
                     cv_mode = "train/val split",
                     cv_or_val_results = cv_or_val_results,
                     start_time = start_time,
                     catchment = catchment,
                     type = type,
                     model_name = model_name,
                     model_short = model_short)
  } else{
    cat("validation RMSE:", RMSE_val, "\n")
    return(RMSE_val)
  }
}

