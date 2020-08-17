wt_fnn_single <- function(catchment, x_train, x_val, x_test,
                          y_train, y_val, y_test, batch_size,
                          data_inputs, layers, units, dropout, ensemble_runs,
                          epochs, user_name, early_stopping_patience,
                          test, return_flag){

  cat("\nRunning FNN with:",
      "\n    layers =", layers,
      "\n    units =", units,
      "\n    dropout =", dropout,
      "\n    batch_size =", batch_size,
      "\n    ensemble_runs =", ensemble_runs, "\n\n")
  # time and folders ---------------------------------------------------------------------
  start_time <- Sys.time()
  model_name <- paste0("FNN_", data_inputs)
  folder_name <- paste0(batch_size, "batchsize_", units, "units_",
                        layers, "layers_", round(dropout, 2), "dropout")


  # define model
  create_model <- function(){
    input <- layer_input(shape = ncol(x_train))
    dropout_layers <- ifelse(dropout != 0, TRUE, FALSE)
    for(lay in 1:layers){
      # first time step
      if(lay == 1) {
        if(dropout_layers){
          output <- input %>%
            layer_alpha_dropout(rate = dropout) %>%
            layer_dense(units = units,
                        activation = 'selu',
                        kernel_initializer = "lecun_normal")
        } else {
          output <- input %>%
            layer_dense(units = units,
                        activation = 'selu',
                        kernel_initializer = "lecun_normal")
        }
      }
      # all other time steps
      if(dropout_layers){
        output <- output %>% layer_alpha_dropout(rate = dropout)
      }
      # add dense layers
      output <- output %>%
        layer_dense(units = units,
                    activation = 'selu',
                    kernel_initializer = "lecun_normal")
    }
    # add last layer
    output <- output %>% layer_dense(1)
    model <- keras_model(input, output)
    # multiple gpu, compile
    try(number_of_gpus <- sum(grepl("device:GPU", tf$config$experimental_list_devices())),
        silent = TRUE)
    try(if(number_of_gpus > 1) model <- multi_gpu_model(model, gpus = number_of_gpus),
        silent = TRUE)
    model %>% compile(
      loss = "mse",
      optimizer = "adam"
    )
    return(model)
  }
  # Training
  if(!dir.exists(paste0(catchment, "/FNN"))){
    dir.create(paste0(catchment, "/FNN"))
  }
  if(!dir.exists(paste0(catchment, "/FNN/", model_name))){
    dir.create(paste0(catchment, "/FNN/", model_name))
  }
  if(!dir.exists(paste0(catchment, "/FNN/", model_name, "/", folder_name))){
    dir.create(paste0(catchment, "/FNN/", model_name, "/", folder_name))
  } else {
    version_numbers <- sum(grepl(folder_name,
                                 list.files(paste0(catchment, "/FNN/", model_name))))
    folder_name <- paste0(folder_name, "_version", version_numbers + 1)
    dir.create(paste0(catchment, "/FNN/", model_name, "/", folder_name))
  }
  if(!dir.exists(paste0(catchment, "/FNN/", model_name, "/", folder_name,
                        "/checkpoints"))){
    dir.create(paste0(catchment, "/FNN/", model_name, "/", folder_name,
                      "/checkpoints"))
  }
  if(!dir.exists(paste0(catchment, "/FNN/", model_name, "/", folder_name,
                        "/training_metrics"))){
    dir.create(paste0(catchment, "/FNN/", model_name, "/", folder_name,
                      "/training_metrics"))
  }
  # Ensemble runs
  for(run in 1:ensemble_runs){
    model <- create_model()
    model_checkpoint <- callback_model_checkpoint(
      filepath = paste0(catchment, "/FNN/", model_name, "/", folder_name,
                        "/checkpoints/model_checkpoint_run", run,
                        "_weights.hdf5"),
      save_best_only = TRUE)
    history <- model %>% fit(
      x_train, y_train,
      epochs = epochs,
      batch_size = batch_size,
      callbacks = list(
        callback_early_stopping(patience = early_stopping_patience,
                                restore_best_weights = TRUE),
        model_checkpoint),
      validation_data = list(x_val, y_val))
    if(run == 1){
      predict_fnn_val <- list(predict(model, x_val))
      predict_fnn_test <- list(predict(model, x_test))
    } else {
      predict_fnn_val[[run]] <- predict(model, x_val)
      predict_fnn_test[[run]] <- predict(model, x_test)
    }
    # Plot training losses
    png(paste0(catchment, "/FNN/", model_name, "/", folder_name,
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
      paste0(catchment, "/FNN/", model_name, "/", folder_name,
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
    best_model_preds_val <- predict_fnn_val
    best_model_preds_test <- predict_fnn_test
  }

  # Create vector with avg prediction of best members of the ensemble
  if(model_subset < ensemble_runs & model_subset > 1){
    if(model_subset > 10){
      message("The best 10% of ensemble runs were used for the prediction.")
    } else {
      message("All ensemble runs were used for the prediction.")
    }

    # RMSE for all model runs, mean timestep RMSE for n_predictions > 1
    all_rmse_val <- sapply(predict_fnn_val,
                           function(x) mean(apply(x, 2, RMSE, observation = y_val)))
    # choose best models
    best_model_preds_val <- predict_fnn_val[order(all_rmse_val) <= model_subset]
    best_model_preds_test <- predict_fnn_test[order(all_rmse_val) <= model_subset]
    # Delete all loss data frames and loss plots from the ensembles not used for prediction
    model_to_delete <- which(order(all_rmse_val) > model_subset)
    if(length(model_to_delete) != 0){
      cp_files <- list.files(paste0(catchment, "/FNN/", model_name, "/",
                                    folder_name, "/checkpoints"))
      cp_numbers <- unlist(lapply(strsplit(cp_files, "_"),
                                  function(x) as.integer(
                                    sub("run", "", x[grep("run", x)])
                                  )))
      txt <- capture.output(file.remove(
        paste0(catchment, "/FNN/", model_name, "/", folder_name,
               "/checkpoints/", cp_files[cp_numbers %in% model_to_delete])))
      txt <- capture.output(
        file.remove(
          paste0(catchment, "/FNN/", model_name, "/", folder_name,
                 "/training_metrics/plot_ensemble_member_",
                 model_to_delete, ".png")))
      txt <- capture.output(
        file.remove(
          paste0(catchment, "/FNN/", model_name, "/", folder_name,
                 "/training_metrics/loss_ensemble_member_",
                 model_to_delete, ".feather")))
    }
  }
  # get mean prediction
  if(model_subset != 1){
    # reshape list to length = 1 with mean predictions
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
  } else {
    mean_pred_results_val <- best_model_preds_val[[1]]
    mean_pred_results_test <- best_model_preds_test[[1]]
  }
  # Model Scores -------------------------------------------------------------------------
  # scores
  RMSE_val <- RMSE(prediction = mean_pred_results_val,
                   observation = y_val)
  NSE_val <- NSE(prediction = mean_pred_results_val,
                 observation = y_val)

  RMSE_test <- RMSE(prediction = mean_pred_results_test,
                    observation = y_test)
  NSE_test <- NSE(prediction = mean_pred_results_test,
                  observation = y_test)

  # run time
  run_time <- paste0(round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
                     " minutes")
  if("model_scores.csv" %in% list.files(paste0(catchment, "/FNN"))){
    model_scores <- read.csv(paste0(catchment, "/FNN/model_scores.csv"))
    model_scores <- rbind(model_scores,
                          data.frame("user_name" = user_name,
                                     "data_inputs" = data_inputs,
                                     "start_time" = as.character(start_time),
                                     "run_time" = run_time,
                                     "layers" = layers,
                                     "units" = units,
                                     "max_epochs" = epochs,
                                     "early_stopping_patience" = early_stopping_patience,
                                     "batch_size" = batch_size,
                                     "dropout" = dropout,
                                     "ensemble_runs" = ensemble_runs,
                                     "RMSE_val" = RMSE_val,
                                     "NSE_val" = NSE_val,
                                     "RMSE_test" = RMSE_test,
                                     "NSE_test" = NSE_test,
                                     stringsAsFactors = FALSE))
    write.csv(model_scores, paste0(catchment, "/FNN/model_scores.csv"), row.names = FALSE)

  } else {
    model_scores <- data.frame("user_name" = user_name,
                               "data_inputs" = data_inputs,
                               "start_time" = as.character(start_time),
                               "run_time" = run_time,
                               "layers" = layers,
                               "units" = units,
                               "max_epochs" = epochs,
                               "early_stopping_patience" = early_stopping_patience,
                               "batch_size" = batch_size,
                               "dropout" = dropout,
                               "ensemble_runs" = ensemble_runs,
                               "RMSE_val" = RMSE_val,
                               "NSE_val" = NSE_val,
                               "RMSE_test" = RMSE_test,
                               "NSE_test" = NSE_test,
                               stringsAsFactors = FALSE)
    write.csv(model_scores, paste0(catchment, "/FNN/model_scores.csv"), row.names = FALSE)
  }

  # get predicted values in the same dim/format as the input data
  test_prediction_full <- test %>%
    drop_na() %>%
    pull(date) %>%
    as.POSIXct() %>%
    data.frame(date = .,
               "prediction" = mean_pred_results_test[, 1], stringsAsFactors = FALSE) %>%
    merge(test, ., by = "date", all.x = TRUE) %>%
    select("date", starts_with("prediction"))
  # save predicted values
  feather::write_feather(test_prediction_full,
                         paste0(catchment, "/FNN/", model_name, "/",
                                folder_name, "/test_prediction.feather"))
  cat("Finished run with validation rmse =", RMSE_val, "\n\n")
  if(return_flag) return(mean(RMSE_val))
}

