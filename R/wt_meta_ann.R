#' ann_metaf
#'
#' @param catchment
#' @param data_inputs
#' @param model_or_optim
#' @param bs
#' @param epochs
#' @param ensemble_runs
#'
#' @return
#'
#' @examples
ann_metaf <- function(catchment, data_inputs,
                      bs, epochs, ensemble_runs,
                      train_mean, train_sd,
                      x_train, y_train, x_val, y_val,
                      x_test, y_test,
                      n_features, model_name){

  model_name <- paste0(data_inputs, "Model")
  folder_name <- paste0(epochs, "epochs_", bs, "batchsize", ensemble_runs, "ensembleRuns")

  if(!file.exists(paste0(catchment, "/ANN/", model_name))){
    dir.create(file.path(paste0(catchment, "/ANN/", model_name)))
  }
  if(!file.exists(paste0(catchment, "/ANN/", model_name, "/", folder_name))){
    dir.create(file.path(paste0(catchment, "/ANN/", model_name, "/", folder_name)))
  }
  if(!file.exists(paste0(catchment, "/ANN/", model_name, "/", folder_name, "/checkpoints"))){
    dir.create(file.path(paste0(catchment, "/ANN/", model_name, "/", folder_name, "/checkpoints")))
  }
  if(!file.exists(paste0(catchment, "/ANN/", model_name, "/", folder_name, "/training_metrics"))){
    dir.create(file.path(paste0(catchment, "/ANN/", model_name, "/", folder_name, "/training_metrics")))
  }

  # start time
  start_time <- Sys.time()

  # ANN ensemble aggregation (ensemble of equally structured ANNs with different initial weights)
  # First: Defining model and layers, Second: Training
  for(run in c(1:ensemble_runs)){
    # Defining model and layers
    model <- keras_model_sequential()
    model %>%
      layer_dense(units = 300, activation = 'selu', input_shape = c(n_features)) %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = 300, activation = 'relu') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units = 150, activation = 'relu') %>%
      layer_dense(units = 50) %>%
      layer_dense(units = 1) %>%
      compile(loss = 'mean_squared_error',
              optimizer = tf$train$AdamOptimizer())
    # Create checkpoint callback
    ANN_checkpoint <- callback_model_checkpoint(
      filepath = paste0(catchment, "/ANN/", model_name, "/", folder_name,
                        "/checkpoints/", "ensemble_member", run, ".hdf5"),
      save_best_only = TRUE, save_weights_only = TRUE)

    cat("\n---------------> ensemble member ", run, "<---------------\n")
    # Training
    history <- model %>% fit(
      x_train, y_train,
      epochs = epochs, batch_size = bs,
      validation_data = list(x_val, y_val),
      callbacks = list(ANN_checkpoint) # pass callback to training
    )
    # Training plot ------------------------------------------------------------------------
    png(paste0(catchment, "/ANN/", model_name, "/", folder_name,
               "/training_metrics/plot_ensemble_member_", run, ".png"),
        width = 800, heigh = 600)
    plot(history$metrics$loss, xlab = "epochs", main = "Model loss", ylim = c(0, max(history$metrics$loss)),
         ylab = "model loss", type="l", col="blue")
    lines(history$metrics$val_loss, col = "darkgreen")
    legend("topright", c("training","validation"), col=c("blue", "darkgreen"), lty=c(1,1), bty = "n")
    dev.off()
    feather::write_feather(
      data.frame(epoch = 1:epochs,
                 train_loss = history$metrics$loss,
                 val_loss = history$metrics$val_loss),
      paste0(catchment, "/ANN/", model_name, "/", folder_name,
             "/training_metrics/loss_ensemble_member_", run, ".feather")
    )
  }
  cat("\nTraining plots and loss values saved in",
      paste0('"', catchment, "/ANN/", model_name, "/", folder_name, '/training_metrics"\n'))

  # Creating ensemble from validation results
  cat("\nCreating ensemble from validation results")

  network_evaluation <- function(new_run){
    #Create new model and predict using checkpoints
    new_model <- keras_model_sequential()
    new_model %>%
      layer_dense(units = 300, activation = 'selu', input_shape = c(n_features)) %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = 300, activation = 'relu') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units = 150, activation = 'relu') %>%
      layer_dense(units = 50) %>%
      layer_dense(units = 1) %>%
      compile(
        loss = 'mean_squared_error',
        optimizer = optimizer_adam())

    new_model %>% load_model_weights_hdf5(
      file.path(paste0(catchment, "/ANN/",
                       model_name, "/", folder_name,
                       "/checkpoints/", "ensemble_member",  new_run, ".hdf5"))
    )

    score <- new_model %>% evaluate(x_val, y_val, verbose = FALSE)
    cat("Predict validation data with trained ensemble member", paste0(new_run, "/", ensemble_runs), "\n")
    predict_ANN <- predict(new_model, x_val)
    return(predict_ANN)
  }

  pred_results <- sapply(1:ensemble_runs, network_evaluation)
  pred_results <- as.data.frame(pred_results)

  # Saving and calculating the mean of the prediction results (mean prediction of ANN ensemble aggregation)
  feather::write_feather(pred_results,
                         paste0(catchment, "/ANN/", model_name, "/", folder_name, "/",
                                "ensemble_prediction_results_validation.feather"))

  # Calculating RMSE for all members of the ensemble
  model_rmse <- function(model_pred){
    residuals_model <- model_pred - y_val
    RMSE <- sqrt(mean(residuals_model^2))
    return(RMSE)
  }
  all_rmse <- apply(pred_results, 2, model_rmse)
  # define model_subset depending on the number of ensembles
  if(ensemble_runs < 100){
    model_subset <- ifelse(ensemble_runs <= 10, ensemble_runs, 10)
  } else {
    model_subset <- ceiling(ensemble_runs*0.1)
  }
  # choose best models
  best_model_preds <- pred_results[, order(all_rmse) <= model_subset]

  # Create vector with avg prediction of best 10% of the members (models) of the ensemble
  if(model_subset == ensemble_runs & model_subset > 1){
    message("All ensemble runs were used for the prediction.")
  }
  if(model_subset < ensemble_runs & model_subset >= 1){
    if(model_subset > 10){
      message("The best 10% of ensemble runs were used for the prediction.")
    } else {
      message("The best 10 of ensemble runs were used for the prediction.")
    }
    # Delete all loss data frames and loss plots from the ensembles not used for prediction
    model_to_delete <- which(order(all_rmse) > model_subset)
    txt <- capture.output(file.remove(paste0(catchment, "/ANN/", model_name, "/", folder_name,
                                             "/training_metrics/loss_ensemble_member_",
                                             model_to_delete, ".feather")))
    txt <- capture.output(file.remove(
      paste0(catchment, "/ANN/", model_name, "/", folder_name, "/training_metrics/plot_ensemble_member_",
             model_to_delete, ".png"))
    )

    cp_files <- list.files(paste0(catchment, "/ANN/", model_name, "/", folder_name, "/checkpoints/"))
    cp_numbers <- lapply(strsplit(cp_files, "\\."), function(x) sub("ensemble_member", "", x[1]))

    cp_to_delete <- cp_files[cp_numbers %in% as.character(model_to_delete)]
    txt <- capture.output(
      file.remove(paste0(catchment, "/ANN/", model_name, "/", folder_name, "/checkpoints/", cp_to_delete))
    )
  }

  if(model_subset != 1){
    mean_pred_results <- apply(best_model_preds, 1, mean)
  } else {
    mean_pred_results <- best_model_preds
    message("Only one model was used and therefore no ensemble prediction was done.")
  }
  # Add it to prediction df
  pred_results$mean <- mean_pred_results

  # Rescale results based on scaling version 1
  pred_results_mean_rescaled <- mean_pred_results*train_sd + train_mean
  y_val_rescaled <- y_val*train_sd + train_mean

  # Val model scores
  residuals_val <- pred_results_mean_rescaled - y_val_rescaled
  RMSE_val <- sqrt(mean(residuals_val^2))
  NSE_val <- 1 - (sum((pred_results_mean_rescaled- y_val_rescaled)^2, na.rm = TRUE) /
                sum( (y_val_rescaled - mean(y_val_rescaled, na.rm = TRUE))^2, na.rm = TRUE ) )


  # Testing
  cat("\nTESTING")

  network_testing <- function(test_run){
    #Create new model and predict using checkpoints
    test_model <- keras_model_sequential()
    test_model %>%
      layer_dense(units = 300, activation = 'selu', input_shape = c(n_features)) %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = 300, activation = 'relu') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units = 150, activation = 'relu') %>%
      layer_dense(units = 50) %>%
      layer_dense(units = 1) %>%
      compile(
        loss = 'mean_squared_error',
        optimizer = optimizer_adam())

    test_model %>% load_model_weights_hdf5(
      file.path(paste0(catchment, "/ANN/",
                       model_name, "/", folder_name, "/checkpoints/", "ensemble_member",  test_run, ".hdf5"))
    )

    test_score <- test_model %>% evaluate(x_train, y_train, verbose = FALSE)
    cat("Predict testing data with trained ensemble member", paste0(test_run, "/", ensemble_runs), "\n")
    test_predict_ANN <- predict(test_model, x_train)
    return(test_predict_ANN)
  }

  test_pred_results <- sapply(1:ensemble_runs, network_testing)






  # Save predicted values
  feather::write_feather(data.frame("predicted_values" = pred_results_mean_rescaled),
                         paste0(catchment, "/ANN/", model_name, "/", folder_name, "/predicted_values.feather"))


  run_time <-  paste0(
    round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
    " minutes")

  # Saving model scores
  if("model_scores.csv" %in% list.files(paste0(catchment, "/ANN/"))){
    model_scores <- read.csv(paste0(catchment, "/ANN/model_scores.csv"))

    model_scores <- rbind(model_scores,
                          data.frame(model = model_name,
                                     "data_inputs" = data_inputs,
                                     "start_time" = as.character(start_time),
                                     "run_time" = run_time,
                                     "ensemble_runs" = ensemble_runs,
                                     "epochs" = epochs,
                                     "batch_size" = bs,
                                     "RMSE" = round(RMSE, 3),
                                     "NSE" = round(NSE, 3)))
    write.csv(model_scores, paste0(catchment, "/ANN/model_scores.csv"), row.names = FALSE)

  } else {
    model_scores <- data.frame(model = model_name,
                               "data_inputs" = data_inputs,
                               "start_time" = as.character(start_time),
                               "run_time" = run_time,
                               "ensemble_runs" = ensemble_runs,
                               "epochs" = epochs,
                               "batch_size" = bs,
                               "RMSE" = round(RMSE, 3),
                               "NSE" = round(NSE, 3))
    write.csv(model_scores, paste0(catchment, "/ANN/model_scores.csv"), row.names = FALSE)

  }
}
