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
                      n_features, model_name, user_name){


  # Folder structure ---------------------------------------------------------------------
  model_name <- paste0(data_inputs, "Model")
  folder_name <- paste0(epochs, "epochs_", bs, "batchsize_", ensemble_runs, "ensembleRuns")

  if(!file.exists(paste0(catchment, "/ANN/", model_name))){
    dir.create(file.path(paste0(catchment, "/ANN/", model_name)))
  }
  if(!file.exists(paste0(catchment, "/ANN/", model_name, "/", folder_name))){
    dir.create(file.path(paste0(catchment, "/ANN/", model_name, "/", folder_name)))
  }
  if(!file.exists(paste0(catchment, "/ANN/", model_name, "/", folder_name,
                         "/checkpoints"))){
    dir.create(file.path(paste0(catchment, "/ANN/", model_name, "/",
                                folder_name, "/checkpoints")))
  }
  if(!file.exists(paste0(catchment, "/ANN/", model_name, "/", folder_name,
                         "/training_metrics"))){
    dir.create(file.path(paste0(catchment, "/ANN/", model_name, "/",
                                folder_name, "/training_metrics")))
  }

  # start time
  start_time <- Sys.time()
  # Training -----------------------------------------------------------------------------
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
              optimizer = optimizer_adam())
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

    if(run == 1){
      predict_results_val <- predict(model, x_val)
      predict_results_test <- predict(model, x_test)
    } else {
      predict_results_val <- cbind(predict_results_val, predict(model, x_val))
      predict_results_test <- cbind(predict_results_test, predict(model, x_test))
    }

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


  # choose best ensemble runs -> delete others, take redictions only from the best

  # Calculating RMSE for all members of the ensemble
  model_rmse <- function(model_pred, y){
    residuals_model <- model_pred - y
    RMSE <- sqrt(mean(residuals_model^2))
    return(RMSE)
  }
  all_rmse_val <- apply(predict_results_val, 2, model_rmse, y = y_val)
  # define model_subset depending on the number of ensembles
  if(ensemble_runs < 100){
    model_subset <- ifelse(ensemble_runs <= 10, ensemble_runs, 10)
  } else {
    model_subset <- ceiling(ensemble_runs*0.1)
  }
  # choose best models
  best_model_preds_val <- predict_results_val[, order(all_rmse_val) <= model_subset]
  best_model_preds_test <- predict_results_test[, order(all_rmse_val) <= model_subset]

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
    model_to_delete <- which(order(all_rmse_val) > model_subset)
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
    mean_pred_results_val <- apply(best_model_preds_val, 1, mean)
    mean_pred_results_test <- apply(best_model_preds_test, 1, mean)
  } else {
    mean_pred_results_val <- best_model_preds_val
    mean_pred_results_test <- best_model_preds_test
    message("Only one model was used and therefore no ensemble prediction was done.")
  }

  # Model Scores -------------------------------------------------------------------------
  # Save predicted values
  feather::write_feather(data.frame("predicted_values" = mean_pred_results_test*train_sd + train_mean),
                         paste0(catchment, "/ANN/", model_name, "/", folder_name, "/predicted_values.feather"))
  # Rscaling and scores
  residuals_val <- (mean_pred_results_val*train_sd + train_mean) - (y_val*train_sd + train_mean)
  RMSE_val <- round(sqrt(mean(residuals_val^2)), 3)
  NSE_val <- round(
    1 - (sum((mean_pred_results_val- y_val)^2, na.rm = TRUE) /
           sum( (y_val - mean(y_val, na.rm = TRUE))^2, na.rm = TRUE ) ),
    3)

  residuals_test <- (mean_pred_results_test*train_sd + train_mean) - (y_test*train_sd + train_mean)
  RMSE_test <- round(sqrt(mean(residuals_test^2)), 3)
  NSE_test <- round(
    1 - (sum((mean_pred_results_test - y_test)^2, na.rm = TRUE) /
           sum( (y_test - mean(y_test, na.rm = TRUE))^2, na.rm = TRUE ) ),
    3)
  run_time <- paste0(
    round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
    " minutes")
  # Saving model scores
  if("model_scores.csv" %in% list.files(paste0(catchment, "/ANN/"))){
    model_scores <- read.csv(paste0(catchment, "/ANN/model_scores.csv"))

    model_scores <- rbind(model_scores,
                          data.frame(user = user_name,
                                     model = model_name,
                                     "data_inputs" = data_inputs,
                                     "start_time" = as.character(start_time),
                                     "run_time" = run_time,
                                     "ensemble_runs" = ensemble_runs,
                                     "epochs" = epochs,
                                     "batch_size" = bs,
                                     "RMSE_val" = round(RMSE_val, 3),
                                     "NSE_val" = round(NSE_val, 3),
                                     "RMSE_test" = round(RMSE_test, 3),
                                     "NSE_test" = round(NSE_test, 3)))
    write.csv(model_scores, paste0(catchment, "/ANN/model_scores.csv"), row.names = FALSE)

  } else {
    model_scores <- data.frame(user = user_name,
                               model = model_name,
                               "data_inputs" = data_inputs,
                               "start_time" = as.character(start_time),
                               "run_time" = run_time,
                               "ensemble_runs" = ensemble_runs,
                               "epochs" = epochs,
                               "batch_size" = bs,
                               "RMSE_val" = round(RMSE_val, 3),
                               "NSE_val" = round(NSE_val, 3),
                               "RMSE_test" = round(RMSE_test, 3),
                               "NSE_test" = round(NSE_test, 3))
    write.csv(model_scores, paste0(catchment, "/ANN/model_scores.csv"), row.names = FALSE)
  }
}




