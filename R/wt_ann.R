#' wt_ann
#'
#' @param catchment
#' @param data_inputs
#' @param model_or_optim
#' @param bs
#' @param epochs
#' @param ensemble_runs
#'
#' @return
#' @export
#'
#' @examples
wt_ann <- function(catchment,
                   data_inputs = NULL,
                   model_or_optim,
                   bs = c(10, 50, 100),
                   epochs = c(100),
                   ensemble_runs = 100){

  if(is.null(data_inputs)){
    warning('\nChoose a valid data_input:
            "simple"    = Q, Tmean and water temperatur observations
            "precip"    = "simple" + additional precipitation observations
            "radiation" = "simple" + additional longwave radiation observations
            "all"       = all the above mentioned observations')
  }
  model_name <- paste0(data_inputs, "Model_", epochs, "epochs_", bs, "bs_", ensemble_runs, "ensembleRuns")

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

  ann_train <- train[, relevant_data]
  ann_val <- val[, relevant_data]
  # Scaling
  train_mean <- mean(ann_train$wt)
  train_sd <- sd(ann_train$wt)
  ann_train[, relevant_data] <- scale(ann_train[, relevant_data])
  ann_val[, relevant_data[-3]] <- scale(ann_val[, relevant_data[-3]])
  ann_val$wt <- (ann_val$wt - train_mean)/train_sd

  # As matrix
  ann_train <- as.matrix(ann_train)
  ann_val <- as.matrix(ann_val)
  # x: Q and Tmean, y: wt
  x_train <- ann_train[, relevant_data[-3]]
  y_train <- ann_train[, c("wt")]
  x_val <- ann_val[, relevant_data[-3]]
  y_val <- ann_val[, c("wt")]

  if (file.exists("ANN")){
    setwd(file.path("ANN"))
  } else {
    dir.create(file.path("ANN"))
    setwd(file.path("ANN"))
  }

  if (file.exists(model_name)){
    setwd(file.path(model_name))
  } else {
    dir.create(file.path(model_name))
    setwd(file.path(model_name))
  }

  if (!file.exists("checkpoints")){
    dir.create(file.path("checkpoints"))
  }

  start_time <- Sys.time()

  # ANN ensemble aggregation (ensemble of equally structured ANNs with different initial weights)
  # First: Defining model and layers, Second: Training
  for(run in c(1:ensemble_runs)){
    # Defining model and layers
    model <- keras_model_sequential()
    model %>%
      layer_dense(units = 150, activation = 'selu', input_shape = c(n_features)) %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = 150, activation = 'relu') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units = 100, activation = 'relu') %>%
      layer_dense(units = 50) %>%
      layer_dense(units = 1) %>%
      compile(loss = 'mean_squared_error',
              optimizer = optimizer_adam(),
              metrics = c('mse'))
    # Create checkpoint callback
    ANN_checkpoint <- callback_model_checkpoint(filepath = paste0("checkpoints/", "run", run, ".hdf5"),
                                                save_best_only = TRUE, save_weights_only = TRUE)

    cat("\n---------------> run ", run, "<---------------\n")
    # Training
    history <- model %>% fit(
      x_train, y_train,
      epochs = epochs, batch_size = bs,
      validation_data = list(x_val, y_val),
      callbacks = list(ANN_checkpoint) # pass callback to training
    )
  }

  network_evaluation <- function(new_run){
    #Create new model and predict using checkpoints
    new_model <- keras_model_sequential()
    new_model %>%
      layer_dense(units = 150, activation = 'selu', input_shape = c(n_features)) %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = 150, activation = 'relu') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units = 100, activation = 'relu') %>%
      layer_dense(units = 50) %>%
      layer_dense(units = 1) %>%
      compile(
        loss = 'mean_squared_error',
        optimizer = optimizer_adam(),
        metrics = c('mse'))

    new_model %>% load_model_weights_hdf5(
      file.path(paste0("checkpoints/", "run",  new_run, ".hdf5"))
    )
    score <- new_model %>% evaluate(x_val, y_val)
    predict_ANN<- predict(new_model, x_val)
    return(predict_ANN)
  }

  pred_results <- sapply(1:ensemble_runs, network_evaluation)
  pred_results <- as.data.frame(pred_results)

  # Saving and calculating the mean of the prediction results (mean prediction of ANN ensemble aggregation)
  feather::write_feather(pred_results, "ensemble_prediction_results.feather")
  #pred_results <- read_feather("pred_results_cp.feather")

  # Calculating RMSE for all members of the ensemble
  model_rmse <- function(model_pred){
    residuals_model <- model_pred - y_val
    RMSE <- sqrt(mean(residuals_model^2))
    return(RMSE)
  }
  all_rmse <- apply(pred_results, 2, model_rmse)
  model_subset <- ifelse(ensemble_runs <= 10, ensemble_runs, ceiling(ensemble_runs*0.1))
  best_model_preds <- pred_results[, order(all_rmse) <= model_subset]
  # Create vector with avg prediction of best 10% of the members (models) of the ensemble
  if(model_subset == ensemble_runs & model_subset > 1){
    message("All ensemble runs were taken for the prediction.")
  }
  if(model_subset < ensemble_runs & model_subset > 1){
    message("Only the best 10% of ensemble runs were taken for the prediction.")
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

  # Model scores
  residuals <- pred_results_mean_rescaled - y_val_rescaled
  MSE <- mean(residuals^2)
  RMSE <- sqrt(mean(residuals^2))
  NSE <- 1 - (sum((pred_results_mean_rescaled- y_val_rescaled)^2, na.rm = TRUE) /
                    sum( (y_val_rescaled - mean(y_val_rescaled, na.rm = TRUE))^2, na.rm = TRUE ) )
  run_time <-  paste0(
    round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
    " minutes")

  if("model_scores.csv" %in% list.files("..")){
    model_scores <- read.csv("../model_scores.csv")

    model_scores <- rbind(model_scores,
                          data.frame(model = model_name,
                                     "data_inputs" = data_inputs,
                                     "start_time" = as.character(start_time),
                                     "run_time" = run_time,
                                     "ensemble_runs" = ensemble_runs,
                                     "epochs" = epochs,
                                     "batch_size" = bs,
                                     "mse" = MSE,
                                     "RMSE" = RMSE,
                                     "NSE" = NSE))
    write.csv(model_scores, "../model_scores.csv", row.names = FALSE)

  } else {
    model_scores <- data.frame(model = model_name,
                               "data_inputs" = data_inputs,
                               "start_time" = as.character(start_time),
                               "run_time" = run_time,
                               "ensemble_runs" = ensemble_runs,
                               "epochs" = epochs,
                               "batch_size" = bs,
                               "mse" = MSE,
                               "RMSE" = RMSE,
                               "NSE" = NSE)
    write.csv(model_scores, "../model_scores.csv", row.names = FALSE)

  }

  setwd(old_wd)

}
