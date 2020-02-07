wt_rnn_optimizer <- function(catchment,
                             data_inputs = NULL,
                             rnn_type = NULL,
                             n_iter = 50,
                             n_predictions = 1,
                             epochs = 100,
                             early_stopping_patience = 5,
                             ensemble_runs = 5,
                             bounds_layers = c(1, 4),
                             bounds_timesteps = c(10, 365),
                             bounds_units = c(5, 365),
                             bounds_dropout = c(0, 0.5),
                             bounds_batch_size = c(5, 150),
                             top_n_models = 5,
                             initial_grid_from_model_scores = FALSE,
                             user_name = "R2D2"){

  # optimize structure with optimization algorithm
  Bopt_rnn_model <- function(layers, timesteps, units, dropout, batch_size){
    results <- wt_rnn(catchment = catchment,
                      data_inputs = data_inputs,
                      rnn_type = rnn_type,
                      layers = layers,
                      n_predictions = n_predictions,
                      timesteps = timesteps,
                      units = units,
                      dropout = dropout,
                      batch_size = batch_size,
                      epochs = epochs,
                      early_stopping_patience = early_stopping_patience,
                      ensemble_runs = 1,
                      user_name = user_name)
    return(list("Score" = results*-1, "Pred" = 0))
  }

  if(initial_grid_from_model_scores){
# get initial grid for optimization from the previous calculated model_scores
    cat("\n*** Using model_scores as initial grid for the Bayesian Optimization ***\n\n")
    model_scores <- read.csv(paste0(catchment, "/RNN/model_scores.csv"), stringsAsFactors = FALSE)
    initial_grid <- model_scores[model_scores$rnn_type == rnn_type, c("layers", "timesteps", "units", "dropout", "batch_size", "RMSE_val")]
  } else {
    grid <- expand.grid("layers" = sample(seq(bounds_layers[1], bounds_layers[2]), 2),
                        "timesteps" = sample(seq(bounds_timesteps[1], bounds_timesteps[2]), 2),
                        "units" = sample(seq(bounds_units[1], bounds_units[2]), 2),
                        "dropout" = sample(seq(bounds_dropout[1], bounds_dropout[2], by = 0.1), 2),
                        "batch_size" = sample(seq(bounds_batch_size[1], bounds_batch_size[2]), 2))

    cat("\n*** Computing the initial grid for the Bayesian Optimization ***
      with Hyperparameter sampled from the given bounds\n\n")

    # run initial grid models
    grid_results <- mapply(wt_rnn,
                           layers = grid$layers,
                           timesteps = grid$timesteps,
                           units = grid$units,
                           dropout = grid$dropout,
                           batch_size = grid$batch_size,
                           MoreArgs = list(
                             catchment = catchment,
                             data_inputs = data_inputs,
                             rnn_type = rnn_type,
                             n_predictions = n_predictions,
                             epochs = epochs,
                             early_stopping_patience = early_stopping_patience,
                             ensemble_runs = 1,
                             user_name = user_name)
    )
    initial_grid <- cbind(grid, grid_results)
  }
  cat("\n*** Starting Bayesian Hyperparameter Optimization ***\n")
    colnames(initial_grid)[6] <- "Value"
  initial_grid$Value <- initial_grid$Value * -1
  Bopt_rnn <- rBayesianOptimization::BayesianOptimization(Bopt_rnn_model,
                                                          bounds = list(layers = as.integer(bounds_layers),
                                                                        timesteps = as.integer(bounds_timesteps),
                                                                        units = as.integer(bounds_units),
                                                                        dropout = bounds_dropout,
                                                                        batch_size = as.integer(bounds_batch_size)),
                                                          n_iter = n_iter,
                                                          init_grid_dt = initial_grid,
                                                          acq = "ucb", kappa = 2.576, eps = 0.0,
                                                          verbose = TRUE)
  cat("\n*** Run the",  top_n_models, " best performing model as ensembles ***")
  # get 5 best models and run them with ensemble_runs
  top_n_model_results <- Bopt_rnn$History %>%
    top_n(n = top_n_models, wt = Value) %>%
    mutate(dropout = round(dropout, 2)) %>%
    select(-Value)
  final_results <- mapply(wt_rnn,
                          layers = top_n_model_results$layers,
                          timesteps = top_n_model_results$timesteps,
                          units = top_n_model_results$units,
                          dropout = top_n_model_results$dropout,
                          batch_size = top_n_model_results$batch_size,
                          MoreArgs = list(
                            catchment = catchment,
                            data_inputs = data_inputs,
                            rnn_type = rnn_type,
                            n_predictions = n_predictions,
                            epochs = epochs,
                            early_stopping_patience = early_stopping_patience,
                            ensemble_runs = ensemble_runs,
                            user_name = user_name)
  )
  cat("\n*** Save the optimization result in RNN folder ***")
  # get the best ensemble rsult
  ensemble_results <- cbind(top_n_model_results, final_results) %>%
    top_n(n = -1, wt = final_results)
  # Save optimization results in RNN folder
  new_opt_results <- data.frame(date = as.character(Sys.time()),
                                data_inputs = data_inputs,
                                rnn_type = rnn_type,
                                optimized_val_rmse = ensemble_results$final_results,
                                layers_opt = ensemble_results$layers,
                                timesteps_opt = ensemble_results$timesteps,
                                units_opt = ensemble_results$units,
                                dropout_opt = ensemble_results$dropout,
                                batch_size_opt = ensemble_results$batch_size,
                                n_iter = n_iter,
                                n_predictions = n_predictions,
                                epochs = epochs,
                                early_stopping_patience = early_stopping_patience,
                                ensemble_runs = ensemble_runs,
                                bounds_layers = paste(bounds_layers, collapse = "-"),
                                bounds_timesteps = paste(bounds_timesteps, collapse = "-"),
                                bounds_units = paste(bounds_units, collapse = "-"),
                                bounds_dropout = paste(bounds_dropout, collapse = "-"),
                                bounds_batch_size = paste(bounds_batch_size, collapse = "-"),
                                top_n_models = top_n_models,
                                user_name = user_name
  )
  if("optimization_results.csv" %in% list.files(paste0(catchment, "/RNN"))){
    opt_results <- read.csv(paste0(catchment, "/RNN/optimization_results.csv"))
    new_opt_results <- rbind(opt_results, new_opt_results)
  }
  write.csv(new_opt_results,
            paste0(catchment, "/RNN/optimization_results.csv"),
            row.names = FALSE, quote = FALSE)
}
