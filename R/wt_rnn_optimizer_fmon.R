#' wt_rnn_optimizer
#'
#' @param catchment
#' @param data_inputs
#' @param rnn_type
#' @param n_iter
#' @param n_random_initial_points
#' @param n_predictions
#' @param epochs
#' @param early_stopping_patience
#' @param ensemble_runs
#' @param bounds_layers
#' @param bounds_timesteps
#' @param bounds_units
#' @param bounds_dropout
#' @param bounds_batch_size
#' @param top_n_models
#' @param initial_grid_from_model_scores
#' @param user_name
#'
#' @return
#' @export
#'
#' @examples
wt_rnn_optimizer_fmon <- function(catchment,
                             data_inputs = NULL,
                             rnn_type = NULL,
                             n_iter = 20,
                             n_random_initial_points = 20,
                             n_predictions = 1,
                             epochs = 100,
                             early_stopping_patience = 5,
                             ensemble_runs = 3,
                             bounds_layers = c(1, 4),
                             bounds_timesteps = c(10, 150),
                             bounds_units = c(5, 150),
                             bounds_dropout = c(0, 0.3),
                             bounds_batch_size = c(5, 150),
                             top_n_models = 3,
                             initial_grid_from_model_scores = TRUE,
                             user_name = "R2D2"){

  # initial value for flag -> if additional initial_grid points should be calculated
  ini_grid_cal_flag <- FALSE
  # if there are no model score available -> set initial_grid_from_model_scores = FALSE
  if(initial_grid_from_model_scores){
    if(!("model_scores.csv" %in% list.files(paste0(catchment, "/RNN_fmon")))) {
      initial_grid_from_model_scores <- FALSE
    }
  }
  if(initial_grid_from_model_scores){
    # get initial grid for optimization from the previous calculated model_scores
    cat("\n*** Using existing model_scores as initial grid for the Bayesian Optimization ***\n\n")
    model_scores <- read.csv(paste0(catchment, "/RNN_fmon/model_scores.csv"),
                             stringsAsFactors = FALSE)
    initial_grid <- model_scores[model_scores$rnn_type == rnn_type &
                                   model_scores$data_inputs == data_inputs,
                                 c("layers", "timesteps", "units", "dropout",
                                   "batch_size", "RMSE_val")]
    if(nrow(initial_grid) < n_random_initial_points) ini_grid_cal_flag <- TRUE

  }
  # should a random grid be calculated first
  if(!initial_grid_from_model_scores | ini_grid_cal_flag) {
    n_random <- ifelse(ini_grid_cal_flag,
                       n_random_initial_points - nrow(initial_grid),
                       n_random_initial_points)
    grid <- data.frame(
      "layers" = replicate(n = n_random,
                           sample(
                             x = seq(bounds_layers[1], bounds_layers[2]),
                             size = 1)),
      "timesteps" = replicate(n = n_random,
                              sample(
                                x = seq(bounds_timesteps[1], bounds_timesteps[2]),
                                size = 1)),
      "units" = replicate(n = n_random,
                          sample(
                            x = seq(bounds_units[1], bounds_units[2]), size = 1)),
      "dropout" = replicate(n = n_random,
                            sample(
                              x = seq(bounds_dropout[1], bounds_dropout[2], by = 0.025),
                              size = 1)),
      "batch_size" = replicate(n = n_random,
                               sample(
                                 x = seq(bounds_batch_size[1], bounds_batch_size[2]),
                                 size = 1))
    )
    cat("\n*** Computing the initial grid for the Bayesian Optimization ***
      with Hyperparameter sampled from the given bounds\n\n")
    # run initial grid models
    grid_results <- mapply(wt_rnn_fmon,
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
    # Define initial_grid for bayesian hyperaparameter optimization
    if(ini_grid_cal_flag){
      additional_grid_points <- cbind(grid, grid_results)
      names(additional_grid_points) <- names(initial_grid)
      initial_grid <- rbind(initial_grid, additional_grid_points)
    } else {
      initial_grid <- cbind(grid, grid_results)
    }
  }
  # Bayesian Hyperparameter optimization
  cat("\n*** Starting Bayesian Hyperparameter Optimization ***\n")
  if(nrow(initial_grid) > n_random_initial_points){
    n_iter <- n_iter - (nrow(initial_grid) - n_random_initial_points)
    cat(nrow(initial_grid) - n_random_initial_points,
        "iterations were already computed\n")
  }
  colnames(initial_grid)[6] <- "Value"

  if(n_iter > 0){
    # function to be optimized
    Bopt_rnn_model <- function(layers, timesteps, units, dropout, batch_size) {
      results <- wt_rnn_fmon(catchment = catchment,
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
    # if there are still iterations left -> compute
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
    all_model_results <- Bopt_rnn$History
  } else {
    # if all iterations are done -> use initial grid
    all_model_results <- initial_grid
    all_model_results$Value <- all_model_results$Value * -1
  }

  # get top_n best models and run them with ensemble_runs
  cat("\n*** Run the",  top_n_models, " best performing model as ensembles ***")
  top_n_model_results <- all_model_results %>%
    top_n(n = top_n_models, wt = Value) %>%
    mutate(dropout = round(dropout, 2)) %>%
    select(-Value)

  final_results <- mapply(wt_rnn_fmon,
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
  cat("\n*** Save the optimization result in RNN_fmon folder ***")
  # get the best ensemble rsult
  ensemble_results <- cbind(top_n_model_results, final_results) %>%
    top_n(n = -1, wt = final_results)

  # get test values from model scores
  model_scores <- read.csv(paste0(catchment, "/RNN_fmon/model_scores.csv"),
                           stringsAsFactors = FALSE)
  # filter gets confused when a variable has the same name as a column
  esp <- early_stopping_patience
  er <- ensemble_runs
  np <- n_predictions
  un <- user_name
  # filter ensemble result information from model_scores
  performance <- model_scores %>% filter(layers == ensemble_results$layers &
                                           timesteps == ensemble_results$timesteps &
                                           units == ensemble_results$units &
                                           dropout == ensemble_results$dropout &
                                           batch_size == ensemble_results$batch_size &
                                           ensemble_runs == er &
                                           n_predictions == np &
                                           early_stopping_patience == esp &
                                           max_epochs == epochs &
                                           user_name == un) %>%
    select(RMSE_val, NSE_val, RMSE_test, NSE_test, start_time)
  if(nrow(performance) > 1) performance <- sapply(performance, mean) %>% t() %>%  as.data.frame()
  # Save optimization results in RNN_fmon folder
  new_opt_results <- data.frame(date = performance$start_time,
                                data_inputs = data_inputs,
                                rnn_type = rnn_type,
                                RMSE_val = performance$RMSE_val,
                                NSE_val = performance$NSE_val,
                                RMSE_test = performance$RMSE_test,
                                NSE_test = performance$NSE_test,
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
                                user_name = user_name,
                                stringsAsFactors = FALSE
  )
  if("optimization_results.csv" %in% list.files(paste0(catchment, "/RNN_fmon"))){
    opt_results <- read.csv(paste0(catchment, "/RNN_fmon/optimization_results.csv"))
    new_opt_results <- rbind(opt_results, new_opt_results)
  }
  write.csv(new_opt_results,
            paste0(catchment, "/RNN_fmon/optimization_results.csv"),
            row.names = FALSE, quote = FALSE)
}
