#' wt_rnn
#'
#' Recurrent neural network implementation for stream water temperature prediction including Bayesian hyperparameter optimization. All results are stored automatically in the folder catchment/model_name.
#'
#' @param train_data Data frame containing training data created by using wt_preprocessing()
#' @param test_data Data frame containing test data created by using wt_preprocessing()
#' @param type RNN cell type to use. Can be either "LSTM" for the Long short-term model, or "GRU" for the Gated recurrent unit.
#' @param catchment Catchment name as string, used for storing results in current working directory.
#' @param model_name Name of this particular model run as string, used for storing results in the catchment folder.
#' @param seed Random seed.
#' @param n_iter Number of iteration steps for bayesian hyperparameter optimization.
#' @param n_random_initial_points Number of sampled initial random points for bayesian hyperparameter optimization
#' @param epochs integer. Number of training epochs
#' @param early_stopping_patience Integer. Early stopping patience, i.e. the number of epochs with no improvement to waite before stopping the training
#' @param ensemble_runs Number of ensembles used for making the finel model.
#' @param bounds_layers Vector containing the lower and upper bound of the numbers of layers used in the bayesian hyperparameter optimization.
#' @param bounds_units Vector containing the lower and upper bound of the numbers of units used in the bayesian hyperparameter optimization.
#' @param bounds_dropout Vector containing the lower and upper bound of the numbers of dropout used in the bayesian hyperparameter optimization.
#' @param bounds_batch_size Vector containing the lower and upper bound of the numbers of batch size used in the bayesian hyperparameter optimization.
#' @param bounds_timesteps Vector containing the lower and upper bound of the numbers of timesteps used in the bayesian hyperparameter optimization.
#' @param initial_grid_from_model_scores logical. Should previous results be used as initial grid for the hyperparameter optimization? These have to be stored in the model_name folder under model_scores.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(test_catchment)
#' wt_preprocess(test_catchment)
#' train_data <- feather::read_feather("test_catchment/train_data.feather")
#' test_data <- feather::read_feather("test_catchment/test_data.feather")
#'
#' wt_rnn(train_data, test_data, "GRU", "test_catchment", "standard_rnn_gru")
#'}

wt_rnn <- function(train_data,
                   test_data = NULL,
                   type = "LSTM",
                   catchment = NULL,
                   model_name = NULL,
                   seed = NULL,
                   n_iter = 40,
                   n_random_initial_points = 20,
                   epochs = 100,
                   early_stopping_patience = 5,
                   ensemble_runs = 5,
                   bounds_layers = c(1, 5),
                   bounds_units = c(5, 300),
                   bounds_dropout = c(0, 0.4),
                   bounds_batch_size = c(5, 150),
                   bounds_timesteps = c(5, 200),
                   initial_grid_from_model_scores = TRUE){
  # checks
  model_short <- "RNN"
  general_checks(catchment, train_data, test_data, model_name, type, cv_mode = NULL, model_short)
  # create folders
  folder_structure(catchment, model_short, model_name, type)
  # Start message
  start_message(catchment, model_short)
  # start time
  start_time <- Sys.time()

  # get wt and date
  train <- train_data[, which(colnames(train_data) %in% c("date", "wt"))]
  if(!is.null(test_data)){
    test <- test_data[, which(colnames(test_data) %in% c("date", "wt"))]
  }
  # remove NA rows
  na_train <- which(is.na(train_data), arr.ind = TRUE)
  if(nrow(na_train) > 0) train_data <- train_data[-unique(na_train[,1]),]
  if(!is.null(test_data)) {
    na_test <- which(is.na(test_data), arr.ind = TRUE)
    if(nrow(na_test) > 0) test_data <- test_data[-na_test[,1],]
  }
  # train/val split
  part_training <- nrow(train_data)/4 * 3
  train_length <- floor(part_training)
  nn_val <- train_data[(train_length + 1):nrow(train_data), ]
  nn_train <- train_data[1:train_length, ]

  # Feature scaling
  date_col <- which(names(nn_train) == "date")
  train_means <- apply(nn_train[, -date_col], 2, mean, na.rm = TRUE)
  train_sds <- apply(nn_train[, -date_col], 2, sd, na.rm = TRUE)
  features_to_scale <- colnames(nn_train)[!(colnames(nn_train) %in% c("date", "wt"))]
  for(col in features_to_scale){
    nn_train[, col] <- (nn_train[, col] - train_means[col]) / train_sds[col]
    train_data[, col] <- (train_data[, col] - train_means[col]) / train_sds[col]
    nn_val[, col] <- (nn_val[, col] - train_means[col]) / train_sds[col]
    if(!is.null(test_data)){
      test_data[, col] <- (test_data[, col] - train_means[col]) / train_sds[col]
    }
  }

  # check if there are time gaps in the data -> split
  utils::capture.output({
    full_train_split <- data_splitter_for_rnn(train_data,
                                              data_name = "train", catchment = catchment)
  })
  train_split <- data_splitter_for_rnn(nn_train, data_name = "train",
                                       catchment = catchment)
  val_split <- data_splitter_for_rnn(nn_val, data_name = "validation",
                                     catchment = catchment)
  if(!is.null(test_data)){
    test_split <- data_splitter_for_rnn(test_data, data_name = "test",
                                        catchment = catchment)
  }
  # split in x & y in
  x_train <- lapply(train_split,
                    function(x) x %>% dplyr::select(-"wt", -date) %>% as.matrix())
  y_train <- lapply(train_split,
                    function(x) x %>% dplyr::select("wt") %>% as.matrix())
  x_full_train <- lapply(full_train_split,
                         function(x) x %>% dplyr::select(-"wt", -date) %>% as.matrix())
  y_full_train <- lapply(full_train_split,
                         function(x) x %>% dplyr::select("wt") %>% as.matrix())
  x_val <- lapply(val_split,
                  function(x) x %>% dplyr::select(-"wt", -date) %>% as.matrix())
  y_val <- lapply(val_split,
                  function(x) x %>% dplyr::select("wt") %>% as.matrix())
  if(!is.null(test_data)){
    x_test <- lapply(test_split,
                     function(x) x %>% dplyr::select(-"wt", -date) %>% as.matrix())
    y_test <- lapply(test_split,
                     function(x) x %>% dplyr::select("wt") %>% as.matrix())
  } else {
    x_test <- y_test <- test <- NULL
  }
  cat("Mean and standard deviation used for feature scaling are saved under",
      paste0(catchment, "/", model_short, "/", type, "/",
             model_name, "/scaling_values.csv\n"))
  suppressWarnings(
    utils::write.csv(rbind(train_means, train_sds),
              paste0(catchment, "/",model_short, "/", type, "/",
                     model_name, "/scaling_values.csv"))
  )

  # initial value for flag -> if additional initial_grid points should be calculated
  ini_grid_cal_flag <- FALSE
  # if there are no model score available -> set initial_grid_from_model_scores = FALSE
  if(initial_grid_from_model_scores){
    if(!("hyperpar_opt_scores.csv" %in%
         list.files(paste0(catchment, "/", model_short, "/", type, "/", model_name)))) {
      initial_grid_from_model_scores <- FALSE
    }
  }
  if(initial_grid_from_model_scores){
    # get initial grid for optimization from the previous calculated model_scores
    cat("Using existing scores as initial grid for the Bayesian Optimization\n")
    hyperpar_opt_scores_csv <- utils::read.csv(
      paste0(catchment, "/", model_short, "/", type, "/",
             model_name, "/hyperpar_opt_scores.csv"),
      stringsAsFactors = FALSE)
    initial_grid <- hyperpar_opt_scores_csv[c("layers", "units", "dropout", "batch_size",
                                              "timesteps", "cv_or_validation_RMSE")]
    if(nrow(initial_grid) < n_random_initial_points) ini_grid_cal_flag <- TRUE

  }
  # should a random grid be calculated first
  if(!initial_grid_from_model_scores | ini_grid_cal_flag) {
    set.seed(seed)
    n_random <- ifelse(ini_grid_cal_flag,
                       n_random_initial_points - nrow(initial_grid),
                       n_random_initial_points)
    grid <- data.frame(
      "layers" = replicate(n = n_random,
                           sample(
                             x = seq(bounds_layers[1], bounds_layers[2]),
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
                                 size = 1)),
      "timesteps" = replicate(n = n_random,
                              sample(
                                x = seq(bounds_timesteps[1], bounds_timesteps[2]),
                                size = 1))
    )

    cat(paste0("\nRandom hyperparameter sampling:\n"))
    # run initial grid models
    grid_results <- mapply(
      wt_nn,
      batch_size = grid$batch_size,
      layers = grid$layers,
      units = grid$units,
      dropout = grid$dropout,
      timesteps = grid$timesteps,
      ensemble_runs = 1,
      MoreArgs = list(catchment = catchment,
                      x_train = x_train, y_train = y_train,
                      x_val = x_val, y_val = y_val,
                      x_full_train = x_full_train,
                      y_full_train = y_full_train,
                      epochs = epochs,
                      early_stopping_patience = early_stopping_patience,
                      test = test,
                      model_short = model_short,
                      model_name = model_name,
                      seed = seed,
                      nn_type = "RNN",
                      type = type,
                      n_features = (ncol(train_data)-2)
      ))
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
  cat("Bayesian Hyperparameter Optimization:\n")
  if(nrow(initial_grid) > n_random_initial_points){
    n_iter <- n_iter - (nrow(initial_grid) - n_random_initial_points)
    cat(nrow(initial_grid) - n_random_initial_points,
        "iterations were already computed\n")
  }
  colnames(initial_grid)[ncol(initial_grid)] <- "Value"
  if(n_iter > 0){
    # function to be optimized
    Bopt_nn_model <- function(layers, units, dropout,  batch_size, timesteps) {
      results <- wt_nn(
        x_train = x_train,
        y_train = y_train,
        x_val = x_val,
        y_val = y_val,
        x_test = x_test,
        y_test = y_test,
        x_full_train = x_full_train,
        y_full_train = y_full_train,
        catchment = catchment,
        layers = layers,
        units = units,
        dropout = dropout,
        batch_size = batch_size,
        timesteps = timesteps,
        epochs = epochs,
        early_stopping_patience = early_stopping_patience,
        ensemble_runs = 1,
        model_short = model_short,
        model_name = model_name,
        seed = seed,
        nn_type = "RNN",
        type = type,
        n_features = (ncol(train_data)-2))
      return(list("Score" = results*-1, "Pred" = 0))
    }
    initial_grid$Value <- initial_grid$Value * -1
    set.seed(seed)
    Bopt_nn <- rBayesianOptimization::BayesianOptimization(
      Bopt_nn_model,
      bounds = list(layers = as.integer(bounds_layers),
                    units = as.integer(bounds_units),
                    dropout = bounds_dropout,
                    batch_size = as.integer(bounds_batch_size),
                    timesteps = as.integer(bounds_timesteps)),
      n_iter = n_iter,
      init_grid_dt = initial_grid,
      acq = "ucb", kappa = 2.576, eps = 0.0,
      verbose = TRUE)
    all_model_results <- Bopt_nn$History
  } else {
    # if all iterations are done -> use initial grid
    all_model_results <- initial_grid
    all_model_results$Value <- all_model_results$Value * -1
  }

  # run best model as ensemble and save results
  cat("Run the best performing model as ensemble:\n")
  . = NULL
  top_n_model_results <- all_model_results %>%
    dplyr::top_n(n = 1, wt = "Value") %>%
    dplyr::mutate("dropout" = round(., 2)) %>%
    dplyr::select(-"Value")
  if(nrow(top_n_model_results) != 1) top_n_model_results <- top_n_model_results[1, ]
  set.seed(seed)
  wt_nn(
    layers = top_n_model_results$layers,
    units = top_n_model_results$units,
    dropout = top_n_model_results$dropout,
    batch_size = top_n_model_results$batch_size,
    timesteps = top_n_model_results$timesteps,
    x_train = x_train,
    y_train = y_train,
    x_val = x_val,
    y_val = y_val,
    x_test = x_test,
    y_test = y_test,
    catchment = catchment,
    epochs = epochs,
    early_stopping_patience = early_stopping_patience,
    ensemble_runs = ensemble_runs,
    model_short = model_short,
    model_name = model_name,
    save_model_and_prediction = TRUE,
    na_test = na_test,
    na_train = na_train,
    x_full_train = x_full_train,
    y_full_train = y_full_train,
    start_time = start_time,
    seed = seed,
    nn_type = "RNN",
    test = test,
    train = train,
    type = type,
    n_features = (ncol(train_data)-2),
    full_train_split = full_train_split,
    test_split = test_split)
}
