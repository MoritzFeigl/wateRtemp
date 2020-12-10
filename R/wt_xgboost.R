#' wt_xgboost
#'
#' @param train_data
#' @param test_data
#' @param catchment
#' @param cv_mode
#' @param model_name
#' @param no_cores
#' @param seed
#' @param n_iter
#' @param n_random_initial_points
#'
#' @return
#' @export
#'
#' @examples
wt_xgboost <- function(train_data,
                       test_data = NULL,
                       catchment = NULL,
                       cv_mode = "repCV",
                       model_name = NULL,
                       no_cores = parallel::detectCores() - 1,
                       seed = NULL,
                       n_iter = 40,
                       n_random_initial_points = 20){

  # checks
  model_short <- "XGBoost"
  type <- NULL
  general_checks(catchment, train_data, test_data, model_name, type, cv_mode, model_short)
  # create folders
  folder_structure(catchment, model_short, model_name, type = NULL)
  # Start message
  start_message(catchment, model_short)
  # start time
  start_time <- Sys.time()
  # keep train and test for output creation and remove date from model data
  train <- train_data[, which(colnames(train_data) %in% c("date", "wt"))]
  test <- test_data[, which(colnames(test_data) %in% c("date", "wt"))]
  train_data <- train_data[, -which(colnames(train_data) == "date")]
  test_data <- test_data[, -which(colnames(test_data) == "date")]
  # remove NA rows
  na_train <- which(is.na(train_data), arr.ind = TRUE)
  if(nrow(na_train) > 0) train_data <- train_data[-unique(na_train[,1]),]
  na_test <- which(is.na(test_data), arr.ind = TRUE)
  if(nrow(na_test) > 0) test_data <- test_data[-na_test[,1],]
  # random seed
  if(is.null(seed)) seed <- sample(1000:100000, 1)

  # XGBoost
  # Train control settings
  if(cv_mode == "timeseriesCV"){
    n_seeds <- ceiling((nrow(train_data) - 730)/60)
    seeds <- vector(mode = "list", length = n_seeds)
    set.seed(seed)
    for(i in 1:n_seeds) seeds[[i]] <- sample(10000, 200)
    tc <- caret::trainControl(method = "timeslice",
                              initialWindow = 730,
                              horizon = 90,
                              fixedWindow = FALSE,
                              allowParallel = TRUE,
                              verboseIter = TRUE,
                              skip = 60,
                              seeds = seeds)
  }
  if(cv_mode == "repCV"){
    seeds <- vector(mode = "list", length = 51)
    set.seed(seed)
    for(i in 1:51) seeds[[i]] <- sample(10000, 200)
    tc <- caret::trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5,
                              allowParallel = TRUE,
                              verboseIter = TRUE,
                              seeds = seeds)
  }

  # optimization parameter bounds
  parameter_bounds <- list(
    "nrounds" = c(300L, 3000L),
    "eta" = c(0.001, 0.3),
    "max_depth" = c(3L, 12L),
    "min_child_weight" = c(1L, 10L),
    "subsample" = c(0.7, 1),
    "colsample_bytree" = c(0.7, 1),
    "gamma" = c(0L, 5L)
  )
  # initial sampled grid
  initial_grid <- parameter_bounds
  for(i in 1:length(parameter_bounds)){
    range <- parameter_bounds[[i]]
    if(is.integer(range)){
      initial_grid[[i]] <- sample(range[1]:range[2], n_random_initial_points, replace = TRUE)
    } else {
      initial_grid[[i]] <- round(sample(seq(range[1], range[2], length.out = 10),
                                        n_random_initial_points, replace = TRUE), 3)
    }
  }
  initial_grid <- as.data.frame(do.call(cbind, initial_grid))

  # run initial grid models
  nul_out <- ifelse(Sys.info()["sysname"] == "Windows", "NUL", "/dev/null")
  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)
  model <- caret::train(wt ~ .,
                        data = train_data,
                        method = "xgbTree",
                        trControl = tc,
                        nthread = 1,
                        tuneGrid = initial_grid)
  parallel::stopCluster(cl)
  hyperpar_opt_scores <- model$results[, c(names(initial_grid), "RMSE", "MAE")]

  write.csv(hyperpar_opt_scores,
            file = paste0(catchment, "/", model_short, "/", model_name, "/hyperpar_opt_scores.csv"),
            row.names = FALSE)
  initial_grid <- hyperpar_opt_scores[, 1:7]
  initial_grid$Value <- hyperpar_opt_scores$RMSE * -1

  # define XGBoost function for optimization
  xgb_optim <- function(nrounds, eta, max_depth, min_child_weight,
                        subsample, colsample_bytree, gamma) {
    tune_grid <- expand.grid(nrounds = nrounds,
                             max_depth = max_depth,
                             eta = eta,
                             gamma = gamma,
                             colsample_bytree = colsample_bytree,
                             subsample = subsample,
                             min_child_weight = min_child_weight)
    cl <- parallel::makePSOCKcluster(no_cores)
    doParallel::registerDoParallel(cl)
    model <- caret::train(wt ~ .,
                          data = train_data,
                          method = "xgbTree",
                          trControl = tc,
                          nthread = 1,
                          tuneGrid = tune_grid)
    parallel::stopCluster(cl)
    # save results of given hyperparameter set
    hyperpar_opt_scores <- read.csv(paste0(catchment, "/", model_short, "/",
                                           model_name, "/hyperpar_opt_scores.csv"))
    hyperpar_opt_scores <- rbind(hyperpar_opt_scores,
                                 model$results[, names(hyperpar_opt_scores)])
    write.csv(hyperpar_opt_scores,
              file = paste0(catchment, "/", model_short, "/",
                            model_name, "/hyperpar_opt_scores.csv"),
              row.names = FALSE)
    return(list(Score = -caret::getTrainPerf(model)[, "TrainRMSE"], Pred = 0))
  }

  set.seed(seed)
  Bopt_xgboost <- rBayesianOptimization::BayesianOptimization(xgb_optim,
                                                              bounds = parameter_bounds,
                                                              init_grid_dt = initial_grid,
                                                              init_points = 0,
                                                              n_iter = n_iter,
                                                              acq = "ucb",
                                                              kappa = 2.576,
                                                              eps = 0.0,
                                                              verbose = TRUE)
  initial_grid$Value <- NULL
  # update colnames of hyperpar_opt_scores
  hyperpar_opt_scores <- read.csv(paste0(catchment, "/", model_short, "/",
                                         model_name, "/hyperpar_opt_scores.csv"))
  hyperpar_opt_scores[, c("RMSE", "MAE")] <- round(hyperpar_opt_scores[, c("RMSE", "MAE")], 3)

  # Optimized hyperparameters
  best_par <- data.frame(nrounds = Bopt_xgboost$History$nrounds,
                         max_depth =  Bopt_xgboost$History$max_depth,
                         eta = Bopt_xgboost$History$eta,
                         gamma = Bopt_xgboost$History$gamma,
                         colsample_bytree = Bopt_xgboost$History$colsample_bytree,
                         subsample = Bopt_xgboost$History$subsample,
                         min_child_weight = Bopt_xgboost$History$min_child_weight)
  cat("Finished hyperparameter optimization, optimized parameters:\n")
  for(i in 2:ncol(best_par)) cat(names(best_par)[i], ":", as.numeric(best_par[1, i]), "\n")
  # Run model with optimized hyperparameters
  cat("Running XGBoost with optimized hyperparameter set...")
  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)
  capture.output({
    model <- caret::train(wt ~ .,
                          data = train_data,
                          method = "xgbTree",
                          trControl = tc,
                          nthread = 1,
                          tuneGrid = best_par)},
    nul_out)
  parallel::stopCluster(cl)
  cat("Done!\n")

  # save all hyperparameter set performances
  hyperpar_opt_scores2 <- model$results[, c(names(initial_grid), "RMSE", "MAE")]
  hyperpar_opt_scores <- rbind(hyperpar_opt_scores,
                               hyperpar_opt_scores2)
  hyperpar_opt_scores[, c("RMSE", "MAE")] <- round(hyperpar_opt_scores[, c("RMSE", "MAE")], 3)
  colnames(hyperpar_opt_scores) <- c(names(initial_grid), "cv_or_validation_RMSE", "cv_or_validation_MAE")
  cat("\nHyperparameter optimization results are saved in",
      paste0("/",catchment, "/", model_short,
             "/", model_name, "/hyperpar_opt_scores.csv\n"))
  write.csv(hyperpar_opt_scores,
            paste0(catchment, "/", model_short,
                   "/", model_name, "/hyperpar_opt_scores.csv"),
            row.names = FALSE)

  cv_or_val_results <- model$results[which.min(model$results$RMSE), c("RMSE", "MAE")]
  # model prediction
  suppressWarnings({train_prediction <- predict(model, train_data)})
  save_prediction_results(train_prediction, train, na_train,
                          model_short, model_name, "train_data", type)
  if(!is.null(test_data)) {
    suppressWarnings({test_prediction <- predict(model, test_data)})
    save_prediction_results(test_prediction, test, na_test,
                            model_short, model_name, "test_data", type)
  }
  # model diagnostics
  model_diagnostic(train_prediction = train_prediction,
                   train_data = train_data,
                   test_prediction = test_prediction,
                   test_data = test_data,
                   cv_mode = cv_mode,
                   cv_or_val_results = cv_or_val_results,
                   start_time = start_time,
                   catchment = catchment,
                   type = type,
                   model_name = model_name,
                   model_short = model_short,
                   model = model)
  # importasnce plot
  save_variable_importance(model, model_short, model_name)
}
