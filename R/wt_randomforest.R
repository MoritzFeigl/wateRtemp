#' wt_randomforest
#'
#' Randomforest implementation for stream water temperature prediction including Bayesian hyperparameter optimization. All results are stored automatically in the folder catchment/model_name.
#'
#' @param train_data Data frame containing training data created by using wt_preprocessing()
#' @param test_data Data frame containing test data created by using wt_preprocessing()
#' @param catchment Catchment name as string, used for storing results in current working directory.
#' @param cv_mode Cross-validation mode. Can either be "repCV" for a 5times repeated 10-fold CV or "timeseriesCV" for a timeslice CV using intial window=730, horizon=90 and skip=60.
#' @param model_name Name of this particular model run as string, used for storing results in the catchment folder.
#' @param no_cores Number of cores used for computation. If NULL parallel::detectCores() - 1 is applied.
#' @param seed Random seed.
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
#' wt_randomforest(train_data, test_data, "test_catchment", "repCV", "standard_rf")
#' }
wt_randomforest <- function(train_data,
                            test_data = NULL,
                            catchment = NULL,
                            cv_mode = "repCV",
                            model_name = NULL,
                            no_cores = parallel::detectCores() - 1,
                            seed = NULL){

  # checks
  model_short <- "RF"
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
  if(!is.null(test_data)) test <- test_data[, which(colnames(test_data) %in% c("date", "wt"))]
  train_data <- train_data[, -which(colnames(train_data) == "date")]
  if(!is.null(test_data)) test_data <- test_data[, -which(colnames(test_data) == "date")]
  # remove NA rows
  na_train <- which(is.na(train_data), arr.ind = TRUE)
  if(nrow(na_train) > 0) train_data <- train_data[-unique(na_train[,1]),]
  if(!is.null(test_data)) {
    na_test <- which(is.na(test_data), arr.ind = TRUE)
    if(nrow(na_test) > 0) test_data <- test_data[-na_test[,1],]
  }
  # random seed
  if(is.null(seed)) seed <- sample(1000:100000, 1)

  # Random Forest
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

  cat("Applying grid search for hyperparameter optimization\n")
  # search grid with every 2nd mtry
  if(ncol(train_data)-1 < 4){
    all_mtries <- 1:(ncol(train_data)-1)
  } else {
    all_mtries <- 3:(ncol(train_data)-1)
  }
  search_grid <- expand.grid(mtry = all_mtries[all_mtries %% 2 == 1],
                             min.node.size = 2:10,
                             splitrule = "extratrees")
  nul_out <- ifelse(Sys.info()["sysname"] == "Windows", "NUL", "/dev/null")
  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)
  utils::capture.output({
    model <- caret::train(wt ~ .,
                          data = train_data,
                          method = "ranger",
                          trControl = tc,
                          tuneGrid = search_grid,
                          num.threads = 1)},
    file = nul_out)
  parallel::stopCluster(cl)
  # hyperpar_opt_scores
  hyperpar_opt_scores <- model$results[, c(names(search_grid), "RMSE", "MAE")]
  # run the best grid model and the two adjacent mtry values
  best_par <- model$bestTune
  search_grid2 <- data.frame(mtry = best_par$mtry + c(-1, 0, 1),
                             splitrule = "extratrees",
                             min.node.size = best_par$min.node.size,
                             stringsAsFactors = FALSE)
  search_grid2 <- search_grid2[search_grid2$mtry > 0 &
                                 search_grid2$mtry <= ncol(train_data)-1, ]
  cl <- parallel::makePSOCKcluster(no_cores)
  doParallel::registerDoParallel(cl)
  utils::capture.output({model <- caret::train(wt ~ .,
                                        data = train_data,
                                        method = "ranger",
                                        trControl = tc,
                                        tuneGrid = search_grid2,
                                        num.threads = 1,
                                        importance = "impurity")},
                 file = nul_out)
  parallel::stopCluster(cl)
  # save all hyperparameter set performances
  hyperpar_opt_scores2 <- model$results[, c(names(search_grid), "RMSE", "MAE")]
  hyperpar_opt_scores <- rbind(hyperpar_opt_scores,
                               hyperpar_opt_scores2)
  hyperpar_opt_scores[, c("RMSE", "MAE")] <- round(hyperpar_opt_scores[, c("RMSE", "MAE")], 3)
  colnames(hyperpar_opt_scores) <- c(names(search_grid),
                                     "cv_or_validation_RMSE", "cv_or_validation_MAE")
  cat("\nHyperparameter optimization results are saved in",
      paste0("/",catchment, "/", model_short,
             "/", model_name, "/hyperpar_opt_scores.csv\n"))
  utils::write.csv(hyperpar_opt_scores,
            paste0(catchment, "/", model_short,
                   "/", model_name, "/hyperpar_opt_scores.csv"),
            row.names = FALSE)

  cv_or_val_results <- model$results[which.min(model$results$RMSE), c("RMSE", "MAE")]
  cat("Finished hyperparameter optimization, best parameters:\n")
  for(i in 1:ncol(model$bestTune)) cat(names(model$bestTune)[i], ": ",
                                       model$bestTune[1, as.integer(i)], "\n", sep = "")
  # model prediction
  suppressWarnings({train_prediction <- stats::predict(model, train_data)})
  save_prediction_results(catchment, train_prediction, train, na_train,
                          model_short, model_name, "train_data", type)
  if(!is.null(test_data)) {
    suppressWarnings({test_prediction <- stats::predict(model, test_data)})
    save_prediction_results(catchment, test_prediction, test, na_test,
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
  if(ncol(train_data)-1 > 1){
    save_variable_importance(model, model_short, model_name)
  }

}
