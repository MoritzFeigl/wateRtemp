#' wt_lm
#'
#' @param train_data
#' @param test_data
#' @param catchment
#' @param type
#' @param cv_mode
#' @param model_name
#' @param no_cores
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
wt_lm <- function(train_data,
                  test_data = NULL,
                  catchment = NULL,
                  type = "stepLM",
                  cv_mode = "repCV",
                  model_name = NULL,
                  no_cores = parallel::detectCores() - 1,
                  seed = NULL){

  # checks
  model_short <- "LM"
  general_checks(catchment, train_data, test_data, model_name, type, cv_mode, model_short)
  # create folders
  folder_structure(catchment, model_short, model_name, type)
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

  # Multiple linear regression model
  if(type == "LM"){
    cat("Applying multiple linear regression\n")
    # Train val split
    cv_mode <- "None"
    model <- lm(wt ~ ., train_data)
    cv_or_val_results <- data.frame("RMSE" = NA, "MAE" = NA)

  }

  # Step-wise multiple linear regression model
  if(type == "stepLM"){
    cat("Applying step-wise linear regression with", cv_mode, "\n")

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
    # parallel step LM model with interaction between non-time variables
    interact_variables <- names(train_data)[!grepl("Fmon", names(train_data)) &
                                              names(train_data) != "wt" &
                                              names(train_data) != "date"]
    non_interact_valiables <- names(train_data)[!(names(train_data) %in% interact_variables)]
    non_interact_valiables <- non_interact_valiables[non_interact_valiables != "wt" &
                                                    non_interact_valiables != "date"]
    step_formular <- formula(paste0("wt ~ ",
                                    paste0(non_interact_valiables, collapse = "+"), "+(",
                                    paste0(interact_variables, collapse = "+"), ")^2"))
    cl <- parallel::makePSOCKcluster(no_cores)
    doParallel::registerDoParallel(cl)
    model <- caret::train(step_formular, data = train_data,
                          method = "glmStepAIC",
                          trControl = tc,
                          trace = FALSE)
    parallel::stopCluster(cl)
    cv_or_val_results <- model$results[, c("RMSE", "MAE")]
  }

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
<<<<<<< HEAD
                   model_short = model_short,
                   model = model)
=======
<<<<<<< HEAD
                   model_short = model_short,
                   model = model)
=======
                   model_short = model_short)
>>>>>>> 665bd01ae0a173f48e7914981360a5a4bd0f02ae
>>>>>>> 1e4add08b8a88605b16127b53087b7aab0597d42
}
