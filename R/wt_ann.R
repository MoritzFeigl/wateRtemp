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
#' @details if ensemble_runs >= 100 only the best 10% are used for the prediction, if ensemble runs < 100, only the best 10 are used. In case there are less than 10 ensemble runs, all are used for prediction.
#'
#' @examples
wt_ann <- function(catchment,
                   data_inputs = NULL,
                   model_or_optim,
                   bs = c(10, 50, 100),
                   epochs = c(100),
                   ensemble_runs = 100,
                   user_name = "R2D2"){

  if(user_name == "R2D2") cat('No user_name was chosen! Default user "R2D2" is running the model.\n')

  if(is.null(data_inputs)){
    stop('\nChoose a valid data_input:
            "simple"    = Q, Tmean and water temperatur observations
            "precip"    = "simple" + additional precipitation observations
            "radiation" = "simple" + additional longwave radiation observations
            "all"       = all the above mentioned observations')
  }

  if(sum(list.files() %in% catchment) < 1){
    stop(paste0("ERROR: Cannot find catchment folder(s) in your current working directory."))
  }

  data_inputs_meta <- data_inputs
  for(catchment in catchment){
    cat("*** Starting computation for catchment", catchment, "***\n")
    if(sum(list.files() %in% catchment) != 1){
      message(paste0("ERROR: There is no folder named ", catchment, " in your current working directory."))
      next
    }
    for(data_inputs in data_inputs_meta){
      # check if there is seperate radiation data
      rad_data <- length(list.files(path = catchment, pattern = "radiation_")) > 0
      # in case of radiation or all data_input, load radiation data
      if(data_inputs == "radiation" & rad_data | data_inputs == "all" & rad_data){
        data_prefix <- "radiation_"
      } else {
        data_prefix <- ""
      }

      train <- read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
      test <- read_feather(paste0(catchment, "/test_", data_prefix, "data.feather"))
      part_training <- nrow(train)/4 * 3
      train_length <- floor(part_training)
      val <- train[(train_length + 1):nrow(train), ]
      train <- train[1:train_length, ]

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
      ann_test <- test[, relevant_data]
      # Scaling
      train_mean <- mean(ann_train$wt)
      train_sd <- sd(ann_train$wt)
      ann_train[, relevant_data] <- scale(ann_train[, relevant_data])
      ann_val[, relevant_data[-3]] <- scale(ann_val[, relevant_data[-3]])
      ann_val$wt <- (ann_val$wt - train_mean)/train_sd
      ann_test[, relevant_data[-3]] <- scale(ann_test[, relevant_data[-3]])
      ann_test$wt <- (ann_test$wt - train_mean)/train_sd

      # As matrix
      ann_train <- as.matrix(ann_train)
      ann_val <- as.matrix(ann_val)
      ann_test <- as.matrix(ann_test)
      # x: Q and Tmean, y: wt
      x_train <- ann_train[, relevant_data[-3]]
      y_train <- ann_train[, c("wt")]
      x_val <- ann_val[, relevant_data[-3]]
      y_val <- ann_val[, c("wt")]
      x_test <- ann_test[, relevant_data[-3]]
      y_test <- ann_test[, c("wt")]

      if(!file.exists(paste0(catchment, "/ANN"))){
        dir.create(file.path(paste0(catchment, "/ANN")))
      }



      # Define grid and apply function
      grid <- expand.grid("bs" = bs,
                          "epochs" = epochs,
                          "ensemble_runs" = ensemble_runs)

      mapply(ann_metaf,
             bs = grid$bs,
             epochs = grid$epochs,
             ensemble_runs = grid$ensemble_runs,
             MoreArgs = list(catchment = catchment,
                             x_train = x_train, y_train = y_train,
                             x_val = x_val, y_val = y_val,
                             x_test = x_test, y_test = y_test,
                             data_inputs = data_inputs,
                             train_mean = train_mean, train_sd = train_sd,
                             n_features = n_features, user_name = user_name))
    }
  }
}
