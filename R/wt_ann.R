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
                   ensemble_runs = 100){

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
      model_name <- paste0(data_inputs, "Model_", epochs, "epochs_", bs, "bs_", ensemble_runs, "ensembleRuns")
      # check if there is seperate radiation data
      rad_data <- length(list.files(path = catchment, pattern = "radiation_")) > 0

      # in case of radiation or all data_input, load radiation data
      if(data_inputs == "radiation" & rad_data | data_inputs == "all" & rad_data){
        data_prefix <- "radiation_"
      } else {
        data_prefix <- ""
      }

      #data <- read_feather(paste0("input_", data_prefix, "data.feather"))
      train <- read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
      val <- read_feather(paste0(catchment, "/val_", data_prefix, "data.feather"))

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

      if(!file.exists(paste0(catchment, "/ANN"))){
        dir.create(file.path(paste0(catchment, "/ANN")))
      }

      if(!file.exists(paste0(catchment, "/ANN/", model_name))){
        dir.create(file.path(paste0(catchment, "/ANN/", model_name)))
      }

      if(!file.exists(paste0(catchment, "/ANN/", model_name, "/checkpoints"))){
        dir.create(file.path(paste0(catchment, "/ANN/", model_name, "/checkpoints")))
      }
      if(!file.exists(paste0(catchment, "/ANN/", model_name, "/training_metrics"))){
        dir.create(file.path(paste0(catchment, "/ANN/", model_name, "/training_metrics")))
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
                             x_val = x_val, y_val = y_val, data_inputs = data_inputs,
                             train_mean = train_mean, train_sd = train_sd,
                             n_features = n_features, model_name = model_name))













    }
  }
}
