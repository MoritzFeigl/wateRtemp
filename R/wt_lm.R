#' wt_lm
#'
#' @param catchment Name of the folder containing the data. Given as a string
#' @param data_inputs
#' @param type
#' @param model_or_optim
#'
#' @return
#' @export
#'
#' @examples
wt_lm <- function(catchment, data_inputs = NULL, type = NULL, model_or_optim){

  if(sum(list.files() %in% catchment) != 1){
    message(paste0("ERROR: There is no folder named ", catchment, " in your current working directory."))
    return()
  }

  if(is.null(data_inputs)){
    stop('\nChoose a valid data_input:
              "simple"    = Q, Tmean and water temperatur observations
              "precip"    = "simple" + additional precipitation observations
              "radiation" = "simple" + additional longwave radiation observations
              "all"       = all the above mentioned observations')
  }
  if(sum(data_inputs %in% c("simple", "precip", "radiation", "all")) == 0){
    stop('\nChoose a valid data_input:
              "simple"    = Q, Tmean and water temperatur observations
              "precip"    = "simple" + additional precipitation observations
              "radiation" = "simple" + additional longwave radiation observations
              "all"       = all the above mentioned observations')
  }
  if(sum(model_or_optim %in% c("model", "optim")) == 0){
    stop('\nChoose a valid model_or_optim option:
              "model"    = pretrained model created with wt_randomforest will be loaded
              "optim"    = a new model with hyperparameter optimization will be trained')
  }
  if(sum(type %in% c("lm", "step")) == 0){
    stop('\nChoose a valid type option:
"lm"    = simple multiple linear model
"step"  = linear model with stepwise model selection')
  }
  if(is.null(type)){
    stop('\nChoose a valid type option:
              "lm"    = simple multiple linear model
              "step"  = linear model with stepwise model selection using the AIC criterium')
  }

  for(data_inputs in data_inputs){
    for(typ in type){
      start_time <- Sys.time()
      if(type == "lm"){
        model_name <- "lm_model"
      } else {
        model_name <- paste0(data_inputs, "Model_", type)
      }
      cat("Loading catchment data.\n")
      # check if there is seperate radiation data
      rad_data <- length(list.files(path = catchment, pattern = "radiation_")) > 0
      # in case of radiation or all data_input, load radiation data
      if(data_inputs == "radiation" | data_inputs == "all" & rad_data){
        data_prefix <- "radiation_"
      } else {model_scores
        data_prefix <- ""
      }
      #data <- read_feather(paste0("input_", data_prefix, "data.feather"))
      train <- read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
      val <- read_feather(paste0(catchment, "/val_", data_prefix, "data.feather"))

      if(type == "step"){
      if(data_inputs == "simple"){
        relevant_data <- c("year", "Q", "Tmean", "wt", "Qdiff", "Tmean_diff",
                           "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5", "Fmon.6",
                           "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11",
                           "Tmean_lag1", "Tmean_lag2", "Tmean_lag3", "Tmean_lag4",
                           "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
      }
      if(data_inputs == "precip"){
        relevant_data <- c("year", "Q", "RR", "Tmean", "wt", "Qdiff", "Tmean_diff",
                           "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5", "Fmon.6",
                           "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11",
                           "Tmean_lag1", "Tmean_lag2", "Tmean_lag3", "Tmean_lag4",
                           "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
      }
      if(data_inputs == "radiation"){
        relevant_data <- c("year", "Q", "GL", "Tmean", "wt", "Qdiff", "Tmean_diff",
                           "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5", "Fmon.6",
                           "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11",
                           "Tmean_lag1", "Tmean_lag2", "Tmean_lag3", "Tmean_lag4",
                           "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
      }
      if(data_inputs == "all"){
        relevant_data <- c("year", "Q", "RR", "GL", "Tmean", "wt", "Qdiff", "Tmean_diff",
                           "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5", "Fmon.6",
                           "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11",
                           "Tmean_lag1", "Tmean_lag2", "Tmean_lag3", "Tmean_lag4",
                           "Q_lag1", "Q_lag2", "Q_lag3", "Q_lag4")
      }
      } else {
        relevant_data <- c("Q", "Tmean", "wt")
      }

      train <- train[, relevant_data] # fuzzy
      val <- val[, relevant_data] # fuzzy
      # remove NA rows resulting from Qdiff, Tmean_diff
      na_train <- which(is.na(train), arr.ind = TRUE)
      if(nrow(na_train) > 0) train <- train[-unique(na_train[,1]),]
      na_val <- which(is.na(val), arr.ind = TRUE)
      if(nrow(na_val) > 0) val <- val[-na_val[,1],]

      cat(paste0("Create LM folder for catchment ", catchment, ".\n"))
      if (!file.exists(paste0(catchment, "/LM"))){
        dir.create(file.path(paste0(catchment, "/LM")))
      }

      if (!file.exists(paste0(catchment, "/LM/", model_name))){
        dir.create(paste0(catchment, "/LM/", model_name))
      }

      if(type == "lm"){
        lm_model <- lm(wt ~ Tmean + Q, train)
        model_diagnostic <- rmse_nse(lm_model, val)
        run_time <-  paste0(
          round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
          " minutes")
        model_diagnostic <- cbind(model = model_name,
                                  start_time = as.character(start_time),
                                  run_time = run_time,
                                  model_diagnostic,
                                  stringsAsFactors = FALSE)
        saveRDS(lm_model, paste0(catchment, "/LM/", model_name, "/lm_model.rds"))
      }

      if(type == "step"){
        # Fuzzy stepwise with interactions LM
        if(data_inputs == "simple"){
          step_formular <- formula("wt ~ . +
                                  Q*Tmean + Q*Tmean_diff + Tmean_lag1*Q_lag1
                                  + Tmean_lag2*Q_lag2 +
                                  Tmean_lag1*Q + Qdiff*Tmean_diff")
        }
        if(data_inputs == "precip"){
          step_formular <- formula("wt ~ . +
                                  Q*Tmean + RR*Tmean + RR*Tmean_lag1 + Q*RR +
                                  Q*Tmean_diff + RR*Tmean_diff +
                                  Tmean_lag1*Q + Qdiff*Tmean_diff")
        }
        if(data_inputs == "radiation"){
          step_formular <- formula("wt ~ . +
                                  Q*Tmean + GL*Tmean + GL*Tmean_lag1 + Q*GL +
                                  Q*Tmean_diff + GL*Tmean_diff +
                                  Tmean_lag1*Q + Qdiff*Tmean_diff")
        }
        if(data_inputs == "all"){
          step_formular <- formula("wt ~ . +
                                  Q*Tmean + Q*Tmean_diff + Q*RR + Q*Tmean_lag1 +
                                  RR*Tmean + RR*Tmean_lag1 + RR*Tmean_diff +
                                  GL*Tmean + GL*Tmean_lag1 + GL*Tmean_diff +
                                  Qdiff*Tmean_diff")
        }

        # Stepwise regression model
        step_lm_model <- lm(step_formular, train)
        step_lm_model <- MASS::stepAIC(step_lm_model, direction = "both",
                                       trace = FALSE)
        model_diagnostic <- rmse_nse(lm_model, val)
        run_time <-  paste0(
          round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
          " minutes")
        model_diagnostic <- cbind(model = model_name,
                                  start_time = as.character(start_time),
                                  run_time = run_time,
                                  model_diagnostic,
                                  stringsAsFactors = FALSE)
        saveRDS(step_lm_model, paste0(catchment, "/LM/", model_name, "/step_lm_model.rds"))
      }

      # save model scores
      if("model_scores.csv" %in% list.files(paste0(catchment, "/LM"))){
        model_scores <- read.csv(paste0(catchment, "/LM/model_scores.csv"), stringsAsFactors = FALSE)
        write.csv(rbind(model_scores, model_diagnostic, stringsAsFactors = FALSE),
                  paste0(catchment, "/LM/model_scores.csv"), row.names = FALSE)
      } else {
        write.csv(model_diagnostic, paste0(catchment, "/LM/model_scores.csv"), row.names = FALSE)
      }
    }
  }
}
