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
  old_wd <- getwd()
  wrong_folder_catcher <- tryCatch({setwd(catchment)},
                                   error = function(e) {
                                     message(paste0("ERROR: There is no folder named ", catchment, " in your current working directory."))
                                     return()
                                   })
  if(is.na(wrong_folder_catcher)) return(NA)

  if(is.null(data_inputs)){
    warning('\nChoose a valid data_input:
              "simple"    = Q, Tmean and water temperatur observations
              "precip"    = "simple" + additional precipitation observations
              "radiation" = "simple" + additional longwave radiation observations
              "all"       = all the above mentioned observations')
  }
  if(sum(data_inputs %in% c("simple", "precip", "radiation", "all")) == 0){
    warning('\nChoose a valid data_input:
              "simple"    = Q, Tmean and water temperatur observations
              "precip"    = "simple" + additional precipitation observations
              "radiation" = "simple" + additional longwave radiation observations
              "all"       = all the above mentioned observations')
  }
  if(sum(model_or_optim %in% c("model", "optim")) == 0){
    warning('\nChoose a valid model_or_optim option:
              "model"    = pretrained model created with wt_randomforest will be loaded
              "optim"    = a new model with hyperparameter optimization will be trained')
  }
  if(sum(type %in% c("lm", "step")) == 0){
    warning('\nChoose a valid type option:
"lm"    = simple multiple linear model
"step"  = linear model with stepwise model selection')
  }
  if(is.null(type)){
    warning('\nChoose a valid type option:
              "lm"    = simple multiple linear model
              "step"  = linear model with stepwise model selection using the AIC criterium')
  }

  for(data_inputs in data_inputs){
    for(typ in type){
      model_name <- paste0(data_inputs, "Model_", type)
      cat("Loading catchment data.\n")
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

      train <- train[, relevant_data] # fuzzy
      val <- val[, relevant_data] # fuzzy
      # remove NA rows resulting from Qdiff, Tmean_diff
      na_train <- which(is.na(train), arr.ind = TRUE)
      if(nrow(na_train) > 0) train <- train[-unique(na_train[,1]),]
      na_val <- which(is.na(val), arr.ind = TRUE)
      if(nrow(na_val) > 0) val <- val[-na_val[,1],]

      cat(paste0("Create LM folder for catchment ", catchment, ".\n"))
      if (file.exists("LM")){
        setwd(file.path("LM"))
      } else {
        dir.create(file.path("LM"))
        setwd(file.path("LM"))
      }

      if (!file.exists(model_name)){
        dir.create(file.path(model_name))
      }

      if(type == "lm"){
        lm_model <- lm(wt ~ Tmean + Q, train)
        model_diagnostic <- rmse_nse(lm_model, val)
        model_diagnostic <- cbind(model = "lm_model",
                                  model_diagnostic,
                                  stringsAsFactors = FALSE)
        saveRDS(lm_model, paste0(model_name, "/lm_model.rds"))
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
        model_diagnostic <- cbind(model = model_name,
                                  model_diagnostic,
                                  stringsAsFactors = FALSE)
        saveRDS(step_lm_model, paste0(model_name, "/step_lm_model.rds"))
      }

      # save model scores
      if(exists("model_scores")){
        model_scores <- read.csv("model_scores.csv")
        write.csv(rbind(model_scores, model_diagnostic, stringsAsFactors = FALSE),
                  "model_scores.csv", row.names = FALSE)
      } else {
        write.csv(model_diagnostic, "model_scores.csv", row.names = FALSE)
      }
    }
  }

  setwd(old_wd)
}
