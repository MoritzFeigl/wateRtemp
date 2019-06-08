#' wt_lm
#'
#' @param catchment Name of the folder containing the data. Given as a string
#' @param data_inputs
#' @param type
#'
#' @return
#' @export
#'
#' @examples
wt_lm <- function(catchment, data_inputs = NULL, type = NULL, user_name = "R2D2"){

  if(user_name == "R2D2") cat('No user_name was chosen! Default user "R2D2" is running the model.\n')

  if(sum(list.files() %in% catchment) < 1){
    stop(paste0("ERROR: Cannot find catchment folder(s) in your current working directory."))
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

  data_inputs_meta <- data_inputs
  type_meta <- type
  for(catchment in catchment){
    cat("*** Starting computation for catchment", catchment, "***\n")
    # Catch wrong catchment name
    if(sum(list.files() %in% catchment) != 1){
      message(paste0("ERROR: There is no folder named ", catchment, " in your current working directory."))
      next
    }
    for(type in type_meta){

      # only calculate all data inputs if type is "step"
      if(type == "step"){
        data_inputs_internal <- data_inputs_meta
      } else {
        cat('Type "lm" (simple linear model) will only be calculated using discharge and mean daily water temperature as predictors! For more predictor variables, choose the option type = "step"')
        data_inputs_internal <- "simple"
      }

      for(data_inputs in data_inputs_internal){
        start_time <- Sys.time()
        if(type == "lm"){
          model_name <- "lm_model"
        } else {
          model_name <- paste0(data_inputs, "Model_", type)
        }
        # check if there is seperate radiation data
        rad_data <- length(list.files(path = catchment, pattern = "radiation_")) > 0
        # in case of radiation or all data_input, load radiation data
        if(data_inputs == "radiation" & rad_data | data_inputs == "all" & rad_data){
          data_prefix <- "radiation_"
        } else {
          data_prefix <- ""
        }
        train <- feather::read_feather(paste0(catchment, "/train_", data_prefix, "data.feather"))
        test <- feather::read_feather(paste0(catchment, "/test_", data_prefix, "data.feather"))
        part_training <- nrow(train)/4 * 3
        train_length <- floor(part_training)
        val <- train[(train_length + 1):nrow(train), ]
        train <- train[1:train_length, ]

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

        train <- train[, relevant_data]
        val <- val[, relevant_data]
        test <- test[, relevant_data]
        # remove NA rows resulting from Qdiff, Tmean_diff
        na_train <- which(is.na(train), arr.ind = TRUE)
        if(nrow(na_train) > 0) train <- train[-unique(na_train[,1]),]
        na_val <- which(is.na(val), arr.ind = TRUE)
        if(nrow(na_val) > 0) val <- val[-na_val[,1],]
        na_test <- which(is.na(test), arr.ind = TRUE)
        if(nrow(na_test) > 0) test <- test[-na_test[,1],]

        if (!file.exists(paste0(catchment, "/LM"))){
          cat(paste0("Create LM folder for catchment ", catchment, ".\n"))
          dir.create(file.path(paste0(catchment, "/LM")))
        }

        if (!file.exists(paste0(catchment, "/LM/", model_name))){
          dir.create(paste0(catchment, "/LM/", model_name))
        }

        if(type == "lm"){
          lm_model <- lm(wt ~ Tmean + Q, train)
          model_diagnostic_val <- rmse_nse(lm_model, val)
          names(model_diagnostic_val) <- paste0(names(model_diagnostic_val), "_val")
          model_diagnostic_test <- rmse_nse(lm_model, test)
          run_time <-  paste0(
            round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
            " minutes")
          # save predicted values
          predict_lm <- predict(lm_model, test)
          feather::write_feather(data.frame("predicted_values" = predict_lm),
                                 paste0(catchment, "/LM/", model_name, "/predicted_values.feather"))
          # scores
          model_diagnostic <- cbind(user_name = user_name,
                                    model = model_name,
                                    start_time = as.character(start_time),
                                    run_time = run_time,
                                    model_diagnostic_val,
                                    model_diagnostic_test,
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
          model_diagnostic_val <- rmse_nse(lm_model, val)
          names(model_diagnostic_val) <- paste0(names(model_diagnostic_val), "_val")
          model_diagnostic_test <- rmse_nse(lm_model, test)
          run_time <-  paste0(
            round((as.numeric(Sys.time()) - as.numeric(start_time))/60, 2),
            " minutes")
          # save predicted values
          predict_lm <- predict(step_lm_model, test)
          feather::write_feather(data.frame("predicted_values" = predict_lm),
                                 paste0(catchment, "/LM/", model_name, "/predicted_values.feather"))
          # scores

          model_diagnostic <- cbind(user_name = user_name,
                                    model = model_name,
                                    start_time = as.character(start_time),
                                    run_time = run_time,
                                    model_diagnostic_val,
                                    model_diagnostic_test,
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
}
