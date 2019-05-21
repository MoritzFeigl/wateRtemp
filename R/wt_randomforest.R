#' wt_RandomForest
#'
#' Random Forest modelling for water temperature data prepared with wt_preprocessing. Can either be used for loading pre-trained models or train a new model. The training is done in parallel and can take a while. Hyperparameter optimization is done by grid search.
#' @param catchment Name of the folder with the data given as a string.
#' @param data_inputs The kind of data to be used for the model run: "simple", "precip", "radiation", "all". See Details
#' @param model_or_optim "model" if a pretrained model should be load, or "optim" if a new model should be trained
#' @param cv_mode The crossvalidation scheme to be used. Can be either: "timeslice", or "repCV"
#' @param no_cores Number of cores used for training as integer.
#' @param plot_ts Should a dygraphy plot of the prediction and observation be plotted? TRUE/FALSE
#' @param save_importance_plot Should the importance plot for the random forest be saved in the folder. TRUE/FALSE
#'
#' @return None, results are saved in RF folder which is created inside the catchment folder.
#' @export
#'
#' @examples
#' wt_preprocess("Ybbs", data = data, year_range = c(1981, 2015))
#' wt_RandomForest(catchment = "Ybbs",
#'                 model_or_optim = "optim",
#'                 cv_mode = "timeslice",
#'                 no.cores = 17,
#'                 plot_ts = FALSE,
#'                 save_importance_plot = TRUE)
#'
wt_randomforest <- function(catchment, data_inputs = NULL, model_or_optim, cv_mode, no_cores = detectCores() - 1,
                            plot_ts = FALSE, save_importance_plot = FALSE){

  old_wd <- getwd()
  wrong_folder_catcher <- tryCatch({setwd(catchment)},
                                   error = function(e) {
                                     message(paste0("ERROR: There is no folder named ", catchment, " in your current working directory."))
                                     return(NA)
                                   })
  if(is.na(wrong_folder_catcher)) return(NA)
  # 1. Data --------------------------------------------------------------------------------
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
  for(cv_mode in cv_mode){
    for(data_inputs in data_inputs){
      model_name <- paste0(data_inputs, "Model_", cv_mode)
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

      ranger_train <- train[, relevant_data] # fuzzy
      ranger_val <- val[, relevant_data] # fuzzy
      # remove NA rows resulting from Qdiff, Tmean_diff
      na_train <- which(is.na(train), arr.ind = TRUE)
      if(nrow(na_train) > 0) train <- train[-unique(na_train[,1]),]
      na_val <- which(is.na(val), arr.ind = TRUE)
      if(nrow(na_val) > 0) val <- val[-na_val[,1],]

      cat(paste0("Create random forest folder for catchment ", catchment, ".\n"))
      if (file.exists("RF")){
        setwd(file.path("RF"))
      } else {
        dir.create(file.path("RF"))
        setwd(file.path("RF"))
      }

      if (!file.exists(model_name)){
        dir.create(file.path(model_name))
      }

      # caret Model --------------------------------------------------------------------------

      # train control
      if(!(cv_mode %in% c("timeslice", "repCV"))) stop("cv_model can be either timeslice or repCV!")
      if(cv_mode == "timeslice"){
        n_seeds <- ceiling((nrow(ranger_train) - 730)/60)
        seeds <- vector(mode = "list", length = n_seeds)
        set.seed(1234)
        for(i in 1:n_seeds) seeds[[i]] <- sample(10000, 60)
        tc <- trainControl(method = "timeslice",
                           initialWindow = 730,
                           horizon = 90,
                           fixedWindow = FALSE,
                           allowParallel = TRUE,
                           verboseIter = TRUE,
                           skip = 60,
                           seeds = seeds)
      }
      if(cv_mode == "repCV"){
        set.seed(1242)
        seeds <- as.list(sample(10000, 51))
        tc <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           allowParallel = TRUE,
                           verboseIter = TRUE,
                           seeds = seeds)
      }

      if(model_or_optim == "optim"){
        cat("Grid search optimization.\n")
        #cat("Calculate initial points for bayesian hyperparameter optimization.\n")
        all_mtries <- 3:(ncol(ranger_train) - 1)
        mtries <- all_mtries[all_mtries%%2 == 1] # take only a subset -> otherwise grid too large
        mns <- c(1, 3, 5, 7, 9)#c(1:10)
        tg <- expand.grid(.mtry = mtries,
                          .splitrule = "extratrees",#c("variance", "extratrees"),
                          .min.node.size = mns)
        cl <- parallel::makeCluster(no_cores)
        #doParallel::registerDoParallel(cores = no_cores)
        doParallel::registerDoParallel(cl)
        suppressWarnings (ranger_fit <- caret::train(wt ~ .,
                                   data = ranger_train,
                                   method = "ranger",
                                   trControl = tc,
                                   tuneGrid = tg,
                                   num.threads = 1,
                                   importance = "impurity"))
        saveRDS(ranger_fit, paste0(model_name, "/optimized_RF_model_", cv_mode, ".rds"))
        best_par <- data.frame(mtry = ranger_fit$bestTune["mtry"],
                               "min.node.size" = ranger_fit$bestTune["min.node.size"],
                               "splitrule" = ranger_fit$bestTune["splitrule"])

        cat("Finished hyperparameter optimization, best parameters:\n")
        for(i in 1:ncol(best_par)) cat(names(best_par)[i], ": ", as.numeric(best_par[1, i]), "\n", sep = "")

      }
      # Load optimized model
      if(model_or_optim == "model"){
        cat("Loading optimized model.\n")
        ranger_fit <- readRDS(paste0(model_name, "/optimized_RF_model_", cv_mode, ".rds"))
      }

      # Prediction and model diagnostics -------------------------------------------------------
      cat("Start prediction and model diagnostics")


      #source("../../../functions/rmse_nse.R")
      model_diagnostic <- rmse_nse(model = ranger_fit, val = ranger_val)
      model_diagnostic <- cbind(model = model_name,
                                model_diagnostic,
                                mtry = ranger_fit$bestTune["mtry"],
                                splitrule = ranger_fit$bestTune["splitrule"],
                                min.node.size = ranger_fit$bestTune["min.node.size"],
                                stringsAsFactors = FALSE)
      cat("\nModel quality criteria: \nRMSE: ", model_diagnostic$RMSE, "\n", " NSE: ", model_diagnostic$NSE, sep = "")

      if(exists("model_scores")){
        model_scores <- read.csv("model_scores.csv")
        write.csv(rbind(model_scores, model_diagnostic, stringsAsFactors = FALSE),
                  "model_scores.csv", row.names = FALSE)
      } else {
        write.csv(model_diagnostic, "model_scores.csv", row.names = FALSE)
      }


      if(plot_ts){
        predict_ranger <- predict(ranger_fit, ranger_val)
        pred_xts_ranger <- xts::xts(cbind(ranger_val, "predictions" = predict_ranger),
                                    order.by = as.POSIXct(paste0(val$year, "-", val$mon, "-", val$day)))
        print(dygraphs::dygraph(pred_xts_ranger[, c("wt", "predictions")], main = "Random forest prediction") %>%
                dygraphs::dyRangeSelector())
      }

      if(save_importance_plot){
        cat("\nSaving importance plot in", getwd(), "\n")
        importance <- varImp(ranger_fit)$importance
        v <- as.numeric(importance[,1])
        w <- rownames(importance)
        DF <- data.frame(w,v, stringsAsFactors = FALSE)
        p <- ggplot(DF, aes(x=reorder(w,v), y=v, fill = v))+
          geom_bar(stat="identity", position="dodge") + coord_flip()+
          ylab("Variable Importance")+
          xlab("")+
          ggtitle("Information Value Summary - Random Forest")+
          guides(fill=F) +
          scale_fill_gradient(low="red", high="blue")
        ggsave(filename = paste0(model_name, "/RF_importance_plot_", cv_mode, ".png"), plot = p, device = "png",
               dpi = "retina")
      }
    }
  }
  setwd(old_wd)

}