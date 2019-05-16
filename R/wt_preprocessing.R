
#' wt_preprocess
#'
#' @param catchment
#' @param data
#' @param year_range
#'
#' @return
#' @export
#'
#' @examples
wt_preprocess <- function(catchment, data,
                          year_range){
  # open correct folder
  old_wd <- getwd()
  wrong_folder_catcher <- tryCatch({setwd(catchment)},
                                   error = function(e) {
                                     message(paste0("ERROR: There is no folder named ", catchment, " in your current working directory."))
                                     return(NA)
                                   })
  if(is.na(wrong_folder_catcher)) return(NA)

  # remove dates outside the relevant range
  data <- data[data$year > year_range[1] & data$year < year_range[2], ]
  # Q and T differences
  data$time_diff <- c(NA, as.numeric(
    data$date[-1] - data$date[-nrow(data)]) )
  data$time_diff[data$time_diff %in% c(23, 24, 25)] <- 1
  data$time_diff[data$time_diff != 1] <- NA
  data$Qdiff <- NA
  for(i in 1:nrow(data)){
    if(!is.na(data$time_diff)[i]){
      data$Qdiff[i] <- data$Q[i] - data$Q[i-1]
    }
  }
  data$Tmean_diff <- NA
  for(i in 1:nrow(data)){
    if(!is.na(data$time_diff)[i]){
      data$Tmean_diff[i] <- data$Tmean[i] - data$Tmean[i-1]
    }
  }

  # differencese of min and max Temp
  data$Tspread <- data$Tmax - data$Tmin

  # 1-hot encoding
  dummod <- caret::dummyVars(" ~ .", data)
  data_mod <- data.frame(predict(dummod, newdata = data))
  data_mod$date <- data$date
  data_mod$mon <- as.integer(as.character(data$mon))
  data_mod$day <- as.integer(as.character(data$day))
  data <- data_mod

  # Fuzzy months
  # loop over each months
  # January
  data$Fmon.1 <- 0
  data$Fmon.1[data$mon.1 == 1 & data$day == 15] <- 1
  for(i in 1:30){
    # forwards unti 31
    if(i < 17){
      data$Fmon.1[data$mon == 1 & data$day == 15 + i] <- (30-i)/30
    } else {
      data$Fmon.1[data$mon == 2 & data$day == -16 + i] <- (30-i)/30
    }
    # backwards until 1
    if(i < 15){
      data$Fmon.1[data$mon == 1 & data$day == 15 - i] <- (30-i)/30
    } else {
      data$Fmon.1[data$mon == 12 & data$day == 46 - i] <- (30-i)/30
    }
  }

  # December
  data$Fmon.12 <- 0
  data$Fmon.12[data$mon == 12 & data$day == 15] <- 1
  for(i in 1:30){
    # forwards unti 31
    if(i < 17){
      data$Fmon.12[data$mon == 12 & data$day == 15 + i] <- (30-i)/30
    } else {
      data$Fmon.12[data$mon == 1 & data$day == -16 + i] <- (30-i)/30
    }
    # backwards until 1
    if(i < 15){
      data$Fmon.12[data$mon == 12 & data$day == 15 - i] <- (30-i)/30
    } else {
      data$Fmon.12[data$mon == 11 & data$day == 46 - i] <- (30-i)/30
    }
  }

  # All other months
  for(mon in 2:11){
    data <- cbind(data, 0)
    colnames(data)[ncol(data)] <- paste0("Fmon.", mon)
    data[, ncol(data)][data$mon == mon & data$day == 15] <- 1
    for(i in 1:30){
      # forwards unti 31
      if(i < 17){
        data[, ncol(data)][data$mon == mon & data$day == 15 + i] <- (30-i)/30
      } else {
        data[, ncol(data)][data$mon == mon + 1 & data$day == -16 + i] <- (30-i)/30
      }
      # backwards until 1
      if(i < 15){
        data[, ncol(data)][data$mon == mon & data$day == 15 - i] <- (30-i)/30
      } else {
        data[, ncol(data)][data$mon == mon - 1 & data$day == 46 - i] <- (30-i)/30
      }
    }
  }

  # Train/Test/Validation Split
  data <- data[, c("date", "year", "mon", "day",
                   "Q", "RR", "Tmin", "Tmax", "Tmean", "wt",
                   "GL", "time_diff", "Qdiff", "Tmean_diff",
                   "mon.1", "mon.2", "mon.3", "mon.4", "mon.5",
                   "mon.6", "mon.7", "mon.8", "mon.9", "mon.10", "mon.11",
                   "mon.12", "day.1", "day.2", "day.3", "day.4", "day.5",
                   "day.6", "day.7", "day.8", "day.9", "day.10", "day.11",
                   "day.12", "day.13", "day.14", "day.15", "day.16", "day.17",
                   "day.18", "day.19", "day.20", "day.21", "day.22", "day.23",
                   "day.24", "day.25", "day.26", "day.27", "day.28", "day.29",
                   "day.30", "day.31",
                   "Fmon.1", "Fmon.12", "Fmon.2", "Fmon.3", "Fmon.4", "Fmon.5",
                   "Fmon.6", "Fmon.7", "Fmon.8", "Fmon.9", "Fmon.10", "Fmon.11")]
  data$time_diff <- NULL
  # Lag Tmean
  data$Tmean_lag1 <- c(NA, data$Tmean[-nrow(data)])
  data$Tmean_lag2 <- c(NA, data$Tmean_lag1[-nrow(data)])
  data$Tmean_lag3 <- c(NA, data$Tmean_lag2[-nrow(data)])
  data$Tmean_lag4 <- c(NA, data$Tmean_lag3[-nrow(data)])
  # Lag Q
  data$Q_lag1 <- c(NA, data$Q[-nrow(data)])
  data$Q_lag2 <- c(NA, data$Q_lag1[-nrow(data)])
  data$Q_lag3 <- c(NA, data$Q_lag2[-nrow(data)])
  data$Q_lag4 <- c(NA, data$Q_lag3[-nrow(data)])

  # Split: train_year_from to split_year; split_year+1 to last year of the data series
  # Split in 2/3 training and 1/3 validation
  cat("Split data into 2/3 training and 1/3 validation...\n")
  train_length <- floor((year_range[2] - year_range[1]) * 2/3)
  split_year <- year_range[1] + train_length
  train <- data[data$year <= split_year,]
  val <- data[data$year > split_year,]
  feather::write_feather(data, "input_data.feather")
  feather::write_feather(train, "train_data.feather")
  feather::write_feather(val, "val_data.feather")

  if(sum(is.na(data$GL)) > 0 & sum(is.na(data$GL)) != nrow(data)){
    cat("Preparing 2nd dataset with all radiation data...\n")
    radiation_data <- data[!is.na(data$GL), ]
    train_length <- floor((max(radiation_data$year) - min(radiation_data$year)) * 2/3)
    split_year <- min(radiation_data$year) + train_length
    radiation_train <- radiation_data[radiation_data$year <= split_year,]
    radiation_val <- radiation_data[radiation_data$year > split_year,]
    cat("Storing preprocessed radiation data in catchment folder...\n")
    feather::write_feather(radiation_data, "input_radiation_data.feather")
    feather::write_feather(radiation_train, "train_radiation_data.feather")
    feather::write_feather(radiation_val, "val_radiation_data.feather")
  }

  cat("Done!")
  setwd(old_wd)
  ###GL train und Val
}

