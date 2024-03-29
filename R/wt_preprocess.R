#' wt_preprocess
#'
#' Preprocessing data for stream water temperature prediction. All results are stored automatically in the folder catchment in the current working directory.

#'
#' @param data data frame containing the variables: year, month, day, wt and additional covariates.
#' @param nlags Number of lags used for for all features except for time features.
#' @param training_fraction Numeric of fraction of data used for training, e.g. 0.8. Testing data will consist of the (1-training_fraction) fraction of data.
#' @param year_range Vector containing the first and last year to use. Only relevant in case not all available years should be used.
#' @param catchment Character used to create the folder in which the resulting preprocessed training and test data sets get stored.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(test_catchment)
#' wt_preprocess(test_catchment)
#' train_data <- feather::read_feather("test_catchment/train_data.feather")
#' test_data <- feather::read_feather("test_catchment/test_data.feather")
#'}
wt_preprocess <- function(data,
                          nlags = 4,
                          training_fraction = 0.8,
                          year_range = c(min(data$year), max(data$year)),
                          catchment = deparse(substitute(data))
                          ){

  if(training_fraction < 0) {
    stop("training_fraction needs to numeric in the range (0,1)")
  }
  if(training_fraction %in% c(0, 1)) {
    stop(cat("training_fraction needs to numeric in the range (0,1).",
    "Values of 0 or 1 would result in either no training data or no test data."))
  }
  if(data$year[1] > utils::tail(data$year, 1)){
    stop(cat("Data needs to be orded from earliest date onwards.",
             "Therefore, the year of the first row needs to be smaller than the last year!"))
  }
  if(sum(c("year", "month", "day") %in% names(data)) != 3){
    stop(cat('\nTime variables not given in correct format or with correct names.',
             'data should include the columns:\n year (integer), month (integer), day (integer)'))
  }
  if(!("wt" %in% names(data))){
    stop('\nCould not find water temperature column named "wt" in data.')
  }
  cat("*** Preprocessing data of catchment", catchment, "***\n")
  # catchment folder
  if(!file.exists(paste0(catchment))){
    dir.create(file.path(paste0(catchment)))
  }

  # cut year_range
  data <- data[data$year >= year_range[1] & data$year <= year_range[2], ]
  # change names

  # Fuzzy months
  # January
  data$Fmon.1 <- 0
  data$Fmon.1[data$month == 1 & data$day == 15] <- 1
  for(i in 1:30){
    # forwards unti 31
    if(i < 17){
      data$Fmon.1[data$month == 1 & data$day == 15 + i] <- (30-i)/30
    } else {
      data$Fmon.1[data$month == 2 & data$day == -16 + i] <- (30-i)/30
    }
    # backwards until 1
    if(i < 15){
      data$Fmon.1[data$month == 1 & data$day == 15 - i] <- (30-i)/30
    } else {
      data$Fmon.1[data$month == 12 & data$day == 46 - i] <- (30-i)/30
    }
  }
  # December
  data$Fmon.12 <- 0
  data$Fmon.12[data$month == 12 & data$day == 15] <- 1
  for(i in 1:30){
    # forwards unti 31
    if(i < 17){
      data$Fmon.12[data$month == 12 & data$day == 15 + i] <- (30-i)/30
    } else {
      data$Fmon.12[data$month == 1 & data$day == -16 + i] <- (30-i)/30
    }
    # backwards until 1
    if(i < 15){
      data$Fmon.12[data$month == 12 & data$day == 15 - i] <- (30-i)/30
    } else {
      data$Fmon.12[data$month == 11 & data$day == 46 - i] <- (30-i)/30
    }
  }
  # All other months
  for(mon in 2:11){
    data <- cbind(data, 0)
    colnames(data)[ncol(data)] <- paste0("Fmon.", mon)
    data[, ncol(data)][data$month == mon & data$day == 15] <- 1
    for(i in 1:30){
      # forwards unti 31
      if(i < 17){
        data[, ncol(data)][data$month == mon & data$day == 15 + i] <- (30-i)/30
      } else {
        data[, ncol(data)][data$month == mon + 1 & data$day == -16 + i] <- (30-i)/30
      }
      # backwards until 1
      if(i < 15){
        data[, ncol(data)][data$month == mon & data$day == 15 - i] <- (30-i)/30
      } else {
        data[, ncol(data)][data$month == mon - 1 & data$day == 46 - i] <- (30-i)/30
      }
    }
  }
  data$date <- as.POSIXct(paste0(data$year, "-", data$month, "-", data$day))
  data <- data[, !(names(data) %in% c("year", "day", "month"))]
  # order needed variables
  variables <- names(data)[!(names(data) %in% c("date", "wt"))]
  variables <- variables[!grepl("Fmon", variables)]
  for(variable in variables) data <- create_lags(data, variable, nlags)

  # Split in training and validation by training_fraction
  train_length <- floor(nrow(data) * training_fraction)
  train <- data[1:train_length,]
  test <- data[(train_length+1):nrow(data),]
  cat("Preprocessed data sets are stored in folder", catchment, ":\n")
  cat("input_data.feather: full preprocessed data set in feather format\n")
  cat("train_data.feather: first", training_fraction*100,
      "% of the preprocessed data set in feather format\n")
  cat("test_data.feather: last", (1-training_fraction)*100,
      "% of the preprocessed data set in feather format\n\n")
  feather::write_feather(data, paste0(catchment, "/", "input_data.feather"))
  feather::write_feather(train, paste0(catchment, "/", "train_data.feather"))
  feather::write_feather(test, paste0(catchment, "/", "test_data.feather"))
}

