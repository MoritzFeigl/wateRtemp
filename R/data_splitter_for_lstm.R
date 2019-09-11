#' data_splitter_for_lstm
#'
#' @param data Time series data as a data frame with the first column containing a POSIXct data and is named "date".
#'
#' @return
#'
#' @examples
data_splitter_for_lstm <- function(data, data_name){
  # create full time series with no gaps
  full_ts <- data.frame(
    date = as.character(format.POSIXct(
      seq(data$date[1], data$date[nrow(data)], by = "day"),
      format = "%Y-%m-%d"), stringsAsFactors = FALSE))
  data$date <- as.character(format.POSIXct(data$date, format = "%Y-%m-%d"))
  # merge with data
  full_train <- merge(full_ts, data, by = "date", all = TRUE)
  full_train$date <- as.character(full_train$date)
  # split data in sub time series without gaps
  missing_days <- unique(which(is.na(full_train), arr.ind = TRUE)[, 1])
  missing_days <- sort(missing_days)
  # if no split is necessary
  if(length(missing_days) != 0){

    # if only one split is necessary
    if(length(missing_days) == 1){
      cut_points <- missing_days
      data_list <- vector(mode = "list")
      data_list[[1]] <- full_train[1:(cut_points[1] - 1), ]
      data_list[[length(data_list) + 1]] <- full_train[(cut_points[length(cut_points)] + 1):nrow(full_train), ]
    } else {
      data_list <- vector(mode = "list")
      for(i in seq_along(missing_days)){
        if(i == 1){
          data_list[[i]] <- full_train[1:(missing_days[i] - 1), ]
        } else {
          if(missing_days[i] - missing_days[i-1] <= 1) next
          data_list[[i]] <- full_train[(missing_days[i-1]+1):(missing_days[i]-1), ]
        }
      }
      # for the last cut point also add the time from the last cutpoint to the end of the data
      data_list[[length(data_list) + 1]] <- full_train[(missing_days[length(missing_days)] + 1):nrow(full_train), ]
      data_list <- data_list [!sapply(data_list, is.null)]
    }

    # Check if all split where done correctly
    for(i in 2:length(data_list)){
      check_if_date_should_be_there <- data_list[[i]][1,1] %in% full_train$date[missing_days + 1]
      if(!check_if_date_should_be_there){
        stop("ERROR: Data splitting procedure for LSTM data is producing an Error. Check data or source code! Or contact moritz.feigl@boku.ac.at for further help.")
      }
    }

  } else {
    data_list <- vector(mode = "list")
    data_list[[1]] <- data
  }

  # Sanity check
  if(sum(sapply(data_list, function(x) sum(is.na(x)))) != 0){
    stop("ERROR: Data splitting procedure for LSTM data is producing an Error. Check data or source code! Or contact moritz.feigl@boku.ac.at for further help.")
  }
  cat(catchment, data_name, "data was sucessfully splitted!\n")
  cat("number of missing days:", length(missing_days), "\n")
  return(data_list)
}
