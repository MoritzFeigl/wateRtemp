#' data_splitter_for_lstm
#'
#' @param data Time series data as a data frame with the first column containing a POSIXct data and is named "date".
#'
#' @return
#'
#' @examples
data_splitter_for_lstm <- function(data){
  # create full time series with no gaps
  full_ts <- data.frame(
    date = as.character(format.POSIXct(
      seq(data$date[1], data$date[nrow(data)], by = "day"),
      format = "%Y-%m-%d"), stringsAsFactors = FALSE))
  data$date <- as.character(data$date)
  # merge with data
  full_train <- merge(full_ts, data, by = "date", all = TRUE)
  # split data in sub time series without gaps
  missing_days <- unique(which(is.na(full_train), arr.ind = TRUE)[, 1])
  # if no split is necessary
  if(length(missing_days) != 0){

    # if only one split is necessary
    if(length(missing_days) == 1){
      cut_points <- missing_days
      data_list <- vector(mode = "list")
      data_list[[1]] <- data[1:(cut_points[1] - 1), ]
    } else {

      # remove consecutive missing days
      rm_ind <- integer()
      for(i in 2:(length(missing_days)-1)){
        if(missing_days[i] - missing_days[i-1] <= 20 &
           missing_days[i+1] - missing_days[i] <= 20) rm_ind <- c(rm_ind, i)
      }
      cut_points <- missing_days[-rm_ind]
      data_list <- vector(mode = "list")
      for(i in seq_along(cut_points)){
        if(i == 1){
          data_list[[i]] <- data[1:(cut_points[i] - 1), ]
        } else {
          data_list[[i]] <- data[(cut_points[i-1]+1):(cut_points[i]-1), ]
        }
      }
    }
  } else {
    data_list <- vector(mode = "list")
    data_list[[1]] <- data
  }
  return(data_list)
}
