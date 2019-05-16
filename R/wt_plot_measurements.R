#' wt_plot_measurements
#'
#' @param catchment
#'
#' @return
#' @export
#'
#' @examples
wt_plot_measurements <- function(catchment){
  # open correct folder
  old_wd <- getwd()
  setwd(paste0("/media/cfgrammar/data/Dropbox/WT_Project/Modelling/data/", catchment))
  # Load data
  file <- list.files(pattern = ".dat")
  lines <- readLines(file, n = 100)
  skip_ind <- which(lines == "Werte:")
  wt_raw <- read.table(file, skip = skip_ind, stringsAsFactors = FALSE)
  names(wt_raw) <- c("date", "time", "wt")
  wt_raw[wt_raw$wt == "L\xfccke", "wt"] <- NA
  wt_raw$wt <- as.numeric(wt_raw$wt)
  wt_raw$year <- as.integer(substr(wt_raw$date, 7, 10))
  wt_raw$mon <- as.integer(substr(wt_raw$date, 4, 5))
  wt_raw$day <- as.integer(substr(wt_raw$date, 1, 2))

  tab_name <- as.POSIXct(names(table(wt_raw$date)), format = "%d.%m.%Y")
  days <- data.frame(days = tab_name, values = table(wt_raw$date))
  library(ggplot2, quietly = TRUE)
  p <- ggplot(days, aes(days, values.Freq)) + geom_line() +
    ylab("Number of measurements per day") +
    scale_x_datetime(name = "time", date_breaks = "year",
                     date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 90, hjust = -1, vjust = 0.6))
  print(p)
}
