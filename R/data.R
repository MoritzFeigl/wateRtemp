#' Stream water temperature, discharge and meteorological data of the wateRtemp test catchment.
#'
#' A synthetic dataset for mean daily stream water temperature prediction. This data sets is made for testing all wateRtemp functionalities and has characteristics mimicking real world data.
#'
#' @docType data
#'
#' @usage data(test_catchment)
#'
#' @keywords datasets
#'
#' @format A data frame with 6851 rows and 10 variables:
#' \describe{
#' \item{year}{Years as integer.}
#' \item{month}{Months as integer.}
#' \item{day}{Days as integer.}
#' \item{Q}{Daily mean discharge in m³/s at the gauging station of the test catchment.}
#' \item{P}{Daily precipitation sums in mm, averaged over the whole catchment.}
#' \item{Ta_min}{Daily minimum air temperature °C, averaged over the whole catchment.}
#' \item{Ta_max}{Daily maximum air temperature °C, averaged over the whole catchment.}
#' \item{Ta}{Daily mean air temperature °C, averaged over the whole catchment.}
#' \item{wt}{Daily mean stream water temperature in °C at the gauging station of the test catchment.}
#' \item{GL}{Daily mean global radiation in W/m², averaged over the whole catchment.}
#' }
#' @references Feigl, M., Lebidzinski, K., Herrnegger, M. and Schulz, K.: Machine learning methods for stream water temperature prediction
#'
#' @source Feigl, M., Lebidzinski, K., Herrnegger, M. and Schulz, K.: Machine learning methods for stream water temperature prediction
#'
#' @examples
#' data(test_catchment)
#' summary(test_catchment)
"test_catchment"
