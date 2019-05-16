#' rmse_nse
#'
#' @param model trained model
#' @param validation validation data
#'
#' @return data frame with RMSE and NSE
#' @export
#'
#' @examples
rmse_nse <- function(model, validationidation){
  m_predict <- predict(model, validation)
  m_res <- validation$wt - m_predict
  RMSE_m <- sqrt(mean(m_res^2, na.rm = TRUE))
  NSE <- 1 - (sum((m_predict- validation$wt)^2, na.rm = TRUE) / sum( (validation$wt - mean(validation$wt, na.rm = TRUE))^2, na.rm = TRUE ) )
  df <- data.frame("RMSE" = RMSE_m, "NSE" = NSE)
  rownames(df) <- model$method
  return(df)
}
