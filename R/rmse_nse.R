#' rmse_nse
#'
#' @param model trained model
#' @param val validation data
#'
#' @return data frame with RMSE and NSE
#'
#' @examples
rmse_nse <- function(model, val){
  m_predict <- predict(model, val)
  m_res <- val$wt - m_predict
  RMSE_m <- sqrt(mean(m_res^2, na.rm = TRUE))
  NSE <- 1 - (sum((m_predict- val$wt)^2, na.rm = TRUE) / sum( (val$wt - mean(val$wt, na.rm = TRUE))^2, na.rm = TRUE ) )
  df <- data.frame("RMSE" = RMSE_m, "NSE" = NSE)
  rownames(df) <- model$method
  return(df)
}
