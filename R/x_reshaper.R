#' x_reshaper
#'
#' Function for reshaping data for LSTM applications. x_reshaper reshapes the feature data to a 3D array of features with (number observations, timesteps, number of features). y_reshaper reshapes the the data to a 1D array with labels corresponding to the 3D feature array.
#' @param x matrix with features for the LSTM
#' @param n_timesteps number of timesteps used for prediction
#' @param n_predictions number of timesteps to be predicted by LSTM
#'
#' @return returns an 3D array with (number observations, timesteps, number of features)
#'
#' @examples
x_reshaper <- function(x, n_timesteps, n_predictions){
  # train data: 3D array with dimesions(sample, n_timesteps, features)
  #             therefore the n_timesteps of observations before our prediction point
  # val data: 2D array with dimensions (sample, 1) -> 1 because we only predict 1 day
  if(is.null(nrow(x))) x <- as.matrix(x, ncol = 1)
  x_list <- vector(mode = "list", length = (nrow(x) - n_timesteps - n_predictions + 1))
  for(i in 1:(nrow(x) - n_timesteps - n_predictions + 1)){
    x_list[[i]] <- t(x[i:(i + (n_timesteps - 1)), ])
  }
  x_arr <- simplify2array(x_list)
  x_arr <- aperm(x_arr)
  return(x_arr)
}
