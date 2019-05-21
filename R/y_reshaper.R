#' y_reshaper
#'
#' Function for reshaping data for LSTM applications. x_reshaper reshapes the label data to a 3D array of features with (number observations, timesteps, number of features). y_reshaper reshapes the the data to a 1D array with labels corresponding to the 3D feature array.
#' @param y matrix with labels for the LSTM
#' @param n_timesteps number of timesteps used for prediction
#' @param n_predictions number of timesteps to be predicted by LSTM
#'
#' @return returns an 1D array with (number observations)
#'
#' @examples
y_reshaper <- function(y, n_timesteps, n_predictions){
  if(n_predictions == 1){
    timestep_length <- dim(y)[1] - n_timesteps
    y_arr <- numeric(timestep_length)
    for(i in 1:timestep_length){
      y_arr[i] <- y[i+n_timesteps]
    }
    y_arr <- array(y_arr, dim = c(timestep_length, n_predictions))
  }
  # multiple step prediction
  if(n_predictions != 1){
    timestep_length <- dim(y)[1] - n_timesteps - n_predictions + 1
    y_arr <- matrix(NA, nrow = timestep_length, ncol = n_predictions)
    for(i in 1:timestep_length){
      y_arr[i,] <- y[(i+n_timesteps):(i+n_timesteps+n_predictions-1)]
    }
  }
  return(y_arr)
}
