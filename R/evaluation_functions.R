# evaluation functions

# Calculating RMSE for all members of the ensemble
RMSE <- function(prediction, observation){
return(sqrt(mean((prediction - observation)^2, na.rm = TRUE)))
}

NSE <- function(prediction, observation){
  round(
    1 - (sum((prediction- observation)^2, na.rm = TRUE) /
           sum( (observation - mean(observation, na.rm = TRUE))^2, na.rm = TRUE ) ),
    3)
}


