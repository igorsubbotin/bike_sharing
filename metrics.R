# Root Mean Squared Error (RMSE) -- https://www.kaggle.com/wiki/RootMeanSquaredError
rmse <- function(prediction, response) {
  rmse <- sqrt((sum((response-prediction)^2))/length(response))
  rmse
}

# Root Mean Squared Logarithmic Error -- https://www.kaggle.com/wiki/RootMeanSquaredLogarithmicError
rmsle <- function(prediction, response) {
  rmsle <- sqrt(sum((log(prediction+1) - log(response+1))^2)/length(response))
  rmsle
}

# Mean Absolute Error (MAE) -- https://www.kaggle.com/wiki/MeanAbsoluteError
mae <- function(prediction, respone) {
  mae
}

# Weighted Mean Absolute Error (WMAE) -- https://www.kaggle.com/wiki/WeightedMeanAbsoluteError
wmae <- function(prediction, response) {
  
}
