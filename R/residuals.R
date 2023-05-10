# Predictions -----------------------------------------------------------------
calc_predictions <- function(object, data) {
  as.numeric(stats::predict(object, data))
}

# Residuals -------------------------------------------------------------------

#> Used to calculate residuals from either fitted values or new values
calc_residuals <- function(object, data, yvar) {
  preds_num <- calc_predictions(object, data)
  acts_num <- data[[yvar]]

  acts_num - preds_num
}

#> Used to extract fitted value residuals from the model object itself
get_residuals <- function(object) {
  as.numeric(stats::residuals(object))
}
