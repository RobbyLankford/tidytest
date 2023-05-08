get_predictions <- function(object, data, ...) {
  UseMethod("get_predictions", object)
}

# Base R ----------------------------------------------------------------------

## `lm()` ----
get_predictions.lm <- function(object, data = NULL) {
  if (rlang::is_null(data)) {
    data <- object[["model"]]
  }

  as.numeric(stats::predict(object, data))
}

## `glm()` ----
get_predictions.lm <- function(object, data) {
  as.numeric(stats::predict(object, data))
}

# tidymodels ------------------------------------------------------------------

## `linear_reg()` engines ----
get_predictions._lm <- function(object, data) {
  get_predictions.lm(object[["fit"]], data)
}

get_predictions._glm <- function(object, data) {
  get_predictions._lm(object, data)
}
