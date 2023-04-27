get_residuals <- function(object, ...) {
  UseMethod("get_residuals")
}

# Base R ----------------------------------------------------------------------

## `lm()` & `glm()` ----
get_residuals.lm <- function(object) {
  as.numeric(stats::residuals(object))
}

# Other Packages --------------------------------------------------------------

## `rpart` ----
get_residuals.rpart <- function(object) {
  as.numeric(stats::residuals(object))
}

# tidymodels ------------------------------------------------------------------

## `linear_reg()` engines ----
get_residuals._lm <- function(object) {
  get_residuals.lm(object[["fit"]])
}

get_residuals._glm <- function(object) {
  get_residuals._lm(object)
}

## `decision_tree()` engines ----
get_residuals._rpart <- function(object) {
  get_residuals.rpart(object[["fit"]])
}
