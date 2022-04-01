get_residuals <- function(object, ...) {
  UseMethod("get_residuals", object)
}

# Base R ------------------------------------------------------------
get_residuals.lm <- function(object) {
  dplyr::tibble(.resid = as.numeric(residuals(object)))
}

# tidymodels --------------------------------------------------------

## `linear_reg()` engines -------------------------------------------
get_residuals._lm <- function(object) {
  get_residuals.lm(object[["fit"]])
}
