# Predictions and Residuals ---------------------------------------------------

#' Calculate Predictions and Residuals From a Model
#'
#' From a model, calculate the predicted values from a set of data and then
#' calculate the residuals of those predictions.
#'
#' @details
#' ## Known Limitations
#' This function will not work if transformations to variables are done in the
#' model formula (*e.g.*, `lm(mpg ~ wt + as.factor(vs), data = mtcars)`).
#' Variable transformations should be done to the data set before creating the
#' model formula.
#'
#' @template params-linreg-obj
#' @param data (Optional) a data set used to create predictions (if omitted,
#'   the fitted values are used).
#'
#' @return A [tibble][tibble::tibble-package] with columns `.pred` and `.resid`.
#'
#' @templateVar fn calculate_residuals
#' @template examples-linreg-lm
#' @examples
#' calculate_residuals(mod_lm_fit, mtcars[1:15, ])
#'
#' @export
calculate_residuals <- function(object, data = NULL) {
  UseMethod("calculate_residuals")
}

#' @rdname calculate_residuals
#' @export
calculate_residuals.lm <- function(object, data = NULL) {
  if (rlang::is_null(data)) {
    data <- object[["model"]]
  }

  terms_lst <- object[["terms"]]
  vars_chr <- as.character(attr(terms_lst, "variables"))
  idx_num <- as.numeric(attr(terms_lst, "response"))

  yvar_chr <- vars_chr[[idx_num + 1]]

  calculate_residuals_impl(object, data, yvar_chr)
}


# Helpers ---------------------------------------------------------------------
calculate_residuals_impl <- function(object, data, yvar) {
  acts_num <- data[[yvar]]
  preds_num <- as.numeric(stats::predict(object, data))
  resids_num <- acts_num - preds_num

  dplyr::tibble(
    .pred = preds_num,
    .resid = resids_num
  )
}


# Utils -----------------------------------------------------------------------

#> Calculate residuals of each data point
calc_residuals <- function(object) {
  as.numeric(stats::residuals(object))
}

#> Calculate standardized residuals of each data point
calc_standardized_residuals <- function(object) {
  as.numeric(stats::rstandard(object))
}
