# Leverage Points -------------------------------------------------------------

#' Identify Extreme Leverage Points
#'
#' A data point with extreme leverage means that it has an extreme value or
#' values in its predictor (x) values and/or an unusual combination of its
#' predictors values. If this is the case, the data point(s) is/are
#' influential, meaning that it has an outsized influence on a regression.
#'
#' @details
#' Extreme leverage points are defined as those values that are greater than
#' the ratio of the number of coefficients to the number of observations,
#' multiplied by some multiplier. A traditional rule-of-thumb is for the
#' multiplier to be three.
#'
#' @inheritParams bruesch_pagan_test
#' @param id (Optional) A vector of values, the same length as the number of
#'   observations, used as an identifier for each data point. If left as NULL,
#'   the row number will be added as the ID column.
#' @param .multiplier (Optional) Used to determine which leverages are
#'   considered to be "extreme". The default is the rule-of-thumb 3
#'  (see details).
#'
#' @return A [`tibble`][tibble::tibble].
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005).
#'   _Applied Linear Statistical Models_. ISBN: 0-07-238688-6.
#'   McGraw-Hill/Irwin.
#'
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(tidytest)
#'
#' mod_fit <- parsnip::linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ disp + wt + hp, data = mtcars)
#'
#' identify_extreme_leverages(mod_fit)
#' identify_extreme_leverages(mod_fit, id = rownames(mtcars))
#'
#' @export
identify_extreme_leverages <- function(object, id = NULL, .multiplier = 3) {
  leverages_tbl <- get_leverages(object, id)

  p <- get_num_coefs(object)
  n <- get_num_obs(object)

  cutoff <- (p / n) * .multiplier

  leverages_tbl %>%
    dplyr::mutate(.cutoff = cutoff) %>%
    dplyr::filter(leverage > .cutoff)
}



# Outliers -------------------------------------------------------------------

#' Identify Outliers
#'
#' A data point flagged as an outlier means that is has an extreme value in its
#' response (y) variable. If this is the case, the data point(s) is/are
#' influential, meaning that it has an outsized influence on a regression.
#'
#' @details
#' Outliers are defined as those data points that have a standardized
#' residual value greater than some cutoff value. A traditional rule-of-thumb
#' is for that cutoff value to be three.
#'
#' @inheritParams identify_extreme_leverages
#' @param .cutoff (Optional) Used to determine which standard residuals are
#'   indicative of an outlier. The default is the rule-of-thumb 3
#'   (see details).
#'
#' @return A [`tibble`][tibble::tibble].
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005).
#'   _Applied Linear Statistical Models_. ISBN: 0-07-238688-6.
#'   McGraw-Hill/Irwin.
#'
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(tidytest)
#'
#' mod_fit <- parsnip::linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ disp + wt + hp, data = mtcars)
#'
#' #> No outliers with default `.cutoff` value
#' identify_outliers(mod_fit)
#'
#' #> Try a lower `.cutoff` value
#' identify_outliers(mod_fit, .cutoff = 2)
#' identify_outliers(mod_fit, id = rownames(mtcars), .cutoff = 2)
#'
#' @export
identify_outliers <- function(object, id = NULL, .cutoff = 3) {
  std_resids_tbl <- get_standardized_residuals(object, id)

  dplyr::filter(std_resids_tbl, std_resid > .cutoff)
}

# Utilities -------------------------------------------------------------------

#> Number of coefficients in a model
get_num_coefs <- function(object, ...) {
  UseMethod("get_num_coefs")
}

get_num_coefs.lm <- function(object) {
  length(stats::coef(object))
}

get_num_coefs._lm <- function(object) {
  length(stats::coef(object[["fit"]]))
}

#> Number of observations used to build a model
get_num_obs <- function(object, ...) {
  UseMethod("get_num_obs")
}

get_num_obs.lm <- function(object) {
  nrow(object[["model"]])
}

get_num_obs._lm <- function(object) {
  nrow(object[["fit"]][["model"]])
}

#> Add an ID column when converting to a tibble
add_id <- function(x, name, id = NULL) {
  if (rlang::is_null(id)) {
    out <- dplyr::tibble({{ name }} := x) %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::select(id, {{ name }})
  } else {
    out <- dplyr::tibble(id = id, {{ name }} := x)
  }

  out
}

#> Calculate leverage of each data point
get_leverages <- function(object, id = NULL, ...) {
  UseMethod("get_leverages")
}

get_leverages.lm <- function(object, id = NULL) {
  leverages <- as.numeric(stats::influence(object)[["hat"]])

  add_id(leverages, name = "leverage", id = id)
}

get_leverages._lm <- function(object, id = NULL) {
  get_leverages.lm(object[["fit"]], id = id)
}

#> Calculate the standardized residual of each data point
get_standardized_residuals <- function(object, id = NULL, ...) {
  UseMethod("get_standardized_residuals")
}

get_standardized_residuals.lm <- function(object, id = NULL) {
  std_resids <- as.numeric(stats::rstandard(object))

  add_id(std_resids, name = "std_resid", id = id)
}

get_standardized_residuals._lm <- function(object, id = NULL) {
  get_standardized_residuals.lm(object[["fit"]], id = id)
}
