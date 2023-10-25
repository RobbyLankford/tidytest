# Leverage --------------------------------------------------------------------

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
#' @template params-linreg-obj
#' @param id (Optional) A vector of values, the same length as the number of
#'   observations, used as an identifier for each data point. If left as NULL,
#'   the row number will be added as the ID column.
#' @param .multiplier (Optional) Used to determine which leverages are
#'   considered to be "extreme". The default is the rule-of-thumb 3
#'  (see details).
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005).
#'   _Applied Linear Statistical Models_. ISBN: 0-07-238688-6.
#'   McGraw-Hill/Irwin.
#'
#' @templateVar fn identify_extreme_leverages
#' @template examples-linreg-lm
#' @template examples-linreg-obs
#'
#' @export
identify_extreme_leverages <- function(object, id = NULL, .multiplier = 3) {
  UseMethod("identify_extreme_leverages")
}

#' @rdname identify_extreme_leverages
#' @export
identify_extreme_leverages.lm <- function(object, id = NULL, .multiplier = 3) {
  identify_extreme_leverages_(object, id, .multiplier)
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
#' @return A [tibble][tibble::tibble-package].
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005).
#'   _Applied Linear Statistical Models_. ISBN: 0-07-238688-6.
#'   McGraw-Hill/Irwin.
#'
#' @templateVar fn identify_outliers
#' @template examples-linreg-lm
#' @template examples-linreg-obs
#'
#' @export
identify_outliers <- function(object, id = NULL, .cutoff = 3) {
  UseMethod("identify_outliers")
}

#' @rdname identify_outliers
#' @export
identify_outliers.lm <- function(object, id = NULL, .cutoff = 3) {
  identify_outliers_(object, id, .cutoff)
}


# Influential -----------------------------------------------------------------

#' Identify Influential Observations (Using Cook's Distance)
#'
#' A data point flagged as an influential observation means that it strongly
#' influences the fitted values of a regression, taking into account both the
#' x and y values of the observation.
#'
#' @details
#' Cook's distance is often used to determine if observations are influential.
#' This function first calculates Cook's distance for each observation and
#' filters out only those that are above a certain cutoff. A traditional
#' rule-of-thumb is for that cutoff value to be 0.5.
#'
#' @inheritParams identify_extreme_leverages
#' @param .cutoff (Optional) Used to determine which Cook's distances are
#'   indicative of an influential observation. The default is the rule-of-thumb
#'   0.5 (see details).
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005).
#'   _Applied Linear Statistical Models_. ISBN: 0-07-238688-6.
#'   McGraw-Hill/Irwin.
#'
#' @templateVar fn identify_influential_obs
#' @template examples-linreg-lm
#' @template examples-linreg-obs
#'
#' @export
identify_influential_obs <- function(object, id = NULL, .cutoff = 0.5) {
  UseMethod("identify_influential_obs")
}

#' @rdname identify_influential_obs
#' @export
identify_influential_obs.lm <- function(object, id = NULL, .cutoff = 0.5) {
  identify_influential_obs_(object, id, .cutoff)
}


# Helper Functions ------------------------------------------------------------

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

## Leverage -------------------------------------------------------------------
identify_extreme_leverages_ <- function(object, id, .multiplier) {
  leverages_num <- calc_leverages(object)
  leverages_tbl <- format_leverages(leverages_num, id = id)

  cutoff_num <- calc_leverage_cutoff(object, .multiplier)

  leverages_tbl %>%
    dplyr::mutate(.cutoff = cutoff_num) %>%
    dplyr::filter(leverage > .cutoff)
}

##> Calculate leverage of each data point
calc_leverages <- function(object) {
  as.numeric(stats::influence(object)[["hat"]])
}

format_leverages <- function(x, id) {
  add_id(x, name = "leverage", id = id)
}

##> Calculate leverage cutoff
calc_num_coefs <- function(object) {
  length(stats::coef(object))
}

calc_num_obs <- function(object) {
  stats::nobs(object)
}

calc_leverage_cutoff <- function(object, .multiplier) {
  p <- calc_num_coefs(object)
  n <- calc_num_obs(object)

  (p / n) * .multiplier
}

## Outliers -------------------------------------------------------------------
identify_outliers_ <- function(object, id, .cutoff) {
  std_residuals_num <- calc_standardized_residuals(object)
  std_residuals_tbl <- format_standardized_residuals(std_residuals_num, id)

  dplyr::filter(std_residuals_tbl, std_resid > .cutoff)
}

##> Calculate standardized residuals of each data point
calc_standardized_residuals <- function(object) {
  as.numeric(stats::rstandard(object))
}

format_standardized_residuals <- function(x, id) {
  add_id(x, name = "std_resid", id = id)
}

## Influential ----------------------------------------------------------------
identify_influential_obs_ <- function(object, id, .cutoff) {
  cooks_dist_num <- calc_cooks_distance(object)
  cooks_dist_tbl <- format_cooks_distance(cooks_dist_num, id)

  dplyr::filter(cooks_dist_tbl, cooks_dist > .cutoff)
}

##> Calculate Cook's distance of each data point
calc_cooks_distance <- function(object) {
  as.numeric(stats::cooks.distance(object))
}

format_cooks_distance <- function(x, id) {
  add_id(x, name = "cooks_dist", id = id)
}
