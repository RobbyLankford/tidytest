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
#' #> `lm` Method
#' mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)
#'
#' identify_extreme_leverages(mod_lm_fit)
#' identify_extreme_leverages(mod_lm_fit, id = rownames(mtcars))
#'
#' #> Tidymodels Method
#' mod_linreg_fit <- linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ disp + wt + hp, data = mtcars)
#'
#' identify_extreme_leverages(mod_linreg_fit)
#' identify_extreme_leverages(mod_linreg_fit, id = rownames(mtcars))
#'
#' @export
identify_extreme_leverages <- function(object, id = NULL, .multiplier = 3) {
  UseMethod("identify_extreme_leverages")
}

#' @rdname identify_extreme_leverages
#' @export
identify_extreme_leverages.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

identify_extreme_leverages.lm <- function(object, id = NULL, .multiplier = 3) {
  identify_extreme_leverages_spec(object, id, .multiplier)
}


identify_extreme_leverages <- function(object, id = NULL, .multiplier = 3) {
  identify_extreme_leverages_spec(object, id, .multiplier)
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
#' #> No influential observations with default `.cutoff` value
#' identify_influential_obs(mod_fit)
#'
#' #> Try a lower `.cutoff` value
#' identify_influential_obs(mod_fit, .cutoff = 0.5)
#' identify_influential_obs(mod_fit, id = rownames(mtcars), .cutoff = 0.1)
#'
#' @export
identify_influential_obs <- function(object, id = NULL, .cutoff = 0.5) {
  cooks_dist_tbl <- get_cooks_distance(object, id)

  dplyr::filter(cooks_dist_tbl, cooks_dist > .cutoff)
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

#> Identify Extreme Leverages
identify_extreme_leverages_spec <- function(object, id, .multiplier) {
  leverages_tbl <- calc_leverages(object, id)
  cutoff <- calc_leverage_cutoff(object, .multiplier)

  leverages_tbl %>%
    dplyr::mutate(.cutoff = cutoff) %>%
    dplyr::filter(leverage > .cutoff)
}

##> Calculate leverage of each data point
calc_leverages <- function(object, id = NULL, ...) {
  UseMethod("calc_leverages")
}

calc_leverages.lm <- function(object, id = NULL) {
  calc_leverages_spec(object, id = id)
}

calc_leverages._lm <- function(object, id = NULL) {
  calc_leverages_spec(object[["fit"]], id = id)
}

calc_leverages_spec <- function(object, id = NULL) {
  leverages <- as.numeric(stats::influence(object)[["hat"]])

  add_id(leverages, name = "leverage", id = id)
}

##> Number of coefficients in a model
calc_num_coefs <- function(object, ...) {
  UseMethod("calc_num_coefs")
}

calc_num_coefs.lm <- function(object) {
  calc_num_coefs_spec(object)
}

calc_num_coefs._lm <- function(object) {
  calc_num_coefs_spec(object[["fit"]])
}

calc_num_coefs_spec <- function(object) {
  length(stats::coef(object))
}

##> Number of observations used to build a model
calc_num_obs <- function(object, ...) {
  UseMethod("calc_num_obs")
}

calc_num_obs.lm <- function(object) {
  calc_num_obs_spec(object)
}

calc_num_obs._lm <- function(object) {
  calc_num_obs_spec(object[["fit"]])
}

calc_num_obs_spec <- function(object) {
  stats::nobs(object)
}

##> Calculate leverage cutoff
calc_leverage_cutoff <- function(object, .multiplier) {
  p <- calc_num_coefs(object)
  n <- calc_num_obs(object)

  (p / n) * .multiplier
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

#> Calculate Cook's distance of each data point
get_cooks_distance <- function(object, id = NULL) {
  UseMethod("get_cooks_distance")
}

get_cooks_distance.lm <- function(object, id = NULL) {
  cooks_dist <- as.numeric(stats::cooks.distance(object))

  add_id(cooks_dist, name = "cooks_dist", id = id)
}

get_cooks_distance._lm <- function(object, id = NULL) {
  get_cooks_distance.lm(object[["fit"]], id = id)
}
