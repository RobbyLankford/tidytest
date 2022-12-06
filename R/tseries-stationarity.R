# Augmented Dickey-Fuller Test ------------------------------------------------

#' Run an Augmented Dickey-Fuller (ADF) Test
#'
#' A wrapper around \code{\link[tseries]{adf.test}} that standardizes the
#' inputs and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: Series has a unit root
#' * Alternative: Series is either stationary or has explosive root
#'   (user defined)
#'
#' @param x a numeric vector or univariate time series (for numeric method),
#'   or a model object (such as a fitted `lm` object).
#' @param alternative the alternative hypothesis, either "stationary" (default)
#'   or "explosive".
#' @param ... further arguments passed to \code{\link[tseries]{adf.test}}.
#' @param .alpha critical p-value used to determine test conclusion.
#'
#' @return a [tibble][tibble::tibble-package].
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
#' aug_dickey_fuller_test(mod_fit)
#' aug_dickey_fuller_test(mod_fit, alternative = "stationary")
#' aug_dickey_fuller_test(mod_fit, alternative = "explosive")
#'
#' @export aug_dickey_fuller_test
aug_dickey_fuller_test <- function(x, ...) {
  UseMethod("aug_dickey_fuller_test")
}

#' @rdname aug_dickey_fuller_test
#' @export
aug_dickey_fuller_test.default <- function(x, ...) {
  stop("No method for object of class ", class(x)[[1]])
}

#' @rdname aug_dickey_fuller_test
#' @export
aug_dickey_fuller_test.numeric <- function(x,
                                           alternative = "stationary",
                                           ...,
                                           .alpha = 0.05) {
  tidy_test(
    x,
    tseries::adf.test,
    alternative = alternative,
    ...,
    .test   = "Augmented Dickey-Fuller",
    .null   = "Not Stationary",
    .alt    = "Stationary",
    .alpha = .alpha
  )
}

#' @rdname aug_dickey_fuller_test
#' @export
aug_dickey_fuller_test.lm <- function(x,
                                      alternative = "stationary",
                                      ...,
                                      .alpha = 0.05) {
  resids <- get_residuals(x)

  aug_dickey_fuller_test.numeric(
    resids,
    alternative = alternative,
    ...,
    .alpha = .alpha
  )
}

#' @rdname aug_dickey_fuller_test
#' @export
aug_dickey_fuller_test._lm <- function(x,
                                       alternative = "stationary",
                                       ...,
                                       .alpha = 0.05) {
  resids <- get_residuals(x[["fit"]])

  aug_dickey_fuller_test.numeric(
    resids,
    alternative = alternative,
    ...,
    .alpha = .alpha
  )
}


# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test -------------------------------

#' Run a Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test
#'
#' A wrapper around \code{\link[tseries]{kpss.test}} that standardizes the
#' inputs and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: Series is either Level or Trend stationary (user defined)
#' * Alternative: Series has a unit root
#'
#' @param x a numeric vector or univariate time series (for numeric method),
#'   or a model object (such as a fitted `lm` object).
#' @param null the null hypothesis, either "Level" (default) or "Trend".
#' @param ... further arguments passed to \code{\link[tseries]{kpss.test}}.
#' @param .alpha critical p-value used to determine test conclusion.
#'
#' @return a [tibble][tibble::tibble-package].
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
#' kpss_test(mod_fit)
#' kpss_test(mod_fit, null = "Level")
#' kpss_test(mod_fit, null = "Trend")
#'
#' @export kpss_test
kpss_test <- function(x, ...) {
  UseMethod("kpss_test")
}

#' @rdname kpss_test
#' @export
kpss_test.default <- function(x, ...) {
  stop("No method for object of class ", class(x)[[1]])
}

#' @rdname kpss_test
#' @export
kpss_test.numeric <- function(x, null = "Level", ..., .alpha = 0.05) {
  tidy_test(
    x,
    tseries::kpss.test,
    null = null,
    ...,
    .test   = "Kwiatkowski-Phillips-Schmidt-Shin",
    .null   = paste(null, "Stationary"),
    .alt    = "Unit Root",
    .alpha = .alpha
  )
}

#' @rdname kpss_test
#' @export
kpss_test.lm <- function(x, null = "Level", ..., .alpha = 0.05) {
  resids <- get_residuals(x)

  kpss_test.numeric(resids, null = null, ..., .alpha = .alpha)
}

#' @rdname kpss_test
#' @export
kpss_test._lm <- function(x, null = "Level", ..., .alpha = 0.05) {
  resids <- get_residuals(x[["fit"]])

  kpss_test.numeric(resids, null = null, ..., .alpha = .alpha)
}


# Phillips-Perron Unit Root Test ----------------------------------------------

#' Run a Phillips-Perron Unit Root Test
#'
#' A wrapper around \code{\link[tseries]{pp.test}} that standardizes the
#' inputs and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: Series has a unit root
#' * Alternative: Series is either stationary or has explosive root
#'   (user defined)
#'
#' @param x a  numeric vector or univariate time series (for numeric method),
#'   or a model object (such as a fitted `lm` object).
#' @param alternative the alternative hypothesis, either "stationary" (default)
#'   or "explosive".
#' @param ... further arguments passed to \code{\link[tseries]{pp.test}}.
#' @param .alpha critical p-value used to determine test conclusion.
#'
#' @return a [tibble][tibble::tibble-package].
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
#' phillips_perron_test(mod_fit)
#' phillips_perron_test(mod_fit, alternative = "stationary")
#' phillips_perron_test(mod_fit, alternative = "explosive")
#'
#' @export phillips_perron_test
phillips_perron_test <- function(x, ...) {
  UseMethod("phillips_perron_test")
}

#' @rdname phillips_perron_test
#' @export
phillips_perron_test.default <- function(x, ...) {
  stop("No method for object of class ", class(x)[[1]])
}

#' @rdname phillips_perron_test
#' @export
phillips_perron_test.numeric <- function(x,
                                         alternative = "stationary",
                                         ...,
                                         .alpha = 0.05) {
  tidy_test(
    x,
    tseries::pp.test,
    alternative = alternative,
    ...,
    .test   = "Phillips-Perron",
    .null   = "Unit Root",
    .alpha  = .alpha,
    .alt    = dplyr::if_else(
      alternative == "stationary",
      "Stationary",
      "Explosive Root"
    )
  )
}

#' @rdname phillips_perron_test
#' @export
phillips_perron_test.lm <- function(x,
                                    alternative = "stationary",
                                    ...,
                                    .alpha = 0.05) {
  resids <- get_residuals(x)

  phillips_perron_test.numeric(
    resids,
    alternative = alternative,
    ...,
    .alpha = .alpha
  )
}

#' @rdname phillips_perron_test
#' @export
phillips_perron_test._lm <- function(x,
                                     alternative = "stationary",
                                     ...,
                                     .alpha = 0.05) {
  resids <- get_residuals(x[["fit"]])

  phillips_perron_test.numeric(
    resids,
    alternative = alternative,
    ...,
    .alpha = .alpha
  )
}
