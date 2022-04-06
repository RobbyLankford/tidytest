# Augmented Dickey-Fuller Test ------------------------------------------------

#' Run an Augmented Dickey-Fuller (ADF) Test
#'
#' A wrapper around \code{\link[tseries]{adf.test}} that standardizes the
#' inputs and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: Series has a unit root
#' * Alternative: Series is either stationary of explosive (user defined)
#'
#' @inheritParams tseries::adf.test
#' @param object a model object (such as a fitted `lm` object).
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
#' dickey_fuller_test(mod_fit)
#' dickey_fuller_test(mod_fit, alternative = "stationary")
#' dickey_fuller_test(mod_fit, alternative = "explosive")
#'
#' @export
dickey_fuller_test <- function(object, alternative = "stationary", ...,
                               .alpha = 0.05) {
  UseMethod("dickey_fuller_test", object)
}

#' @export
dickey_fuller_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object)[[1]])
}

#' @export
dickey_fuller_test.lm <- function(object, alternative = "stationary", ...,
                                  .alpha = 0.05) {
  object %>%
    get_residuals() %>%
    tseries::adf.test(alternative = alternative, ...) %>%
    tidy_test(
      statistic, p.value,
      test  = "Augmented Dickey-Fuller",
      null  = "Not Stationary",
      alt   = "Stationary",
      alpha = .alpha
    )
}

#' @export
dickey_fuller_test._lm <- function(object, alternative = "stationary", ...,
                                   .alpha = 0.05) {
  dickey_fuller_test.lm(
    object[["fit"]], alternative = alternative, ..., .alpha = .alpha
  )
}
