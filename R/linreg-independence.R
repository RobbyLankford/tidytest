# Durbin-Watson Test ----------------------------------------------------------

#' Run a Durbin-Watston Test
#'
#' A wrapper around \code{\link[lmtest]{dwtest}} that standardizes the inputs
#' and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: No Autocorrelation
#' * Alternative: Autocorrelation
#'
#' @inheritParams lmtest::dwtest
#' @param object a model object (such as a fitted `lm` object).
#' @param ... further arguments passed to \code{\link[lmtest]{dwtest}}.
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
#' durbin_watson_test(mod_fit)
#' durbin_watson_test(mod_fit, alternative = "greater")
#' durbin_watson_test(mod_fit, alternative = "two.sided")
#'
#' @export
durbin_watson_test <- function(object, alternative = "two.sided", ...,
                              .alpha = 0.05) {
  UseMethod("durbin_watson_test")
}

#' @export
durbin_watson_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @export
durbin_watson_test.lm <- function(object, alternative = "two.sided", ...,
                                  .alpha = 0.05) {
  object %>%
    lmtest::dwtest(alternative = alternative, ...) %>%
    tidy_test(
      statistic, p.value,
      test  = "Durbin-Watson",
      null  = "No Autocorrelation",
      alt   = "Autocorrelation",
      alpha = .alpha
    )
}

#' @export
durbin_watson_test._lm <- function(object, alternative = "two.sided", ...,
                                  .alpha = 0.05) {
  durbin_watson_test.lm(
    object[["fit"]], alternative = alternative, ..., .alpha = .alpha
  )
}

#' @export
durbin_watson_test._glm <- function(object, alternative = "two.sided", ...,
                                    .alpha = 0.05) {
  durbin_watson_test._lm(
    object, alternative = alternative, ..., .alpha = .alpha
  )
}

# Ljung-Box Test --------------------------------------------------------------

#' Run a Ljung-Box Test
#'
#' A wrapper around \code{\link[stats]{Box.test}} that standardizes the inputs
#' and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: No Autocorrelation
#' * Alternative: Autocorrelation
#'
#' @param object a model object (such as a fitted `lm` object).
#' @param ... further arguments passed to \code{\link[stats]{Box.test}}.
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
#' ljung_box_test(mod_fit)
#'
#' @export
ljung_box_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("ljung_box_test")
}

#' @export
ljung_box_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @export
ljung_box_test.lm <- function(object, ..., .alpha = 0.05) {
  object %>%
    get_residuals() %>%
    Box.test(type = "Ljung-Box", ...) %>%
    tidy_test(
      statistic, p.value,
      test  = "Ljung-Box",
      null  = "No Autocorrelation",
      alt   = "Autocorrelation",
      alpha = .alpha
    )
}

#' @export
ljung_box_test._lm <- function(object, ..., .alpha = 0.05) {
  ljung_box_test.lm(object[["fit"]], ..., .alpha = .alpha)
}

#' @export
ljung_box_test._glm <- function(object, ..., .alpha = 0.05) {
  ljung_box_test._lm(object, ..., .alpha = .alpha)
}
