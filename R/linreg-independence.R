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
#' @inheritParams goldfeld_quandt_test
#' @param ... Further arguments passed to \code{\link[lmtest]{dwtest}}.
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(tidytest)
#'
#' #> `lm` Method
#' mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)
#'
#' durbin_watson_test(mod_lm_fit)
#' durbin_watson_test(mod_lm_fit, alternative = "greater")
#' durbin_watson_test(mod_lm_fit, alternative = "less")
#'
#' #> Tidymodels Method
#' mod_linreg_fit <- parsnip::linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ disp + wt + hp, data = mtcars)
#'
#' durbin_watson_test(mod_linreg_fit)
#' durbin_watson_test(mod_linreg_fit, alternative = "greater")
#' durbin_watson_test(mod_linreg_fit, alternative = "less")
#'
#' @export
durbin_watson_test <- function(object,
                               alternative = "two.sided",
                               ...,
                              .alpha = 0.05) {
  UseMethod("durbin_watson_test")
}

#' @rdname durbin_watson_test
#' @export
durbin_watson_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @rdname durbin_watson_test
#' @export
durbin_watson_test.lm <- function(object,
                                  alternative = "two.sided",
                                  ...,
                                  .alpha = 0.05) {
  durbin_watson_test_spec(object, alternative, ..., .alpha = .alpha)
}

#' @rdname durbin_watson_test
#' @export
durbin_watson_test._lm <- function(object,
                                   alternative = "two.sided",
                                   ...,
                                  .alpha = 0.05) {
  durbin_watson_test_spec(object[["fit"]], alternative, ..., .alpha = .alpha)
}

#' @rdname durbin_watson_test
#' @export
durbin_watson_test._glm <- function(object,
                                    alternative = "two.sided",
                                    ...,
                                    .alpha = 0.05) {
  durbin_watson_test_spec(object[["fit"]], alternative, ..., .alpha = .alpha)
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
#' @param x For numeric method, a vector of residuals.
#' @inheritParams bruesch_pagan_test
#' @param ... Further arguments passed to \code{\link[stats]{Box.test}}.
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(tidytest)
#'
#' #> Numeric Method
#' set.seed(1914)
#' resids <- rnorm(n = 100)
#'
#' ljung_box_test(resids)
#'
#' #> `lm` Method
#' mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)
#'
#' ljung_box_test(mod_lm_fit)
#'
#' #> Tidymodels Method
#' mod_linreg_fit <- parsnip::linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ disp + wt + hp, data = mtcars)
#'
#' ljung_box_test(mod_linreg_fit)
#'
#' @export
ljung_box_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("ljung_box_test")
}

#' @rdname ljung_box_test
#' @export
ljung_box_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @rdname ljung_box_test
#' @export
ljung_box_test.numeric <- function(x, ..., .alpha = 0.05) {
  ljung_box_test_spec(x, ..., .alpha = .alpha)
}

#' @rdname ljung_box_test
#' @export
ljung_box_test.lm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  ljung_box_test_spec(resids, ..., .alpha = .alpha)
}

#' @rdname ljung_box_test
#' @export
ljung_box_test._lm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  ljung_box_test_spec(resids, ..., .alpha = .alpha)
}

#' @rdname ljung_box_test
#' @export
ljung_box_test._glm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  ljung_box_test_spec(resids, ..., .alpha = .alpha)
}


# Helper Functions ------------------------------------------------------------
durbin_watson_test_spec <- function(object,
                                    alternative = "two.sided",
                                    ...,
                                    .alpha = 0.05) {
  tidy_test(
    object,
    lmtest::dwtest,
    alternative = alternative,
    ...,
    .test   = "Durbin-Watson",
    .null   = "No Autocorrelation",
    .alt    = "Autocorrelation",
    .alpha = .alpha
  )
}

ljung_box_test_spec <- function(resids, ..., .alpha = 0.05) {
  tidy_test(
    resids,
    Box.test,
    type = "Ljung-Box",
    ...,
    .test   = "Ljung-Box",
    .null   = "No Autocorrelation",
    .alt    = "Autocorrelation",
    .alpha = .alpha
  )
}
