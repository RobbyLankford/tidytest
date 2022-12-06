# Anderson-Darling Test -------------------------------------------------------

#' Run an Anderson-Darling Test
#'
#' A wrapper around \code{\link[nortest]{ad.test}} that standardizes the
#' inputs and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: Follows a Normal Distribution
#' * Alternative: Does Not Follow a Normal Distribution
#'
#' @param object a model object (such as a fitted `lm` object).
#' @param ... not currently used.
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
#' anderson_darling_test(mod_fit)
#'
#' @export
anderson_darling_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("anderson_darling_test")
}

#' @export
anderson_darling_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @export
anderson_darling_test.lm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  tidy_test(
    resids,
    nortest::ad.test,
    ...,
    .test   = "Anderson-Darling",
    .null   = "Follows a Normal Distribution",
    .alt    = "Does Not Follow a Normal Distribution",
    .alpha = .alpha
  )
}

#' @export
anderson_darling_test._lm <- function(object, ..., .alpha = 0.05) {
  anderson_darling_test.lm(object[["fit"]], ..., .alpha = .alpha)
}

#' @export
anderson_darling_test._glm <- function(object, ..., .alpha = 0.05) {
  anderson_darling_test._lm(object, ..., .alpha = .alpha)
}

# Shapiro-Wilk Test -----------------------------------------------------------

#' Run a Shapiro-Wilk Test
#'
#' A wrapper around \code{\link[stats]{shapiro.test}} that standardizes the
#' inputs and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: Follows a Normal Distribution
#' * Alternative: Does Not Follow a Normal Distribution
#'
#' @param object a model object (such as a fitted `lm` object).
#' @param ... not currently used.
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
#' shapiro_wilk_test(mod_fit)
#'
#' @export
shapiro_wilk_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("shapiro_wilk_test")
}

#' @export
shapiro_wilk_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @export
shapiro_wilk_test.lm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  tidy_test(
    resids,
    shapiro.test,
    ...,
    .test   = "Shapiro-Wilk",
    .null   = "Follows a Normal Distribution",
    .alt    = "Does Not Follow a Normal Distribution",
    .alpha = .alpha
  )
}

#' @export
shapiro_wilk_test._lm <- function(object, ..., .alpha = 0.05) {
  shapiro_wilk_test.lm(object[["fit"]], ..., .alpha = .alpha)
}

#' @export
shapiro_wilk_test._glm <- function(object, ..., .alpha = 0.05) {
  shapiro_wilk_test._lm(object, ..., .alpha = .alpha)
}
