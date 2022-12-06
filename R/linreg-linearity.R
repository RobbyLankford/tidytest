# Ramsey's RESET Test ---------------------------------------------------------

#' Run a Ramsey's Regression Equation Specification Error Test (RESET)
#'
#' A wrapper around \code{\link[lmtest]{resettest}} that standardizes the
#' inputs and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: Linear Specification is Valid
#' * Alternative: Linear Specification is Not Valid
#'
#' @inheritParams lmtest::resettest
#' @param object a model object (such as a fitted `lm` object).
#' @param ... further arguments passed to \code{\link[lmtest]{resettest}}.
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
#' ramsey_reset_test(mod_fit)
#'
#' @export
ramsey_reset_test <- function(object, power = 2:3, ..., .alpha = 0.05) {
  UseMethod("ramsey_reset_test")
}

#' @export
ramsey_reset_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @export
ramsey_reset_test.lm <- function(object, power = 2:3, ..., .alpha = 0.05) {
  tidy_test(
    object,
    lmtest::resettest,
    power = power,
    ...,
    .test   = "Ramsey's RESET",
    .null   = "Linear Specification is Valid",
    .alt    = "Linear Specification is Not Valid",
    .alpha = .alpha
  )
}

#' @export
ramsey_reset_test._lm <- function(object, power = 2:3, ..., .alpha = 0.05) {
  ramsey_reset_test.lm(object[["fit"]], power = power, ..., .alpha = .alpha)
}

#' @export
ramsey_reset_test._glm <- function(object, power = 2:3, ..., .alpha = 0.05) {
  ramsey_reset_test._lm(object, power = power, ..., .alpha = .alpha)
}


# Harvey-Collier Test ---------------------------------------------------------

#' Run a Harvey-Collier Test
#'
#' A wrapper around \code{\link[lmtest]{harvtest}} that standardizes the
#' inputs and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: True Relationship is Linear
#' * Alternative: True Relationship is Not Linear (Convex or Concave)
#'
#' @param object a model object (such as a fitted `lm` object).
#' @param ... further arguments passed to \code{\link[lmtest]{harvtest}}.
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
#' harvey_collier_test(mod_fit)
#'
#' @export
harvey_collier_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("harvey_collier_test")
}

#' @export
harvey_collier_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @export
harvey_collier_test.lm <- function(object, ..., .alpha = 0.05) {
  tidy_test(
    object,
    lmtest::harvtest,
    ...,
    .test   = "Harvey-Collier",
    .null   = "True Relationship is Linear",
    .alt    = "True Relationship is Not Linear (Convex or Concave)",
    .alpha = .alpha
  )
}

#' @export
harvey_collier_test._lm <- function(object, ..., .alpha = 0.05) {
  harvey_collier_test.lm(object[["fit"]], ..., .alpha = .alpha)
}

#' @export
harvey_collier_test._glm <- function(object, ..., .alpha = 0.05) {
  harvey_collier_test._lm(object, ..., .alpha = .alpha)
}
