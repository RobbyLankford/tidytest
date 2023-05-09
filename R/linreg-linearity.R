# Ramsey's RESET Test ---------------------------------------------------------

#' Run a Ramsey's Regression Equation Specification Error Test (RESET)
#'
#' @details
#' The hypotheses for this test are:
#'
#' * Null: Linear Specification is Valid
#' * Alternative: Linear Specification is Not Valid
#'
#' @templateVar link lmtest::resettest
#' @template desc-linreg-tests
#'
#' @family linearity tests
#' @template return
#'
#' @template params-linreg
#' @template params-dots
#' @param power (Optional) A vector of positive integers indicating the powers
#'   of the variables that should be included. The default is 2:3, meaning
#'   quadratic or cubic influence of the fitted response.
#'
#' @templateVar fn ramsey_reset_test
#' @template examples-linreg-tests
#'
#' @export
ramsey_reset_test <- function(object, power = 2:3, ..., .alpha = 0.05) {
  UseMethod("ramsey_reset_test")
}

#' @rdname ramsey_reset_test
#' @export
ramsey_reset_test.lm <- function(object, power = 2:3, ..., .alpha = 0.05) {
  ramsey_reset_test_spec(object, power = power, ..., .alpha = .alpha)
}

#' @rdname ramsey_reset_test
#' @export
ramsey_reset_test._lm <- function(object, power = 2:3, ..., .alpha = 0.05) {
  ramsey_reset_test_spec(object[["fit"]], power = power, ..., .alpha = .alpha)
}

#' @rdname ramsey_reset_test
#' @export
ramsey_reset_test._glm <- function(object, power = 2:3, ..., .alpha = 0.05) {
  ramsey_reset_test_spec(object[["fit"]], power = power, ..., .alpha = .alpha)
}


# Harvey-Collier Test ---------------------------------------------------------

#' Run a Harvey-Collier Test
#'
#' @details
#' The hypotheses for this test are:
#'
#' * Null: True Relationship is Linear
#' * Alternative: True Relationship is Not Linear (Convex or Concave)
#'
#' @templateVar link lmtest::harvtest
#' @template desc-linreg-tests
#'
#' @family linearity tests
#' @template return
#'
#' @template params-linreg
#' @template params-dots
#'
#' @templateVar fn harvey_collier_test
#' @template examples-linreg-tests
#'
#' @export
harvey_collier_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("harvey_collier_test")
}

#' @rdname harvey_collier_test
#' @export
harvey_collier_test.lm <- function(object, ..., .alpha = 0.05) {
  harvey_collier_test_spec(object, ..., .alpha = .alpha)
}

#' @rdname harvey_collier_test
#' @export
harvey_collier_test._lm <- function(object, ..., .alpha = 0.05) {
  harvey_collier_test_spec(object[["fit"]], ..., .alpha = .alpha)
}

#' @rdname harvey_collier_test
#' @export
harvey_collier_test._glm <- function(object, ..., .alpha = 0.05) {
  harvey_collier_test_spec(object[["fit"]], ..., .alpha = .alpha)
}


# Helper Functions ------------------------------------------------------------
ramsey_reset_test_spec <- function(object, power = 2:3, ..., .alpha = 0.05) {
  tidy_test(
    object,
    lmtest::resettest,
    power = power,
    ...,
    .test  = "Ramsey's RESET",
    .null  = "Linear Specification is Valid",
    .alt   = "Linear Specification is Not Valid",
    .alpha = .alpha
  )
}

harvey_collier_test_spec <- function(object, ..., .alpha = 0.05) {
  tidy_test(
    object,
    lmtest::harvtest,
    ...,
    .test  = "Harvey-Collier",
    .null  = "True Relationship is Linear",
    .alt   = "True Relationship is Not Linear (Convex or Concave)",
    .alpha = .alpha
  )
}
