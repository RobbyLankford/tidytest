# Anderson-Darling Test -------------------------------------------------------

#' Run an Anderson-Darling Test
#'
#' @details
#' The hypotheses for this test are:
#'
#' * Null: Follows a Normal Distribution
#' * Alternative: Does Not Follow a Normal Distribution
#'
#' @templateVar link nortest::ad.test
#' @template desc-linreg-tests
#'
#' @family normality tests
#' @template return
#'
#' @template params-linreg-obj
#' @template params-linreg-vec
#' @template params-alpha
#' @param ... Not currently used.
#'
#' @templateVar fn anderson_darling_test
#' @template examples-linreg-lm
#' @template examples-linreg-linear_reg
#' @template examples-linreg-tests-vec
#'
#' @export
anderson_darling_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("anderson_darling_test")
}

#' @rdname anderson_darling_test
#' @export
anderson_darling_test.lm <- function(object, ..., .alpha = 0.05) {
  anderson_darling_test_impl(calc_residuals(object), ..., .alpha = .alpha)
}

#' @rdname anderson_darling_test
#' @export
anderson_darling_test._lm <- function(object, ..., .alpha = 0.05) {
  anderson_darling_test_impl(
    calc_residuals(object[["fit"]]), ..., .alpha = .alpha
  )
}

#' @rdname anderson_darling_test
#' @export
anderson_darling_test._glm <- function(object, ..., .alpha = 0.05) {
  anderson_darling_test_impl(
    calc_residuals(object[["fit"]]), ..., .alpha = .alpha
  )
}

#' @rdname anderson_darling_test
#' @export
anderson_darling_test_vec <- function(x, ..., .alpha = 0.05) {
  anderson_darling_test_impl(x, ..., .alpha = .alpha)
}


# Shapiro-Wilk Test -----------------------------------------------------------

#' Run a Shapiro-Wilk Test
#'
#' @details
#' The hypotheses for this test are:
#'
#' * Null: Follows a Normal Distribution
#' * Alternative: Does Not Follow a Normal Distribution
#'
#' @templateVar link stats::shapiro.test
#' @template desc-linreg-tests
#'
#' @family normality tests
#' @template return
#'
#' @template params-linreg-obj
#' @template params-linreg-vec
#' @template params-alpha
#' @param ... Not currently used.
#'
#' @templateVar fn shapiro_wilk_test
#' @template examples-linreg-lm
#' @template examples-linreg-linear_reg
#' @template examples-linreg-tests-vec
#'
#' @export
shapiro_wilk_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("shapiro_wilk_test")
}

#' @rdname shapiro_wilk_test
#' @export
shapiro_wilk_test.lm <- function(object, ..., .alpha = 0.05) {
  shapiro_wilk_test_impl(calc_residuals(object), ..., .alpha = 0.05)
}

#' @rdname shapiro_wilk_test
#' @export
shapiro_wilk_test._lm <- function(object, ..., .alpha = 0.05) {
  shapiro_wilk_test_impl(calc_residuals(object[["fit"]]), ..., .alpha = 0.05)
}

#' @rdname shapiro_wilk_test
#' @export
shapiro_wilk_test._glm <- function(object, ..., .alpha = 0.05) {
  shapiro_wilk_test_impl(calc_residuals(object[["fit"]]), ..., .alpha = 0.05)
}

#' @rdname shapiro_wilk_test
#' @export
shapiro_wilk_test_vec <- function(x, ..., .alpha = 0.05) {
  shapiro_wilk_test_impl(x, ..., .alpha = .alpha)
}


# Helper Functions ------------------------------------------------------------
anderson_darling_test_impl <- function(resids, ..., .alpha = 0.05) {
  check_installed("nortest", reason = "to use `anderson_darling_test()`")

  tidy_test(
    resids,
    nortest::ad.test,
    ...,
    .test  = "Anderson-Darling",
    .null  = "Follows a Normal Distribution",
    .alt   = "Does Not Follow a Normal Distribution",
    .alpha = .alpha
  )
}

shapiro_wilk_test_impl <- function(resids, ..., .alpha = 0.05) {
  tidy_test(
    resids,
    shapiro.test,
    ...,
    .test  = "Shapiro-Wilk",
    .null  = "Follows a Normal Distribution",
    .alt   = "Does Not Follow a Normal Distribution",
    .alpha = .alpha
  )
}
