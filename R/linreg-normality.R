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
#' @inheritParams bruesch_pagan_test
#' @inheritParams ljung_box_test
#' @param ... Not currently used.
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
#' anderson_darling_test(resids)
#'
#' #> `lm` Method
#' mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)
#'
#' anderson_darling_test(mod_lm_fit)
#'
#' #> Tidymodels Method
#' mod_linreg_fit <- parsnip::linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ disp + wt + hp, data = mtcars)
#'
#' anderson_darling_test(mod_linreg_fit)
#'
#' @export
anderson_darling_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("anderson_darling_test")
}

#' @rdname anderson_darling_test
#' @export
anderson_darling_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @rdname anderson_darling_test
#' @export
anderson_darling_test.numeric <- function(x, ..., .alpha = 0.05) {
  anderson_darling_test_spec(x, ..., .alpha = .alpha)
}

#' @rdname anderson_darling_test
#' @export
anderson_darling_test.lm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  anderson_darling_test_spec(resids, ..., .alpha = .alpha)
}

#' @rdname anderson_darling_test
#' @export
anderson_darling_test._lm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  anderson_darling_test_spec(resids, ..., .alpha = .alpha)
}

#' @rdname anderson_darling_test
#' @export
anderson_darling_test._glm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  anderson_darling_test_spec(resids, ..., .alpha = .alpha)
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
#' @inheritParams anderson_darling_test
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
#' shapiro_wilk_test(resids)
#'
#' #> `lm` Method
#' mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)
#'
#' shapiro_wilk_test(mod_lm_fit)
#'
#' #> Tidymodels Method
#' mod_linreg_fit <- parsnip::linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ disp + wt + hp, data = mtcars)
#'
#' shapiro_wilk_test(mod_linreg_fit)
#'
#' @export
shapiro_wilk_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("shapiro_wilk_test")
}

#' @rdname shapiro_wilk_test
#' @export
shapiro_wilk_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @rdname shapiro_wilk_test
#' @export
shapiro_wilk_test.numeric <- function(x, ..., .alpha = 0.05) {
  shapiro_wilk_test_spec(x, ..., .alpha = .alpha)
}

#' @rdname shapiro_wilk_test
#' @export
shapiro_wilk_test.lm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  shapiro_wilk_test_spec(resids, ..., .alpha = 0.05)
}

#' @rdname shapiro_wilk_test
#' @export
shapiro_wilk_test._lm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  shapiro_wilk_test_spec(resids, ..., .alpha = 0.05)
}

#' @rdname shapiro_wilk_test
#' @export
shapiro_wilk_test._glm <- function(object, ..., .alpha = 0.05) {
  resids <- get_residuals(object)

  shapiro_wilk_test_spec(resids, ..., .alpha = 0.05)
}


# Helper Functions ------------------------------------------------------------
anderson_darling_test_spec <- function(resids, ..., .alpha = 0.05) {
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

shapiro_wilk_test_spec <- function(resids, ..., .alpha = 0.05) {
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
