# Bruesch-Pagan Test ----------------------------------------------------------

#' Run a Bruesch-Pagan Test
#'
#' A wrapper around \code{\link[lmtest]{bptest}} that standardizes the inputs
#' and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: Variances are Equal (Homoscedastic)
#' * Alternative: Variances are Not Equal (Heteroscedastic)
#'
#' @param object a model object (such as a fitted `lm` object).
#' @param ... further arguments passed to \code{\link[lmtest]{bptest}}.
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
#' bruesch_pagan_test(mod_fit)
#'
#' @export
bruesch_pagan_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("bruesch_pagan_test")
}

#' @export
bruesch_pagan_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @export
bruesch_pagan_test.lm <- function(object, ..., .alpha = 0.05) {
  object %>%
    lmtest::bptest(...) %>%
    tidy_test(
      statistic, p.value,
      test  = "Bruesch-Pagan",
      null  = "Variances Are Equal",
      alt   = "Variances Are Not Equal",
      alpha = .alpha
    )
}

#' @export
bruesch_pagan_test._lm <- function(object, ..., .alpha = 0.05) {
  bruesch_pagan_test.lm(object[["fit"]], ..., .alpha = .alpha)
}

#' @export
bruesch_pagan_test._glm <- function(object, ..., .alpha = 0.05) {
  bruesch_pagan_test._lm(object, ..., .alpha = .alpha)
}


# Goldfeld-Quandt Test --------------------------------------------------------

#' Run a Goldfeld-Quandt Test
#'
#' A wrapper around \code{\link[lmtest]{gqtest}} that standardizes the inputs
#' and outputs.
#'
#' The hypotheses for this test are:
#'
#' * Null: Variances are Equal (Homoscedastic)
#' * Alternative: Variances are Not Equal (Heteroscedastic)
#'
#' @inheritParams lmtest::gqtest
#' @param object a model object (such as a fitted `lm` object).
#' @param ... further arguments passed to \code{\link[lmtest]{gqtest}}.
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
#' goldfeld_quandt_test(mod_fit)
#'
#' @export
goldfeld_quandt_test <- function(object, alternative = "two.sided", ...,
                                 .alpha = 0.05) {
  UseMethod("goldfeld_quandt_test")
}

#' @export
goldfeld_quandt_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @export
goldfeld_quandt_test.lm <- function(object, alternative = "two.sided", ...,
                                    .alpha = 0.05) {
  object %>%
    lmtest::gqtest(...) %>%
    tidy_test(
      df1, df2, statistic, p.value,
      test  = "Goldfeld-Quandt",
      null  = "Variances Are Equal",
      alt   = "Variances Are Not Equal",
      alpha = .alpha
    )
}

#' @export
goldfeld_quandt_test._lm <- function(object, alternative = "two.sided", ...,
                                     .alpha = 0.05) {
  goldfeld_quandt_test.lm(
    object[["fit"]], alternative = alternative, ..., .alpha = .alpha
  )
}

#' @export
goldfeld_quandt_test._glm <- function(object, ..., .alpha = 0.05) {
  goldfeld_quandt_test._lm(
    object, alternative = alternative, ..., .alpha = .alpha
  )
}
