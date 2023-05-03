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
#' @param object A model object (such as a fitted [`lm`][stats::lm] object).
#' @param ... Further arguments passed to \code{\link[lmtest]{bptest}}.
#' @param .alpha (Optional) Critical p-value used to determine test conclusion.
#'   The default is 0.05 (5%).
#'
#' @return A [tibble][tibble::tibble-package].
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

#' @rdname bruesch_pagan_test
#' @export
bruesch_pagan_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @rdname bruesch_pagan_test
#' @export
bruesch_pagan_test.lm <- function(object, ..., .alpha = 0.05) {
  tidy_test(
    object,
    lmtest::bptest,
    ...,
    .test   = "Bruesch-Pagan",
    .null   = "Variances Are Equal",
    .alt    = "Variances Are Not Equal",
    .alpha = .alpha
  )
}

#' @rdname bruesch_pagan_test
#' @export
bruesch_pagan_test._lm <- function(object, ..., .alpha = 0.05) {
  bruesch_pagan_test.lm(object[["fit"]], ..., .alpha = .alpha)
}

#' @rdname bruesch_pagan_test
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
#' @inheritParams bruesch_pagan_test
#' @param alternative The alternative hypothesis, one of "two.sided" (default),
#'   "greater", or "less".
#' @param ... Further arguments passed to \code{\link[lmtest]{gqtest}}.
#'
#' @return A [tibble][tibble::tibble-package].
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
goldfeld_quandt_test <- function(object,
                                 alternative = "two.sided",
                                 ...,
                                 .alpha = 0.05) {
  UseMethod("goldfeld_quandt_test")
}

#' @rdname goldfeld_quandt_test
#' @export
goldfeld_quandt_test.default <- function(object, ...) {
  stop("No method for object of class ", class(object))
}

#' @rdname goldfeld_quandt_test
#' @export
goldfeld_quandt_test.lm <- function(object,
                                    alternative = "two.sided",
                                    ...,
                                    .alpha = 0.05) {
  tidy_test(
    object,
    lmtest::gqtest,
    alternative = alternative,
    ...,
    .test   = "Goldfeld-Quandt",
    .null   = "Variances Are Equal",
    .alt    = "Variances Are Not Equal",
    .alpha = .alpha
  )
}

#' @rdname goldfeld_quandt_test
#' @export
goldfeld_quandt_test._lm <- function(object,
                                     alternative = "two.sided",
                                     ...,
                                     .alpha = 0.05) {
  goldfeld_quandt_test.lm(
    object[["fit"]], alternative = alternative, ..., .alpha = .alpha
  )
}

#' @rdname goldfeld_quandt_test
#' @export
goldfeld_quandt_test._glm <- function(object,
                                      alternative = "two.sided",
                                      ...,
                                      .alpha = 0.05) {
  goldfeld_quandt_test._lm(
    object, alternative = alternative, ..., .alpha = .alpha
  )
}
