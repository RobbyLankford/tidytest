# Bruesch-Pagan Test ----------------------------------------------------------

#' Run a Bruesch-Pagan Test
#'
#' @details
#' The hypotheses for this test are:
#'
#' * Null: Variances are Equal (Homoscedastic)
#' * Alternative: Variances are Not Equal (Heteroscedastic)
#'
#' @templateVar link lmtest::bptest
#' @template desc-linreg-tests
#'
#' @family homoscedasticity tests
#' @template return
#'
#' @template params-linreg-obj
#' @template params-dots
#' @template params-alpha
#'
#' @templateVar fn bruesch_pagan_test
#' @template examples-linreg-lm
#' @template examples-linreg-linear_reg
#'
#' @export
bruesch_pagan_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("bruesch_pagan_test")
}

#' @rdname bruesch_pagan_test
#' @export
bruesch_pagan_test.lm <- function(object, ..., .alpha = 0.05) {
  bruesch_pagan_test_impl(object, ..., .alpha = .alpha)
}

#' @rdname bruesch_pagan_test
#' @export
bruesch_pagan_test._lm <- function(object, ..., .alpha = 0.05) {
  bruesch_pagan_test_impl(object[["fit"]], ..., .alpha = .alpha)
}

#' @rdname bruesch_pagan_test
#' @export
bruesch_pagan_test._glm <- function(object, ..., .alpha = 0.05) {
  bruesch_pagan_test_impl(object[["fit"]], ..., .alpha = .alpha)
}


# Goldfeld-Quandt Test --------------------------------------------------------

#' Run a Goldfeld-Quandt Test
#'
#' @details
#' The hypotheses for this test are:
#'
#' * Null: Variances are Equal (Homoscedastic)
#' * Alternative: Variances are Not Equal (Heteroscedastic)
#'
#' @templateVar link lmtest::gqtest
#' @template desc-linreg-tests
#'
#' @family homoscedasticity tests
#' @template return
#'
#' @template params-linreg-obj
#' @template params-linreg-alt
#' @template params-dots
#' @template params-alpha
#'
#' @templateVar fn goldfeld_quandt_test
#' @template examples-linreg-lm
#' @template examples-linreg-linear_reg
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
goldfeld_quandt_test.lm <- function(object,
                                    alternative = "two.sided",
                                    ...,
                                    .alpha = 0.05) {
  goldfeld_quandt_test_impl(object, alternative, ..., .alpha = .alpha)
}

#' @rdname goldfeld_quandt_test
#' @export
goldfeld_quandt_test._lm <- function(object,
                                     alternative = "two.sided",
                                     ...,
                                     .alpha = 0.05) {
  goldfeld_quandt_test_impl(object[["fit"]], alternative, ..., .alpha = .alpha)
}

#' @rdname goldfeld_quandt_test
#' @export
goldfeld_quandt_test._glm <- function(object,
                                      alternative = "two.sided",
                                      ...,
                                      .alpha = 0.05) {
  goldfeld_quandt_test_impl(object[["fit"]], alternative, ..., .alpha = .alpha)
}


# Helper Functions ------------------------------------------------------------
bruesch_pagan_test_impl <- function(object, ..., .alpha = 0.05) {
  tidy_test(
    object,
    lmtest::bptest,
    ...,
    .test  = "Bruesch-Pagan",
    .null  = "Variances Are Equal",
    .alt   = "Variances Are Not Equal",
    .alpha = .alpha
  )
}

goldfeld_quandt_test_impl <- function(object,
                                      alternative = "two.sided",
                                      ...,
                                      .alpha = 0.05) {
  tidy_test(
    object,
    lmtest::gqtest,
    alternative = alternative,
    ...,
    .test  = "Goldfeld-Quandt",
    .null  = "Variances Are Equal",
    .alt   = "Variances Are Not Equal",
    .alpha = .alpha
  )
}
