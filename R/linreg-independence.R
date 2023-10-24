# Durbin-Watson Test ----------------------------------------------------------

#' Run a Durbin-Watston Test
#'
#' @details
#' The hypotheses for this test are:
#'
#' * Null: No Autocorrelation
#' * Alternative: Autocorrelation
#'
#' @templateVar link lmtest::dwtest
#' @template desc-linreg-tests
#'
#' @family independence tests
#' @template return
#'
#' @template params-linreg-obj
#' @template params-linreg-alt
#' @template params-dots
#' @template params-alpha
#'
#' @templateVar fn durbin_watson_test
#' @template examples-linreg-lm
#' @template examples-linreg-linear_reg
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
durbin_watson_test.lm <- function(object,
                                  alternative = "two.sided",
                                  ...,
                                  .alpha = 0.05) {
  durbin_watson_test_(object, alternative, ..., .alpha = .alpha)
}

#' @rdname durbin_watson_test
#' @export
durbin_watson_test._lm <- function(object,
                                   alternative = "two.sided",
                                   ...,
                                  .alpha = 0.05) {
  durbin_watson_test_(object[["fit"]], alternative, ..., .alpha = .alpha)
}

#' @rdname durbin_watson_test
#' @export
durbin_watson_test._glm <- function(object,
                                    alternative = "two.sided",
                                    ...,
                                    .alpha = 0.05) {
  durbin_watson_test_(object[["fit"]], alternative, ..., .alpha = .alpha)
}


# Ljung-Box Test --------------------------------------------------------------

#' Run a Ljung-Box Test
#'
#' @details
#' The hypotheses for this test are:
#'
#' * Null: No Autocorrelation
#' * Alternative: Autocorrelation
#'
#' @templateVar link stats::Box.test
#' @template desc-linreg-tests
#'
#' @family independence tests
#' @template return
#'
#' @template params-linreg-obj
#' @template params-linreg-vec
#' @template params-dots
#' @template params-alpha
#'
#' @templateVar fn ljung_box_test
#' @template examples-linreg-lm
#' @template examples-linreg-linear_reg
#' @template examples-linreg-tests-vec
#'
#' @export
ljung_box_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("ljung_box_test")
}

#' @rdname ljung_box_test
#' @export
ljung_box_test.lm <- function(object, ..., .alpha = 0.05) {
  resids <- calc_residuals(object)

  ljung_box_test_(resids, ..., .alpha = .alpha)
}

#' @rdname ljung_box_test
#' @export
ljung_box_test._lm <- function(object, ..., .alpha = 0.05) {
  resids <- calc_residuals(object[["fit"]])

  ljung_box_test_(resids, ..., .alpha = .alpha)
}

#' @rdname ljung_box_test
#' @export
ljung_box_test._glm <- function(object, ..., .alpha = 0.05) {
  resids <- calc_residuals(object[["fit"]])

  ljung_box_test_(resids, ..., .alpha = .alpha)
}

#' @rdname ljung_box_test
#' @export
ljung_box_test_vec <- function(x, ..., .alpha = 0.05) {
  ljung_box_test_(x, ..., .alpha = .alpha)
}


# Helper Functions ------------------------------------------------------------
durbin_watson_test_ <- function(object,
                                    alternative = "two.sided",
                                    ...,
                                    .alpha = 0.05) {
  tidy_test(
    object,
    dwtest,
    alternative = alternative,
    ...,
    .test  = "Durbin-Watson",
    .null  = "No Autocorrelation",
    .alt   = "Autocorrelation",
    .alpha = .alpha
  )
}

ljung_box_test_ <- function(resids, ..., .alpha = 0.05) {
  tidy_test(
    resids,
    Box.test,
    type = "Ljung-Box",
    ...,
    .test  = "Ljung-Box",
    .null  = "No Autocorrelation",
    .alt   = "Autocorrelation",
    .alpha = .alpha
  )
}
