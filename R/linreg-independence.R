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
#' @family independence tests
#' @template return
#'
#' @template params-linreg
#' @param ... Further arguments passed to \code{\link[lmtest]{dwtest}}.
#'
#' @templateVar fn durbin_watson_test
#' @template examples-linreg-tests
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
#' @family independence tests
#' @template return
#'
#' @template params-linreg
#' @template params-linreg-vec
#' @param ... Further arguments passed to \code{\link[stats]{Box.test}}.
#'
#' @templateVar fn ljung_box_test
#' @template examples-linreg-tests
#' @template examples-linreg-tests-vec
#'
#' @export
ljung_box_test <- function(object, ..., .alpha = 0.05) {
  UseMethod("ljung_box_test")
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

#' @rdname ljung_box_test
#' @export
ljung_box_test_vec <- function(x, ..., .alpha = 0.05) {
  ljung_box_test_spec(x, ..., .alpha = .alpha)
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
    .test  = "Durbin-Watson",
    .null  = "No Autocorrelation",
    .alt   = "Autocorrelation",
    .alpha = .alpha
  )
}

ljung_box_test_spec <- function(resids, ..., .alpha = 0.05) {
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
