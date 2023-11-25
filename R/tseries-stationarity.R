# Augmented Dickey-Fuller Test ------------------------------------------------

#' Run an Augmented Dickey-Fuller (ADF) Test
#'
#' The hypotheses for this test are:
#'
#' * Null: Series has a unit root
#' * Alternative: Series is either stationary or has explosive root
#'   (user defined)
#'
#' @templateVar link tseries::adf.test
#' @template desc-linreg-tests
#'
#' @family stationarity tests
#' @template return
#'
#' @template params-linreg-obj
#' @template params-linreg-vec
#' @template params-dots
#' @template params-alpha
#'
#' @param alternative (Optional) The alternative hypothesis, either
#'   "stationary" (default) or "explosive".
#'
#' @templateVar fn aug_dickey_fuller_test
#' @template examples-linreg-lm
#'
#' @examples
#' aug_dickey_fuller_test(mod_lm_fit, alternative = "explosive")
#'
#' @template examples-linreg-linear_reg
#'
#' @examples
#' aug_dickey_fuller_test(mod_linreg_fit, alternative = "explosive")
#'
#' @template examples-linreg-tests-vec
#'
#' @examples
#' aug_dickey_fuller_test_vec(resids, alternative = "explosive")
#'
#' @export
aug_dickey_fuller_test <- function(object,
                                   alternative = "stationary",
                                   ...,
                                   .alpha = 0.05) {
  UseMethod("aug_dickey_fuller_test")
}

#' @rdname aug_dickey_fuller_test
#' @export
aug_dickey_fuller_test.lm <- function(object,
                                      alternative = "stationary",
                                      ...,
                                      .alpha = 0.05) {
  aug_dickey_fuller_test_impl(
    calc_residuals(object), alternative, ..., .alpha = .alpha
  )
}

#' @rdname aug_dickey_fuller_test
#' @export
aug_dickey_fuller_test._lm <- function(object,
                                       alternative = "stationary",
                                       ...,
                                       .alpha = 0.05) {
  aug_dickey_fuller_test_impl(
    calc_residuals(object[["fit"]]), alternative, ..., .alpha = .alpha
  )
}

#' @rdname aug_dickey_fuller_test
#' @export
aug_dickey_fuller_test_vec <- function(x,
                                       alternative = "stationary",
                                       ...,
                                       .alpha = 0.05) {
  aug_dickey_fuller_test_impl(x, alternative, ..., .alpha = .alpha)
}


# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test -------------------------------

#' Run a Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test
#'
#' The hypotheses for this test are:
#'
#' * Null: Series is either Level or Trend stationary (user defined)
#' * Alternative: Series has a unit root
#'
#' @templateVar link tseries::kpss.test
#' @template desc-linreg-tests
#'
#' @family stationarity tests
#' @template return
#'
#' @template params-linreg-obj
#' @template params-linreg-vec
#' @template params-dots
#' @template params-alpha
#' @param null (Optional) The null hypothesis, either "Level" (default) or
#'   "Trend".
#'
#' @templateVar fn kpss_test
#' @template examples-linreg-lm
#'
#' @examples
#' kpss_test(mod_lm_fit, null = "Trend")
#'
#' @template examples-linreg-linear_reg
#'
#' @examples
#' kpss_test(mod_linreg_fit, null = "Trend")
#'
#' @template examples-linreg-tests-vec
#'
#' @examples
#' kpss_test_vec(resids, null = "Trend")
#'
#' @export
kpss_test <- function(object, null = "Level", ..., .alpha = 0.05) {
  UseMethod("kpss_test")
}

#' @rdname kpss_test
#' @export
kpss_test.lm <- function(object, null = "Level", ..., .alpha = 0.05) {
  kpss_test_impl(calc_residuals(object), null = null, ..., .alpha = .alpha)
}

#' @rdname kpss_test
#' @export
kpss_test._lm <- function(object, null = "Level", ..., .alpha = 0.05) {
  kpss_test_impl(
    calc_residuals(object[["fit"]]), null = null, ..., .alpha = .alpha
  )
}

#' @rdname kpss_test
#' @export
kpss_test_vec <- function(x, null = "Level", ..., .alpha = 0.05) {
  kpss_test_impl(x, null = null, ..., .alpha = .alpha)
}


# Phillips-Perron Unit Root Test ----------------------------------------------

#' Run a Phillips-Perron Unit Root Test
#'
#' The hypotheses for this test are:
#'
#' * Null: Series has a unit root
#' * Alternative: Series is either stationary or has explosive root
#'   (user defined)
#'
#' @templateVar link tseries::pp.test
#' @template desc-linreg-tests
#'
#' @family stationarity tests
#' @template return
#'
#' @template params-linreg-obj
#' @template params-linreg-vec
#' @template params-dots
#' @template params-alpha
#' @param alternative (Optional) The alternative hypothesis, either
#'   "stationary" (default) or "explosive".
#'
#' @templateVar fn phillips_perron_test
#' @template examples-linreg-lm
#'
#' @examples
#' phillips_perron_test(mod_lm_fit, alternative = "explosive")
#'
#' @template examples-linreg-linear_reg
#'
#' @examples
#' phillips_perron_test(mod_linreg_fit, alternative = "explosive")
#'
#' @template examples-linreg-tests-vec
#'
#' @examples
#' phillips_perron_test_vec(resids, alternative = "explosive")
#'
#' @export phillips_perron_test
phillips_perron_test <- function(object,
                                 alternative = "stationary",
                                 ...,
                                 .alpha = 0.05) {
  UseMethod("phillips_perron_test")
}

#' @rdname phillips_perron_test
#' @export
phillips_perron_test.lm <- function(object,
                                    alternative = "stationary",
                                    ...,
                                    .alpha = 0.05) {
  phillips_perron_test_impl(
    calc_residuals(object), alternative, ..., .alpha = .alpha
  )
}

#' @rdname phillips_perron_test
#' @export
phillips_perron_test._lm <- function(object,
                                     alternative = "stationary",
                                     ...,
                                     .alpha = 0.05) {
  phillips_perron_test_impl(
    calc_residuals(object[["fit"]]), alternative, ..., .alpha = .alpha
  )
}

#' @rdname phillips_perron_test
#' @export
phillips_perron_test_vec <- function(x,
                                     alternative = "stationary",
                                     ...,
                                     .alpha = 0.05) {
  phillips_perron_test_impl(x, alternative, ..., .alpha = .alpha)
}


# Helper Functions ------------------------------------------------------------
aug_dickey_fuller_test_impl <- function(resids,
                                        alternative = "stationary",
                                        ...,
                                        .alpha = 0.05) {
  tidy_test(
    resids,
    adf.test,
    alternative = alternative,
    ...,
    .test  = "Augmented Dickey-Fuller",
    .null  = "Not Stationary",
    .alt   = "Stationary",
    .alpha = .alpha
  )
}

kpss_test_impl <- function(resids, null = "Level", ..., .alpha = 0.05) {
  tidy_test(
    resids,
    kpss.test,
    null = null,
    ...,
    .test  = "Kwiatkowski-Phillips-Schmidt-Shin",
    .null  = paste(null, "Stationary"),
    .alt   = "Unit Root",
    .alpha = .alpha
  )
}

phillips_perron_test_impl <- function(resids,
                                      alternative = "stationary",
                                      ...,
                                      .alpha = 0.05) {
  tidy_test(
    resids,
    pp.test,
    alternative = alternative,
    ...,
    .test   = "Phillips-Perron",
    .null   = "Unit Root",
    .alpha  = .alpha,
    .alt    = ifelse(
      alternative == "stationary",
      "Stationary",
      "Explosive Root"
    )
  )
}
