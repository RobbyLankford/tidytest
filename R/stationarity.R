#' Run an Augmented Dickey-Fuller Test
#'
#' A wrapper around \code{\link[tseries]{adf.test}} that standardizes the
#' inputs and outputs.
#'
#' @inheritParams tseries::adf.test
#' @param ... further arguments passed to \code{\link[tseries]{adf.test}}.
#' @param .alpha critical p-value used to determine test conclusion.
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @examples
#' set.seed(123)
#' x <- runif(n = 1000)
#'
#' adf_test(x)
#' adf_test(x, alternative = "explosive")
#'
#' @export
adf_test <- function(x, alternative = "stationary", ..., .alpha = 0.05) {
  suppressWarnings(tseries::adf.test(x, alternative = alternative, ...)) %>%
    tidy_test(method, statistic, p.value) %>%
    finalize_test(
      null        = "Non-Stationary",
      alternative = alternative
    )
}


#' Run a Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test
#'
#' A wrapper around \code{\link[tseries]{kpss.test}} that standardizes the
#' inputs and outputs.
#'
#' @inheritParams tseries::kpss.test
#' @param ... further arguments passed to \code{\link[tseries]{adf.test}}.
#' @param .alpha critical p-value used to determine test conclusion.
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @examples
#' set.seed(123)
#' x <- runif(n = 1000)
#'
#' kpss_test(x)
#' kpss_test(x, null = "Trend")
#'
#' @export
kpss_test <- function(x, null = "Level", ..., .alpha = 0.05) {
  suppressWarnings(tseries::kpss.test(x, null = null, ...)) %>%
    tidy_test(method, statistic, p.value) %>%
    finalize_test(
      null        = paste(null, "Stationary"),
      alternative = "Non-Stationary"
    )
}
