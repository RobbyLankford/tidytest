#' Calculate Autocorrelation Function (ACF) or Partial Autocorrelation Function
#'   (PACF) Values
#'
#' Wrappers around [stats::acf()] and [stats::pacf()] that standardize the
#' inputs and outputs.
#'
#' @param x A numeric vector.
#' @param .lags (Optional) Maximum lag value used in calculations. If omitted,
#'   will be limited to one less than the number of observations in `x`.
#' @param .conf (Optional) Confidence interval coverage probability. Default is
#'   for a 95% confidence interval.
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @seealso [plot_acf()] [plot_pacf()]
#'
#' @examples
#' library(tidytest)
#'
#' set.seed(1914)
#' x <- rnorm(100)
#'
#' #> ACF
#' calculate_acf(x)
#' calculate_acf(x, .lags = 10, .conf = 0.8)
#'
#' #> PACF
#' calculate_pacf(x)
#' calculate_pacf(x, .lags = 10, .conf = 0.8)
#'
#' @export
calculate_acf <- function(x, .lags = NULL, .conf = 0.95) {
  UseMethod("calculate_acf")
}

#' @rdname calculate_acf
#' @export
calculate_acf.numeric <- function(x, .lags = NULL, .conf = 0.95) {
  calculate_acf_impl(x, .lags = .lags, .conf = .conf, .type = "correlation")
}

#' @rdname calculate_acf
#' @export
calculate_pacf <- function(x, .lags = NULL, .conf = 0.95) {
  UseMethod("calculate_pacf")
}

#' @rdname calculate_acf
#' @export
calculate_pacf.numeric <- function(x, .lags = NULL, .conf = 0.95) {
  calculate_acf_impl(x, .lags = .lags, .conf = .conf, .type = "partial")
}


# Helper Functions ------------------------------------------------------------
calculate_acf_impl <- function(x,
                               .lags,
                               .conf,
                               .type = c("correlation", "partial")) {
  acfs_lst <- stats::acf(x, lag.max = .lags, type = .type[[1]], plot = FALSE)

  col_chr <- if (.type[[1]] == "correlation") "acf" else "pacf"

  dplyr::tibble(
    lag = as.factor(as.numeric(acfs_lst[["lag"]])),

    {{ col_chr }} := as.numeric(acfs_lst[["acf"]]),

    .conf_lo = calc_conf(.conf, acfs_lst[["n.used"]], .negate = TRUE),
    .conf_hi = calc_conf(.conf, acfs_lst[["n.used"]])
  )
}

calc_conf <- function(ci, n, .negate = FALSE) {
  ci_num <- stats::qnorm((1 + ci) / 2) / sqrt(n)

  if (.negate) ci_num <- (-1 * ci_num)

  ci_num
}
