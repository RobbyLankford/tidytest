# Predictors vs Residuals Plot ------------------------------------------------

#' Calculate Predictions and Residuals From a Model
#'
#' From a model, calculate the predicted values from a set of data and then
#' calculate the residuals of those predictions.
#'
#' @template params-linreg-obj
#' @param data (Optional) a data set used to create predictions (if omitted,
#'   the fitted values are used).
#'
#' @return A [tibble][tibble::tibble-package] with columns `.pred` and `.resid`.
#'
#' @template examples-linreg
#' @examples
#'
#' calc_pred_vs_resid(mod_lm_fit)
#' calc_pred_vs_resid(mod_lm_fit, mtcars[1:15, ])
#'
#' @export
calc_pred_vs_resid <- function(object, data) {
  UseMethod("calc_pred_vs_resid")
}

#' @rdname calc_pred_vs_resid
#' @export
calc_pred_vs_resid.lm <- function(object, data) {
  if (rlang::is_missing(data)) {
    data <- object[["model"]]
  }

  xvars_chr <- names(object[["coefficients"]])
  vars_chr <- names(object[["model"]])
  yvar_chr <- setdiff(vars_chr, xvars_chr[xvars_chr != "(Intercept)"])

  calc_pred_vs_resid_(object, data, yvar_chr)
}


#' Create a Plot of Predictors vs Residuals
#'
#' A predictors vs residuals plot is useful for visually inspecting if a linear
#' regression model violates both the Linear and Equal Variance assumptions
#' of its residuals. The plot plots predicted values on the x-axis and
#' residuals on the y-axis. If the resulting scatterplot shows a discernible
#' pattern, then either or both of the assumptions are likely violated.
#'
#' @param .data The output of [calc_pred_vs_resid()].
#' @param .hline (Optional) The `linetype` of the line drawn at `y = 0`.
#'
#' @return A [`ggplot`][ggplot2::ggplot] object.
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005).
#'   _Applied Linear Statistical Models_. ISBN: 0-07-238688-6.
#'   McGraw-Hill/Irwin.
#'
#' @template examples-linreg
#' @examples
#'
#' pred_vs_resid_tbl <- calc_pred_vs_resid(mod_lm_fit)
#' plot_pred_vs_resid(pred_vs_resid_tbl)
#'
#' @export
plot_pred_vs_resid <- function(.data, .hline = "dashed") {
  UseMethod("plot_pred_vs_resid")
}

#' @rdname plot_pred_vs_resid
#' @export
plot_pred_vs_resid.data.frame <- function(.data, .hline = "dashed") {
  plot_pred_vs_resid_(.data, .hline = .hline)
}


# Q-Q Plot --------------------------------------------------------------------

#' Create a Quantile-Quantile (Q-Q) Plot
#'
#' A QQ plot is useful for visually inspecting if a linear regression model
#' violates the Normality assumption of its residuals. The plot plots the
#' theoretical quantiles of a standard normal distribution on the x-axis and
#' the sample quantiles of the residuals on the y-axis. If more than just a few
#' of the points fall outside of the confidence band, the assumption is likely
#' violated.
#'
#' @inheritParams plot_pred_vs_resid
#'
#' @return A [`ggplot`][ggplot2::ggplot] object.
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005).
#'   _Applied Linear Statistical Models_. ISBN: 0-07-238688-6.
#'   McGraw-Hill/Irwin.
#'
#' @template examples-linreg
#' @examples
#'
#' pred_vs_resid_tbl <- calc_pred_vs_resid(mod_lm_fit)
#' plot_qq_norm(pred_vs_resid_tbl)
#'
#' @export
plot_qq_norm <- function(.data) {
  UseMethod("plot_qq_norm")
}

#' @rdname plot_qq_norm
#' @export
plot_qq_norm.data.frame <- function(.data) {
  plot_qq_norm_(.data)
}


# Helpers ---------------------------------------------------------------------
calc_pred_vs_resid_ <- function(object, data, yvar) {
  dplyr::tibble(
    .pred = calc_predictions(object, data),
    .resid = calc_residuals(object, data, yvar)
  )
}

plot_pred_vs_resid_ <- function(.data, .hline) {
  ggplot2::ggplot(
    data = .data,
    mapping = ggplot2::aes(x = .pred, y = .resid)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = .hline) +
    ggplot2::labs(x = "Predictions", y = "Residuals")
}

plot_qq_norm_ <- function(.data) {
  ggplot2::ggplot(data = .data, mapping = ggplot2::aes(sample = .resid)) +
    qqplotr::stat_qq_band() +
    qqplotr::stat_qq_line() +
    qqplotr::stat_qq_point() +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
}
