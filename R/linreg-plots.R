# Predictions and Residuals ---------------------------------------------------

#' Calculate Predictions and Residuals From a Model
#'
#' From a model, calculate the predicted values from a set of data and then
#' calculate the residuals of those predictions.
#'
#' @details
#' ## Known Limitations
#' This function will not work if transformations to variables are done in the
#' model formula (*e.g.*, `lm(mpg ~ wt + as.factor(vs), data = mtcars)`).
#' Variable transformations should be done to the data set before creating the
#' model formula.
#'
#' @template params-linreg-obj
#' @param data (Optional) a data set used to create predictions (if omitted,
#'   the fitted values are used).
#'
#' @return A [tibble][tibble::tibble-package] with columns `.pred` and `.resid`.
#'
#' @templateVar fn calculate_residuals
#' @template examples-linreg-lm
#' @examples
#' calculate_residuals(mod_lm_fit, mtcars[1:15, ])
#'
#' @export
calculate_residuals <- function(object, data = NULL) {
  UseMethod("calculate_residuals")
}

#' @rdname calculate_residuals
#' @export
calculate_residuals.lm <- function(object, data = NULL) {
  if (rlang::is_null(data)) {
    data <- object[["model"]]
  }

  terms_lst <- object[["terms"]]
  vars_chr <- as.character(attr(terms_lst, "variables"))
  idx_num <- as.numeric(attr(terms_lst, "response"))

  yvar_chr <- vars_chr[[idx_num + 1]]

  calculate_residuals_impl(object, data, yvar_chr)
}


# Predictions vs Residuals Plot -----------------------------------------------

#' Create a Plot of Predictions vs Residuals
#'
#' A predictors vs residuals plot is useful for visually inspecting if a linear
#' regression model violates both the Linear and Equal Variance assumptions
#' of its residuals. The plot plots predicted values on the x-axis and
#' residuals on the y-axis. If the resulting scatterplot shows a discernible
#' pattern, then either or both of the assumptions are likely violated.
#'
#' @param .data The output of [calculate_residuals()].
#' @param .hline (Optional) The `linetype` of the line drawn at `y = 0`.
#'
#' @return A [`ggplot`][ggplot2::ggplot] object.
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005).
#'   _Applied Linear Statistical Models_. ISBN: 0-07-238688-6.
#'   McGraw-Hill/Irwin.
#'
#' @seealso [calculate_residuals()]
#'
#' @examples
#' library(tidytest)
#'
#' mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)
#'
#' pred_vs_resid_tbl <- calculate_residuals(mod_lm_fit)
#'
#' plot_predictions_vs_residuals(pred_vs_resid_tbl)
#'
#' @export
plot_predictions_vs_residuals <- function(.data, .hline = "dashed") {
  UseMethod("plot_predictions_vs_residuals")
}

#' @rdname plot_predictions_vs_residuals
#' @export
plot_predictions_vs_residuals.data.frame <- function(.data, .hline = "dashed") {
  plot_predictions_vs_residuals_impl(.data, .hline = .hline)
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
#' @inheritParams plot_predictions_vs_residuals
#'
#' @return A [`ggplot`][ggplot2::ggplot] object.
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005).
#'   _Applied Linear Statistical Models_. ISBN: 0-07-238688-6.
#'   McGraw-Hill/Irwin.
#'
#' @seealso [calculate_residuals()]
#'
#' @examples
#' library(tidytest)
#'
#' mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)
#'
#' pred_vs_resid_tbl <- calculate_residuals(mod_lm_fit)
#'
#' plot_qq_normality(pred_vs_resid_tbl)
#'
#' @export
plot_qq_normality <- function(.data) {
  UseMethod("plot_qq_normality")
}

#' @rdname plot_qq_normality
#' @export
plot_qq_normality.data.frame <- function(.data) {
  plot_qq_normality_impl(.data)
}


# Helpers ---------------------------------------------------------------------
calculate_residuals_impl <- function(object, data, yvar) {
  acts_num <- data[[yvar]]
  preds_num <- as.numeric(stats::predict(object, data))
  resids_num <- acts_num - preds_num

  dplyr::tibble(
    .pred = preds_num,
    .resid = resids_num
  )
}

plot_predictions_vs_residuals_impl <- function(.data, .hline) {
  ggplot2::ggplot(
    data = .data,
    mapping = ggplot2::aes(x = .pred, y = .resid)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = .hline) +
    ggplot2::labs(x = "Predictions", y = "Residuals")
}

plot_qq_normality_impl <- function(.data) {
  ggplot2::ggplot(data = .data, mapping = ggplot2::aes(sample = .resid)) +
    qqplotr::stat_qq_band() +
    qqplotr::stat_qq_line() +
    qqplotr::stat_qq_point() +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
}
