# Predictors vs Residuals Plot ------------------------------------------------

#' Create a Plot of Predictors vs Residuals
#'
#' A predictors vs residuals plot is useful for visually inspecting if a linear
#' regression model violates both the Linear and Equal Variance assumptions
#' of its residuals. The plot plots predicted values on the x-axis and
#' residuals on the y-axis. If the resulting scatterplot shows a discernible
#' pattern, then either or both of the assumptions are likely violated.
#'
#' @inheritParams bruesch_pagan_test
#' @param .hline (Optional) The `linetype` of the line drawn at the `y = 0`.
#'
#' @return A [`ggplot`][ggplot2::ggplot] object.
#'
#' @references Kutner, M., Nachtsheim, C., Neter, J. and Li, W. (2005).
#'   _Applied Linear Statistical Models_. ISBN: 0-07-238688-6.
#'   McGraw-Hill/Irwin.
#'
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(tidytest)
#'
#' #> `lm` Method
#' mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)
#'
#' plot_pred_vs_resid(mod_lm_fit)
#'
#' @export
plot_pred_vs_resid <- function(object, .hline = "dashed") {
  UseMethod("plot_pred_vs_resid")
}

#' @rdname plot_pred_vs_resid
#' @export
plot_pred_vs_resid.lm <- function(object, .hline = "dashed") {
  object %>%
    get_preds_vs_resid() %>%
    plot_pred_vs_resid_(.hline = .hline)
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
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(tidytest)
#'
#' #> `lm` Method
#' mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)
#'
#' plot_qq_norm(mod_lm_fit)
#'
#' @export
plot_qq_norm <- function(object) {
  UseMethod("plot_qq_norm")
}

#' @rdname plot_qq_norm
#' @export
plot_qq_norm.lm <- function(object) {
  object %>%
    get_resid() %>%
    plot_qq_norm_()
}


# Helpers ---------------------------------------------------------------------
get_preds_vs_resid <- function(object, ...) {
  dplyr::tibble(
    .pred = get_predictions(object, ...),
    .resid = get_residuals(object)
  )
}

get_resid <- function(object) {
  dplyr::tibble(.resid = get_residuals(object))
}

plot_pred_vs_resid_ <- function(.data, .hline) {
  .data %>%
    ggplot2::ggplot(ggplot2::aes(x = .pred, y = .resid)) +
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
