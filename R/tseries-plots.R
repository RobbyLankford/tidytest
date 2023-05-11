# ACF & PACF Plots ------------------------------------------------------------

#' Plot Autocorrelation Function (ACF) or Partial Autocorrelation Function
#'   (PACF) Values
#'
#' @param .data The output of [calculate_acf()] or [calculate_pacf()].
#' @param .color (Optional) The color of the plot's bars. Default is black.
#' @param .conf_color (Optional) The color of the confidence interval lines.
#'   Default is blue.
#' @param .title (Optional) The title for the plot.
#' @param .origin_width (Optional) The width of the line at `y = 0`.
#' @param .conf_width (Optional) The width of the confidence interval lines.
#'
#' @return A [`ggplot`][ggplot2::ggplot] object.
#'
#' @seealso [calculate_acf()] [calculate_pacf()]
#'
#' @examples
#' library(tidytest)
#'
#' x <- rnorm(100)
#'
#' #> ACF
#' acf_tbl <- calculate_acf(x)
#' plot_acf(acf_tbl)
#'
#' #> PACF
#' pacf_tbl <- calculate_pacf(x)
#' plot_pacf(pacf_tbl)
#'
#' @export
plot_acf <- function(.data,
                     .color = "black",
                     .conf_color = "blue",
                     .title = "ACF Plot",
                     .origin_width = 1,
                     .conf_width = 0.5) {
  UseMethod("plot_acf")
}

#' @rdname plot_acf
#' @export
plot_acf.data.frame <- function(.data,
                                .color = "black",
                                .conf_color = "blue",
                                .title = "ACF Plot",
                                .origin_width = 1,
                                .conf_width = 0.5) {
  plot_acf_(
    .data, acf,
    .color = .color,
    .conf_color = .conf_color,
    .title = .title,
    .origin_width = .origin_width,
    .conf_width = 0.5
  )
}

#' @rdname plot_acf
#' @export
plot_pacf <- function(.data,
                      .color = "black",
                      .conf_color = "blue",
                      .title = "PACF Plot",
                      .origin_width = 1,
                      .conf_width = 0.5) {
  UseMethod("plot_pacf")
}

#' @rdname plot_acf
#' @export
plot_pacf.data.frame <- function(.data,
                                 .color = "black",
                                 .conf_color = "blue",
                                 .title = "PACF Plot",
                                 .origin_width = 1,
                                 .conf_width = 0.5) {
  plot_acf_(
    .data, pacf,
    .color = .color,
    .conf_color = .conf_color,
    .title = .title,
    .origin_width = .origin_width,
    .conf_width = 0.5
  )
}


# Helper Functions ------------------------------------------------------------
plot_acf_ <- function(.data,
                      col,
                      .color,
                      .conf_color,
                      .title,
                      .origin_width,
                      .conf_width) {
  ggplot2::ggplot(
    data = .data,
    mapping = ggplot2::aes(x = lag, y = {{ col }})
  ) +
    ggplot2::geom_col(fill = .color, color = .color) +
    ggplot2::geom_hline(
      mapping = ggplot2::aes(yintercept = .conf_lo),
      linetype = "dashed",
      color = .conf_color,
      linewidth = .conf_width
    ) +
    ggplot2::geom_hline(
      mapping = ggplot2::aes(yintercept = 0),
      linewidth = .origin_width
    ) +
    ggplot2::geom_hline(
      mapping = ggplot2::aes(yintercept = .conf_hi),
      linetype = "dashed",
      color = .conf_color,
      linewidth = .conf_width
    ) +
    ggplot2::ggtitle(.title)
}
