#' Calculate Multicollinearity of Variables in a Linear Regression
#'
#' @templateVar link car::vif
#' @template desc-linreg-tests
#'
#' @template return
#'
#' @template params-linreg-obj
#'
#' @templateVar fn identify_multicollinearity
#' @template examples-linreg-lm
#' @template examples-linreg-linear_reg
#'
#' @export
identify_multicollinearity <- function(object) {
  UseMethod("identify_multicollinearity")
}

#' @rdname identify_multicollinearity
#' @export
identify_multicollinearity.lm <- function(object) {
  identify_multicollinearity_impl(object)
}

#' @rdname identify_multicollinearity
#' @export
identify_multicollinearity._lm <- function(object) {
  identify_multicollinearity_impl(object[["fit"]])
}


# Helper Functions ------------------------------------------------------------
check_enough_terms <- function(object) {
  #> Need at least two terms for VIFs to be calculated
  coefs <- object[["coefficients"]]

  length(coefs[names(coefs) != "(Intercept)"]) > 1
}

calculate_vifs <- function(object) {
  check_installed("car", reason = "to use `identify_multicollinearity()`")

  car::vif(object)
}

format_vifs <- function(x) {
  if (is.matrix(x)) x[ ,ncol(x)] else x
}

finalize_vifs <- function(x) {
  vifs_tbl <- new_tibble(df_list(variable = names(x), vif = as.numeric(x)))

  vifs_tbl$result <- "moderately correlated"
  vifs_tbl$result[vifs_tbl$vif == 1] <- "not correlated"
  vifs_tbl$result[vifs_tbl$vif > 5] <- "highly correlated"

  vifs_tbl
}

identify_multicollinearity_impl <- function(object, .call = caller_env()) {
  if (!check_enough_terms(object)) {
    cli_abort(c(
      "Model contains fewer than 2 terms. VIFs cannot be calculated."
    ), call = .call)
  }

  vifs_raw_tbl <- calculate_vifs(object)
  vifs_fmt_tbl <- format_vifs(vifs_raw_tbl)

  finalize_vifs(vifs_fmt_tbl)
}
