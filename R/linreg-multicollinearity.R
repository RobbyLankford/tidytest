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
#' @template examples-linreg-tests
#'
#' @export
identify_multicollinearity <- function(object) {
  UseMethod("identify_multicollinearity")
}

#' @rdname identify_multicollinearity
#' @export
identify_multicollinearity.lm <- function(object) {
  identify_multicollinearity_spec(object)
}

#' @rdname identify_multicollinearity
#' @export
identify_multicollinearity._lm <- function(object) {
  identify_multicollinearity_spec(object[["fit"]])
}


# Helper Functions ------------------------------------------------------------
calculate_vifs <- function(object) {
  car::vif(object)
}

format_vifs <- function(x) {
  if (is.matrix(x)) x[ ,ncol(x)] else x
}

finalize_vifs <- function(x) {
  vifs_tbl <- dplyr::tibble(
    variable = names(x),
    vif = as.numeric(x)
  )

  dplyr::mutate(
    vifs_tbl,

    result = dplyr::case_when(
      vif == 1 ~ "not correlated",
      vif > 5  ~ "highly correlated",
      TRUE     ~ "moderately correlated"
    )
  )
}

identify_multicollinearity_spec <- function(object) {
  object %>%
    calculate_vifs() %>%
    format_vifs() %>%
    finalize_vifs()
}
