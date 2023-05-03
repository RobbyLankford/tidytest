#' Calculate Multicollinearity of Variables in a Linear Regression
#'
#' A wrapper around \code{\link[car]{vif}} that standardizes the inputs and
#' outputs.
#'
#' @param object A model object (such as a fitted `lm` object).
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(tidytest)
#'
#' mod_fit <- linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ disp + wt + hp, data = mtcars)
#'
#' calculate_vifs(mod_fit)
#'
#' @export
calculate_vifs <- function(object) {
  UseMethod("calculate_vifs")
}

#' @rdname calculate_vifs
#' @export
calculate_vifs.lm <- function(object) {
  vifs <- car::vif(object)

  vifs_num <- if (is.matrix(vifs)) vifs[ ,ncol(vifs)] else vifs

  vifs_tbl <- dplyr::tibble(
    variable = names(vifs_num),
    vif = as.numeric(vifs_num)
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

#' @rdname calculate_vifs
#' @export
calculate_vifs._lm <- function(object) {
  calculate_vifs.lm(object[["fit"]])
}
