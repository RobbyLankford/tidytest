#' Calculate Multicollinearity of Variables in a Linear Regression
#'
#' A wrapper around \code{\link[car]{vif}} that standardizes the inputs and
#' outputs.
#'
#' @inheritParams bruesch_pagan_test
#'
#' @return A [tibble][tibble::tibble-package].
#'
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(tidytest)
#'
#' #> `lm` Method
#' mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)
#'
#' calculate_vifs(mod_lm_fit)
#'
#' #> Tidymodels Method
#' mod_linreg_fit <- linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ disp + wt + hp, data = mtcars)
#'
#' calculate_vifs(mod_linreg_fit)
#'
#' @export
calculate_vifs <- function(object) {
  UseMethod("calculate_vifs")
}

#' @rdname calculate_vifs
#' @export
calculate_vifs.default <- function(object) {
  stop("No method for object of class ", class(object))
}

#' @rdname calculate_vifs
#' @export
calculate_vifs.lm <- function(object) {
  calculate_vifs_spec(object)
}

#' @rdname calculate_vifs
#' @export
calculate_vifs._lm <- function(object) {
  calculate_vifs_spec(object[["fit"]])
}


# Helper Functions ------------------------------------------------------------
calculate_vifs_spec <- function(object) {
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
