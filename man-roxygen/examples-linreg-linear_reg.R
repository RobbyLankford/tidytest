#' @examples
#'
#' #> `linear_reg` Method
#' library(parsnip)
#'
#' mod_linreg_fit <- linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ disp + wt + hp, data = mtcars)
#'
#' <%=fn %>(mod_linreg_fit)
