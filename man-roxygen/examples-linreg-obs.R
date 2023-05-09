#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(tidytest)
#'
#' #> `lm` Method
#' mod_lm_fit <- lm(mpg ~ ., data = mtcars)
#'
#' <%=fn %>(mod_lm_fit)
#' <%=fn %>(mod_lm_fit, id = rownames(mtcars))
