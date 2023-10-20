# Models ----

## Points ----
set.seed(1914)
resids <- rnorm(100)

## `lm` ----
mod_lm_fit <- lm(mpg ~ disp + wt + hp, data = mtcars)

## `linear_reg` ----
mod_linreg_fit <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm") %>%
  parsnip::fit(mpg ~ disp + wt + hp, data = mtcars)

# Functions ----
check_names <- function(.data, .names) {
  expect_identical(names(.data), .names)
}

check_tidy_test_names <- function(.data) {
  check_names(
    .data,
    c("test", "statistic", "p_value", "result", "outcome", ".notes")
  )
}
