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

mod_linreg2_fit <- parsnip::linear_reg() %>%
  parsnip::set_engine("glm") %>%
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

check_notes_names <- function(.data) {
  check_names(.data, c("parameters", "null", "alt"))
}

check_no_params <- function(.data) {
  check_names(.data, "None")
}

check_params_df <- function(.data) {
  check_names(.data, "df")
}

check_params_df1_df2 <- function(.data) {
  check_names(.data, c("df1", "df2"))
}
