mod_lm_1var_fit <- lm(mpg ~ wt, data = mtcars)

mod_linreg_1var_fit <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm") %>%
  parsnip::fit(mpg ~ wt, data = mtcars)

result_lm_tbl <- identify_multicollinearity(mod_lm_fit)
result_linreg_tbl <- identify_multicollinearity(mod_linreg_fit)


test_that("a correctly named `tibble` from `lm` model", {
  expect_s3_class(result_lm_tbl, "tbl_df")
  check_names(result_lm_tbl, c("variable", "vif", "result"))
})

test_that("a correctly named `tibble` from `_lm` model", {
  expect_s3_class(result_linreg_tbl, "tbl_df")
  check_names(result_linreg_tbl, c("variable", "vif", "result"))
})


test_that("variables with multicollinearity are returned for `lm` model", {
  expect_snapshot(result_lm_tbl)
})

test_that("variables with multicollinearity are returned for `_lm` model", {
  expect_snapshot(result_linreg_tbl)
})


test_that("an error is thrown for a `lm` model with only one variable", {
  expect_error(identify_multicollinearity(mod_lm_1var_fit))
})

test_that("an error is thrown for a `_lm` model with only one variable", {
  expect_error(identify_multicollinearity(mod_linreg_1var_fit))
})
