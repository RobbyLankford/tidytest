test_that("a correctly named `tibble` from `lm` model", {
  result_tbl <- identify_multicollinearity(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_names(result_tbl, c("variable", "vif", "result"))
})

test_that("a correctly named `tibble` from `_lm` model", {
  result_tbl <- identify_multicollinearity(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_names(result_tbl, c("variable", "vif", "result"))
})
