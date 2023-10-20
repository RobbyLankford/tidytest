# Ramsey's RESET --------------------------------------------------------------
test_that("(Ramsey's RESET): a correctly named `tibble` from `lm` model", {
  result_tbl <- ramsey_reset_test(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Ramsey's RESET): a correctly named `tibble` from `_lm` model", {
  result_tbl <- ramsey_reset_test(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

# Harvey-Collier --------------------------------------------------------------
test_that("(Harvey-Collier): a correctly named `tibble` from `lm` model", {
  result_tbl <- harvey_collier_test(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Harvey-Collier): a correctly named `tibble` from `_lm` model", {
  result_tbl <- harvey_collier_test(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})
