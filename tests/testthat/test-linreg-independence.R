# Durbin-Watson ---------------------------------------------------------------
test_that("(Durbin-Watson): a correctly named `tibble` from `lm` model", {
  result_tbl <- durbin_watson_test(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Durbin-Watson): a correctly named `tibble` from `_lm` model", {
  result_tbl <- durbin_watson_test(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})


# Ljung-Box -------------------------------------------------------------------
test_that("(Ljung-Box): a correctly named `tibble` from numeric input", {
  result_tbl <- ljung_box_test_vec(resids)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Ljung-Box): a correctly named `tibble` from `lm` model", {
  result_tbl <- ljung_box_test(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Ljung-Box): a correctly named `tibble` from `_lm` model", {
  result_tbl <- ljung_box_test(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})
