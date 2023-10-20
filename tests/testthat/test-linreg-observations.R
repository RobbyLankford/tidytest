# Leverage --------------------------------------------------------------------
test_that("a correctly named `tibble` from `lm` model", {
  result_tbl <- identify_extreme_leverages(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_names(result_tbl, c("id", "leverage", ".cutoff"))
})


# Outliers -------------------------------------------------------------------
test_that("a correctly named `tibble` from `lm` model", {
  result_tbl <- identify_outliers(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_names(result_tbl, c("id", "std_resid"))
})


# Influential -----------------------------------------------------------------
test_that("a correctly named `tibble` from `lm` model", {
  result_tbl <- identify_influential_obs(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_names(result_tbl, c("id", "cooks_dist"))
})
