# Anderson-Darling ------------------------------------------------------------
test_that("(Anderson-Darling): a correctly named `tibble` from numeric input", {
  result_tbl <- anderson_darling_test_vec(resids)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Anderson-Darling): a correctly named `tibble` from `lm` model", {
  result_tbl <- anderson_darling_test(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Anderson-Darling): a correctly named `tibble` from `_lm` model", {
  result_tbl <- anderson_darling_test(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})


# Shapiro-Wilk ----------------------------------------------------------------
test_that("(Shapiro-Wilk): a correctly named `tibble` from numeric input", {
  result_tbl <- shapiro_wilk_test_vec(resids)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Shapiro-Wilk): a correctly named `tibble` from `lm` model", {
  result_tbl <- shapiro_wilk_test(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Shapiro-Wilk): a correctly named `tibble` from `_lm` model", {
  result_tbl <- shapiro_wilk_test(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})
