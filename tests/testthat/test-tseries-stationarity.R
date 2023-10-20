# Augmented Dickey-Fuller -----------------------------------------------------
test_that("(ADF): a correctly named `tibble` from numeric input", {
  result_tbl <- suppressWarnings(aug_dickey_fuller_test_vec(resids))

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(ADF): a correctly named `tibble` from `lm` model", {
  result_tbl <- suppressWarnings(aug_dickey_fuller_test(mod_lm_fit))

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(ADF): a correctly named `tibble` from `_lm` model", {
  result_tbl <- suppressWarnings(aug_dickey_fuller_test(mod_linreg_fit))

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})


# Kwiatkowski-Phillips-Schmidt-Shin -------------------------------------------
test_that("(KPSS): a correctly named `tibble` from numeric input", {
  result_tbl <- suppressWarnings(kpss_test_vec(resids))

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(KPSS): a correctly named `tibble` from `lm` model", {
  result_tbl <- suppressWarnings(kpss_test(mod_lm_fit))

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(KPSS): a correctly named `tibble` from `_lm` model", {
  result_tbl <- suppressWarnings(kpss_test(mod_linreg_fit))

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

# Phillips-Perron -------------------------------------------------------------
test_that("(Phillips-Perron): a correctly named `tibble` from numeric input", {
  result_tbl <- suppressWarnings(phillips_perron_test_vec(resids))

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Phillips-Perron): a correctly named `tibble` from `lm` model", {
  result_tbl <- suppressWarnings(phillips_perron_test(mod_lm_fit))

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Phillips-Perron): a correctly named `tibble` from `_lm` model", {
  result_tbl <- suppressWarnings(phillips_perron_test(mod_linreg_fit))

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})
