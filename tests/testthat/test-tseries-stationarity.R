# Augmented Dickey-Fuller -----------------------------------------------------
adf_result_vec_tbl <- suppressWarnings(aug_dickey_fuller_test_vec(resids))
adf_result_lm_tbl <- suppressWarnings(aug_dickey_fuller_test(mod_lm_fit))
adf_result_linreg_tbl <- suppressWarnings(aug_dickey_fuller_test(mod_linreg_fit))


test_that("(ADF): a correctly named `tibble` from numeric input", {
  expect_s3_class(adf_result_vec_tbl, "tbl_df")
  check_tidy_test_names(adf_result_vec_tbl)
})

test_that("(ADF): a correctly named `tibble` from `lm` model", {
  expect_s3_class(adf_result_lm_tbl, "tbl_df")
  check_tidy_test_names(adf_result_lm_tbl)
})

test_that("(ADF): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(adf_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(adf_result_linreg_tbl)
})


test_that("(ADF): `.notes` is created correctly for numeric input", {
  notes_tbl <- adf_result_vec_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Lag order")
})

test_that("(ADF): `.notes` is created correctly for `lm` model", {
  notes_tbl <- adf_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Lag order")
})

test_that("(ADF): `.notes` is created correctly for `_lm` model", {
  notes_tbl <- adf_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Lag order")
})


test_that("(ADF): returns the correct test name for numeric input", {
  test_name_chr <- as.character(adf_result_vec_tbl$test)

  expect_identical(test_name_chr, "Augmented Dickey-Fuller")
})

test_that("(ADF): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(adf_result_lm_tbl$test)

  expect_identical(test_name_chr, "Augmented Dickey-Fuller")
})

test_that("(ADF): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(adf_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Augmented Dickey-Fuller")
})


test_that("(ADF): returns the correct statistics for numeric input", {
  test_statistic_dbl <- as.numeric(adf_result_vec_tbl$statistic)
  test_p_value_dbl <- as.numeric(adf_result_vec_tbl$p_value)

  expect_equal(test_statistic_dbl, -4.64)
  expect_equal(test_p_value_dbl, 0.01)
})

test_that("(ADF): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(adf_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(adf_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, -3.04)
  expect_equal(test_p_value_dbl, 0.173)
})

test_that("(ADF): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(adf_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(adf_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, -3.04)
  expect_equal(test_p_value_dbl, 0.173)
})


test_that("(ADF): returns the correct conclusions for numeric input", {
  test_result_chr <- as.character(adf_result_vec_tbl$result)
  test_outcome_chr <- as.character(adf_result_vec_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Stationary")
})

test_that("(ADF): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(adf_result_lm_tbl$result)
  test_outcome_chr <- as.character(adf_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Not Stationary")
})

test_that("(ADF): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(adf_result_linreg_tbl$result)
  test_outcome_chr <- as.character(adf_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Not Stationary")
})


# Kwiatkowski-Phillips-Schmidt-Shin -------------------------------------------
kpss_result_vec_tbl <- suppressWarnings(kpss_test_vec(resids))
kpss_result_lm_tbl <- suppressWarnings(kpss_test(mod_lm_fit))
kpss_result_linreg_tbl <- suppressWarnings(kpss_test(mod_linreg_fit))


test_that("(KPSS): a correctly named `tibble` from numeric input", {
  expect_s3_class(kpss_result_vec_tbl, "tbl_df")
  check_tidy_test_names(kpss_result_vec_tbl)
})

test_that("(KPSS): a correctly named `tibble` from `lm` model", {
  expect_s3_class(kpss_result_lm_tbl, "tbl_df")
  check_tidy_test_names(kpss_result_lm_tbl)
})

test_that("(KPSS): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(kpss_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(kpss_result_linreg_tbl)
})


test_that("(KPSS): `.notes` is created correctly for numeric input", {
  notes_tbl <- kpss_result_vec_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Truncation lag parameter")
})

test_that("(KPSS): `.notes` is created correctly for `lm` input", {
  notes_tbl <- kpss_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Truncation lag parameter")
})

test_that("(KPSS): `.notes` is created correctly for `_lm` input", {
  notes_tbl <- kpss_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Truncation lag parameter")
})


test_that("(KPSS): returns the correct test name for numeric input", {
  test_name_chr <- as.character(kpss_result_vec_tbl$test)

  expect_identical(test_name_chr, "Kwiatkowski-Phillips-Schmidt-Shin")
})

test_that("(KPSS): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(kpss_result_lm_tbl$test)

  expect_identical(test_name_chr, "Kwiatkowski-Phillips-Schmidt-Shin")
})

test_that("(KPSS): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(kpss_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Kwiatkowski-Phillips-Schmidt-Shin")
})


test_that("(KPSS): returns the correct statistics for numeric input", {
  test_statistic_dbl <- as.numeric(kpss_result_vec_tbl$statistic)
  test_p_value_dbl <- as.numeric(kpss_result_vec_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.15)
  expect_equal(test_p_value_dbl, 0.1)
})

test_that("(KPSS): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(kpss_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(kpss_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.14)
  expect_equal(test_p_value_dbl, 0.1)
})

test_that("(KPSS): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(kpss_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(kpss_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.14)
  expect_equal(test_p_value_dbl, 0.1)
})


test_that("(KPSS): returns the correct conclusions for numeric input", {
  test_result_chr <- as.character(kpss_result_vec_tbl$result)
  test_outcome_chr <- as.character(kpss_result_vec_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Level Stationary")
})

test_that("(KPSS): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(kpss_result_lm_tbl$result)
  test_outcome_chr <- as.character(kpss_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Level Stationary")
})

test_that("(KPSS): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(kpss_result_linreg_tbl$result)
  test_outcome_chr <- as.character(kpss_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Level Stationary")
})


# Phillips-Perron -------------------------------------------------------------
pp_result_vec_tbl <- suppressWarnings(phillips_perron_test_vec(resids))
pp_result_lm_tbl <- suppressWarnings(phillips_perron_test(mod_lm_fit))
pp_result_linreg_tbl <- suppressWarnings(phillips_perron_test(mod_linreg_fit))


test_that("(Phillips-Perron): a correctly named `tibble` from numeric input", {
  expect_s3_class(pp_result_vec_tbl, "tbl_df")
  check_tidy_test_names(pp_result_vec_tbl)
})

test_that("(Phillips-Perron): a correctly named `tibble` from `lm` model", {
  expect_s3_class(pp_result_lm_tbl, "tbl_df")
  check_tidy_test_names(pp_result_lm_tbl)
})

test_that("(Phillips-Perron): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(pp_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(pp_result_linreg_tbl)
})


test_that("(Phillips-Perron): `.notes` is created correctly for numeric input", {
  notes_tbl <- pp_result_vec_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Truncation lag parameter")
})

test_that("(Phillips-Perron): `.notes` is created correctly for `lm` input", {
  notes_tbl <- pp_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Truncation lag parameter")
})

test_that("(Phillips-Perron): `.notes` is created correctly for `_lm` input", {
  notes_tbl <- pp_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Truncation lag parameter")
})


test_that("(Phillips-Perron): returns the correct test name for numeric input", {
  test_name_chr <- as.character(pp_result_vec_tbl$test)

  expect_identical(test_name_chr, "Phillips-Perron")
})

test_that("(Phillips-Perron): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(pp_result_lm_tbl$test)

  expect_identical(test_name_chr, "Phillips-Perron")
})

test_that("(Phillips-Perron): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(pp_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Phillips-Perron")
})


test_that("(Phillips-Perron): returns the correct statistics for numeric input", {
  test_statistic_dbl <- as.numeric(pp_result_vec_tbl$statistic)
  test_p_value_dbl <- as.numeric(pp_result_vec_tbl$p_value)

  expect_equal(test_statistic_dbl, -96.25)
  expect_equal(test_p_value_dbl, 0.01)
})

test_that("(Phillips-Perron): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(pp_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(pp_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, -21.95)
  expect_equal(test_p_value_dbl, 0.017)
})

test_that("(Phillips-Perron): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(pp_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(pp_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, -21.95)
  expect_equal(test_p_value_dbl, 0.017)
})


test_that("(Phillips-Perron): returns the correct conclusions for numeric input", {
  test_result_chr <- as.character(pp_result_vec_tbl$result)
  test_outcome_chr <- as.character(pp_result_vec_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Stationary")
})

test_that("(Phillips-Perron): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(pp_result_lm_tbl$result)
  test_outcome_chr <- as.character(pp_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Stationary")
})

test_that("(Phillips-Perron): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(pp_result_linreg_tbl$result)
  test_outcome_chr <- as.character(pp_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Stationary")
})
