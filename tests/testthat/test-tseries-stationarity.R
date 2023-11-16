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

test_that("(ADF): `.notes` is created correctly for `lm` input", {
  notes_tbl <- adf_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Lag order")
})

test_that("(ADF): `.notes` is created correctly for `_lm` input", {
  notes_tbl <- adf_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_names(param_tbl, "Lag order")
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
