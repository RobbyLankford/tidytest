# Anderson-Darling ------------------------------------------------------------
ad_result_vec_tbl <- anderson_darling_test_vec(resids)
ad_result_lm_tbl <- anderson_darling_test(mod_lm_fit)
ad_result_linreg_tbl <- anderson_darling_test(mod_linreg_fit)

test_that("(Anderson-Darling): a correctly named `tibble` from numeric input", {
  expect_s3_class(ad_result_vec_tbl, "tbl_df")
  check_tidy_test_names(ad_result_vec_tbl)
})

test_that("(Anderson-Darling): a correctly named `tibble` from `lm` model", {
  expect_s3_class(ad_result_lm_tbl, "tbl_df")
  check_tidy_test_names(ad_result_lm_tbl)
})

test_that("(Anderson-Darling): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(ad_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(ad_result_linreg_tbl)
})

test_that("(Anderson-Darling): `.notes` is created correctly for numeric input", {
  notes_tbl <- ad_result_vec_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})

test_that("(Anderson-Darling): `.notes` is created correctly for `lm` input", {
  notes_tbl <- ad_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})

test_that("(Anderson-Darling): `.notes` is created correctly for `_lm` input", {
  notes_tbl <- ad_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})


# Shapiro-Wilk ----------------------------------------------------------------
sw_result_vec_tbl <- shapiro_wilk_test_vec(resids)
sw_result_lm_tbl <- shapiro_wilk_test(mod_lm_fit)
sw_result_linreg_tbl <- shapiro_wilk_test(mod_linreg_fit)

test_that("(Shapiro-Wilk): a correctly named `tibble` from numeric input", {
  expect_s3_class(sw_result_vec_tbl, "tbl_df")
  check_tidy_test_names(sw_result_vec_tbl)
})

test_that("(Shapiro-Wilk): a correctly named `tibble` from `lm` model", {
  expect_s3_class(sw_result_lm_tbl, "tbl_df")
  check_tidy_test_names(sw_result_lm_tbl)
})

test_that("(Shapiro-Wilk): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(sw_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(sw_result_linreg_tbl)
})

test_that("(Shapiro-Wilk): `.notes` is created correctly for numeric input", {
  notes_tbl <- sw_result_vec_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})

test_that("(Shapiro-Wilk): `.notes` is created correctly for `lm` input", {
  notes_tbl <- sw_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})

test_that("(Shapiro-Wilk): `.notes` is created correctly for `_lm` input", {
  notes_tbl <- sw_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})
