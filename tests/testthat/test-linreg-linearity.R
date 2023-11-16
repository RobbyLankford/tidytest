# Ramsey's RESET --------------------------------------------------------------
reset_result_lm_tbl <- ramsey_reset_test(mod_lm_fit)
reset_result_linreg_tbl <- ramsey_reset_test(mod_linreg_fit)

test_that("(Ramsey's RESET): a correctly named `tibble` from `lm` model", {
  expect_s3_class(reset_result_lm_tbl, "tbl_df")
  check_tidy_test_names(reset_result_lm_tbl)
})

test_that("(Ramsey's RESET): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(reset_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(reset_result_linreg_tbl)
})

test_that("(Ramsey's RESET): `.notes` is created correctly for `lm` input", {
  notes_tbl <- reset_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df1_df2(param_tbl)
})

test_that("(Ramsey's RESET): `.notes` is created correctly for `_lm` input", {
  notes_tbl <- reset_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df1_df2(param_tbl)
})

# Harvey-Collier --------------------------------------------------------------
hc_result_lm_tbl <- harvey_collier_test(mod_lm_fit)
hc_result_linreg_tbl <- harvey_collier_test(mod_linreg_fit)

test_that("(Harvey-Collier): a correctly named `tibble` from `lm` model", {
  expect_s3_class(hc_result_lm_tbl, "tbl_df")
  check_tidy_test_names(hc_result_lm_tbl)
})

test_that("(Harvey-Collier): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(hc_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(hc_result_linreg_tbl)
})

test_that("(Harvey-Collier): `.notes` is created correctly for `lm` input", {
  notes_tbl <- hc_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})

test_that("(Harvey-Collier): `.notes` is created correctly for `_lm` input", {
  notes_tbl <- hc_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})
