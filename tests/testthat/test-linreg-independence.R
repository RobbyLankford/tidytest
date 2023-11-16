# Durbin-Watson ---------------------------------------------------------------
dw_result_lm_tbl <- durbin_watson_test(mod_lm_fit)
dw_result_linreg_tbl <- durbin_watson_test(mod_linreg_fit)

test_that("(Durbin-Watson): a correctly named `tibble` from `lm` model", {
  expect_s3_class(dw_result_lm_tbl, "tbl_df")
  check_tidy_test_names(dw_result_lm_tbl)
})

test_that("(Durbin-Watson): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(dw_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(dw_result_linreg_tbl)
})

test_that("(Durbin-Watson): `.notes` is created correctly for `lm` model", {
  notes_tbl <- dw_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})

test_that("(Durbin-Watson): `.notes` is created correctly for `_lm` model", {
  notes_tbl <- dw_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})


# Ljung-Box -------------------------------------------------------------------
lb_result_vec_tbl <- ljung_box_test_vec(resids)
lb_result_lm_tbl <- ljung_box_test(mod_lm_fit)
lb_result_linreg_tbl <- ljung_box_test(mod_linreg_fit)

test_that("(Ljung-Box): a correctly named `tibble` from numeric input", {
  expect_s3_class(lb_result_vec_tbl, "tbl_df")
  check_tidy_test_names(lb_result_vec_tbl)
})

test_that("(Ljung-Box): a correctly named `tibble` from `lm` model", {
  expect_s3_class(lb_result_lm_tbl, "tbl_df")
  check_tidy_test_names(lb_result_lm_tbl)
})

test_that("(Ljung-Box): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(lb_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(lb_result_linreg_tbl)
})

test_that("(Ljung-Box): `.notes` is created correctly for numeric input", {
  notes_tbl <- lb_result_vec_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})

test_that("(Ljung-Box): `.notes` is created correctly for `lm` input", {
  notes_tbl <- lb_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})

test_that("(Ljung-Box): `.notes` is created correctly for `_lm` input", {
  notes_tbl <- lb_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})
