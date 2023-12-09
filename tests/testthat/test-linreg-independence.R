# Durbin-Watson ---------------------------------------------------------------
dw_result_lm_tbl <- durbin_watson_test(mod_lm_fit)
dw_result_linreg_tbl <- durbin_watson_test(mod_linreg_fit)
dw_result_linreg2_tbl <- durbin_watson_test(mod_linreg2_fit)


test_that("(Durbin-Watson): a correctly named `tibble` from `lm` model", {
  expect_s3_class(dw_result_lm_tbl, "tbl_df")
  check_tidy_test_names(dw_result_lm_tbl)
})

test_that("(Durbin-Watson): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(dw_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(dw_result_linreg_tbl)
})

test_that("(Durbin-Watson): a correctly named `tibble` from `_glm` model", {
  expect_s3_class(dw_result_linreg2_tbl, "tbl_df")
  check_tidy_test_names(dw_result_linreg2_tbl)
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

test_that("(Durbin-Watson): `.notes` is created correctly for `_glm` model", {
  notes_tbl <- dw_result_linreg2_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})


test_that("(Durbin-Watson): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(dw_result_lm_tbl$test)

  expect_identical(test_name_chr, "Durbin-Watson")
})

test_that("(Durbin-Watson): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(dw_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Durbin-Watson")
})

test_that("(Durbin-Watson): returns the correct test name for `_glm` model", {
  test_name_chr <- as.character(dw_result_linreg2_tbl$test)

  expect_identical(test_name_chr, "Durbin-Watson")
})


test_that("(Durbin-Watson): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(dw_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(dw_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, 1.37)
  expect_equal(test_p_value_dbl, 0.032)
})

test_that("(Durbin-Watson): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(dw_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(dw_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, 1.37)
  expect_equal(test_p_value_dbl, 0.032)
})

test_that("(Durbin-Watson): returns the correct statistics for `_glm` model", {
  test_statistic_dbl <- as.numeric(dw_result_linreg2_tbl$statistic)
  test_p_value_dbl <- as.numeric(dw_result_linreg2_tbl$p_value)

  expect_equal(test_statistic_dbl, 1.37)
  expect_equal(test_p_value_dbl, 0.032)
})


test_that("(Durbin-Watson): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(dw_result_lm_tbl$result)
  test_outcome_chr <- as.character(dw_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Autocorrelation")
})

test_that("(Durbin-Watson): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(dw_result_linreg_tbl$result)
  test_outcome_chr <- as.character(dw_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Autocorrelation")
})

test_that("(Durbin-Watson): returns the correct conclusions for `_glm` model", {
  test_result_chr <- as.character(dw_result_linreg2_tbl$result)
  test_outcome_chr <- as.character(dw_result_linreg2_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Autocorrelation")
})


# Ljung-Box -------------------------------------------------------------------
lb_result_vec_tbl <- ljung_box_test_vec(resids)
lb_result_lm_tbl <- ljung_box_test(mod_lm_fit)
lb_result_linreg_tbl <- ljung_box_test(mod_linreg_fit)
lb_result_linreg2_tbl <- ljung_box_test(mod_linreg2_fit)


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

test_that("(Ljung-Box): a correctly named `tibble` from `_glm` model", {
  expect_s3_class(lb_result_linreg2_tbl, "tbl_df")
  check_tidy_test_names(lb_result_linreg2_tbl)
})


test_that("(Ljung-Box): `.notes` is created correctly for numeric input", {
  notes_tbl <- lb_result_vec_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})

test_that("(Ljung-Box): `.notes` is created correctly for `lm` model", {
  notes_tbl <- lb_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})

test_that("(Ljung-Box): `.notes` is created correctly for `_lm` model", {
  notes_tbl <- lb_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})

test_that("(Ljung-Box): `.notes` is created correctly for `_glm` model", {
  notes_tbl <- lb_result_linreg2_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})


test_that("(Ljung-Box): returns the correct test name for numeric input", {
  test_name_chr <- as.character(lb_result_vec_tbl$test)

  expect_identical(test_name_chr, "Ljung-Box")
})

test_that("(Ljung-Box): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(lb_result_lm_tbl$test)

  expect_identical(test_name_chr, "Ljung-Box")
})

test_that("(Ljung-Box): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(lb_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Ljung-Box")
})

test_that("(Ljung-Box): returns the correct test name for `_glm` model", {
  test_name_chr <- as.character(lb_result_linreg2_tbl$test)

  expect_identical(test_name_chr, "Ljung-Box")
})


test_that("(Ljung-Box): returns the correct statistics for numeric input", {
  test_statistic_dbl <- as.numeric(lb_result_vec_tbl$statistic)
  test_p_value_dbl <- as.numeric(lb_result_vec_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.39)
  expect_equal(test_p_value_dbl, 0.531)
})

test_that("(Ljung-Box): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(lb_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(lb_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, 3.01)
  expect_equal(test_p_value_dbl, 0.083)
})

test_that("(Ljung-Box): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(lb_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(lb_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, 3.01)
  expect_equal(test_p_value_dbl, 0.083)
})

test_that("(Ljung-Box): returns the correct statistics for `_glm` model", {
  test_statistic_dbl <- as.numeric(lb_result_linreg2_tbl$statistic)
  test_p_value_dbl <- as.numeric(lb_result_linreg2_tbl$p_value)

  expect_equal(test_statistic_dbl, 3.01)
  expect_equal(test_p_value_dbl, 0.083)
})


test_that("(Ljung-Box): returns the correct conclusions for numeric input", {
  test_result_chr <- as.character(lb_result_vec_tbl$result)
  test_outcome_chr <- as.character(lb_result_vec_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "No Autocorrelation")
})

test_that("(Ljung-Box): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(lb_result_lm_tbl$result)
  test_outcome_chr <- as.character(lb_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "No Autocorrelation")
})

test_that("(Ljung-Box): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(lb_result_linreg_tbl$result)
  test_outcome_chr <- as.character(lb_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "No Autocorrelation")
})

test_that("(Ljung-Box): returns the correct conclusions for `_glm` model", {
  test_result_chr <- as.character(lb_result_linreg2_tbl$result)
  test_outcome_chr <- as.character(lb_result_linreg2_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "No Autocorrelation")
})
