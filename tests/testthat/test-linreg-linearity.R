# Ramsey's RESET --------------------------------------------------------------
reset_result_lm_tbl <- ramsey_reset_test(mod_lm_fit)
reset_result_linreg_tbl <- ramsey_reset_test(mod_linreg_fit)
reset_result_linreg2_tbl <- ramsey_reset_test(mod_linreg2_fit)


test_that("(Ramsey's RESET): a correctly named `tibble` from `lm` model", {
  expect_s3_class(reset_result_lm_tbl, "tbl_df")
  check_tidy_test_names(reset_result_lm_tbl)
})

test_that("(Ramsey's RESET): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(reset_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(reset_result_linreg_tbl)
})

test_that("(Ramsey's RESET): a correctly named `tibble` from `_glm` model", {
  expect_s3_class(reset_result_linreg2_tbl, "tbl_df")
  check_tidy_test_names(reset_result_linreg2_tbl)
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

test_that("(Ramsey's RESET): `.notes` is created correctly for `_glm` input", {
  notes_tbl <- reset_result_linreg2_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df1_df2(param_tbl)
})


test_that("(Ramsey's RESET): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(reset_result_lm_tbl$test)

  expect_identical(test_name_chr, "Ramsey's RESET")
})

test_that("(Ramsey's RESET): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(reset_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Ramsey's RESET")
})

test_that("(Ramsey's RESET): returns the correct test name for `_glm` model", {
  test_name_chr <- as.character(reset_result_linreg2_tbl$test)

  expect_identical(test_name_chr, "Ramsey's RESET")
})


test_that("(Ramsey's RESET): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(reset_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(reset_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, 7.24)
  expect_equal(test_p_value_dbl, 0.003)
})

test_that("(Ramsey's RESET): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(reset_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(reset_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, 7.24)
  expect_equal(test_p_value_dbl, 0.003)
})

test_that("(Ramsey's RESET): returns the correct statistics for `_glm` model", {
  test_statistic_dbl <- as.numeric(reset_result_linreg2_tbl$statistic)
  test_p_value_dbl <- as.numeric(reset_result_linreg2_tbl$p_value)

  expect_equal(test_statistic_dbl, 7.24)
  expect_equal(test_p_value_dbl, 0.003)
})


test_that("(Ramsey's RESET): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(reset_result_lm_tbl$result)
  test_outcome_chr <- as.character(reset_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Linear Specification is Not Valid")
})

test_that("(Ramsey's RESET): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(reset_result_linreg_tbl$result)
  test_outcome_chr <- as.character(reset_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Linear Specification is Not Valid")
})

test_that("(Ramsey's RESET): returns the correct conclusions for `_glm` model", {
  test_result_chr <- as.character(reset_result_linreg2_tbl$result)
  test_outcome_chr <- as.character(reset_result_linreg2_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Linear Specification is Not Valid")
})


# Harvey-Collier --------------------------------------------------------------
hc_result_lm_tbl <- harvey_collier_test(mod_lm_fit)
hc_result_linreg_tbl <- harvey_collier_test(mod_linreg_fit)
hc_result_linreg2_tbl <- harvey_collier_test(mod_linreg2_fit)


test_that("(Harvey-Collier): a correctly named `tibble` from `lm` model", {
  expect_s3_class(hc_result_lm_tbl, "tbl_df")
  check_tidy_test_names(hc_result_lm_tbl)
})

test_that("(Harvey-Collier): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(hc_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(hc_result_linreg_tbl)
})

test_that("(Harvey-Collier): a correctly named `tibble` from `_glm` model", {
  expect_s3_class(hc_result_linreg2_tbl, "tbl_df")
  check_tidy_test_names(hc_result_linreg2_tbl)
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

test_that("(Harvey-Collier): `.notes` is created correctly for `_glm` input", {
  notes_tbl <- hc_result_linreg2_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})


test_that("(Harvey-Collier): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(hc_result_lm_tbl$test)

  expect_identical(test_name_chr, "Harvey-Collier")
})

test_that("(Harvey-Collier): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(hc_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Harvey-Collier")
})

test_that("(Harvey-Collier): returns the correct test name for `_glm` model", {
  test_name_chr <- as.character(hc_result_linreg2_tbl$test)

  expect_identical(test_name_chr, "Harvey-Collier")
})


test_that("(Harvey-Collier): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(hc_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(hc_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, 1.17)
  expect_equal(test_p_value_dbl, 0.254)
})

test_that("(Harvey-Collier): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(hc_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(hc_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, 1.17)
  expect_equal(test_p_value_dbl, 0.254)
})

test_that("(Harvey-Collier): returns the correct statistics for `_glm` model", {
  test_statistic_dbl <- as.numeric(hc_result_linreg2_tbl$statistic)
  test_p_value_dbl <- as.numeric(hc_result_linreg2_tbl$p_value)

  expect_equal(test_statistic_dbl, 1.17)
  expect_equal(test_p_value_dbl, 0.254)
})


test_that("(Harvey-Collier): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(hc_result_lm_tbl$result)
  test_outcome_chr <- as.character(hc_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "True Relationship is Linear")
})

test_that("(Harvey-Collier): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(hc_result_linreg_tbl$result)
  test_outcome_chr <- as.character(hc_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "True Relationship is Linear")
})

test_that("(Harvey-Collier): returns the correct conclusions for `_glm` model", {
  test_result_chr <- as.character(hc_result_linreg2_tbl$result)
  test_outcome_chr <- as.character(hc_result_linreg2_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "True Relationship is Linear")
})
