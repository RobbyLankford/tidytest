# Bruesch-Pagan ---------------------------------------------------------------
bp_result_lm_tbl <- bruesch_pagan_test(mod_lm_fit)
bp_result_linreg_tbl <- bruesch_pagan_test(mod_linreg_fit)
bp_result_linreg2_tbl <- bruesch_pagan_test(mod_linreg2_fit)


test_that("(Bruesch-Pagan): a correctly named `tibble` from `lm` model", {
  expect_s3_class(bp_result_lm_tbl, "tbl_df")
  check_tidy_test_names(bp_result_lm_tbl)
})

test_that("(Bruesch-Pagan): a correctly named `tibble` from `_lm` model", {
  result_tbl <- bruesch_pagan_test(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Bruesch-Pagan): a correctly named `tibble` from `_glm` model", {
  result_tbl <- bruesch_pagan_test(mod_linreg2_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})


test_that("(Bruesch-Pagan): `.notes` is created correctly for `lm` model", {
  notes_tbl <- bp_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})

test_that("(Bruesch-Pagan): `.notes` is created correctly for `_lm` model", {
  notes_tbl <- bp_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})

test_that("(Bruesch-Pagan): `.notes` is created correctly for `_glm` model", {
  notes_tbl <- bp_result_linreg2_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df(param_tbl)
})


test_that("(Bruesch-Pagan): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(bp_result_lm_tbl$test)

  expect_identical(test_name_chr, "Bruesch-Pagan")
})

test_that("(Bruesch-Pagan): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(bp_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Bruesch-Pagan")
})

test_that("(Bruesch-Pagan): returns the correct test name for `_glm` model", {
  test_name_chr <- as.character(bp_result_linreg2_tbl$test)

  expect_identical(test_name_chr, "Bruesch-Pagan")
})


test_that("(Bruesch-Pagan): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(bp_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(bp_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.95)
  expect_equal(test_p_value_dbl, 0.814)
})

test_that("(Bruesch-Pagan): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(bp_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(bp_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.95)
  expect_equal(test_p_value_dbl, 0.814)
})

test_that("(Bruesch-Pagan): returns the correct statistics for `_glm` model", {
  test_statistic_dbl <- as.numeric(bp_result_linreg2_tbl$statistic)
  test_p_value_dbl <- as.numeric(bp_result_linreg2_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.95)
  expect_equal(test_p_value_dbl, 0.814)
})


test_that("(Bruesch-Pagan): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(bp_result_lm_tbl$result)
  test_outcome_chr <- as.character(bp_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Variances Are Equal")
})

test_that("(Bruesch-Pagan): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(bp_result_linreg_tbl$result)
  test_outcome_chr <- as.character(bp_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Variances Are Equal")
})

test_that("(Bruesch-Pagan): returns the correct conclusions for `_glm` model", {
  test_result_chr <- as.character(bp_result_linreg2_tbl$result)
  test_outcome_chr <- as.character(bp_result_linreg2_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Variances Are Equal")
})


# Goldfeld-Quandt -------------------------------------------------------------
gq_result_lm_tbl <- goldfeld_quandt_test(mod_lm_fit)
gq_result_linreg_tbl <- goldfeld_quandt_test(mod_linreg_fit)
gq_result_linreg2_tbl <- goldfeld_quandt_test(mod_linreg2_fit)


test_that("(Goldfeld-Quandt): a correctly named `tibble` from `lm` model", {
  expect_s3_class(gq_result_lm_tbl, "tbl_df")
  check_tidy_test_names(gq_result_lm_tbl)
})

test_that("(Goldfeld-Quandt): a correctly named `tibble` from `_lm` model", {
  expect_s3_class(gq_result_linreg_tbl, "tbl_df")
  check_tidy_test_names(gq_result_linreg_tbl)
})

test_that("(Goldfeld-Quandt): a correctly named `tibble` from `_glm` model", {
  expect_s3_class(gq_result_linreg2_tbl, "tbl_df")
  check_tidy_test_names(gq_result_linreg2_tbl)
})


test_that("(Goldfeld-Quandt): `.notes` is created correctly for `lm` model", {
  notes_tbl <- gq_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df1_df2(param_tbl)
})

test_that("(Goldfeld-Quandt): `.notes` is created correctly for `_lm` model", {
  notes_tbl <- gq_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df1_df2(param_tbl)
})

test_that("(Goldfeld-Quandt): `.notes` is created correctly for `_glm` model", {
  notes_tbl <- gq_result_linreg2_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_params_df1_df2(param_tbl)
})


test_that("(Goldfeld-Quandt): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(gq_result_lm_tbl$test)

  expect_identical(test_name_chr, "Goldfeld-Quandt")
})

test_that("(Goldfeld-Quandt): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(gq_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Goldfeld-Quandt")
})

test_that("(Goldfeld-Quandt): returns the correct test name for `_glm` model", {
  test_name_chr <- as.character(gq_result_linreg2_tbl$test)

  expect_identical(test_name_chr, "Goldfeld-Quandt")
})


test_that("(Goldfeld-Quandt): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(gq_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(gq_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, 7.41)
  expect_equal(test_p_value_dbl, 0.002)
})

test_that("(Goldfeld-Quandt): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(gq_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(gq_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, 7.41)
  expect_equal(test_p_value_dbl, 0.002)
})

test_that("(Goldfeld-Quandt): returns the correct statistics for `_glm` model", {
  test_statistic_dbl <- as.numeric(gq_result_linreg2_tbl$statistic)
  test_p_value_dbl <- as.numeric(gq_result_linreg2_tbl$p_value)

  expect_equal(test_statistic_dbl, 7.41)
  expect_equal(test_p_value_dbl, 0.002)
})


test_that("(Goldfeld-Quandt): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(gq_result_lm_tbl$result)
  test_outcome_chr <- as.character(gq_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Variances Are Not Equal")
})

test_that("(Goldfeld-Quandt): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(gq_result_linreg_tbl$result)
  test_outcome_chr <- as.character(gq_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Variances Are Not Equal")
})

test_that("(Goldfeld-Quandt): returns the correct conclusions for `_glm` model", {
  test_result_chr <- as.character(gq_result_linreg2_tbl$result)
  test_outcome_chr <- as.character(gq_result_linreg2_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Variances Are Not Equal")
})
