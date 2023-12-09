# Anderson-Darling ------------------------------------------------------------
ad_result_vec_tbl <- anderson_darling_test_vec(resids)
ad_result_lm_tbl <- anderson_darling_test(mod_lm_fit)
ad_result_linreg_tbl <- anderson_darling_test(mod_linreg_fit)
ad_result_linreg2_tbl <- anderson_darling_test(mod_linreg2_fit)


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

test_that("(Anderson-Darling): a correctly named `tibble` from `_glm` model", {
  expect_s3_class(ad_result_linreg2_tbl, "tbl_df")
  check_tidy_test_names(ad_result_linreg2_tbl)
})


test_that("(Anderson-Darling): `.notes` is created correctly for numeric input", {
  notes_tbl <- ad_result_vec_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})

test_that("(Anderson-Darling): `.notes` is created correctly for `lm` model", {
  notes_tbl <- ad_result_lm_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})

test_that("(Anderson-Darling): `.notes` is created correctly for `_lm` model", {
  notes_tbl <- ad_result_linreg_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})

test_that("(Anderson-Darling): `.notes` is created correctly for `_glm` model", {
  notes_tbl <- ad_result_linreg2_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})


test_that("(Anderson-Darling): returns the correct test name for numeric input", {
  test_name_chr <- as.character(ad_result_vec_tbl$test)

  expect_identical(test_name_chr, "Anderson-Darling")
})

test_that("(Anderson-Darling): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(ad_result_lm_tbl$test)

  expect_identical(test_name_chr, "Anderson-Darling")
})

test_that("(Anderson-Darling): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(ad_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Anderson-Darling")
})

test_that("(Anderson-Darling): returns the correct test name for `_glm` model", {
  test_name_chr <- as.character(ad_result_linreg2_tbl$test)

  expect_identical(test_name_chr, "Anderson-Darling")
})


test_that("(Anderson-Darling): returns the correct statistics for numeric input", {
  test_statistic_dbl <- as.numeric(ad_result_vec_tbl$statistic)
  test_p_value_dbl <- as.numeric(ad_result_vec_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.41)
  expect_equal(test_p_value_dbl, 0.346)
})

test_that("(Anderson-Darling): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(ad_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(ad_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.67)
  expect_equal(test_p_value_dbl, 0.074)
})

test_that("(Anderson-Darling): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(ad_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(ad_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.67)
  expect_equal(test_p_value_dbl, 0.074)
})

test_that("(Anderson-Darling): returns the correct statistics for `_glm` model", {
  test_statistic_dbl <- as.numeric(ad_result_linreg2_tbl$statistic)
  test_p_value_dbl <- as.numeric(ad_result_linreg2_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.67)
  expect_equal(test_p_value_dbl, 0.074)
})


test_that("(Anderson-Darling): returns the correct conclusions for numeric input", {
  test_result_chr <- as.character(ad_result_vec_tbl$result)
  test_outcome_chr <- as.character(ad_result_vec_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Follows a Normal Distribution")
})

test_that("(Anderson-Darling): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(ad_result_lm_tbl$result)
  test_outcome_chr <- as.character(ad_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Follows a Normal Distribution")
})

test_that("(Anderson-Darling): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(ad_result_linreg_tbl$result)
  test_outcome_chr <- as.character(ad_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Follows a Normal Distribution")
})

test_that("(Anderson-Darling): returns the correct conclusions for `_glm` model", {
  test_result_chr <- as.character(ad_result_linreg2_tbl$result)
  test_outcome_chr <- as.character(ad_result_linreg2_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Follows a Normal Distribution")
})


# Shapiro-Wilk ----------------------------------------------------------------
sw_result_vec_tbl <- shapiro_wilk_test_vec(resids)
sw_result_lm_tbl <- shapiro_wilk_test(mod_lm_fit)
sw_result_linreg_tbl <- shapiro_wilk_test(mod_linreg_fit)
sw_result_linreg2_tbl <- shapiro_wilk_test(mod_linreg2_fit)


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

test_that("(Shapiro-Wilk): a correctly named `tibble` from `_glm` model", {
  expect_s3_class(sw_result_linreg2_tbl, "tbl_df")
  check_tidy_test_names(sw_result_linreg2_tbl)
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

test_that("(Shapiro-Wilk): `.notes` is created correctly for `_glm` input", {
  notes_tbl <- sw_result_linreg2_tbl$.notes[[1]]
  param_tbl <- notes_tbl$parameters[[1]]

  check_notes_names(notes_tbl)
  check_no_params(param_tbl)
})


test_that("(Shapiro-Wilk): returns the correct test name for numeric input", {
  test_name_chr <- as.character(sw_result_vec_tbl$test)

  expect_identical(test_name_chr, "Shapiro-Wilk")
})

test_that("(Shapiro-Wilk): returns the correct test name for `lm` model", {
  test_name_chr <- as.character(sw_result_lm_tbl$test)

  expect_identical(test_name_chr, "Shapiro-Wilk")
})

test_that("(Shapiro-Wilk): returns the correct test name for `_lm` model", {
  test_name_chr <- as.character(sw_result_linreg_tbl$test)

  expect_identical(test_name_chr, "Shapiro-Wilk")
})

test_that("(Shapiro-Wilk): returns the correct test name for `_glm` model", {
  test_name_chr <- as.character(sw_result_linreg2_tbl$test)

  expect_identical(test_name_chr, "Shapiro-Wilk")
})


test_that("(Shapiro-Wilk): returns the correct statistics for numeric input", {
  test_statistic_dbl <- as.numeric(sw_result_vec_tbl$statistic)
  test_p_value_dbl <- as.numeric(sw_result_vec_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.99)
  expect_equal(test_p_value_dbl, 0.416)
})

test_that("(Shapiro-Wilk): returns the correct statistics for `lm` model", {
  test_statistic_dbl <- as.numeric(sw_result_lm_tbl$statistic)
  test_p_value_dbl <- as.numeric(sw_result_lm_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.93)
  expect_equal(test_p_value_dbl, 0.033)
})

test_that("(Shapiro-Wilk): returns the correct statistics for `_lm` model", {
  test_statistic_dbl <- as.numeric(sw_result_linreg_tbl$statistic)
  test_p_value_dbl <- as.numeric(sw_result_linreg_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.93)
  expect_equal(test_p_value_dbl, 0.033)
})

test_that("(Shapiro-Wilk): returns the correct statistics for `_glm` model", {
  test_statistic_dbl <- as.numeric(sw_result_linreg2_tbl$statistic)
  test_p_value_dbl <- as.numeric(sw_result_linreg2_tbl$p_value)

  expect_equal(test_statistic_dbl, 0.93)
  expect_equal(test_p_value_dbl, 0.033)
})


test_that("(Shapiro-Wilk): returns the correct conclusions for numeric input", {
  test_result_chr <- as.character(sw_result_vec_tbl$result)
  test_outcome_chr <- as.character(sw_result_vec_tbl$outcome)

  expect_identical(test_result_chr, "Fail to Reject")
  expect_identical(test_outcome_chr, "Follows a Normal Distribution")
})

test_that("(Shapiro-Wilk): returns the correct conclusions for `lm` model", {
  test_result_chr <- as.character(sw_result_lm_tbl$result)
  test_outcome_chr <- as.character(sw_result_lm_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Does Not Follow a Normal Distribution")
})

test_that("(Shapiro-Wilk): returns the correct conclusions for `_lm` model", {
  test_result_chr <- as.character(sw_result_linreg_tbl$result)
  test_outcome_chr <- as.character(sw_result_linreg_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Does Not Follow a Normal Distribution")
})

test_that("(Shapiro-Wilk): returns the correct conclusions for `_glm` model", {
  test_result_chr <- as.character(sw_result_linreg2_tbl$result)
  test_outcome_chr <- as.character(sw_result_linreg2_tbl$outcome)

  expect_identical(test_result_chr, "Reject")
  expect_identical(test_outcome_chr, "Does Not Follow a Normal Distribution")
})
