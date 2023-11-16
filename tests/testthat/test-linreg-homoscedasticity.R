# Bruesch-Pagan ---------------------------------------------------------------
bp_result_lm_tbl <- bruesch_pagan_test(mod_lm_fit)
bp_result_linreg_tbl <- bruesch_pagan_test(mod_linreg_fit)

test_that("(Bruesch-Pagan): a correctly named `tibble` from `lm` model", {
  expect_s3_class(bp_result_lm_tbl, "tbl_df")
  check_tidy_test_names(bp_result_lm_tbl)
})

test_that("(Bruesch-Pagan): a correctly named `tibble` from `_lm` model", {
  result_tbl <- bruesch_pagan_test(mod_linreg_fit)

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


# Goldfeld-Quandt -------------------------------------------------------------
gq_result_lm_tbl <- goldfeld_quandt_test(mod_lm_fit)
gq_result_linreg_tbl <- goldfeld_quandt_test(mod_linreg_fit)

test_that("(Goldfeld-Quandt): a correctly named `tibble` from `lm` model", {
  expect_s3_class(gq_result_lm_tbl, "tbl_df")
  check_tidy_test_names(gq_result_lm_tbl)
})

test_that("(Goldfeld-Quandt): a correctly named `tibble` from `_lm` model", {
  result_tbl <- goldfeld_quandt_test(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
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
