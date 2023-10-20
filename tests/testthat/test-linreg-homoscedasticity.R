# Bruesch-Pagan ---------------------------------------------------------------
test_that("(Bruesch-Pagan): a correctly named `tibble` from `lm` model", {
  result_tbl <- bruesch_pagan_test(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Bruesch-Pagan): a correctly named `tibble` from `_lm` model", {
  result_tbl <- bruesch_pagan_test(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})


# Goldfeld-Quandt -------------------------------------------------------------
test_that("(Goldfeld-Quandt): a correctly named `tibble` from `lm` model", {
  result_tbl <- goldfeld_quandt_test(mod_lm_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})

test_that("(Goldfeld-Quandt): a correctly named `tibble` from `_lm` model", {
  result_tbl <- goldfeld_quandt_test(mod_linreg_fit)

  expect_s3_class(result_tbl, "tbl_df")
  check_tidy_test_names(result_tbl)
})
