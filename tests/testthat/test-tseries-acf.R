# ACF -------------------------------------------------------------------------
test_that("(ACF): a correctly named `tibble` from numeric input", {
  result_tbl <- calculate_acf(resids)

  expect_s3_class(result_tbl, "tbl_df")
  check_names(result_tbl, c("lag", "acf", ".conf_lo", ".conf_hi"))
})


# PACF ------------------------------------------------------------------------
test_that("(PACF): a correctly named `tibble` from numeric input", {
  result_tbl <- calculate_pacf(resids)

  expect_s3_class(result_tbl, "tbl_df")
  check_names(result_tbl, c("lag", "pacf", ".conf_lo", ".conf_hi"))
})
