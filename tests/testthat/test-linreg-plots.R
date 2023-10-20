# Residuals -------------------------------------------------------------------
test_that("a `tibble` of residuals are extracted from `lm` model", {
  local_edition(3)

  resids_tbl <- calculate_residuals(mod_lm_fit)

  expect_s3_class(resids_tbl, "tbl_df")
  check_names(resids_tbl, c(".pred", ".resid"))

  expect_snapshot(utils::head(resids_tbl, n = 10))
})

test_that("a `tibble` of residuals is calculated using an `lm` model", {
  local_edition(3)

  resids_tbl <- calculate_residuals(mod_lm_fit, mtcars[1:10, ])

  expect_s3_class(resids_tbl, "tbl_df")
  check_names(resids_tbl, c(".pred", ".resid"))

  expect_snapshot(utils::head(resids_tbl, n = 10))
})


# Plots -----------------------------------------------------------------------
test_that("can plot a predictions vs residuals plot from `data.frame` input", {
  local_edition(3)

  skip_if_not_installed("vdiffr")

  resids_df <- data.frame(
    .pred = c(23.6, 22.6, 25.3, 21.2, 18.2, 20.5, 15.6),
    .resid = c(-2.57, -1.60, -2.49, 0.183, 0.459, -2.37, -1.27)
  )

  vdiffr::expect_doppelganger(
    title = "Predictions vs Residuals Plot",
    fig = plot_predictions_vs_residuals(resids_df)
  )
})

test_that("can plot a QQ-plot from `data.frame` input", {
  local_edition(3)

  skip_if_not_installed("vdiffr")

  resids_df <- data.frame(
    .pred = c(23.6, 22.6, 25.3, 21.2, 18.2, 20.5, 15.6),
    .resid = c(-2.57, -1.60, -2.49, 0.183, 0.459, -2.37, -1.27)
  )

  vdiffr::expect_doppelganger(
    title = "QQ-Plot",
    fig = plot_qq_normality(resids_df)
  )
})
