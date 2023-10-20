test_that("can plot an ACF plot from a `data.frame` input", {
  local_edition(3)

  skip_if_not_installed("vdiffr")

  acf_df <- data.frame(
    lag = 0:9,
    acf = c(1, -0.0569, 0.0339, -0.124, -0.0934,
            0.142, 0.0873, -0.0462, 0.0513, -0.0697),
    .conf_lo = rep(-0.196, 10),
    .conf_hi = rep(0.196, 10)
  )

  vdiffr::expect_doppelganger(
    title = "ACF Plot",
    fig = plot_acf(acf_df)
  )
})

test_that("can plot a PACF plot from a `data.frame` input", {
  local_edition(3)

  skip_if_not_installed("vdiffr")

  pacf_df <- data.frame(
    lag = 1:10,
    pacf = c(-0.0617, -0.126, -0.0786, 0.0263, -0.00573,
             0.0705, -0.112, -0.00623, 0.124, -0.0529),
    .conf_lo = rep(-0.196, 10),
    .conf_hi = rep(0.196, 10)
  )

  vdiffr::expect_doppelganger(
    title = "PACF Plot",
    fig = plot_pacf(pacf_df)
  )
})
