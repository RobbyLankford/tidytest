# Main `tidy_test` Function ---------------------------------------------------
tidy_test <- function(x, .f, ..., .test, .null, .alt, .alpha = 0.05) {
  .f(x, ...) %>%
    tidy_results() %>%
    dplyr::mutate(
      test    = .test,
      null    = .null,
      alt     = .alt,
      result  = dplyr::if_else(p_value > .alpha, "Fail to Reject", "Reject"),
      outcome = dplyr::if_else(p_value > .alpha, null, alt),
    ) %>%
    tidyr::nest(.notes = c(null, alt, parameters)) %>%
    dplyr::select(test, statistic, p_value, result, outcome, .notes)
}

# Tidy Results to Tibble ------------------------------------------------------
tidy_results <- function(object, ...) {
  UseMethod("tidy_results")
}

tidy_results.htest <- function(object, ...) {
  statistic  <- object[["statistic"]]
  parameters <- object[["parameter"]]
  p_value    <- object[["p.value"]]

  if (rlang::is_null(parameters)) {
    parameters <- NA_character_
    names(parameters) <- "None"
  }

  dplyr::tibble(
    statistic  = round(statistic, digits = 2),
    p_value    = round(p_value, digits = 3),
    parameters = list(format_parameters(parameters))
  )
}

# Helper Functions ------------------------------------------------------------
format_parameters <- function(x) {
  x %>%
    tibble::enframe() %>%
    tidyr::pivot_wider()
}
