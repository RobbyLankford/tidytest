tidy_test <- function(.data, ...) {
  .data %>%
    broom::tidy() %>%
    dplyr::select(...)
}

finalize_test <- function(.data, null, alternative, .alpha = 0.05) {
  .data %>%
    dplyr::mutate(
      result     = dplyr::if_else(p.value > .alpha, "Fail to Reject", "Reject"),
      conclusion = dplyr::if_else(p.value > .alpha, null, alternative)
    )
}
