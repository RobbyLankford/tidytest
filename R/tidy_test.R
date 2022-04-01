tidy_test <- function(.data, ..., test, null, alt, alpha = 0.05) {
  .data %>%
    broom::tidy() %>%
    dplyr::select(...) %>%
    dplyr::mutate(
      test     = test,
      null     = null,
      alt      = alt,
      result   = dplyr::if_else(p.value > alpha, "Fail to Reject", "Reject"),
      conclude = dplyr::if_else(p.value > alpha, null, alt)
    ) %>%
    dplyr::relocate(test)
}
