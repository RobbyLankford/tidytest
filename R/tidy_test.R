# Main `tidy_test` Function ---------------------------------------------------
tidy_test <- function(x, .f, ..., .test, .null, .alt, .alpha = 0.05) {
  nest_cols_chr <- c("test", "statistic", "p_value", "result", "outcome")
  out_cols_chr  <- c(nest_cols_chr, ".notes")

  tidy_res_tbl <- tidy_results(.f(x, ...))
  tidy_res_tbl <- add_info_cols(tidy_res_tbl, .test, .null, .alt, .alpha)

  tidy_res_nested_tbl <- nest_notes(tidy_res_tbl, nest_cols_chr)

  tidy_res_nested_tbl[out_cols_chr]
}


# Tidy Results to tibble ------------------------------------------------------
tidy_results <- function(object, ...) {
  UseMethod("tidy_results")
}

tidy_results.htest <- function(object, ...) {
  statistic  <- object[["statistic"]]
  parameters <- object[["parameter"]]
  p_value    <- object[["p.value"]]

  if (is_null(parameters)) {
    parameters <- NA_character_
    names(parameters) <- "None"
  }

  new_tibble(
    df_list(
      statistic  = round(statistic, digits = 2),
      p_value    = round(p_value, digits = 3),
      parameters = list(format_parameters(parameters))
    )
  )
}


# Helper Functions ------------------------------------------------------------
format_parameters <- function(x) pivot_wider(enframe(x))

add_info_cols <- function(.data, .test, .null, .alt, .alpha) {
  .data$test <- .test
  .data$null <- .null
  .data$alt  <- .alt

  .data$result  <- ifelse(.data$p_value > .alpha, "Fail to Reject", "Reject")
  .data$outcome <- ifelse(.data$p_value > .alpha, .null, .alt)

  .data
}

nest_notes <- function(.data, cols) {
  #> https://www.tidyverse.org/blog/2023/04/performant-packages/#nest
  split_tbl <- vec_split(
    x  = .data[setdiff(colnames(.data), cols)],
    by = .data[cols]
  )

  vec_cbind(split_tbl$key, new_tibble(list(.notes = split_tbl$val)))
}
