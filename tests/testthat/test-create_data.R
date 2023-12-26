# Test function to create dataset for marginal comparisons works
testthat::test_that("data marginal comparisons", {
  df <- cobalt::lalonde |>
    tibble::as_tibble() |>
    dplyr::select(-re74, -re75) |>
    dplyr::mutate(
      id = paste0("subj_", dplyr::row_number())
    )

  to_test <- create_df_marginal_comparisons(
    dat = df,
    var = "re78",
    percentiles = c(0.2, 0.8),
    by_var = "race"
  )
  invisible(lapply(as.character(unique(df$race)), function(x) {
    v1 <- as.numeric(quantile(df[df$race == x, ][["re78"]], 0.2))
    v1_test <- as.numeric(unique(to_test[to_test$race == x, ][["low"]]))
    testthat::expect_identical(v1_test, v1)

    v2 <- as.numeric(quantile(df[df$race == x, ][["re78"]], 0.8))
    v2_test <- as.numeric(unique(to_test[to_test$race == x, ][["high"]]))
    testthat::expect_identical(v2_test, v2)
  }))

  to_test <- create_df_marginal_comparisons(
    dat = df,
    var = "re78",
    percentiles = c(0.1, 0.6),
    by_var = "race"
  )
  invisible(lapply(as.character(unique(df$race)), function(x) {
    v1 <- as.numeric(quantile(df[df$race == x, ][["re78"]], 0.1))
    v1_test <- as.numeric(unique(to_test[to_test$race == x, ][["low"]]))
    testthat::expect_identical(v1_test, v1)

    v2 <- as.numeric(quantile(df[df$race == x, ][["re78"]], 0.6))
    v2_test <- as.numeric(unique(to_test[to_test$race == x, ][["high"]]))
    testthat::expect_identical(v2_test, v2)
  }))
})
