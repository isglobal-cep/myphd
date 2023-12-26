# Test that handling values below the LOD works
testthat::test_that("test LOD", {
  ##############################################################################
})

# Test that handling missing values works
testthat::test_that("test missings", {
  # No missing values so no changes
  df <- cobalt::lalonde |>
    tibble::as_tibble() |>
    dplyr::select(-re74, -re75) |>
    dplyr::mutate(
      id = paste0("subj_", dplyr::row_number())
    )
  to_test <- handle_missing_values(
    dat = df,
    covariates = NULL,
    use_additional_covariates = FALSE,
    selected_covariates = NULL,
    id_var = "id",
    by_var = "race",
    threshold_within = 0.5,
    threshold_overall = 0.4,
    method_imputation = "vim.knn",
    k = 5,
    path_save_res = NULL
  )
  testthat::expect_identical(
    to_test$dat_imputed, df
  )
  rm(to_test)

  # Insert enough missing values (overall) to drop all columns
  df_too_many <- df |>
    dplyr::mutate(
      dplyr::across(
        c("treat", "age", "educ", "married", "nodegree", "re78"),
        \(x) replace(
          x,
          sample(length(x), 0.6 * length(x)), NA
        )
      )
    )
  to_test <- handle_missing_values(
    dat = df_too_many,
    covariates = NULL,
    use_additional_covariates = FALSE,
    selected_covariates = NULL,
    id_var = "id",
    by_var = "race",
    threshold_within = 0.5,
    threshold_overall = 0.4,
    method_imputation = "vim.knn",
    k = 5,
    path_save_res = NULL
  )
  testthat::expect_identical(
    length(colnames(to_test$dat_imputed)), as.integer(2)
  )
  testthat::expect_contains(
    colnames(to_test$dat_imputed),
    c("race", "id")
  )
  rm(list = c("df_too_many", "to_test"))

  # Insert enough missing values (within) to drop some columns
  df_some <- df |>
    dplyr::group_by(race) |>
    dplyr::mutate(
      dplyr::across(
        c("age", "educ", "married", "nodegree", "re78"),
        \(x) replace(
          x,
          sample(length(x), 0.4 * length(x)), NA
        )
      )
    ) |>
    dplyr::ungroup()
  to_test <- handle_missing_values(
    dat = df_some,
    covariates = NULL,
    use_additional_covariates = FALSE,
    selected_covariates = NULL,
    id_var = "id",
    by_var = "race",
    threshold_within = 0.3,
    threshold_overall = 0.4,
    method_imputation = "vim.knn",
    k = 5,
    path_save_res = NULL
  )
  testthat::expect_identical(
    length(colnames(to_test$dat_imputed)), as.integer(3)
  )
  testthat::expect_contains(
    colnames(to_test$dat_imputed),
    c("treat", "race", "id")
  )
})

# Test that adjusting for creatinine works
testthat::test_that("test creatinine", {
  ##############################################################################
})

# Test that transformations work
testthat::test_that("test transformations", {
  df <- cobalt::lalonde |>
    tibble::as_tibble() |>
    dplyr::select(-re74, -re75) |>
    dplyr::mutate(
      id = paste0("subj_", dplyr::row_number()),
      # Otherwise log(0.0)
      re78 = re78 + 1
    ) |>
    dplyr::select(id, race, re78)

  # Check identical after back transformation
  to_test <- handle_transformation(
    dat = df,
    id_var = "id",
    by_var = "race",
    transformation_fun = log
  )
  back_transform <- to_test |>
    dplyr::mutate(
      re78 = exp(re78)
    )
  testthat::expect_equal(
    back_transform, df
  )
})

# Test that standardization works
testthat::test_that("test standardization", {
  df <- cobalt::lalonde |>
    tibble::as_tibble() |>
    dplyr::select(-re74, -re75) |>
    dplyr::mutate(
      id = paste0("subj_", dplyr::row_number())
    )

  # Mean and sd
  to_test <- handle_standardization(
    dat = df,
    id_var = "id",
    by_var = "race",
    center_fun = mean,
    scale_fun = sd
  )
  tests <- lapply(setdiff(colnames(df), c("id", "race")), function(x) {
    testthat::expect_equal(
      mean(to_test[[x]]), 0
    )
    testthat::expect_equal(
      sd(to_test[[x]]), 1
    )
  })
})

# Test that variable is bounded between 0 and 1
testthat::test_that("test bound", {
  df <- cobalt::lalonde |>
    tibble::as_tibble() |>
    dplyr::select(-re74, -re75) |>
    dplyr::mutate(
      id = paste0("subj_", dplyr::row_number())
    )

  to_test <- bound_outcome(df, "re78")

  testthat::expect_true(
    min(to_test$re78) >= 0
  )
  testthat::expect_true(
    max(to_test$re78) <= 1
  )
})
