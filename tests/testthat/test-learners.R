# Checking that function to create formulas works
testthat::test_that("formula is correct", {
  df <- cobalt::lalonde |>
    tibble::as_tibble() |>
    dplyr::select(-re74, -re75) |>
    dplyr::mutate(
      id = paste0("subj_", dplyr::row_number())
    )

  # My formula (simple)
  my_form <- create_formula(
    dat = df,
    outcome = "re78",
    exposure = "treat",
    covariates = c(
      "age", "educ", "race", "married", "nodegree"
    ),
    id_var = "id",
    method = "glm",
    add_inter_exposure = FALSE,
    add_inter_exposure_specific = c(),
    add_splines_exposure = FALSE,
    df_splines = FALSE,
    threshold_smooth = NULL,
    threshold_k = NULL
  )
  expected_form <- "re78 ~ treat + (age + educ + married + nodegree) + factor(race)"
  testthat::expect_identical(my_form, expected_form)

  # My formula (interaction)
  my_form <- create_formula(
    dat = df,
    outcome = "re78",
    exposure = "treat",
    covariates = c(
      "age", "educ", "race", "married", "nodegree"
    ),
    id_var = "id",
    method = "glm",
    add_inter_exposure = TRUE,
    add_inter_exposure_specific = c(),
    add_splines_exposure = FALSE,
    df_splines = FALSE,
    threshold_smooth = NULL,
    threshold_k = NULL
  )
  expected_form <- "re78 ~ treat * (age + educ + married + nodegree) + factor(race)"
  testthat::expect_identical(my_form, expected_form)

  # My formula (specific interaction)
  my_form <- create_formula(
    dat = df,
    outcome = "re78",
    exposure = "treat",
    covariates = c(
      "age", "educ", "race", "married", "nodegree"
    ),
    id_var = "id",
    method = "glm",
    add_inter_exposure = FALSE,
    add_inter_exposure_specific = c("educ"),
    add_splines_exposure = FALSE,
    df_splines = FALSE,
    threshold_smooth = NULL,
    threshold_k = NULL
  )
  expected_form <- "re78 ~ treat * (educ) + (age + married + nodegree) + factor(race)"
  testthat::expect_identical(my_form, expected_form)

  # My formula (specific interaction and splines)
  my_form <- create_formula(
    dat = df,
    outcome = "re78",
    exposure = "treat",
    covariates = c(
      "age", "educ", "race", "married", "nodegree"
    ),
    id_var = "id",
    method = "glm",
    add_inter_exposure = FALSE,
    add_inter_exposure_specific = c("educ"),
    add_splines_exposure = TRUE,
    df_splines = 5,
    threshold_smooth = NULL,
    threshold_k = NULL
  )
  expected_form <- "re78 ~ splines::ns(treat, df = 5) * (educ) + (age + married + nodegree) + factor(race)"
  testthat::expect_identical(my_form, expected_form)
})
