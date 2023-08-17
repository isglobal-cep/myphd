#' Title
#'
#' @param dat
#' @param outcome
#' @param exposure
#' @param covariates
#' @param method
#' @param add_inter_exposure
#' @param add_splines_exposure
#' @param df_splines
#' @param threshold_smooth
#' @param threshold_k
#'
#' @return
#'
#' @export
create_formula <- function(dat,
                           outcome, exposure, covariates,
                           method,
                           add_inter_exposure,
                           add_splines_exposure, df_splines,
                           threshold_smooth, threshold_k) {
  # Extract covariates
  covariates_continuous <- dat |>
    dplyr::select(dplyr::where(is.numeric)) |>
    colnames()
  covariates_continuous <- setdiff(covariates_continuous,
                                   c(outcome, exposure))
  covariates_factor <- dat |>
    dplyr::select(!dplyr::where(is.numeric)) |>
    colnames()
  covariates_factor <- setdiff(covariates_factor,
                               c(outcome, exposure))
  assertthat::assert_that(identical(sort(covariates),
                                    sort(c(covariates_continuous, covariates_factor))),
                          msg = "The covariates do not match the originals.")
  assertthat::assert_that(!exposure %in% covariates,
                          msg = "The exposure was found among the covariates.")

  # Create formula (weights estimation)
  if (is.null(outcome)) {
    form <- paste0(
      exposure, " ~ ",
      paste0(covariates_continuous,
             collapse = " + ")
    )
    form <- paste0(
      form, " + ",
      paste0(covariates_factor,
             collapse = " + ")
    )

    return(form)
  } # End formula weights estimation

  # Create formula (outcome model)
  if (method %in% c("lm", "glm")) {
    ## Step 1: add outcome, exposure, and continuous covariates
    form <- paste0(
      outcome, " ~ ",
      ifelse(
        add_splines_exposure == TRUE,
        paste0(
          "splines::ns(", exposure, ", df = ", df_splines, ")"
        ),
        exposure
      ),
      ifelse(
        add_inter_exposure == TRUE,
        " * ", " + "
      ),
      "(",
      paste0(covariates_continuous,
             collapse = " + "),
      ")"
    )
    ## Step 2: add remaining covariates
    form <- paste0(
      form, " + ",
      paste0("factor(",
             covariates_factor,
             ")",
             collapse = " + ")
    )
  } else if (method %in% c("gam")) {
    less_than_y <- apply(dat[, covariates_continuous], 2, function(x) {
      length(unique(x)) < threshold_smooth
    })
    threshold_k <- lapply(covariates_continuous[less_than_y], function(x) {
      ifelse(
        length(unique(dat[[x]])) < threshold_k,
        length(unique(dat[[x]])) - 1,
        threshold_k
      )
    }) |>
      unlist()
    form <- paste0(
      outcome, " ~ ",
      "s(", exposure, ")", " + ",
      paste0(
        "s(",
        covariates_continuous[!less_than_y],
        ")",
        collapse = " + "
      ),
      " + ",
      paste0(
        "s(",
        covariates_continuous[less_than_y],
        ", k = ", threshold_k,
        ")",
        collapse = " + "
      ),
      " + ",
      paste0(
        covariates_factor,
        collapse = " + "
      )
    )
  } else {
    stop(
      glue::glue("The {method} method is not currently supported.",
                 method = method)
    )
  } # End formula outcome model

  return(form)
}

#' Fit various models with weighting
#'
#' @description
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param outcome
#' @param exposure The name of the variable corresponding to the exposure. A string.
#' @param covariates A vector of covariates' names. A vector.
#' @param weights The `weights` element of the result of the call
#' to [estimate_weights()]. A \link[WeightIt]{weightit} object.
#' @param method
#' @param method_args A named list with the following variables:
#' * `family`, .
#' * `add_inter_exposure`, .
#' * `add_splines_exposure`, .
#' * `df_splines`, .
#' * `threshold_smooth`, .
#' * `threshold_k`, .
#' @md
#'
#' @return
#'
#' @export
fit_model_weighted <- function(dat,
                               outcome,
                               exposure,
                               covariates,
                               weights,
                               method,
                               method_args) {
  # Setup
  ## Create formula
  form <- create_formula(
    dat = dat,
    outcome = outcome,
    exposure = exposure,
    covariates = covariates,
    method = method,
    add_inter_exposure = method_args$add_inter_exposure,
    add_splines_exposure = method_args$add_splines_exposure,
    df_splines = method_args$df_splines,
    threshold_smooth = method_args$threshold_smooth,
    threshold_k = method_args$threshold_k
  )

  # Fit model
  if (method == "glm") {
    fit <- glm(
      formula = as.formula(form),
      data = dat,
      weights = weights,
      family = method_args$family
    )
  } else if (method == "orm") {
  } else if (method == "gam") {
    fit <- mgcv::bam(
      formula = as.formula(form),
      family = method_args$family,
      data = dat,
      weights = weights,
      method = "fREML",
      select = FALSE,
      discrete = FALSE,
      control = list(
        nthreads = 4,
        ncv.threads = 6,
        maxit = 400
      )
    )
  } else if (method == "super") {
  } else {
    stop("Invalid `method`.")
  } # End if `method`

  return(list(
    fit = fit
  ))
}

#' Title
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param outcome
#' @param exposure The name of the variable corresponding to the exposure. A string.
#' @param covariates A vector of covariates' names. A vector.
#' @param model
#'
#' @return
#'
#' @export
estimate_marginal_effects <- function(dat,
                                      outcome,
                                      exposure,
                                      covariates,
                                      model) {}
