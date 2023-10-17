#' Programmatic way of constructing model formulas
#'
#' @description
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param outcome A string indicating the outcome variable. A string.
#' @param exposure A string indicating the exposure variable. A string.
#' @param covariates A list of covariate names. A list or vector.
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param method The name of the model to be fitted (e.g., `glm`). A string.
#' @param add_inter_exposure Whether to add an interaction between the exposure
#' and all the covariates. A bool.
#' @param add_inter_exposure_specific A list of covariate names for which
#' an interaction with the exposure is desired. A list or vector.
#' @param add_splines_exposure Whether to model the exposure with natural splines. A bool.
#' @param df_splines The degrees of freedom for the natural splines. An integer.
#' @param threshold_smooth For GAMs, threshold for the number of unique values
#' of continuous variables. If the number of unique values is greater or equal than
#' this variable, the dimension of the basis used to represent the smooth term (`k`),
#' is chosen automatically. An integer.
#' @param threshold_k
#'
#' @returns
#'
#' @export
create_formula <- function(dat,
                           outcome,
                           exposure,
                           covariates,
                           id_var,
                           method,
                           add_inter_exposure,
                           add_inter_exposure_specific,
                           add_splines_exposure,
                           df_splines,
                           threshold_smooth,
                           threshold_k) {
  # Extract covariates
  covariates_continuous <- dat |>
    tidylog::select(dplyr::where(is.numeric)) |>
    colnames()
  covariates_continuous <- setdiff(
    covariates_continuous,
    c(outcome, exposure, id_var, add_inter_exposure_specific)
  )
  covariates_factor <- dat |>
    tidylog::select(!dplyr::where(is.numeric)) |>
    colnames()
  covariates_factor <- setdiff(
    covariates_factor,
    c(outcome, exposure, id_var, add_inter_exposure_specific)
  )
  assertthat::assert_that(
    identical(
      sort(covariates),
      sort(
        c(
          covariates_continuous, covariates_factor,
          add_inter_exposure_specific
        )
      )
    ),
    msg = "The covariates do not match the originals."
  )
  assertthat::assert_that(!exposure %in% covariates,
    msg = "The exposure was found among the covariates."
  )

  ##############################################################################
  # Create formula (weights estimation)
  ##############################################################################
  if (is.null(outcome)) {
    form <- paste0(
      exposure,
      " ~ ",
      paste0(covariates_continuous,
        collapse = " + "
      )
    )
    form <- paste0(
      form, " + ",
      paste0(covariates_factor,
        collapse = " + "
      )
    )

    return(form)
  } # End formula weights estimation

  ##############################################################################
  # Create formula (outcome model)
  ##############################################################################
  #################################
  if (method %in% c("lm", "glm")) {
    #################################
    ## Step 1: add outcome, exposure, and continuous covariates
    form <- paste0(
      outcome,
      " ~ ",
      ifelse(
        add_splines_exposure == TRUE,
        paste0("splines::ns(", exposure, ", df = ", df_splines, ")"),
        exposure
      ),
      ifelse(
        length(add_inter_exposure_specific) > 0,
        paste0(
          " * (",
          paste0(
            add_inter_exposure_specific,
            collapse = " * "
          ),
          ")"
        ),
        ""
      ),
      ifelse(
        length(covariates_continuous) > 0,
        paste0(
          ifelse(add_inter_exposure == TRUE,
            " * ", " + "
          ),
          "(",
          paste0(covariates_continuous,
            collapse = " + "
          ),
          ")"
        ),
        ""
      )
    )
    ## Step 2: add remaining covariates
    if (length(covariates_factor) > 0) {
      form <- paste0(
        form,
        " + ",
        paste0("factor(",
          covariates_factor,
          ")",
          collapse = " + "
        )
      )
    }
    ##################################
  } else if (method %in% c("gam")) {
    ##################################
    less_than_y <- apply(dat[, covariates_continuous], 2, function(x) {
      length(unique(x)) < threshold_smooth
    })
    threshold_k <-
      lapply(covariates_continuous[less_than_y], function(x) {
        ifelse(length(unique(dat[[x]])) < threshold_k,
          length(unique(dat[[x]])) - 1,
          threshold_k
        )
      }) |>
      unlist()
    form <- paste0(
      outcome,
      " ~ ",
      "s(",
      exposure,
      ")",
      " + ",
      paste0("s(",
        covariates_continuous[!less_than_y],
        ")",
        collapse = " + "
      ),
      " + ",
      paste0(
        "s(",
        covariates_continuous[less_than_y],
        ", k = ",
        threshold_k,
        ")",
        collapse = " + "
      ),
      " + ",
      paste0(covariates_factor,
        collapse = " + "
      )
    )
    ########
  } else {
    ########
    stop(glue::glue("The {method} method is not currently supported.",
      method = method
    ))
  } # End formula outcome model

  return(form)
}

#' Fit various models with weighting
#'
#' @description
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param outcome A string indicating the outcome variable. A string.
#' @param exposure A string indicating the exposure variable. A string.
#' @param covariates A list of covariate names. A list or vector.
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param weights The `weights` element of the result of the call
#' to [estimate_weights()]. A \link[WeightIt]{weightit} object.
#' @param method The name of the model to be fitted (e.g., `glm`). A string.
#' @param method_args A named list with the following variables:
#' * `family`, family to be used within the model to be fitted.
#' * `add_inter_exposure`, check argument in [create_formula()].
#' * `add_inter_exposure_specific`, check argument in [create_formula()].
#' * `add_splines_exposure`, check argument in [create_formula()].
#' * `df_splines`, check argument in [create_formula()].
#' * `threshold_smooth`, check argument in [create_formula()].
#' * `threshold_k`, check argument in [create_formula()].
#' @md
#'
#' @returns
#'
#' @export
fit_model_weighted <- function(dat,
                               outcome,
                               exposure,
                               covariates,
                               id_var,
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
    id_var = id_var,
    method = method,
    add_inter_exposure = method_args$add_inter_exposure,
    add_inter_exposure_specific = method_args$add_inter_exposure_specific,
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

  return(list(fit = fit))
}
