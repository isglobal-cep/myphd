#' Wrapper function to estimate balancing weights
#'
#' @description
#' This functions is essentially a wrapper around the
#' \link[WeightIt]{weightit} function. The user can specify the
#' method to be used to estimate the weights.
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param exposure The name of the variable corresponding to the exposure. A string.
#' @param covariates A optional vector of covariates' names.
#' If not provided, they are extracted from `dat` excluding the exposure. A vector.
#' @param method The method to be used to estimate the weights. A string.
#' @param method_args A named list with the following variables:
#' * `use_kernel`, whether to use kernel density estimation
#' to estimate the numerator and denominator densities for the weights. A logical.
#' * `sl_discrete`, whether to use discrete SuperLearner, which
#' selects the best performing method, or to find the optimal combination of predictions.
#' * `sl_lib`, either a vector of learners or `FALSE`, in which case
#' it uses a fixed library of learners.
#' @md
#'
#' @returns A named list containing the estimated weights, and the names
#' of the exposure and covariates used.
#'
#' @export
estimate_weights <- function(dat,
                             exposure,
                             covariates = NULL,
                             method,
                             method_args) {
  # Create formula from exposure and covariates
  if (is.null(covariates)) {
    covariates <- setdiff(colnames(dat), exposure)
  }
  assertthat::assert_that(!exposure %in% colnames(covariates))
  form <- as.formula(
    paste0(exposure,
           " ~ ",
           paste0(covariates, collapse = " + "))
    )

  # Estimate weights using `WeightIt`
  other_args <- list(
    use_kernel = method_args$use_kernel,
    discrete = ifelse(method == "super",
                      method_args$sl_discrete,
                      FALSE)
  )
  if (method_args$sl_lib == FALSE) {
    method_args$sl_lib <- c("SL.bartMachine", "SL.earth", "SL.gam",
                            "SL.gbm", "SL.glm", "SL.glmnet",
                            "SL.loess", "SL.ranger")
  }
  ret <- WeightIt::weightit(formula = form,
                            data = dat,
                            method = method,
                            #estimand = method_args$estimand,
                            #stabilize = method_args$stabilize,
                            #by = method_args$by,
                            #s.weights = method_args$s.weights,
                            ps = NULL,
                            SL.library = method_args$sl_lib,
                            subclass = NULL,
                            missing = ,
                            verbose = FALSE,
                            include.obj = TRUE,
                            ... = other_args)

  return(list(
    weights = ret,
    exposure = exposure,
    covariates = covariates
  ))
}

#' Assess balance on covariate distributions generated through weighting
#'
#' @description
#' Given a dataset, this function performs the following steps:
#' * Generate balance statistics on covariates in relation to a
#' exposure variable, using the \link[cobalt]{bal.tab} function.
#' * Generate plots displaying distributional balance
#' between exposure and covariates,
#' using the \link[cobalt]{bal.plot} function.
#' * Generate a Love plot displaying covariate balance
#' before and after adjusting, using the \link[cobalt]{love.plot} function.
#' @md
#'
#' @param exposure The name of the variable corresponding to the exposure. A string.
#' @param covariates A vector of covariates' names. A vector.
#' @param weights The `weights` element of the result of the call
#' to [estimate_weights()]. A \link[WeightIt]{weightit} object.
#' @param threshold_cor The balance threshold. A double.
#'
#' @return A named list containing the exposure name and the results of the
#' steps described above. A list.
#'
#' @export
explore_balance <- function(exposure,
                            covariates,
                            weights,
                            threshold_cor = 0.1) {
  # Assessing balance numerically
  tab <- cobalt::bal.tab(weights,
                         stats = c("c", "k"),
                         un = TRUE,
                         thresholds = c(cor = threshold_cor),
                         poly = 3)

  # Assessing balance graphically
  graph <- lapply(covariates, function(x) {
    cobalt::bal.plot(weights,
                     var.name = x,
                     which = "both")
  })

  # Summarizing balance in a Love plot
  love <- cobalt::love.plot(weights,
                            stats = c("c", "ks"),
                            abs = TRUE,
                            var.order = "unadjusted",
                            thresholds = c(cor = threshold_cor),
                            line = TRUE,
                            title = exposure)

  return(list(
    exposure = exposure,
    tab = tab,
    graph = graph,
    love = love
  ))
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
#' @param method_args
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
  assertthat::are_equal(sort(covariates),
                        sort(c(covariates_continuous,
                               covariates_factor)))
  form <- paste0(
    outcome, " ~ ",
    exposure, " + ",
    paste0(covariates_continuous,
           collapse = " + ")
  )
  form <- paste0(
    form, " + ",
    paste0("factor(",
           covariates_factor,
           ")",
           collapse = " + ")
  )

  # Fit model
  if (method == "glm") {
    fit <- glm(
      formula = as.formula(form),
      data = dat,
      weights = weights
    )
  } else if (method == "gam") {
  } else if (method == "super") {
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
