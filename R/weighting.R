#' Wrapper function to estimate balancing weights
#'
#' @description
#' This functions is essentially a wrapper around the
#' \link[WeightIt]{weightit} function. The user can specify the
#' method to be used to estimate the weights.
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param exposure The name of the variable corresponding to the exposure. A string.
#' @param covariates A vector of covariates' names.
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
                             covariates,
                             method,
                             method_args) {
  # Create formula from exposure and covariates
  form <- create_formula(
    dat = dat,
    outcome = NULL,
    exposure = exposure,
    covariates = covariates,
    method = method,
    add_inter_exposure = NULL,
    add_splines_exposure = NULL,
    df_splines = NULL,
    threshold_smooth = NULL,
    threshold_k = NULL
  )

  # Estimate weights using `WeightIt`
  ret <- WeightIt::weightit(formula = as.formula(form),
                            data = dat,
                            method = method,
                            ps = NULL,
                            subclass = NULL,
                            missing = "ind",
                            SL.library = method_args$sl_lib,
                            cvControl = list(
                              V = 3,
                              shuffle = TRUE
                            ),
                            discrete = FALSE,
                            use_kernel = method_args$use_kernel,
                            plot = ifelse(
                              method_args$use_kernel == TRUE,
                              TRUE,
                              FALSE
                            ),
                            verbose = FALSE,
                            include.obj = TRUE)

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
                         stats = c("c"),
                         un = TRUE,
                         thresholds = c(cor = threshold_cor),
                         int = TRUE,
                         poly = 1)

  # Assessing balance graphically
  graph <- lapply(covariates, function(x) {
    cobalt::bal.plot(weights,
                     var.name = x,
                     which = "both")
  })

  # Summarizing balance in a Love plot
  love <- cobalt::love.plot(weights,
                            stats = c("c"),
                            abs = FALSE,
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
#' @param method_args A named list with the following variables:
#' * `family`, .
#' * `add_inter_exposure`, .
#' * `add_splines_exposure`, .
#' * `df_splines`, .
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
