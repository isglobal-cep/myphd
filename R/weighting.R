#' Wrapper function to estimate balancing weights
#'
#' @description
#' This functions is essentially a wrapper around the
#' \link[WeightIt]{weightit} function. The user can specify the
#' method to be used to estimate the weights.
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param exposure The name of the variable corresponding to the exposure. A string.
#' @param covariates A vector of covariates' names. A vector.
#' @param s.weights Sampling weights. A vector.
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param method The method to be used by \link[WeightIt]{weightit} to estimate the weights. A string.
#' @param method_args A named list with the following variables:
#' * `stabilize`, whether to stabilize the weights or not.
#' * `by`, a string containing the name of the variable
#' for which weighting is to be done within categories.
#' * `sl_lib`, either a vector of learners or `FALSE`, in which case
#' it uses a fixed library of learners.
#' * `sl_discrete`, whether to use discrete SuperLearner, which
#' selects the best performing method, or to find the optimal combination of predictions.
#' * `use_kernel`, whether to use kernel density estimation
#' to estimate the numerator and denominator densities for the weights. A logical.
#' * `family_link`, family to be used within the fitted model.
#' @md
#'
#' @returns A named list containing the estimated weights, and the names
#' of the exposure and covariates used.
#'
#' @export
estimate_weights <- function(dat,
                             exposure,
                             covariates,
                             s.weights,
                             id_var,
                             method,
                             method_args) {
  # Create formula from exposure and covariates
  form <- create_formula(
    dat = dat,
    outcome = NULL,
    exposure = exposure,
    covariates = covariates,
    id_var = id_var,
    method = NULL,
    add_inter_exposure = NULL,
    add_inter_exposure_specific = NULL,
    add_splines_exposure = NULL,
    df_splines = NULL,
    threshold_smooth = NULL,
    threshold_k = NULL
  )

  # Estimate weights using `WeightIt`
  ret <- WeightIt::weightit(
    formula = as.formula(form),
    data = dat,
    method = method,
    link = method_args$family_link,
    stabilize = method_args$stabilize,
    by = method_args$by,
    ps = NULL,
    s.weights = s.weights,
    subclass = NULL,
    missing = "ind",
    verbose = FALSE,
    include.obj = TRUE,
    SL.library = method_args$sl_lib,
    cvControl = list(
      V = 3,
      shuffle = FALSE
    ),
    discrete = FALSE,
    use_kernel = method_args$use_kernel,
    plot = ifelse(method_args$use_kernel == TRUE,
                  TRUE,
                  FALSE
    )
  )

  return(list(
    weights = ret,
    exposure = exposure,
    covariates = covariates
  ))
}
################################################################################

#' Title
#'
#' @param dat
#' @param idxs_selected
#' @param id_var
#' @param filter_out
#' @param list_covars
#' @param method
#' @param method_args
#' @param trim_weights
#' @param threshold_trim
#'
#' @return
#' @export
estimate_selection_weights <- function(dat, idxs_selected, id_var,
                                       filter_out,
                                       list_covars,
                                       method, method_args,
                                       trim_weights, threshold_trim) {
  # Add variable indicating whether subject was in fact selected
  dat <- dat |>
    tidylog::mutate(
      selected = ifelse(
        .data[[id_var]] %in% idxs_selected,
        1, 0
      ),
      selected = as.integer(selected)
    )
  ## Eventually filter out observations
  if (length(filter_out) > 0) {
    old_dat <- dat
    dat <- dat |>
      tidylog::filter(
        !.data[[names(filter_out)]] %in% filter_out[[1]]
      )
  }

  assertthat::assert_that(
    sum(as.integer(dat$selected)) == length(
      base::intersect(dat[[id_var]], idxs_selected)
    ),
    msg = "Number of subjects selected does not match."
  )

  # Estimate probability of selection given covariates
  sel_weights <- estimate_weights(
    dat = dat |>
      tidylog::select(
        selected,
        dplyr::all_of(list_covars)
      ),
    exposure = "selected",
    covariates = list_covars,
    s.weights = NULL,
    id_var = id_var,
    method = method,
    method_args = method_args
  )
  if (trim_weights == TRUE) {
    sel_weights <- WeightIt::trim(
      sel_weights$weights,
      at = threshold_trim,
      lower = TRUE
    )
  } else {
    sel_weights <- sel_weights$weights
  }

  # Tidy return
  ret <- tibble::tibble(
    {{ id_var }} := dat[[id_var]],
    selected = dat$selected,
    selection_weights = sel_weights$weights
  )
  ret_full <- tidylog::left_join(
    old_dat |>
      tidylog::select(-selected),
    ret,
    by = id_var
  ) |>
    tidylog::select(dplyr::all_of(c(
      id_var, "selected", "selection_weights"
    ))) |>
    tidylog::replace_na(
      list(
        selected = 0,
        selection_weights = 0
      )
    )
  ret_full <- ret_full[match(
    old_dat[[id_var]], ret_full[[id_var]]
  ), ] |>
    tibble::as_tibble()
  assertthat::assert_that(
    nrow(ret_full) == nrow(old_dat),
    msg = "Something went wrong when merging results of weighting with complete data."
  )

  return(ret)
}
################################################################################

#' Assess balance on covariate distributions generated through weighting
#'
#' @description
#' Given a dataframe, this function performs the following steps:
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
#' @param type_exposure
#' @param covariates A vector of covariates' names. A vector.
#' @param weights The `weights` element of the result of the call
#' to [estimate_weights()]. A \link[WeightIt]{weightit} object.
#' @param threshold_cor The balance threshold. A double.
#'
#' @returns A named list containing the exposure name and the results of the
#' steps described above. A list.
#'
#' @export
explore_balance <- function(exposure, type_exposure,
                            covariates,
                            weights,
                            threshold_cor) {
  # Assessing balance numerically
  tab <- cobalt::bal.tab(
    weights,
    stats = ifelse(
      type_exposure == "continuous",
      "correlations",
      "mean.diffs"
    ),
    un = TRUE,
    thresholds = c(cor = threshold_cor),
    int = TRUE,
    poly = 1
  )

  # Assessing balance graphically
  graph <- lapply(covariates, function(x) {
    cobalt::bal.plot(weights,
                     var.name = x,
                     which = "both"
    )
  })

  # Summarizing balance in a Love plot
  love <- cobalt::love.plot(
    weights,
    stats = ifelse(
      type_exposure == "continuous",
      "correlations",
      "mean.diffs"
    ),
    abs = FALSE,
    var.order = "unadjusted",
    thresholds = c(cor = threshold_cor),
    line = TRUE,
    title = exposure
  )

  return(list(
    exposure = exposure,
    tab = tab,
    graph = graph,
    love = love
  ))
}
################################################################################
