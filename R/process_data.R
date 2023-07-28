#' Extract the cohort label from the subject ID
#'
#' @description
#' This function extracts the cohort ID (e.g., SAB) from the subject ID
#' (e.g., SubjectID). It assumes that the cohort ID corresponds to the first
#' three letters of the subject ID.
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param id_var The variable name to be used to identify subjects. A string.
#'
#' @return A dataframe containing a new column named `cohort`. A tibble.
#'
#' @export
extract_cohort <- function(dat, id_var) {
  warning("Creating cohort variable from ID variable: it ",
          "assumes that the first 3 letters represent the cohort.")
  dat <- dat |>
    dplyr::mutate(cohort = substr(.data[[id_var]], 1, 3))
  dat$cohort <- as.factor(dat$cohort)

  return(dat)
}

#' Convert time variables to season
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param cols A vector of column names as strings. A vector.
#'
#' @return A dataframe with additional columns for seasons. A tibble.
#'
#' @export
convert_time_season <- function(dat, cols) {
  ret <- dat |>
    dplyr::mutate(dplyr::across(dplyr::any_of(cols),
                                ~ lubridate::month(..1))) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(cols),
                                ~ dplyr::case_when(
                                  ..1 %in% c(12, 1, 2) ~ "winter",
                                  ..1 %in% c(3, 4, 5) ~ "spring",
                                  ..1 %in% c(6, 7, 8) ~ "summer",
                                  ..1 %in% c(9, 10, 11) ~ "autumn"
                                )))

  return(ret)
}

#' Basic pre-processing of datasets
#'
#' @description
#' This function performs basic pre-processing of a given dataset:
#' \itemize{
#'  \item Data cleaning:
#'    \itemize{
#'      \item Removal of incorrect/unwanted variables.
#'      \item Remove of incorrect/unwanted subjects.
#'      \item Handling of missing values.
#'    }
#'  \item Variable transformations:
#'  \itemize{
#'      \item Transformation (e.g., log-transformation, bound outcome).
#'      \item Standardization (zero mean and unit standard deviation).
#'      \item Normalization (subtract the minimum and divide by the range).
#'    }
#' }
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param covariates A dataframe containing additional variables. A tibble.
#' @param outcome A string indicating the outcome variable. A string.
#' @param creatinine_var_names
#' @param creatinine_covariates_names
#' @param creatinine_name
#' @param dic_steps A nested named list of steps to perform. A list. It can
#' include the following elements:
#' * `llod`, to handle values <LOD/LOQ. A named list with elements:
#'  * `method`, the method to be used. A string.
#' * `missings`, to handle missing values. A named list with elements:
#'  * `threshold_within`, the missing value threshold within each group. An integer.
#'  * `threshold_overall`, the overall missing value threshold. An integer.
#' * `creatinine`, to handle confounding by dilution. A named list with elements:
#'  * `method_args`, options for fitting the models. A list.
#' * `standardization`, to standardize variables. A named list with elements:
#'  * `center_fun`, the centering function (e.g., `median`).
#'  * `scale_fun`, the scaling function (e.g., `IQR`).
#' * `bound`, to bound the outcome variable. A named list with elements:
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param by_var The variable name to group by. A string.
#' @md
#'
#' @returns A pre-processed dataset. A tibble.
#'
#' @export
preproc_data <- function(dat, covariates, outcome,
                         creatinine_var_names, creatinine_covariates_names, creatinine_name,
                         dic_steps,
                         id_var, by_var) {
  dat_ret <- dat

  for (step in names(dic_steps)) {
    dat_ret <- switch (step,
      "llodq" = handle_llodq(dat = dat_ret,
                             method = dic_steps$llodq$method),
      "creatinine" = handle_creatinine_confounding(dat = dat,
                                                   covariates = covariates,
                                                   id_var = id_var,
                                                   var_names = creatinine_var_names,
                                                   covariates_names = creatinine_covariates_names,
                                                   creatinine = creatinine_name,
                                                   method = dic_steps$creatinine$method,
                                                   method_fit_args = dic_steps$creatinine$method_fit_args),
      "missings" = handle_missing_values(dat = dat_ret,
                                         covariates = covariates,
                                         id_var = id_var,
                                         by_var = by_var,
                                         threshold_within = dic_steps$missings$threshold_within,
                                         threshold_overall = dic_steps$missings$threshold_overall)$dat_imputed,
      "standardization" = handle_standardization(dat = dat_ret,
                                                 id_var = id_var, by_var = by_var,
                                                 center_fun = dic_steps$standardization$center_fun,
                                                 scale_fun = dic_steps$standardization$scale_fun),
      "bound" = bound_outcome(dat = dat_ret,
                              var = outcome),
      stop("Invalid `step` option.")
    )
  } # End loop over steps

  return(dat = dat_ret)
}

#' Title
#'
#' @param dat
#' @param covariates
#' @param id_var
#' @param var_names
#' @param covariates_names
#' @param creatinine
#' @param method
#' @param method_fit_args
#'
#' @return
#'
#' @export
handle_creatinine_confounding <- function(dat, covariates,
                                          id_var,
                                          var_names, covariates_names, creatinine,
                                          method, method_fit_args) {
  # Covariate-adjusted standardization
  cas <- function() {
    warning("Creatinine values are currently predicted without weights.",
            call. = TRUE)

    ret <- lapply(var_names, function(var) {
      dat_proc <- dplyr::full_join(dat |>
                                     dplyr::select(dplyr::all_of(c(id_var,
                                                                 var))),
                                   covariates |>
                                     dplyr::select(dplyr::all_of(c(id_var,
                                                                 covariates_names))),
                                   by = id_var) |>
        dplyr::select(-dplyr::any_of(id_var))

      # Step 1: estimate weights for creatinine
      wts <- rep(1, lenght = nrow(dat_proc))

      # Step 2: predict creatinine with weights
      form <- paste0(
        creatinine, " ~ ",
        paste0(setdiff(covariates_names, creatinine),
               collapse = " + ")
      )
      mod_creatine <- glm(
        formula = as.formula(form),
        data = dat_proc,
        weights = wts,
        family = method_fit_args$family
      )
      cpred <- predict(mod_creatine,
                       type = "response")

      # Step 3: compute `Cratio = exposure / (C_obs / Cpred)`
      cratio <- dat_proc[[var]] / (dat_proc[[creatinine]] / cpred)

      return(cratio)
    }) |> # End loop over variables to process
      dplyr::bind_cols() |>
      tibble::as_tibble()

    return(ret)
  } # End function cas

  dat_ret <- switch (method,
    "cas" = cas(),
    stop("Invalid `method`.")
  )

  return(dat_ret)
}

#' Various strategies to handle values below the limit of detection/quantification
#'
#' @description
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param method
#'
#' @return
#'
#' @export
handle_llodq <- function(dat, method) {

  return(dat)
}

#' Various strategies to handle missing values
#'
#' @description
#' Given a dataset, this function performs the following steps:
#' * Removal of variables with a fraction of missing values greater than
#' the chosen threshold, within each group.
#' * Removal of variables with a fraction of missing values greater than
#' the chosen threshold, for the entire dataset.
#' * Imputation of the remaining variables with Random Forests using the
#' \link[missRanger]{missRanger} function.
#' @md
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param covariates A dataframe containing additional variables. A tibble.
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param by_var The variable name to group by. A string.
#' @param threshold_within The missing value threshold within each group. An integer.
#' @param threshold_overall The overall missing value threshold. An integer.
#'
#' @return A named list containing the results of the steps described above.
#' The imputed dataset is named `dat_imputed`.
#'
#' @export
handle_missing_values <- function(dat, covariates,
                                  id_var, by_var,
                                  threshold_within,
                                  threshold_overall) {

  # Step 1: group by factor and remove variables with a high
  #         fraction of missing values within each group
  step1 <- dat |>
    dplyr::select(-dplyr::all_of(id_var)) |>
    dplyr::group_by(.data[[by_var]]) |>
    naniar::miss_var_summary() |>
    dplyr::filter(pct_miss >= threshold_within) |>
    dplyr::ungroup()
  dat <- dat |>
    dplyr::select(-dplyr::all_of(step1$variable))

  # Step 2: remove variables with a high fraction of missing
  #         values overall
  step2 <- dat |>
    dplyr::select(-dplyr::all_of(id_var)) |>
    naniar::miss_var_summary() |>
    dplyr::filter(pct_miss >= threshold_overall)
  dat <- dat |>
    dplyr::select(-dplyr::all_of(step2$variable))

  # Step 3: impute the remaining variables
  vis_miss_before <- naniar::vis_miss(dat)

  ## Check whether to perform imputation by including additional variables
  if (!is.null(covariates)) {
    cols_to_remove <- colnames(covariates)
    dat <- dplyr::full_join(
      dat, covariates,
      by = id_var
    )
  }

  dat_imp <- missRanger::missRanger(data = dat,
                                    formula = as.formula(glue::glue(". ~ . -{id_var}")),
                                    num.trees = 10,
                                    pmm.k = 5)

  if (!is.null(covariates)) {
    dat <- dat |>
      dplyr::select(-dplyr::all_of(cols_to_remove))
  }

  vis_miss_after <- naniar::vis_miss(dat_imp)

  return(list(
    step1 = step1,
    step2 = step2,
    dat_imputed = dat_imp,
    vis_miss_before = vis_miss_before,
    vis_miss_after = vis_miss_after
  ))
}

#' Title
#'
#' @param dat
#' @param id_var
#' @param by_var
#' @param center_fun
#' @param scale_fun
#'
#' @return
#'
#' @export
handle_standardization <- function(dat,
                                   id_var, by_var,
                                   center_fun, scale_fun) {
  dat_ret <- dat |>
    dplyr::select(-dplyr::any_of(c(id_var, by_var))) |>
    robustHD::robStandardize(centerFun = center_fun,
                             scaleFun = scale_fun) |>
    tibble::as_tibble()
  dat_ret[[id_var]] <- dat[[id_var]]
  dat_ret <- dplyr::relocate(dat_ret, id_var)

  return(dat_ret)
}

#' Bound a outcome variable
#'
#' @description
#' In order to be able to use e.g., TMLE, with a continuous outcome,
#' it is necessary to bound it between 0 and 1. This function performs
#' the necessary steps.
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param var The variable name corresponding to the outcome to be bounded. A string.
#'
#' @return A dataframe containing the bounded outcome. A tibble.
#'
#' @export
bound_outcome <- function(dat, var) {
  b <- max(dat[[var]], na.rm = TRUE)
  a <- min(dat[[var]], na.rm = TRUE)
  num <- dat[[var]] - a
  den <- b - a
  dat[[var]] <- num / den

  assertthat::assert_that(min(dat[[var]], na.rm = TRUE) >= 0,
                          msg = "Outcome is not bounded for TMLE (min).")
  assertthat::assert_that(max(dat[[var]], na.rm = TRUE) <= 1,
                          msg = "Outcome is not bounded for TMLE (max).")

  return(dat)
}
