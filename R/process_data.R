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

#' Handle values below the limit of quantification
#'
#' @description
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param strategy
#'
#' @return
#'
#' @export
handle_loq <- function(dat, strategy) {
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
#' @param outcome A optional string indicating the outcome variable. A string.
#' @param dic_steps A nested named list of steps to perform. A list. It can
#' include the following elements:
#' * `missings`, to handle missing values. A named list with elements:
#'  * `do`, a logical indicating whether to perform this step or not. A logical.
#'  * `threshold_within`, the missing value threshold within each group. An integer.
#'  * `threshold_overall`, the overall missing value threshold. An integer.
#' * `standardization`, to standardize variables. A named list with elements:
#'  * `do`, a logical indicating whether to perform this step or not. A logical.
#'  * `center_fun`, the centering function (e.g., `mean`).
#'  * `scale_fun`, the scaling function (e.g., `sd`).
#' * `bound`, to bound the outcome variable. A named list with elements:
#'  * `do`, a logical indicating whether to perform this step or not. A logical.
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param by_var The variable name to group by. A string.
#' @md
#'
#' @returns A pre-processed dataset. A tibble.
#'
#' @export
preproc_data <- function(dat, outcome = NULL, dic_steps,
                         id_var, by_var) {
  dat_ret <- dat

  # Variable transformations: bound outcome
  if ("bound" %in% names(dic_steps)) {
    if (dic_steps$bound$do) {
      message("Bounding the outcome for TMLE.")
      dat_ret <- bound_outcome_tmle(dat_ret,
                                    var = outcome)
    }
  }

  # Data cleaning: missing values imputation
  if ("missings" %in% names(dic_steps)) {
    if (dic_steps$missings$do) {
      message("Imputing missing values.")
      res_missings <- handle_missing_values(dat = dat_ret,
                                            id_var = id_var,
                                            by_var = by_var,
                                            threshold_within = dic_steps$missings$threshold_within,
                                            threshold_overall = dic_steps$missings$threshold_overall)
      dat_ret <- res_missings$dat_imputed
    }
  }

  # Variable transformations: standardization
  if ("standardization" %in% names(dic_steps)) {
    if (dic_steps$standardization$do) {
      message("Standardizing variables using robStandardize.")
      dat_ret <- dat_ret |>
        dplyr::select(-dplyr::any_of(c(id_var, by_var))) |>
        robustHD::robStandardize(centerFun = dic_steps$standardization$center_fun,
                                 scaleFun = dic_steps$standardization$scale_fun) |>
        tibble::as_tibble()
      dat_ret[[id_var]] <- dat[[id_var]]
      dat_ret <- dplyr::relocate(dat_ret, id_var)
    }
  }

  return(dat = dat_ret)
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
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param by_var The variable name to group by. A string.
#' @param threshold_within The missing value threshold within each group. An integer.
#' @param threshold_overall The overall missing value threshold. An integer.
#'
#' @return A named list containing the results of the steps described above.
#' The imputed dataset is named `dat_imputed`.
#'
#' @export
handle_missing_values <- function(dat, id_var, by_var,
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
  dat_imp <- missRanger::missRanger(data = dat,
                                    formula = as.formula(glue::glue(". ~ . -{id_var}")),
                                    num.trees = 10,
                                    pmm.k = 5)
  vis_miss_after <- naniar::vis_miss(dat_imp)

  return(list(
    step1 = step1,
    step2 = step2,
    dat_imputed = dat_imp,
    vis_miss_before = vis_miss_before,
    vis_miss_after = vis_miss_after
  ))
}

#' Bound an outcome variable
#'
#' @description
#' In order to be able to use TMLE with a continuous outcome,
#' it is necessary to bound it between 0 and 1. This function performs
#' the necessary steps.
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param var The variable name corresponding to the outcome to be bounded. A string.
#'
#' @return A dataframe containing the bounded outcome. A tibble.
#'
#' @export
bound_outcome_tmle <- function(dat, var) {
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
