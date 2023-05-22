#' Basic pre-processing of datasets
#'
#' @description
#' Perform basic pre-processing of a given dataset:
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
#' @param dat A dataset with variables as columns. A data.table.
#' @param dic_steps A named list of steps to perform. A list.
#' @returns A pre-processed dataset. A data.table.
#' @export
preproc_data <- function(dat, outcome = NULL, dic_steps) {
  dat_ret <- dat

  # Variable transformations: bound outcome
  if (exists("bound", dic_steps) & dic_steps$bound$do) {
    dat_ret <- bound_outcome_tmle(dat_ret,
                                  var = outcome)
  }

  # Data cleaning: missing values imputation
  if (exists("missings", dic_steps) & dic_steps$missings$do) {
    warning("The value of `pmm.k` for missRanger must be selected.")
    dat_ret <- missRanger::missRanger(data = dat_ret,
                                      formula = . ~ . - HelixID,
                                      pmm.k = 3)
  }

  # Variable transformations: standardization
  if (exists("standardization", dic_steps) & dic_steps$standardization$do) {
    dat_ret <- scale(dplyr::select(dat_ret, -HelixID),
                     center = dic_steps$standardization$center,
                     scale = dic_steps$standardization$scale)
    dat_ret <- dat_ret |>
      tibble::as_tibble() |>
      dplyr::mutate(HelixID = dat$HelixID) |>
      dplyr::relocate(HelixID)
  }

  return(dat_ret)
}

#' Title
#'
#' @param dat
#' @param by_var
#' @param threshold
#' @param path_store_reports
#'
#' @return
#' @export
handle_missing_values <- function(dat, by_var, threshold, path_store_reports) {
  if (!is.null(by_var)) {
    missings <- dat |>
      dplyr::group_by(by_var) |>
      naniar::miss_var_summary() |>
      dplyr::filter(pct_miss > threshold) |>
      dplyr::ungroup()
  }
}

#' Title
#'
#' @param dat
#' @param var
#'
#' @return
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
