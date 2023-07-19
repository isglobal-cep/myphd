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
  assertthat::are_equal(sort(covariates),
                        sort(c(covariates_continuous,
                               covariates_factor)))
  assertthat::assert_that(!exposure %in% covariates)

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