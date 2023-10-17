#' Visualize the distribution of the shifted exposures
#'
#' @description
#' Compare the distribution, using a violin plot, of the original
#' and shifted exposures. The shifted exposures are obtained
#' applying a shift function, that can be multiplicative or
#' additive.
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param shift_type A string indicating whether to apply a
#' multiplicative (`mul`) or additive (`add`) type of shift. A string.
#' @param shift_amount The shift value to be applied. A double.
#'
#' @returns A \link[ggplot2]{ggplot} object.
#'
#' @export
explore_shift <- function(dat,
                          shift_type,
                          shift_amount) {
  # Apply shift function
  shifted <- lapply(colnames(dat), function(x) {
    dd <- switch(shift_type,
      "mul" = dat[[x]] * shift_amount,
      "add" = dat[[x]] + shift_amount
    )
    return(dd)
  })
  shifted <- suppressWarnings(
    tibble::as_tibble(shifted, .name_repair = "unique")
  )
  colnames(shifted) <- colnames(dat)

  # Prepare data
  shifted <- shifted |>
    tidylog::pivot_longer(cols = dplyr::everything()) |>
    tidylog::mutate(shifted = TRUE)
  dat_long <- dat |>
    tidylog::pivot_longer(cols = dplyr::everything()) |>
    tidylog::mutate(shifted = FALSE)
  dat_gg <- dplyr::bind_rows(dat_long, shifted)

  # Compare distributions
  ret <- dat_gg |>
    tidylog::group_by(name) |>
    ggplot2::ggplot(ggplot2::aes(
      x = name,
      y = value,
      fill = shifted
    )) +
    ggplot2::geom_violin() +
    ggplot2::facet_grid(shifted ~ .,
      scales = "free_y"
    )

  return(ret)
}

#' A function to describe the used dataframe
#'
#' @description
#' Given a dataframe, this function performs the following steps:
#' * Simple diagnose of numerical and categorical
#' variables with the \link[dlookr]{diagnose_numeric} and
#' \link[dlookr]{diagnose_category} functions, respectively.
#' * Diagnose of numerical and categorical
#' variables with the \link[dlookr]{diagnose_numeric} and
#' \link[dlookr]{diagnose_category} functions, respectively,
#' after grouping by a factor (e.g., cohort).
#' * Concise summary of continuous and categorical variables with
#' the \link[gtsummary]{tbl_summary} function.
#' * Detailed summary of continuous and categorical variables with
#' the \link[gtsummary]{tbl_summary} function.
#' * Visualization of the correlation structure of the dataframe using
#' the \link[corrr]{correlate} and \link[corrr]{rplot} functions,
#' with both Pearson's and Spearman's correlation.
#' @md
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param by_var The variable name to group by. A string.
#'
#' @returns A nested named list containing the objects corresponding to the
#' steps described above.
#'
#' @export
describe_data <- function(dat, id_var, by_var) {
  dat <- dat |>
    tidylog::select(-dplyr::all_of(id_var))

  ##############################################################################
  # Step 1: simple diagnose of numerical and categorical variables w/ `dlookr`
  step1_cat <- NULL
  step1_num <- dlookr::diagnose_numeric(dat) |>
    dplyr::arrange(variables)
  suppressWarnings(
    step1_cat <- dlookr::diagnose_category(dat) |>
      dplyr::arrange(variables)
  )
  ##############################################################################

  ##############################################################################
  # Step 2: diagnose of numerical and categorical variables by factor w/ `dlookr`
  step2_num <- step2_cat <- NULL
  if (!is.null(by_var)) {
    dat <- dat |>
      tidylog::group_by(dplyr::across(by_var),
        .drop = FALSE
      )
    step2_num <- dlookr::diagnose_numeric(dat) |>
      dplyr::arrange(variables, by_var)
    suppressWarnings(
      step2_cat <- dlookr::diagnose_category(dat, -by_var)
    )
    if (!is.null(step2_cat)) {
      step2_cat <- dplyr::arrange(step2_cat, variables, by_var)
    }
    dat <- tidylog::ungroup(dat)
  }
  ##############################################################################

  ##############################################################################
  # Step 3: summary of variables w/ `gtsummary`
  step3 <- dat |>
    gtsummary::tbl_summary(
      by = by_var,
      statistic = list(
        gtsummary::all_continuous() ~ c(
          "{median} ({p25}, {p75})"
        ),
        gtsummary::all_categorical() ~ "{n} ({p}%)"
      )
    )
  if (!is.null(by_var)) {
    step3 <- step3 |>
      gtsummary::add_overall()
  }
  ##############################################################################

  ##############################################################################
  # Step 4: detailed summary of variables w/ `gtsummary`
  step4 <- dat |>
    gtsummary::tbl_summary(
      by = by_var,
      type = list(
        gtsummary::all_continuous() ~ "continuous2",
        gtsummary::all_categorical() ~ "categorical"
      ),
      statistic = list(
        gtsummary::all_continuous() ~ c(
          "{N_obs} ({p_nonmiss}%)",
          "{median} ({p25}, {p75})",
          "{min}, {max}"
        ),
        gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
      )
    )
  if (!is.null(by_var)) {
    step4 <- step4 |>
      gtsummary::add_overall()
  }
  ##############################################################################

  ##############################################################################
  # Step 5: correlation structure variables
  uses <- c("everything", "complete.obs", "pairwise.complete.obs")
  corrs_pearson <- corrs_spearman <- NULL
  viz_corr_pearson <- viz_corr_spearman <- NULL

  ## Pearson
  computed <- FALSE
  i <- 1
  while ((computed == FALSE) & (i <= length(uses))) {
    use <- uses[[i]]
    i <- i + 1
    corrs_pearson <- tryCatch(
      {
        ret <- corrr::correlate(
          x = dat,
          method = "pearson",
          use = use
        ) |>
          corrr::rearrange(absolute = TRUE)
        computed <- TRUE
        ret
      },
      error = function(ee) {
        warning("Error while computing correlation matrix:\n", ee)
      }
    )
  } # End while for Pearson
  if (!is.null(corrs_pearson)) {
    viz_corr_pearson <- corrr::rplot(corrs_pearson,
      print_cor = TRUE
    ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(
        angle = 60, hjust = 1
      )) +
      ggplot2::labs(
        caption = paste0(
          "Correlation use: ",
          use, "."
        )
      )
  }

  ## Spearman
  computed <- FALSE
  i <- 1
  while ((computed == FALSE) & (i <= length(uses))) {
    use <- uses[[i]]
    i <- i + 1
    corrs_spearman <- tryCatch(
      {
        ret <- corrr::correlate(
          x = dat,
          method = "spearman",
          use = use
        ) |>
          corrr::rearrange(absolute = TRUE)
        computed <- TRUE
        ret
      },
      error = function(ee) {
        warning("Error while computing correlation matrix:\n", ee)
      }
    )
  } # End while for Spearman
  if (!is.null(corrs_spearman)) {
    viz_corr_spearman <- corrr::rplot(corrs_spearman,
      print_cor = TRUE
    ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(
        angle = 60, hjust = 1
      )) +
      ggplot2::labs(
        caption = paste0(
          "Correlation use: ",
          use, "."
        )
      )
  }
  ##############################################################################

  return(list(
    step1 = list(
      num = step1_num,
      cat = step1_cat
    ),
    step2 = list(
      num = step2_num,
      cat = step2_cat
    ),
    step3 = step3,
    step4 = step4,
    step5 = list(
      corr_pearson = corrs_pearson,
      viz_corr_pearson = viz_corr_pearson,
      corr_spearman = corrs_spearman,
      viz_corr_spearman = viz_corr_spearman
    )
  ))
}

#' A function to explore the missing values of a dataframe
#'
#' @description
#' Given a dataframe, this function performs the following steps:
#' * Summary of missing values for the variables, grouped by a factor,
#' using the \link[naniar]{miss_var_summary} function.
#' The information is the same as that provided in the previous step,
#' but also in long format.
#' * Summary of missing values for the cases
#' using the \link[naniar]{miss_case_summary} function.
#' * Missingness report using the \link[visdat]{vis_miss} function.
#' * Little's test statistic to assess if data is missing
#' completely at random (MCAR), using the \link[naniar]{mcar_test} function.
#' The null hypothesis in this test is that the data is MCAR.
#' @md
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param by_var The variable name to group by. A string.
#' @param path_save The path where to store the missingness report. A string.
#'
#' @returns A named list containing the objects corresponding to the
#' steps described above.
#'
#' @export
explore_missings <- function(dat, id_var, by_var, path_save) {
  # Step 1: checks
  if (!by_var %in% colnames(dat)) {
    stop("Grouping variable not found.", call. = TRUE)
  }

  ##############################################################################
  # Step 2: summary missings of variables by grouping variable (`naniar`)
  step2 <- dat |>
    tidylog::group_by(.data[[by_var]]) |>
    naniar::miss_var_summary() |>
    tidylog::ungroup()

  step2_wide <- dat |>
    tidylog::group_by(.data[[by_var]]) |>
    naniar::miss_var_summary() |>
    tidylog::select(-c(n_miss)) |>
    tidylog::pivot_wider(
      names_from = c(by_var),
      values_from = c(pct_miss)
    ) |>
    dplyr::arrange(variable) |>
    tidylog::mutate(dplyr::across(
      dplyr::where(is.numeric),
      \(x) round(x, digits = 0)
    )) |>
    tidylog::ungroup()
  ##############################################################################

  ##############################################################################
  # Step 3: summary missings of cases (`naniar`)
  step3 <- dat |>
    naniar::miss_case_summary(order = FALSE)
  step3 <- step3 |>
    tidylog::mutate(case = dat[[id_var]]) |>
    dplyr::arrange(desc(pct_miss), id_var)
  ##############################################################################

  ##############################################################################
  # Step 4: visualize missingness reports
  plt <- visdat::vis_miss(dat,
    show_perc = TRUE
  ) +
    ggplot2::scale_y_discrete(
      limits = dat[[by_var]],
      labels = dat[[by_var]]
    )
  ggplot2::ggsave(
    filename = paste0(
      path_save,
      "vis_miss",
      ".png"
    ),
    dpi = 720,
    height = 15,
    width = 30,
    bg = "white"
  )

  plt <- naniar::gg_miss_fct(
    x = dat,
    fct = !!rlang::ensym(by_var)
  )
  ggplot2::ggsave(
    filename = paste0(
      path_save,
      "gg_miss",
      ".png"
    ),
    dpi = 720,
    height = 15,
    width = 30,
    bg = "white"
  )
  ##############################################################################

  ##############################################################################
  # Step 5: MCAR test
  step5 <- lapply(
    levels(dat[[by_var]]),
    function(x) {
      tryCatch(
        dat |>
          tidylog::filter(.data[[by_var]] == x) |>
          tidylog::select(-dplyr::any_of(c(
            id_var,
            by_var
          ))) |>
          naniar::mcar_test(),
        error = function(e) NULL
      )
    }
  )
  names(step5) <- levels(dat[[by_var]])
  ##############################################################################

  return(list(
    summary_missings_byFct_eachVar = step2,
    summary_missings_byFct_eachVar_wide = step2_wide,
    summary_missings_byCases = step3,
    mcar_tests_byFct = step5
  ))
}
