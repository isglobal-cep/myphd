#' Visualize the distribution of the shifted exposures
#'
#' @description
#' Compare the distribution, using a violin plot, of the original
#' and shifted exposures. The shifted exposures are obtained
#' applying a shift function, that can be multiplicative or
#' additive.
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param shift_type A string indicating whether to apply a
#' multiplicative (`mul`) or additive (`add`) type of shift. A string.
#' @param shift_amount The shift value to be applied. A double.
#'
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @export
explore_shift <- function(dat,
                          shift_type,
                          shift_amount) {

  # Apply shift function
  shifted <- lapply(colnames(dat), function(x) {
    dd <- switch(shift_type,
                 "mul" = dat[[x]] * shift_amount,
                 "add" = dat[[x]] + shift_amount)
    return(dd)
  })
  shifted <- suppressWarnings(
    tibble::as_tibble(shifted, .name_repair = "unique")
  )
  colnames(shifted) <- colnames(dat)

  # Prepare data
  shifted <- shifted |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::mutate(shifted = TRUE)
  dat_long <- dat |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::mutate(shifted = FALSE)
  dat_gg <- dplyr::bind_rows(dat_long, shifted)

  # Compare distributions
  ret <- dat_gg |>
    dplyr::group_by(name) |>
    ggplot2::ggplot(ggplot2::aes(x = name,
                                y = value,
                                fill = shifted)) +
    ggplot2::geom_violin() +
    ggplot2::facet_grid(shifted ~ .,
                        scales = "free_y")

  return(ret)
}

#' A function to describe the used dataset
#'
#' @description
#' Given a dataset, this function performs the following steps:
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
#' * Visualization of variables' summaries with the
#' \link[Hmisc]{summaryM} function.
#' * Visualization of the correlation structure of the dataset using
#' the \link[corrr]{correlate} and \link[corrr]{rplot} functions,
#' with both Pearson's and Spearman's correlation.
#' @md
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param grouping_var The variable name to group by. A string.
#'
#' @return A nested named list containing the objects corresponding to the
#' steps described above.
#'
#' @export
describe_data <- function(dat, id_var, grouping_var) {
  dat <- extract_cohort(dat, id_var)
  dat <- dat |>
    dplyr::select(-dplyr::all_of(id_var))

  ##############################################################################
  # Step 1: simple diagnose of numerical and categorical variables w/ `dlookr`
  step1_num <- dlookr::diagnose_numeric(dat) |>
    dplyr::arrange(variables)
  step1_cat <- dlookr::diagnose_category(dat) |>
    dplyr::arrange(variables)
  ##############################################################################

  ##############################################################################
  # Step 2: diagnose of numerical and categorical variables by factor w/ `dlookr`
  step2_num <- step2_cat <- NULL
  if (!is.null(grouping_var)) {
    dat <- dat |>
      dplyr::group_by(dplyr::across(grouping_var),
                      .drop = FALSE)
    step2_num <- dlookr::diagnose_numeric(dat) |>
      dplyr::arrange(variables, cohort)
    suppressWarnings(
      step2_cat <- dlookr::diagnose_category(dat, -grouping_var)
    )
    if (!is.null(step2_cat)) {
      step2_cat <- dplyr::arrange(step2_cat, variables, cohort)
    }
    dat <- dplyr::ungroup(dat)
  }
  ##############################################################################

  ##############################################################################
  # Step 3: summary of variables w/ `gtsummary`
  step3 <- dat |>
    gtsummary::tbl_summary(by = grouping_var,
                           statistic = list(
                             gtsummary::all_continuous() ~ c(
                               "{median} ({p25}, {p75})"
                             ),
                             gtsummary::all_categorical() ~ "{n} ({p}%)"
                           )) |>
    gtsummary::add_overall()
  ##############################################################################

  ##############################################################################
  # Step 4: detailed summary of variables w/ `gtsummary`
  step4 <- dat |>
    gtsummary::tbl_summary(by = grouping_var,
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
                           )) |>
    gtsummary::add_overall()
  ##############################################################################

  ##############################################################################
  # Step 5: visualize summaries w/ `summaryM` from `Hmisc`
  vars <- setdiff(names(dat), grouping_var)
  form <- as.formula(paste(paste(vars, collapse = '+'),
                           '~',
                           grouping_var))
  step5 <- Hmisc::summaryM(formula = form, data = dat,
                           test = TRUE,
                           na.include = TRUE, overall = TRUE)
  #step5_plot_con <- plot(step5, which = "continuous")
  #step5_plot_cat <- plot(step5, which = "categorical")
  ##############################################################################

  ##############################################################################
  # Step 6: correlation structure variables
  corrs_pearson <- corrr::correlate(x = dat,
                                    method = "pearson") |>
    corrr::rearrange(absolute = TRUE)
  corrs_spearman <- corrr::correlate(x = dat,
                                     method = "spearman") |>
    corrr::rearrange(absolute = TRUE)
  viz_corr_pearson <- corrr::rplot(corrs_pearson,
                                   print_cor = TRUE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 60, hjust = 1
    ))
  viz_corr_spearman <- corrr::rplot(corrs_spearman,
                                    print_cor = TRUE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 60, hjust = 1
    ))
  ##############################################################################

  return(list(
    step1 = list(num = step1_num,
                 cat = step1_cat),
    step2 = list(num = step2_num,
                 cat = step2_cat),
    step3 = step3,
    step4 = step4,
    step5 = step5,
    step6 = list(corr_pearson = corrs_pearson,
                 viz_corr_pearson = viz_corr_pearson,
                 corr_spearman = corrs_spearman,
                 viz_corr_spearman = viz_corr_spearman)
  ))
}

#' A function to explore the missing values of a dataset
#'
#' @description
#' Given a dataset, this function performs the following steps:
#' * Count and percentage of missing values by cohort, for each variable.
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
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param grouping_var The variable name to group by. A string.
#' @param path_save The path where to store the missingness report. A string.
#'
#' @return A named list containing the objects corresponding to the
#' steps described above.
#'
#' @export
explore_missings <- function(dat, id_var, grouping_var, path_save) {
  dat <- extract_cohort(dat, id_var)

  ##############################################################################
  # Step 1: Count and percentage of missings by cohort, for each variable
  step1 <- aggregate(
    formula = . ~ cohort,
    data = dat,
    FUN = function(x) { sum(is.na(x)) },
    na.action = NULL
  )
  rownames(step1) <- levels(dat$cohort)
  step1 <- tibble::as_tibble(step1)
  subjects_by_cohort <- dat |>
    dplyr::group_by(cohort) |>
    dplyr::summarise(n = dplyr::n())
  step1_perc <- step1 |>
    dplyr::mutate(dplyr::across(!c(cohort),
                                ~ round(. / subjects_by_cohort$n * 100, 0))) |>
    tibble::as_tibble()
  ##############################################################################

  ##############################################################################
  # Step 2: summary missings of variables by grouping variable (`naniar`)
  # Same information as in step1 but in long format
  step2 <- dat |>
    dplyr::group_by(.data[[grouping_var]]) |>
    naniar::miss_var_summary() |>
    dplyr::ungroup()

  step2_wide <- dat |>
    dplyr::group_by(.data[[grouping_var]]) |>
    naniar::miss_var_summary() |>
    dplyr::select(-c(n_miss)) |>
    tidyr::pivot_wider(names_from = c(cohort),
                       values_from = c(pct_miss)) |>
    dplyr::arrange(variable) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                round, 0))
  ##############################################################################

  ##############################################################################
  # Step 3: summary missings of cases (`naniar`)
  step3 <- dat |>
    naniar::miss_case_summary(order = FALSE)
  step3 <- step3 |>
    dplyr::mutate(case = dat[[id_var]]) |>
    dplyr::arrange(desc(pct_miss), id_var)
  ##############################################################################

  ##############################################################################
  # Step 4: visualize missingness report
  plt <- visdat::vis_miss(dat,
                          show_perc = TRUE) +
    ggplot2::scale_y_discrete(limits = dat$cohort,
                              labels = dat$cohort)
  ggplot2::ggsave(filename = paste0(path_save,
                                    "vis_miss_",
                                    ".png"),
                  dpi = 720,
                  height = 15,
                  width = 30,
                  bg = "white")
  ##############################################################################

  ##############################################################################
  # Step 5: MCAR test
  step5 <- lapply(
    levels(dat$cohort),
    function(x) {
      tryCatch(
        dat |>
          dplyr::filter(cohort == x) |>
          dplyr::select(-c(HelixID, cohort)) |>
          naniar::mcar_test(),
        error = function(e) NULL
      )
    }
  )
  names(step5) <- levels(dat$cohort)
  ##############################################################################

  return(list(
    missings_byCohort_eachVar = step1,
    missings_byCohort_eachVar_perc = step1_perc,
    summary_missings_byCohort_eachVar = step2,
    summary_missings_byCohort_eachVar_wide = step2_wide,
    summary_missings_byCases = step3,
    mcar_tests_byCohort = step5
  ))
}