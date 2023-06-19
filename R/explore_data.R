#' Title
#'
#' @param shift_func
#'
#' @return
#' @export
#'
#' @examples
explore_shift <- function(dat,
                          shift_type,
                          shift_amount,
                          shift_lower_bound,
                          shift_upper_bound) {

  # Apply shift function
  shifted <- lapply(colnames(dat), function(x) {
    dd <- switch(shift_type,
                 "mul" = dat[[x]] * shift_amount,
                 "add" = dat[[x]] + shift_amount)
    #dd[dd < shift_lower_bound] <- shift_lower_bound
    #dd[dd > shift_upper_bound] <- shift_upper_bound
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

#' Title
#'
#' @param dat
#' @param id_var
#' @param grouping_var
#'
#' @return
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

#' Title
#'
#' @param dat
#' @param id_var
#' @param grouping_var
#' @param path_save
#'
#' @return
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
