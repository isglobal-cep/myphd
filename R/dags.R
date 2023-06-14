#' Find common confounders
#'
#' Given a DAG with multiple adjustment sets, find
#' all the common nodes.
#'
#' @param dag A \pkg{dagitty} object.
#' @param type_mas The type of adjustment set.
#' @param type_effect The type of effect to be estimated.
#' @returns A list.
#' @export
find_common_confounders <- function(dag, type_mas, type_effect) {
  adj_sets <- dagitty::adjustmentSets(x = dag,
                                      type = type_mas,
                                      effect = type_effect)
  common_nodes <- adj_sets |>
    unlist() |>
    unname() |>
    unique()

  return(common_nodes)
}

#' Title
#'
#' @param dat
#' @param meta
#' @param adjustment_sets
#'
#' @return
#' @export
minimize_missings <- function(dat, meta, adjustment_sets) {
  cohorts <- levels(dat$cohort)

  # List of dataframes with covariates from adjustment sets
  dfs_covars <- lapply(adjustment_sets, function(x) {
    mapping_covars <- meta[meta$dag %in% x, ]$variable |>
      as.character()
    tmp <- dat |>
      dplyr::select("cohort",
                    dplyr::all_of(mapping_covars))
  })

  # List of dataframes with fractions of missing values by cohort,
  # for each adjustment set
  ret_miss <- suppressMessages(lapply(dfs_covars, function(x) {
    nans <- x |>
      dplyr::group_split(cohort, .keep = FALSE) |>
      lapply(function(y) {
        round(sum(is.na(y)) / (nrow(y) * ncol(y)) * 100, 0)
      }) |>
      unname() |>
      unlist()
    nans <- tibble::as_tibble(nans)
  }) |>
    purrr::reduce(dplyr::bind_cols))
  colnames(ret_miss) <- paste0("adj_set_", 1:ncol(ret_miss))
  ret_miss <- ret_miss |>
    dplyr::mutate(cohort = cohorts) |>
    dplyr::relocate(cohort)

  # Sum of missing values, for each adjustment set
  ret <- ret_miss |>
    dplyr::select(-cohort) |>
    colSums() |>
    which.min() |>
    as.integer()

  return(ret)
}

#' Visualize DAG
#'
#' Wrapper function to plot DAGs using \pkg{ggdag}.
#'
#' @param dag A \pkg{dagitty} object.
#' @returns A \pkg{ggdag} object.
#' @export
visualize_dag <- function(dag) {
  ret <- dag |>
    ggdag::tidy_dagitty() |>
    ggdag::node_status() |>
    ggplot2::ggplot(ggplot2::aes(x, y,
                                 xend = xend, yend = yend,
                                 color = status)) +
    ggdag::geom_dag_edges() +
    ggdag::geom_dag_point() +
    geom_dag_text_repel() +
    ggokabeito::scale_color_okabe_ito(na.value = "grey90") +
    theme_dag() +
    ggplot2::coord_cartesian(clip = "off")

  return(ret)
}

#' Convert a \pkg{dagitty} DAG to a formatted string
#'
#' @description
#' Convert a DAG from \pkg{dagitty} to a formatted
#' string that can be used to create DAGs with \pkg{ggdag}
#' using its formula-based input method.
#'
#' @param dag A \pkg{dagitty} DAG. A string.
#' @returns A formatted string.
#' @export
from_dagitty_to_ggdag <- function(dag) {
  dag <- dagitty::dagitty(dag)

  to_ggdag <- ggdag::tidy_dagitty(dag) |>
    (\(x) x$data) () |>
    dplyr::select(name, to) |>
    dplyr::rename(from = name) |>
    dplyr::group_by(to) |>
    dplyr::summarise(from_formula = paste(from, collapse = " + ")) |>
    tidyr::drop_na(to) |>
    tidyr::unite(dag_formula, to, from_formula, sep = " ~ ")

  return(to_ggdag)
}


#' Title
#'
#' @param dag_code
#' @param dat
#' @param meta
#' @param params
#'
#' @return
#' @export
test_npsem <- function(dag, dat, meta, params) {
  # Step 1: extract adjustment set(s)
  dag_as <- dagitty::adjustmentSets(x = dag,
                                    type = params$type_mas,
                                    effect = params$type_effect)

  res <- lapply(dag_as, function(as) {
    ret <- list()
    ret$mapping_covars <- meta[meta$dag %in% as, ] |>
      dplyr::distinct(dag, .keep_all = TRUE) |>
      dplyr::select(variable) |>
      c() |> unname() |> unlist() |> as.character()
    ret$adjustment_set <- as

    # Step 2: map nodes in DAG to variable names in dataset and extract columns
    covariates <- dat$covariates |>
      dplyr::select(params$identifier,
                    dplyr::all_of(ret$mapping_covars))
    colnames(covariates) <- c(params$identifier,
                              meta[meta$variable %in% ret$mapping_covars, ]$dag |>
                                as.character())
    covariates <- covariates[, !duplicated(colnames(covariates))]

    # Step 3: test independencies for each exposure
    exposure_list <- setdiff(colnames(dat$exposures), params$identifier)
    ret$tests <- lapply(exposure_list, function(expo) {
      dat_test <- purrr::reduce(list(covariates,
                                     dat$exposures |>
                                       dplyr::select(params$identifier,
                                                     .data[[expo]]),
                                     dat$outcome),
                                dplyr::inner_join,
                                by = params$identifier)
      test <- dagitty::localTests(x = dag,
                                  data = dat_test,
                                  type = "cis.loess",
                                  R = 3)
      return(test)
    }) # End lapply over exposures

    return(ret)
  }) # End lapply over adjustment sets

  return(res)
}
