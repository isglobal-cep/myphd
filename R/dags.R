#' Find common confounders
#'
#' @description
#' Given a DAG with multiple adjustment sets, find
#' all the common nodes.
#'
#' @param dag A \link[dagitty]{dagitty} object.
#' @param type_mas The type of adjustment set. A string.
#' @param type_effect The type of effect to be estimated. A string.
#'
#' @returns The list of common nodes. A list.
#'
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

#' Select confounders to minimize missing values
#'
#' @description
#' Given a dataset and a list of adjustment sets
#' identified with the \link[dagitty]{adjustmentSets} function,
#' find the set that minimizes the number of missing values
#' for the confounders.
#'
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param meta A dataframe containing the metadata. A tibble.
#' @param adjustment_sets A list of adjustment sets. A character vector.
#' @param grouping_var
#'
#' @returns An integer corresponding to the index of the adjustment
#' set that minimizes the number of missing values.
#'
#' @export
minimize_missings <- function(dat, meta, adjustment_sets,
                              grouping_var) {
  levels_group_var <- sort(levels(dat[[grouping_var]]))

  # List of dataframes with covariates from adjustment sets
  dfs_covars <- lapply(adjustment_sets, function(x) {
    mapping_covars <- meta[meta$dag %in% x, ]$variable |>
      as.character()
    tmp <- dat |>
      tidylog::select(dplyr::all_of(c(grouping_var,
                                      mapping_covars)))
  })

  # List of dataframes with fractions of missing values by grouping variable,
  # for each adjustment set
  ret_miss <- suppressMessages(lapply(dfs_covars, function(x) {
    nans <- x |>
      tidylog::group_split(.data[[grouping_var]], .keep = FALSE) |>
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
    tidylog::mutate({{grouping_var}} := levels_group_var) |>
    tidylog::relocate(.data[[grouping_var]])

  # Sum of missing values, for each adjustment set
  ret <- ret_miss |>
    tidylog::select(-{{grouping_var}}) |>
    colSums() |>
    which.min() |>
    as.integer()

  return(ret)
}

#' Visualize DAG
#'
#' Wrapper function to plot DAGs using \pkg{ggdag}.
#'
#' @param dag A \link[dagitty]{dagitty} object.
#'
#' @returns A \link[ggdag]{ggdag} object.
#'
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

#' Convert a DAG to a formatted string
#'
#' @description
#' Convert a DAG to a formatted
#' string that can be used to create DAGs with \link[ggdag]{ggdag}
#' using its formula-based input method.
#'
#' @param dag A textual description of a DAG that can
#' be read by the \link[dagitty]{dagitty} function. A string.
#'
#' @returns A formatted string.
#'
#' @export
from_dagitty_to_ggdag <- function(dag) {
  dag <- dagitty::dagitty(dag)

  to_ggdag <- ggdag::tidy_dagitty(dag) |>
    (\(x) x$data) () |>
    tidylog::select(name, to) |>
    tidylog::rename(from = name) |>
    tidylog::group_by(to) |>
    tidylog::summarise(from_formula = paste(from, collapse = " + ")) |>
    tidylog::drop_na(to) |>
    tidyr::unite(dag_formula, to, from_formula, sep = " ~ ") |>
    tidylog::ungroup()

  return(to_ggdag)
}


#' Wrapper function to derive a DAG's testable implications
#'
#' @description
#' Given a graphical model (e.g., a DAG) and a dataset, this function
#' derives the testable implications and it tests them against
#' the given dataset. It is a wrapper around the \link[dagitty]{localTests}
#' function to take into account the following points:
#' * A single DAG can result in multiple adjustment sets, and we
#' might want to test all of them.
#' * The DAG might have been built using nodes with names that differ
#' from the variables in the dataset, so we need to map them.
#' * We might have multiple exposure variables in the dataset,
#' and only one exposure node in the DAG. So we might want to
#' test all of them.
#' @md
#'
#' @param dag A \link[dagitty]{dagitty} object.
#' @param dat A dataframe containing the variables of interest. A tibble.
#' @param meta A dataframe containing the metadata. A tibble.
#' @param params A named list with the following variables:
#' * `type_mas`, the type of adjustment set.
#' * `type_effect`, the type of effect to be estimated.
#' * `identifier`, the name of the variable in `dat` to be used as
#' subject identifier.
#' @md
#'
#' @returns A named list containing the results of the call to
#' the \link[dagitty]{localTests} function for each adjustment set
#' and each exposure. A list.
#'
#' @export
test_npsem <- function(dag, dat, meta, params) {
  warning("This function has not been tested yet.",
          call. = TRUE)

  # Step 1: extract adjustment set(s)
  dag_as <- dagitty::adjustmentSets(x = dag,
                                    type = params$type_mas,
                                    effect = params$type_effect)

  res <- lapply(dag_as, function(as) {
    ret <- list()
    ret$mapping_covars <- meta[meta$dag %in% as, ] |>
      tidylog::distinct(dag, .keep_all = TRUE) |>
      tidylog::select(variable) |>
      c() |> unname() |> unlist() |> as.character()
    ret$adjustment_set <- as

    # Step 2: map nodes in DAG to variable names in dataset and extract columns
    covariates <- dat$covariates |>
      tidylog::select(params$identifier,
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
                                       tidylog::select(params$identifier,
                                                       .data[[expo]]),
                                     dat$outcome),
                                tidylog::full_join,
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
