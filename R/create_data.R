#' Create toy dataset given options
#'
#' @description
#' Given a list of options, creates a toy dataset satisfying
#' those requirements.
#'
#' @param variables .
#' @param num_rows .
#' @param num_cols .
#' @param ids .
#' @returns .
#' @export
create_data <- function(variables, num_rows, num_cols, ids) {
  if (is.null(ids)) {
    ids <- paste0("id_", 1:num_rows, sep = "")
  }
}

#' Mapping between causal nodes and variables' names
#'
#' @description
#' Given a dictionary mapping the nodes of a DAG to the
#' variables' names in a dataset, and a list of nodes from the
#' same DAG, it returns the corresponding variables in the
#' dataset.
#'
#' @param dict_mapping A named list of vectors. A list.
#' @param adjustment_set A list of confounders from `dagitty::adjustmentSets`. A list.
#' @returns A list of variables' names.
#' @export
map_covars <- function(dict_mapping, adjustment_set) {
  vars <- dict_mapping[[adjustment_set]] |>
    unlist()

  return(vars)
}

#' Create dataframe for mapping of variables
#'
#' @description
#' Given a dictionary of the form `var_dag = c()`, and an optional
#' dataframe of variables' description, creates a unnested
#' dataframe of the form `var_dag, var_helix, description`.
#'
#' @param dictionary A named list of vectors. A list.
#' @param description A dataframe.
#' @returns A dataframe.
#' @export
df_mapping_vars <- function(dictionary, description) {
  ret <- dictionary |>
    purrr::map_df(tibble::enframe, .id = "name") |>
    tidyr::unnest(cols = c("value")) |>
    dplyr::rename(var_dag = name, var_helix = value)

  if (!is.null(description)) {
    description <- description |>
      dplyr::rename(var_helix = var_name)
    ret <- ret |>
      dplyr::full_join(description, by = c("var_helix"))
  }

  return(ret)
}
