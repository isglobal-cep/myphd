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
#' @examples
#' myphd::from_dagitty_to_ggdag(dag)$dag_formula |> cat(sep = ",\n")
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

#' Test conditional independencies
#'
#' @description
#' Check whether the assumptions encoded in the DAG
#' are consistent with the data.
#'
#' @param dag_code Input DAG name. A string.
#' @param dat The dataset to test the DAG against. A dataframe.
#' @param params A named list containing eventual parameters.
#' @returns The results of `dagitty::localTests`.
#' @export
test_npsem <- function(dag_code, dat, params) {
  # Step 0: create dagitty object
  dag <- dagitty::dagitty(dag_code) |>
    ggdag::tidy_dagitty()

  # Step 1: extract adjustment set(s)
  dag_as <- dagitty::adjustmentSets(x = dag,
                                    type = params$type_mas,
                                    effect = params$type_effect)
  if (length(dag_as) > 1) {
    print(dag_as)
    cat("\n")
    idx = readline(prompt = "There are multiple adjustment sets. Select one: ") |>
      as.integer()
    dag_as <- dag_as[[idx]]
  }

  # Step 2: map nodes in DAG to variable names in dataset and extract columns
  dag_as_clean <- map_covariates(adj_set = dag_as, params = params)
  dat_test <- dat |>
    dplyr::select(dplyr::all_of(dag_as_clean))

  # Step 3: test independencies
  ret <- dagitty::localTests(x = dag, data = dat_test,
                             type = "cis.loess", R = 5)
}
