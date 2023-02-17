#' Visualize DAG
#'
#' Wrapper function to plot DAGs using \pkg{ggdag}.
#'
#' @param dag_code Input DAG name. A string.
#' @returns A \pkg{ggplot} object.
#' @export
visualize_dag <- function(dag_code) {
  # Create dagitty object
  dag <- dagitty::dagitty(dag_code) |>
    ggdag::tidy_dagitty()

  # Plot DAG
  plt <- ggdag::ggdag(dag) +
    ggdag::geom_dag_edges() +
    ggdag::geom_dag_point(fill = "white", color = "black",
                          shape = 21) +
    ggdag::geom_dag_text(col = "black") +
    ggdag::scale_adjusted() +
    ggdag::theme_dag()
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
