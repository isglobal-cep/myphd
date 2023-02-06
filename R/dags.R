#' Visualize DAG.
#' `visualize.dag` is a wrapper function to plot DAGs
#' using \pkg{ggdag}.
#'
#' @param dag.code Input DAG name. A string.
#' @returns A \pkg{ggplot} object.
#' @importFrom magrittr %>%
#' @export
visualize.dag <- function(dag.code) {
  # Create dagitty object
  dag <- dagitty::dagitty(dag.code) %>%
    ggdag::tidy_dagitty()

  # Plot DAG
  plt <- ggdag::ggdag(dag) +
    ggdag::geom_dag_edges() +
    ggdag::geom_dag_point(fill = "white", color = "black",
                          shape = 21) +
    ggdag::geom_dag_text(col = "black") +
    ggdag::scale_adjusted() +
    ggdag::theme_dag()

  return(plt)
}

#' Test conditional independencies.
#' `test.npsem` checks whether the assumptions
#' encoded in the DAG are consistent with the data.
#'
#' @param dag.code Input DAG name. A string.
#' @param dat The dataset to test the DAG against. A dataframe.
#' @param params A named list containing eventual parameters.
#' @returns The results of `dagitty::localTests`.
#' @importFrom magrittr %>%
#' @export
test.npsem <- function(dag.code, dat, params) {
  # Step 0: create dagitty object
  dag <- dagitty::dagitty(dag.code) %>%
    ggdag::tidy_dagitty()

  # Step 1: extract adjustment set(s)
  dag.as <- dagitty::adjustmentSets(x = dag,
                                    type = params$type.mas,
                                    effect = params$type.effect)
  if (length(dag.as) > 1) {
    print(dag.as)
    cat("\n")
    idx = readline(prompt = "There are multiple adjustment sets. Select one: ") %>%
      as.integer()
    dag.as <- dag.as[[idx]]
  }

  # Step 2: map nodes in DAG to variable names in dataset and extract columns
  dag.as.clean <- map.covariates(adj.set = dag.as, params = params)
  dat.test <- dat %>% dplyr::select(dplyr::all_of(dag.as.clean))

  # Step 3: test independencies
  ret <- dagitty::localTests(x = dag, data = dat.test,
                             type = "cis.loess", R = 5)

  return(ret)
}
