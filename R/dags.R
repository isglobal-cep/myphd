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
