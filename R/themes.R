options <- list(
  ggplot2.discrete.colour = ggokabeito::palette_okabe_ito(),
  ggplot2.discrete.fill = ggokabeito::palette_okabe_ito(),
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis",
  book.base_family = "sans",
  book.base_size = 14
)

#' Minimalist DAG theme
#'
#' @rdname theme_dag
#' @export
theme_dag <- function() {
  ggdag::theme_dag(
    base_family = options$book.base_family,
    legend.position = "bottom",
    legend.key.size = ggplot2::unit(7, "cm"),
    legend.key.height = ggplot2::unit(7, "cm"),
    legend.key.width = ggplot2::unit(7, "cm"),
    legend.title = ggplot2::element_text(
      size = options$book.base_size * 3
    ),
    legend.text = ggplot2::element_text(
      size = options$book.base_size * 3
    )
  )
}

#' Repulsive textual annotations
#'
#' @export
geom_dag_text_repel <- function(..., seed = 10) {
  ggdag::geom_dag_label_repel(
    ggplot2::aes(x, y, label = name),
    box.padding = 3.5,
    inherit.aes = FALSE,
    max.overlaps = Inf,
    family = options$book.base_family,
    seed = seed,
    label.size = NA,
    label.padding = 0.1,
    size = options$book.base_size,
    ...
  )
}
