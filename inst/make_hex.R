coords <- tibble::tribble(
  ~name, ~x, ~y,
  "x", 0, 1,
  "y", 1, 1,
  "v", 0.5, 1.5
)
plt <- ggdag::dagify(
  y ~ x,
  y ~ v,
  x ~ v,
  exposure = "x",
  outcome = "y",
  coords = coords
) |>
  ggplot2::ggplot(ggplot2::aes(
    x = x, y = y,
    xend = xend, yend = yend
  )) +
  ggdag::geom_dag_point(
    size = 3,
    color = "#3e363f",
    show.legend = FALSE
  ) +
  ggdag::geom_dag_edges(
    arrow_directed = grid::arrow(
      length = grid::unit(3, "pt"),
      type = "open"
    ),
    edge_width = 0.3,
    edge_colour = "#3e363f",
    start_cap = ggraph::circle(2, "mm"),
    end_cap = ggraph::circle(2, "mm")
  ) +
  ggdag::geom_dag_text() +
  ggdag::theme_dag()

hexSticker::sticker(
  plt,
  package = "myphd",
  #p_y = 1,
  p_size = 20,
  p_color = "#3e363f",
  s_x = 1,
  s_y = 0.75,
  s_width = 1.3,
  s_height = 1,
  h_fill = "#73a580",
  h_color = "#e1ad01",
  filename = "man/figures/sticker.png"
)
