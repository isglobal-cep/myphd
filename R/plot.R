#' Title
#'
#' @param dat
#'
#' @return
#'
#' @export
plot_effect_estimates <- function(dat, size_points) {
  # Add type based on CI of effect estimate and round digits
  dat <- dat |>
    dplyr::mutate(
      type = ifelse(
        (estimate - 1.96 * se) * (estimate + 1.96 * se) > 0,
        "significant", "non-significant"
      ),
      estimate = round(estimate, 3)
    )

  # Plot all effect estimates in single figure
  plt <- dat |>
    ggplot2::ggplot(ggplot2::aes(x = variable,
                                 y = estimate,
                                 label = estimate)) +
    ggplot2::geom_point(ggplot2::aes(col = type,
                                     size = .data[[size_points]])) +
    ggplot2::geom_errorbar(ggplot2::aes(
      col = type,
      ymin = estimate - 1.96 * se,
      ymax = estimate + 1.96 * se
    ),
    width = 0,
    linewidth = 0.3) +
    ggrepel::geom_text_repel() +
    ggplot2::geom_hline(yintercept = 0,
                        col = "black") +
    ggplot2::coord_flip() +
    ggplot2::guides(size = "none") +
    ggplot2::theme_minimal()

  return(plt)
}
