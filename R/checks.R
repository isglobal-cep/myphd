#' Title
#'
#' @param model
#' @param path_save_res
#'
#' @return
#'
#' @export
check_model <- function(model, path_save_res) {
  # Check for multi-collinearity of model terms
  multicorr <- performance::multicollinearity(x = model) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      col = dplyr::case_when(
        VIF < 5 ~ "Low (<5)",
        VIF < 10 ~ "Moderate (<10)",
        VIF >= 10 ~ "High (>=10)"
      ),
      col = factor(col,
                   levels = c("Low (<5)",
                              "Moderate (<10)",
                              "High (>=10)"))
    ) |>
    dplyr::arrange(dplyr::desc(VIF)) |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      x = reorder(Term, -VIF),
      y = VIF,
      color = col
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = VIF_CI_low,
        ymax = VIF_CI_high
      ),
      width = 0.2
    ) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::labs(
      x = "term",
      y = "VIF (log10-scaled)",
      title = "Variance Inflation Factors",
      color = "VIF"
    ) +
    ggplot2::theme(
      aspect.ratio = 1,
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    )

  # Posterior predictive checks
  preds <- performance::check_predictions(object = model) |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::mutate(
      col = dplyr::case_match(
        name,
        "y" ~ "Observed data",
        .default = "Model-predicted data"
      )
    )
  preds <- preds |>
    ggplot2::ggplot() +
    ggplot2::geom_density(
      data = preds[preds$name != "y", ],
      mapping = ggplot2::aes(x = value,
                             group = name,
                             color = col)
    ) +
    ggplot2::geom_density(
      data = preds[preds$name == "y", ],
      mapping = ggplot2::aes(x = value,
                             group = name,
                             color = col)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Model-predicted data" = "grey69",
        "Observed data" = "black"
      )
    ) +
    ggplot2::labs(
      x = "outcome",
      title = "Posterior Predictive Check"
    ) +
    ggplot2::theme(
      aspect.ratio = 1,
      legend.title = ggplot2::element_blank()
    )

  # Combine and save plots
  ret <- patchwork::wrap_plots(
    preds, multicorr,
    ncol = 2
  )
  ggplot2::ggsave(
    filename = path_save_res,
    plot = ret
  )

  return(ret)
}
