#' Title
#'
#' @param df
#' @param df_preds
#' @param exposure
#' @param vals_exposure
#'
#' @return
#' @export
plot_adrf <- function(df, df_preds, exposure, vals_exposure) {
  adrf <- df_preds |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[exposure]]
    )) +
    ggplot2::geom_point(
      ggplot2::aes(y = estimate)
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = estimate),
      linewidth = 0.2
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = conf.low,
        ymax = conf.high
      ),
      alpha = 0.2
    ) +
    ggplot2::geom_rug(
      mapping = ggplot2::aes(x = .data[[exposure]]),
      data = df,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(
        vals_exposure[1],
        vals_exposure[length(vals_exposure)]
      )
    ) +
    ggplot2::labs(
      x = exposure,
      y = "E[Y|A]"
    ) +
    ggplot2::theme_minimal()

  return(adrf)
}

#' Title
#'
#' @param df
#' @param df_slopes
#' @param exposure
#' @param vals_exposure
#'
#' @return
#' @export
plot_amef <- function(df, df_slopes, exposure, vals_exposure) {
  amef <- df_slopes |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[[exposure]])
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = estimate)
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = estimate),
      linewidth = 0.2
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = conf.low,
        ymax = conf.high
      ),
      alpha = 0.2
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::geom_rug(
      mapping = ggplot2::aes(x = .data[[exposure]]),
      data = df,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(
        vals_exposure[1],
        vals_exposure[length(vals_exposure)]
      )
    ) +
    ggplot2::labs(
      x = exposure,
      y = "dE[Y|A]/dA"
    ) +
    ggplot2::theme_minimal()

  return(amef)
}

#' Plot effect estimates with confidence intervals
#'
#' @description
#' Given a dataframe with columns `variable`, `estimate`, `se`,
#' and a column for the size of the points, this function creates
#' a sort of forest plot. The dataframe can contain as many rows as
#' results to be plotted. The `variable` column can contain e.g., the
#' names of the exposures, `estimate` is the point estimate obtained
#' e.g., with a linear model, while `se` is the standard error of the estimate.
#'
#' @param dat A dataframe of results to be plotted. A dataframe.
#' @param size_points Name of the column in `dat` to use to change
#' the size of the points in the forest plot.
#'
#' @returns A \link[ggplot2]{ggplot} object.
#'
#' @export
plot_effect_estimates <- function(dat, size_points) {
  # Add type based on CI of effect estimate and round digits
  dat <- dat |>
    tidylog::mutate(
      type = ifelse(
        (estimate - 1.96 * se) * (estimate + 1.96 * se) > 0,
        "significant", "non-significant"
      ),
      estimate = round(estimate, 3)
    )

  # Plot all effect estimates in single figure
  plt <- dat |>
    ggplot2::ggplot(ggplot2::aes(
      x = variable,
      y = estimate,
      label = estimate
    )) +
    ggplot2::geom_point(ggplot2::aes(
      col = type,
      size = .data[[size_points]]
    )) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        col = type,
        ymin = estimate - 1.96 * se,
        ymax = estimate + 1.96 * se
      ),
      width = 0,
      linewidth = 0.3
    ) +
    ggrepel::geom_text_repel() +
    ggplot2::geom_hline(
      yintercept = 0,
      col = "black"
    ) +
    ggplot2::coord_flip() +
    ggplot2::guides(size = "none") +
    ggplot2::theme_minimal()

  return(plt)
}
