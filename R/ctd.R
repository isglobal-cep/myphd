#' Load local CTD file
#'
#' @param path Path to the CTD file. A string.
#' @param filter_evidence Indicates whether to filter the type of
#'                        direct evidence. A bool.
#'
#' @returns A dataframe containing the CTD database.
#'
#' @export
load_ctd <- function(path, filter_evidence) {
  ctd <- read.csv(path) |>
    tibble::as_tibble()

  if (filter_evidence == TRUE) {
    ctd <- ctd |>
      tidylog::filter(
        DirectEvidence %in% c(
          "marker/mechanism",
          "therapeutic",
          "marker/mechanism|therapeutic"
        )
      )
  }

  ctd <- ctd |>
    tidyr::separate_longer_delim(DiseaseCategories,
      delim = "|"
    )

  ctd$DiseaseCategories <- factor(ctd$DiseaseCategories,
    levels = sort(unique(ctd$DiseaseCategories),
      decreasing = TRUE
    )
  )

  return(ctd)
}
################################################################################

#' Visualize the CTD dataset after loading
#'
#' @description
#' Given a dataframe with the data from the CTD, create a bar plot of the
#' categories.
#'
#' @param dat The CTD dataset. A dataframe.
#' @param group The faceting variable. A string.
#'
#' @returns A ggplot object.
#'
#' @export
plot_ctd <- function(dat, group) {
  plt <- dat |>
    tidylog::filter(DiseaseCategories != "") |>
    tidylog::group_by(DiseaseCategories) |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      x = DiseaseCategories,
      fill = factor(ChemicalName)
    )) +
    ggplot2::geom_bar() +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Disease categories") +
    colorspace::scale_fill_discrete_qualitative(
      name = "Chemicals",
      palette = "Dynamic"
    )

  if (!is.null(group)) {
    plt <- plt +
      ggplot2::facet_grid(~ .data[[group]])
  }

  return(plt)
}
################################################################################
