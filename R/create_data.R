#' Title
#'
#' @param dat
#' @param var
#' @param percentiles
#' @param group_var
#'
#' @return
#' @export
create_df_marginal_comparisons <- function(dat, var, percentiles, group_var) {
  # Checks
  if (percentiles[1] < 0 | percentiles[2] > 1) {
    stop("The percentiles must lie between 0 and 1.",
         call. = TRUE)
  }

  # Compute `low` and `high` values of the variable of interest, by group
  ret <- dat |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::mutate(
      low = quantile(.data[[var]], percentiles[1]),
      high = quantile(.data[[var]], percentiles[2])
    ) |>
    dplyr::ungroup()

  return(ret)
}

#' Create a variable's dictionary for its labels
#'
#' @description
#' Given the labels and the codes of a variable,
#' create a named list.
#'
#' @param labels A string of labels separated by a comma and a space. A string.
#' @param code A string of values separated by a comma. A string.
#'
#' @returns A named list mapping labels to values.
#'
#' @export
create_mapping_labels <- function(labels, codes) {
  labels <- strsplit(as.character(labels), ", ") |>
    unlist() |>
    as.character()
  codes <- strsplit(as.character(codes), ",") |>
    unlist() |>
    as.integer()

  ret <- codes
  ret <- setNames(ret, labels)

  return(ret)
}

#' Add metadata to dataset
#'
#' @description
#' Given a dataset with observations as rows and variables as columns,
#' and given a dataset of metadata with variables as rows and
#' information as columns, add metadata to the variables present in
#' the dataset.
#'
#' @param dat A dataframe or tibble of data. A dataframe.
#' @param metadat A dataframe or tibble of metadata. A dataframe.
#' @param categorical_types
#'
#' @returns A tibble containing both data and metadata.
#'
#' @export
add_metadata <- function(dat, metadat, categorical_types) {
  # Helper function to tidy certain variables' names
  .tidy_string <- function(x) {
    x <- gsub(" *\\(.*?\\) *", "", x)
    ret <- x |>
      stringr::str_trim(side = "both") |>
      stringr::str_to_sentence()
    return(ret)
  }

  dat_modified <- dat
  metadat <- metadat |>
    dplyr::mutate(
      type = as.character(type),
      type = ifelse(
        type %in% categorical_types,
        "categorical",
        type
      )
    )

  # Add metadata to each column of dataset
  for (x in names(dat_modified)) {
    # Extract and tidy metadata for each variable
    if (nrow(metadat |> dplyr::filter(variable == x)) == 0) {
      next
    }
    info <- metadat |>
      dplyr::filter(variable == x) |>
      as.list()
    info$description <- .tidy_string(info$description)
    info$remark <- .tidy_string(info$remark)
    info$type <- stringr::str_to_lower(info$type)
    info$comments <- stringr::str_to_lower(info$comments)
    info <- lapply(info, as.character)

    # Add metadata
    attr(dat_modified[[x]], "label") <- info$description
    attr(dat_modified[[x]], "units") <- info$comments
    attr(dat_modified[[x]], "remarks") <- info$remark
    attr(dat_modified[[x]], "dag_var") <- info$dag
    attr(dat_modified[[x]], "period") <- info$period
    if (info$type == "categorical") {
      dat_modified[[x]] <- labelled::labelled(
        dat_modified[[x]],
        labels = create_mapping_labels(info$label, info$code),
        label = info$description
      ) |>
        labelled::to_factor()
    }
  } # End loop over variables

  return(dat_modified)
}
