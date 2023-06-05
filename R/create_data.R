#' Create a variable's dictionary for its labels
#'
#' @description
#' Given the labels and the codes of a variable,
#' create a named list.
#'
#' @param labels A string of labels separated by a comma and a space. A string.
#' @param code A string of values separated by a comma. A string.
#' @returns A named list.
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
#' @returns A tibble.
#' @export
add_metadata <- function(dat, metadat) {
  # Helper function to tidy certain variables' names
  .tidy_string <- function(x) {
    x <- gsub(" *\\(.*?\\) *", "", x)
    ret <- x |>
      stringr::str_trim(side = "both") |>
      stringr::str_to_sentence()
    return(ret)
  }

  dat_modified <- dat
  # Add metadata to each column of dataset
  for (x in names(dat_modified)) {
    # Extract and tidy metadata for each variable
    info <- metadat |>
      dplyr::filter(variable == x) |>
      as.list()
    info$description <- .tidy_string(info$description)
    info$remark <- .tidy_string(info$remark)
    info$type <- stringr::str_to_title(info$type)
    info$comments <- stringr::str_to_lower(info$comments)

    # Add metadata
    dat_modified <- codebookr::cb_add_col_attributes(
      dat_modified,
      !!x,
      description = info$description,
      # Manually adding `label` attribute to work with gtsummary
      label = info$description,
      value_lables = ifelse(
        info$type == "Categorical",
        create_mapping_labels(info$label, info$code),
        NA
      ),
      col_type = info$type,
      units = info$comments,
      remarks = info$remark,
      dag_var = info$dag,
      period = info$period
    )
  }

  return(dat_modified)
}

#' Mapping between causal nodes and variables' names
#'
#' @description
#'
#' @param  A. .
#' @returns A list of variables' names.
#' @export
map_covars <- function() {
}
