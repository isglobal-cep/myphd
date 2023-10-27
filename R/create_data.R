#' Create dataframe for marginal comparisons
#'
#' @description
#' Given a dataframe with the variable of interest and a vector of lower and
#' upper percentiles, the function creates two new columns called `low` and
#' `high` corresponding to the quantiles of interest.
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param var The name of the variable of interest. A string.
#' @param percentiles A vector with two elements corresponding to the
#' lower and upper percentiles. A vector.
#' @param by_var The variable name to group by. A string.
#'
#' @returns A dataframe with two new columns, `low` and `high`, to be used for
#' marginal comparisons.
#'
#' @export
create_df_marginal_comparisons <-
  function(dat, var, percentiles, by_var) {
    # Checks
    if (percentiles[1] < 0 | percentiles[2] > 1) {
      stop("The percentiles must lie between 0 and 1.",
        call. = TRUE
      )
    }

    # Compute `low` and `high` values of the variable of interest, by group
    ret <- dat |>
      tidylog::group_by(.data[[by_var]]) |>
      tidylog::mutate(
        low = quantile(.data[[var]], percentiles[1]),
        high = quantile(.data[[var]], percentiles[2])
      ) |>
      tidylog::ungroup()

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

#' Add metadata to dataframe
#'
#' @description
#' Given a dataframe with observations as rows and variables as columns,
#' and given a dataframe of metadata with variables as rows and
#' information as columns, add metadata to the variables present in
#' the dataframe.
#'
#' @param dat A dataframe or tibble of data. A dataframe.
#' @param metadat A dataframe or tibble of metadata.
#' Currently, the following fields are required:
#' * `description`, a text description of the variable.
#' * `remark`, any remark about the variable (e.g., if certain values were imputed).
#' * `type`, type of variable (e.g., numerical).
#' * `comments`, any other remark about the variable (e.g., unit of measurement).
#' * `dag`, name of the variable as present in the DAG.
#' * `period`, period of measurement of the variable (e.g., pregnancy).
#' * `label`, labels of the levels in case of categorical variables,
#' separated by a comma and a space.
#' * `code`, levels of the variable if categorical, separated by a comma.
#' A dataframe.
#' @param categorical_types Identifiers, as strings, of categorical types. A vector.
#' @param cols_to_exclude
#' @md
#'
#' @returns A dataframe containing annotated variables.
#'
#' @export
add_metadata <- function(dat, metadat, categorical_types, cols_to_exclude) {
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
    tidylog::mutate(
      type = as.character(type),
      type = ifelse(type %in% categorical_types,
        "categorical",
        type
      )
    )

  # Add metadata to each column of dataframe
  for (x in names(dat_modified)) {
    if (x %in% cols_to_exclude) next

    # Extract and tidy metadata for each variable
    if (nrow(metadat |> tidylog::filter(variable == x)) == 0) {
      next
    }
    info <- metadat |>
      tidylog::filter(variable == x) |>
      as.list()
    info$description <- .tidy_string(info$description)
    info$remark <- .tidy_string(info$remark)
    info$type <- stringr::str_to_lower(info$type)
    info$comments <- stringr::str_to_lower(info$comments)
    info <- lapply(info, as.character)

    # Add metadata
    if (info$comments != "na") {
      info$description <- paste0(
        info$description, " (", tolower(info$comments), ")"
      )
    }
    attr(dat_modified[[x]], "label") <- info$description
    attr(dat_modified[[x]], "units") <- info$comments
    attr(dat_modified[[x]], "remarks") <- info$remark
    attr(dat_modified[[x]], "dag_var") <- info$dag
    attr(dat_modified[[x]], "period") <- info$period
    if (info$type == "categorical") {
      labels_raw <- create_mapping_labels(info$label, info$code)
      if (min(labels_raw) == 0) {
        labels <- labels_raw + 1
      } else {
        labels <- labels_raw
      }

      dat_modified[[x]] <- labelled::labelled(
        dat_modified[[x]],
        labels = labels,
        label = info$description
      ) |>
        labelled::to_factor()
    }
  } # End loop over variables

  return(dat_modified)
}
