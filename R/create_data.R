#' Create a variable dictionary for the labels
#'
#' @description
#'
#' @param  A. A .
#' @param  A. A .
#' @param  A. A .
#' @returns .
#' @export
create_mapping_labels <- function(var_name, labels, codes) {
  labels <- strsplit(labels, ", ") |>
    unlist()
  codes <- strsplit(codes, ",") |>
    unlist()
}

#' Process and tidy variables
#'
#' @description
#'
#' @param  A. A .
#' @param  A. A .
#' @returns .
#' @export
add_metadata <- function(dat, metadat) {
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
