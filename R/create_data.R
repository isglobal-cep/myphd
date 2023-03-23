#' Create toy dataset given options
#'
#' @description
#' Given a list of options, creates a toy dataset satisfying
#' those requirements.
#'
#' @param variables .
#' @param num_rows .
#' @param num_cols .
#' @param ids .
#' @returns .
#' @export
create_data <- function(variables, num_rows, num_cols, ids) {
  if (is.null(ids)) {
    ids <- paste0("id_", 1:num_rows, sep = "")
  }
}
