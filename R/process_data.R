#' Basic pre-processing of datasets
#'
#' @description
#' Perform basic pre-processing of a given dataset:
#' \itemize{
#'  \item Data cleaning
#'    \itemize{
#'      \item Removal of incorrect/unwanted variables
#'      \item Remove of incorrect/unwanted subjects
#'      \item Handling of missing values
#'    }
#'  \item Variable transformations
#'  \itemize{
#'      \item Transformation (e.g., log-transformation)
#'      \item Standardization (zero mean and unit standard deviation)
#'      \item Normalization (subtract the minimum and divide by the range)
#'    }
#' }
#'
#' @param dat A dataset with variables as columns. A data.table.
#' @param dic_steps A named list of steps to perform. A list.
#' @returns A pre-processed dataset. A data.table.
#' @export
preproc_data <- function(dat, dic_steps) {

}
