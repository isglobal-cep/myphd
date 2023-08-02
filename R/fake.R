#' Create a fake dataset based on a real one
#'
#' @description
#' Given a dataset with variables as columns, this function creates a
#' random dataset that can be stored on the local machine
#' without security and privacy issues. The type of the
#' variables is matched.
#'
#' @param dat A dataset with variables as columns.
#' A \link[data.table]{data.table} object.
#'
#' @returns A random dataset with matching variables.
#' A \link[data.table]{data.table} object.
#'
#' @export
create_fake_data <- function(dat) {
  ret <- data.table::as.data.table(
    lapply(dat, function(x) {
      if (class(x) == "numeric") {
        rnorm(nrow(dat),
              mean = mean(x, na.rm = TRUE),
              sd = sd(x, na.rm = TRUE))
      } else if (class(x) == "factor") {
        as.factor(sample(levels(x),
                         nrow(dat),
                         replace = T))
      }
    })
  )

  return(ret)
}