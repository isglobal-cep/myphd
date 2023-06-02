% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_data.R
\name{preproc_data}
\alias{preproc_data}
\title{Basic pre-processing of datasets}
\usage{
preproc_data(dat, outcome = NULL, dic_steps, id_var, by_var)
}
\arguments{
\item{dat}{A dataset with variables as columns. A data.table.}

\item{dic_steps}{A named list of steps to perform. A list.}
}
\value{
A pre-processed dataset. A data.table.
}
\description{
Perform basic pre-processing of a given dataset:
\itemize{
\item Data cleaning:
\itemize{
\item Removal of incorrect/unwanted variables.
\item Remove of incorrect/unwanted subjects.
\item Handling of missing values.
}
\item Variable transformations:
\itemize{
\item Transformation (e.g., log-transformation, bound outcome).
\item Standardization (zero mean and unit standard deviation).
\item Normalization (subtract the minimum and divide by the range).
}
}
}