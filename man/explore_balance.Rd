% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighting.R
\name{explore_balance}
\alias{explore_balance}
\title{Assess balance on covariate distributions generated through weighting}
\usage{
explore_balance(exposure, type_exposure, covariates, weights, threshold_cor)
}
\arguments{
\item{exposure}{The name of the variable corresponding to the exposure. A string.}

\item{covariates}{A vector of covariates' names. A vector.}

\item{weights}{The \code{weights} element of the result of the call
to \code{\link[=estimate_weights]{estimate_weights()}}. A \link[WeightIt]{weightit} object.}

\item{threshold_cor}{The balance threshold. A double.}
}
\value{
A named list containing the exposure name and the results of the
steps described above. A list.
}
\description{
Given a dataframe, this function performs the following steps:
\itemize{
\item Generate balance statistics on covariates in relation to a
exposure variable, using the \link[cobalt]{bal.tab} function.
\item Generate plots displaying distributional balance
between exposure and covariates,
using the \link[cobalt]{bal.plot} function.
\item Generate a Love plot displaying covariate balance
before and after adjusting, using the \link[cobalt]{love.plot} function.
}
}
