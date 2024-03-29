% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighting.R
\name{estimate_weights}
\alias{estimate_weights}
\title{Wrapper function to estimate balancing weights}
\usage{
estimate_weights(
  dat,
  exposure,
  covariates,
  s.weights,
  id_var,
  method,
  method_args
)
}
\arguments{
\item{dat}{A dataframe containing the variables of interest. A dataframe.}

\item{exposure}{The name of the variable corresponding to the exposure. A string.}

\item{covariates}{A vector of covariates' names. A vector.}

\item{s.weights}{Sampling weights. A vector.}

\item{id_var}{The variable name to be used to identify subjects. A string.}

\item{method}{The method to be used by \link[WeightIt]{weightit} to estimate the weights. A string.}

\item{method_args}{A named list with the following variables:
\itemize{
\item \code{stabilize}, whether to stabilize the weights or not.
\item \code{by}, a string containing the name of the variable
for which weighting is to be done within categories.
\item \code{sl_lib}, either a vector of learners or \code{FALSE}, in which case
it uses a fixed library of learners.
\item \code{sl_discrete}, whether to use discrete SuperLearner, which
selects the best performing method, or to find the optimal combination of predictions.
\item \code{use_kernel}, whether to use kernel density estimation
to estimate the numerator and denominator densities for the weights. A logical.
\item \code{family_link}, family to be used within the fitted model.
}}
}
\value{
A named list containing the estimated weights, and the names
of the exposure and covariates used.
}
\description{
This functions is essentially a wrapper around the
\link[WeightIt]{weightit} function. The user can specify the
method to be used to estimate the weights.
}
