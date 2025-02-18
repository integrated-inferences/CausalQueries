% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_model.R
\name{get_estimands}
\alias{get_estimands}
\title{helper to get estimands}
\usage{
get_estimands(jobs, given_types, query_types, type_posteriors)
}
\arguments{
\item{jobs}{a data frame of argument combinations}

\item{given_types}{output from \code{queries_to_types}}

\item{query_types}{output from \code{queries_to_types}}

\item{type_posteriors}{output from \code{get_type_posteriors}}
}
\value{
a list of estimands
}
\description{
helper to get estimands
}
\keyword{internal}
