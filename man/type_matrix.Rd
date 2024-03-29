% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nodal_types.R
\name{type_matrix}
\alias{type_matrix}
\title{Generate type matrix}
\usage{
type_matrix(parent_n)
}
\arguments{
\item{parent_n}{An integer. Number of parents of a given child.}
}
\value{
A \code{data.frame} whose rows contain digits of
  each causal types in a model
}
\description{
Generate type matrix
}
\examples{
\donttest{
CausalQueries:::type_matrix(2)
}
}
\keyword{internal}
