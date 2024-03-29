% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_query_to_nodal_type.R
\name{add_dots}
\alias{add_dots}
\title{Helper to fill in missing do operators in causal expression}
\usage{
add_dots(q, model)
}
\arguments{
\item{q}{A character string. Causal query with at least one parent node
missing their do operator.}

\item{model}{A \code{causal_model}. A model object generated by
\code{\link{make_model}}.}
}
\value{
A causal query expression with all parents nodes set to
  either 0, 1 or wildcard '.'.
}
\description{
Helper to fill in missing do operators in causal expression
}
\examples{
\donttest{
model <- make_model('X -> Y <- M')
CausalQueries:::add_dots('Y[X=1]', model)
CausalQueries:::add_dots('Y[]', model)
}
}
\keyword{internal}
