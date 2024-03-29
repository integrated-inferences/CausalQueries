% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_helpers.R
\name{minimal_event_data}
\alias{minimal_event_data}
\title{Creates a compact data frame for case with no data}
\usage{
minimal_event_data(model)
}
\arguments{
\item{model}{A \code{causal_model}. A model object generated by
\code{\link{make_model}}.}
}
\value{
A compact data frame where each row represents an element from the
  exhaustive set of events of a model. The count for each event is
  set to zero.
}
\description{
Creates a compact data frame for case with no data
}
\examples{
\donttest{
model <- make_model('X -> K -> Y')
CausalQueries:::minimal_event_data(model)
}
}
\keyword{internal}
