% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ambiguities.R
\name{get_ambiguities_matrix}
\alias{get_ambiguities_matrix}
\title{Get ambiguities matrix}
\usage{
get_ambiguities_matrix(model)
}
\arguments{
\item{model}{A model created by \code{make_model}}
}
\value{
A data frame that maps types (rows) to possible data realizations (columns).
}
\description{
Get matrix that maps types to data realizations
}
\examples{

model <- make_model("X -> Y")

get_ambiguities_matrix(model = model)

}