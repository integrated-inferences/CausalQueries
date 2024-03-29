% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_par_values.R
\name{construct_commands_other_args}
\alias{construct_commands_other_args}
\title{make_par_values}
\usage{
construct_commands_other_args(
  node,
  nodal_type,
  param_set,
  given,
  statement,
  model,
  join_by
)
}
\arguments{
\item{node}{string indicating nodes which are to be altered}

\item{nodal_type}{string. Label for nodal type indicating nodal types for
which values are to be altered}

\item{param_set}{string indicating  the name of the set of parameters
to be altered}

\item{given}{string indicates the node on which the parameter
to be altered depends}

\item{statement}{causal query that determines nodal types for
which values are to be altered}

\item{model}{model created with \code{make_model}}

\item{join_by}{string specifying the logical operator joining expanded
types when \code{statement} contains wildcards.
Can take values \code{'&'} (logical AND) or \code{'|'} (logical OR).}
}
\value{
string specifying a filter command
}
\description{
helper to generate filter commands specifying rows of parameters_df that
should be altered given combinations of nodes, nodal_types, param_sets,
givens and statements
}
\keyword{internal}
