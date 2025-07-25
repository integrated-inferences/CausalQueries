% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{interpret_type}
\alias{interpret_type}
\title{Interpret or find position in nodal type}
\usage{
interpret_type(
  model,
  condition = NULL,
  position = NULL,
  nodes = model$parents_df[!model$parents_df$root, 1]
)
}
\arguments{
\item{model}{A \code{causal_model}. A model object generated by
\code{\link{make_model}}.}

\item{condition}{A vector of characters. Strings specifying the child node,
followed by '|' (given) and the values of its parent nodes in \code{model}.}

\item{position}{A named list of integers. The name is the name of the child
node in \code{model}, and its value a vector of digit positions in that
node's nodal type to be interpreted. See `Details`.}

\item{nodes}{A vector of names of nodes. Can be used to limit interpretation to
selected nodes. By default limited to non root nodes.}
}
\value{
A named \code{list} with interpretation of positions of
  the digits in a nodal type
}
\description{
Interprets the position of one or more digits (specified by \code{position})
in a nodal type. Alternatively returns nodal type digit positions that
correspond to one or more given \code{condition}.
}
\details{
A node for a child node X with \code{k} parents has a nodal type
  represented by X followed by \code{2^k} digits. Argument \code{position}
  allows user to interpret the meaning of one or more digit positions in any
  nodal type. For example \code{position = list(X = 1:3)} will return the
  interpretation of the first three digits in causal types for X.
  Argument \code{condition} allows users to query the digit position in the
  nodal type by providing instead the values of the parent nodes of a given
  child. For example, \code{condition = 'X | Z=0 & R=1'} returns the digit
  position that corresponds to values X takes when Z = 0 and R = 1.
}
\examples{
model <- make_model('R -> X; Z -> X; X -> Y')
# Return interpretation of all digit positions of all nodes
interpret_type(model)
# Example using digit position
interpret_type(model, position = list(X = c(3,4), Y = 1))
interpret_type(model, position = list(R = 1))
# Example using condition
interpret_type(model, condition = c('X | Z=0 & R=1', 'X | Z=0 & R=0'))
# Example using node names
interpret_type(model, nodes = c("Y", "R"))
}
