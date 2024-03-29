% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{expand_wildcard}
\alias{expand_wildcard}
\title{Expand wildcard}
\usage{
expand_wildcard(to_expand, join_by = "|", verbose = TRUE)
}
\arguments{
\item{to_expand}{A character vector of length 1L.}

\item{join_by}{A logical operator. Used to connect causal statements:
\emph{AND} ('&') or \emph{OR} ('|'). Defaults to '|'.}

\item{verbose}{Logical. Whether to print expanded query on the console.}
}
\value{
A character string with the expanded expression.
  Wildcard '.' is replaced by 0 and 1.
}
\description{
Expand statement containing wildcard
}
\examples{

# Position of parentheses matters for type of expansion
# In the "global expansion" versions of the entire statement are joined
expand_wildcard('(Y[X=1, M=.] > Y[X=1, M=.])')
# In the "local expansion" versions of indicated parts are joined
expand_wildcard('(Y[X=1, M=.]) > (Y[X=1, M=.])')

# If parentheses are missing global expansion used.
expand_wildcard('Y[X=1, M=.] > Y[X=1, M=.]')

# Expressions not requiring expansion are allowed
expand_wildcard('(Y[X=1])')
}
