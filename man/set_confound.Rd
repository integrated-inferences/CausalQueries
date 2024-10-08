% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_confounds.R
\name{set_confound}
\alias{set_confound}
\title{Set confound}
\usage{
set_confound(model, confound = NULL)
}
\arguments{
\item{model}{A \code{causal_model}. A model object generated by
\code{\link{make_model}}.}

\item{confound}{A \code{list} of statements indicating pairs of nodes whose
types are jointly distributed (e.g. list("A <-> B", "C <-> D")).}
}
\value{
An object of class \code{causal_model} with updated parameters_df
  and parameter matrix.
}
\description{
Adjust parameter matrix to allow confounding.
}
\details{
Confounding between X and Y arises when the nodal types for X and Y are not
independently distributed. In the X -> Y graph, for instance, there are 2
nodal types for X and 4 for Y. There are thus 8 joint nodal types:
\preformatted{
|          | t^X                |                    |           |
|-----|----|--------------------|--------------------|-----------|
|     |    | 0                  | 1                  | Sum       |
|-----|----|--------------------|--------------------|-----------|
| t^Y | 00 | Pr(t^X=0 & t^Y=00) | Pr(t^X=1 & t^Y=00) | Pr(t^Y=00)|
|     | 10 | .                  | .                  | .         |
|     | 01 | .                  | .                  | .         |
|     | 11 | .                  | .                  | .         |
|-----|----|--------------------|--------------------|-----------|
|     |Sum | Pr(t^X=0)          | Pr(t^X=1)          | 1         |
}

This table has 8 interior elements and so an unconstrained joint
distribution would have 7 degrees of freedom. A no confounding assumption
means that Pr(t^X | t^Y) = Pr(t^X), or  Pr(t^X, t^Y) = Pr(t^X)Pr(t^Y).
In this case there would be 3 degrees of freedom for Y and 1 for X,
totaling 4 rather than 7.

\code{set_confound} lets you relax this assumption by increasing the
number of parameters characterizing the joint distribution. Using the fact
that P(A,B) = P(A)P(B|A) new parameters are introduced to capture P(B|A=a)
rather than simply P(B). For instance here two parameters
(and one degree of freedom) govern the distribution of types X  and four
parameters (with 3 degrees of freedom) govern  the types for Y given the
type of X for a total of 1+3+3 = 7 degrees of freedom.
}
\examples{

make_model('X -> Y; X <-> Y') |>
inspect("parameters")

make_model('X -> M -> Y; X <-> Y') |>
inspect("parameters")

model <- make_model('X -> M -> Y; X <-> Y; M <-> Y')
inspect(model, "parameters_df")

# Example where set_confound is implemented after restrictions
make_model("A -> B -> C") |>
set_restrictions(increasing("A", "B")) |>
set_confound("B <-> C") |>
inspect("parameters")

# Example where two parents are confounded
make_model('A -> B <- C; A <-> C') |>
  set_parameters(node = "C", c(0.05, .95, .95, 0.05)) |>
  make_data(n = 50) |>
  cor()

 # Example with two confounds, added sequentially
model <- make_model('A -> B -> C') |>
  set_confound(list("A <-> B", "B <-> C"))
inspect(model, "statement")
# plot(model)
}
\seealso{
Other set: 
\code{\link{set_prior_distribution}()},
\code{\link{set_restrictions}()}
}
\concept{set}
