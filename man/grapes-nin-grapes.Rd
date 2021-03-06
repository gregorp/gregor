% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary_operators.R
\name{\%nin\%}
\alias{\%nin\%}
\title{not in operator}
\usage{
x \%nin\% table
}
\arguments{
\item{x}{Sequence to test against table.}

\item{table}{Sequence x is tested against.}
}
\description{
'Not in' - the negation of `%in%`. Returns a vector of length equal to the
left hand side indicating TRUE for the LHS elements that are *not* in the
RHS (and FALSE otherwise). Alternative to the ugly '! vec1 %in% vec2'.
}

