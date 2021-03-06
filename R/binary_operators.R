#' btwn operator
#'
#' Tests (element-wise) if the first argument falls within
#' the range of the second.
#'
#' @param a Sequence to test against min/max of b.
#' @param b Sequence for which min/max will be determined and tested against.
#'
#' @examples
#' 1 %btwn% c(0, 5)
#' 0:4 %btwn% c(0.5, 3, 3.5)
#'
#' @export
`%btwn%` <- function(a, b) a >= min(b) & a <= max(b)

#' not in operator
#'
#' 'Not in' - the negation of `%in%`. Returns a vector of length equal to the
#' left hand side indicating TRUE for the LHS elements that are *not* in the
#' RHS (and FALSE otherwise). Alternative to the ugly '! vec1 %in% vec2'.
#'
#' @param x Sequence to test against table.
#' @param table Sequence x is tested against.
#'
#' @export
`%nin%` <- function (x, table) match(x, table, nomatch = 0) == 0
