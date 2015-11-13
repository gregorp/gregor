#' Factor to numeric via character
#'
#' Shorthand to avoid the all-to-common bug of going straight from
#' factor to numeric.
#'
#' @param x factor to be converted to numeric
#'
#' @export
#' @examples
#' year = factor(1998:2004)
#' # bad
#' as.numeric(year)
#' # good
#' as.numeric(as.character(year))
#' # not much less typing, but no nesting (and it will auto-complete)
#' as_numeric_factor(year)
as_numeric_factor <- function(x) {
    if (! is.factor(x)) warning("factor_to_number: Didn't start as a factor.")
    return(as.numeric(as.character(x)))
}
