#' Coalesce - take the first non-missing value
#'
#' @param ... vectors to coalesce
#' @author Martin Morgan on Stack Overflow
#' @references \url{http://stackoverflow.com/a/19257945/903061}
#' @export
coalesce = function(...) {
    answer = ..1
    for (elt in list(...)[-1]) {
        i = which(is.na(answer))
        answer[i] = elt[i]
    }
    answer
}
