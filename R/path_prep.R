#' Converts Windows filepath to R format
#'
#' Keep input in the clipboard (or paste it in).
#' Returns a valid R filepath string.
#'
#' @author Tyler Rinker (found on Stack Overflow).
#' @param path Defaults to clipboard.
#'
#' @export
path_prep <- function(path = "clipboard") {
    y <- if (path == "clipboard") {
        readClipboard()
    } else {
        y <- path
        #cat("Please enter the path:\n\n")
        #readline()
    }
    x <- chartr("\\", "/", y)
    writeClipboard(x)
    return(x)
}
