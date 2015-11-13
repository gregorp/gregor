#' A blank ggplot theme, borrowed from somebody on github
#'
#' @param base_size defaults to 12, should be fine mostly
#' @param ... additional arguments based to theme_gray
#'
#' @export
#' @import ggplot2
theme_clean = function(base_size = 12, ...) {
    theme_grey(base_size, ...) %+replace%
        theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.ticks.length = grid::unit(0, "cm"),
            axis.ticks.margin = grid::unit(0.01, "cm"),
            panel.margin = grid::unit(0, "lines"),
            plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
            complete = TRUE
        )
}

#' Nice theme for ggplot
#'
#' Based off of \code{theme_bw}, this frames the plot in dark blue,
#' uses filled in rectangles with white text for facet labels,
#' eliminates minor grid lines, and has a different default font.
#' There are also some other options such as gridlines and angles
#' for x-axis labels.
#'
#' @param font to use, defaults to "Eras Medium ITC".
#' @param slant boolean for whether x-axis labels should be slanted. (FALSE)
#' @param gridlines boolean for whether there should be any gridlines (TRUE)
#' @param expand.margin boolean for whether the right margin needs padding.
#' Useful if \code{slant}
#' is \code{TRUE} and the labels on the right side are lengthy.
#' @param ... additional arguments passed to theme_bw
#' @param theme_color Color for plot margin lines and facet strip backgrounds.
#'  Defaults to a nice dark blue.
#'
#' @export
#' @import ggplot2
theme_g = function(
    font = c("Eras Medium ITC"),
    slant = FALSE,
    gridlines = TRUE,
    expand.margin = FALSE,
    theme_color = "#3B6E8F",
    ...
) {
    if (font %nin% extrafont::fonts()) {
        warning("Looks like you don't have ", font, " installed. Attempting to use Arial.")
        font = "Arial"
    }
    base_theme = theme_bw(..., base_family = font) +
        theme(panel.border = element_rect(size = 0.5, colour = theme_color),
              axis.ticks = element_line(size = 0.5, colour = theme_color),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(fill = theme_color),
              strip.text.x = element_text(colour = "white"),
              strip.text.x = element_text(colour = "white"))
    if (slant) base_theme =
        base_theme + theme(axis.text.x = element_text(angle = -25, hjust = 0))
    if (expand.margin) base_theme =
        base_theme + theme(plot.margin = grid::unit(c(1, 1, 1, 1) * 5, "mm"))
    if (!gridlines) base_theme =
        base_theme + theme(panel.grid = element_blank())
    return(base_theme)
}

