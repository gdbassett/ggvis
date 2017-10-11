#' return a vega title object
#'
#' https://vega.github.io/vega/docs/title/
#' The title directive adds a descriptive title to a chart. Similar to scales, axes,
#'   and legends, a title can be defined at the top-level of a specification or as part
#'   of a group mark.
#'
#'  To create themes, new default values for many title properties can be set using a
#'  config object.
#'
#' @param text string. Required. The title text.
#' @param orient string. The orientation of the title relative to the chart. One of top
#'   (the default), bottom, left, or right.
#' @param anchor string. The anchor position for placing the title. One of start,
#'   middle (the default), or end. For example, with an orientation of top these anchor
#'   positions map to a left-, center-, or right-aligned title.
#' @param encode list. The anchor position for placing the title. One of start, middle
#'   (the default), or end. For example, with an orientation of top these anchor
#'   positions map to a left-, center-, or right-aligned title.
#' @param interactive logical.  Optional mark encodings for custom title styling. This
#'   is a standard encode block for text marks, and may contain enter, exit, update, and
#'   hover property sets. To set a custom font, font size, etc. for a title, one can
#'   either use custom encode blocks or update the title config.
#' @param name string. A mark name property to apply to the title text mark.
#' @param style string. A mark style property to apply to the title text mark. If not
#'   specified, a default style of "group-title" is applied.
#' @param offset number.  The orthogonal offset in pixels by which to displace the title
#'   from its position along the edge of the chart.
#' @param zindex number. The integer z-index indicating the layering of the title group
#'   relative to other axis, mark and legend groups. The default value is 0.
#' @export
#' @examples
#'mtcars %>%
#'  dplyr::group_by(cyl) %>%
#'  dplyr::summarize(mpg = median(mpg)) %>%
#'  dplyr::mutate(cyl = as.factor(cyl)) %>%
#'  ggvis::ggvis(x=~cyl, y=~mpg) %>%
#'  ggvis::layer_bars() %>%
#'  ggvis::add_title(text="MPG by Cylinder")
#' mtcars %>%
#'  dplyr::group_by(cyl) %>%
#'  dplyr::summarize(mpg = median(mpg)) %>%
#'  dplyr::mutate(cyl = as.factor(cyl)) %>%
#'  ggvis::ggvis(x=~cyl, y=~mpg) %>%
#'  ggvis::layer_bars() %>%
#'  ggvis::add_title(text="MPG by Cylinder", orient="top", anchor="start")
vega_title <- function(text,
                       name=NULL,
                       orient=NULL,
                       anchor=NULL,
                       style=NULL,
                       zindex=NULL,
                       interactive=NULL,
                       offset=NULL,
                       encode=NULL)
{
  args <- list(text=text, name=name, orient=orient, anchor=anchor, style=style, zindex=zindex, interactive=interactive,
               offset=offset, encode=encode)
  args <- args[!unlist(lapply(args, is.null))]
  # schema allows string or object
  if (length(args) == 1) {
    if (inherits(text, "character") & length(text) == 1) {
      return(paste(text, collapse=))
    } else {
      stop("Title text must be a character string.")
    }
  } else {
    is.vega_title(args)
    return(args)
  }
}
