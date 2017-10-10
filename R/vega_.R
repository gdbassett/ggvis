#' Visualise a data set with a ggvis graphic.
#'
#' https://vega.github.io/vega/docs/specification/
#'
#' @param data A data object.
#' @param schema URL The URL for the Vega schema.
#' @param description String  A text description of the visualization.
#' @param background  Color The background color of the entire view (defaults to transparent).
#' @param width Number  The width in pixels of the data rectangle.
#' @param height  Number  The height in pixels of the data rectangle.
#' @param padding Number | Object The padding in pixels to add around the visualization. If a number, specifies padding for all sides. If an object, the value should have the format {"left": 5, "top": 5, "right": 5, "bottom": 5}. Padding is applied after autosize layout completes.
#' @param autosize  String | Autosize Sets how the visualization size should be determined. If a string, should be one of pad (default), fit or none. Object values can additionally specify parameters for content sizing and automatic resizing. See the autosize section below for more.
#' @param title Title Title text to describe a visualization.
#' @param ... Property mappings. If not named, the first two mappings are
#'   taken to be \code{x} and \code{y}. Common properties are \code{x},
#'   \code{y}, \code{stroke} (color), \code{fill}, \code{opacity}, \code{shape}
#' @return visualization object
#' @export
vega <- function(data=NULL,
                 ...,
                 schema="https://vega.github.io/schema/vega/v3.0.json",
                 description=NULL,
                 background=NULL,
                 width=NULL,
                 height=NULL,
                 padding=NULL,
                 autosize="pad",
                 title=NULL) {
  # create vega object
  vis <- structure(list(properties <- rlang::quos(...), vega = list()), class = "ggvega")

  # add root properties
  vis <- update_vega(vis, schema=schema, description=description, background=background, width=width,
                     height=height, padding=padding, autosize=autosize, title=title)

  # add data
  vis <- add_data_(vis, data)

  # return
  vis
}


#' Add dataset to a visualisation
#'
#' @param vis Visualisation to modify.
#' @param data Data set to add.
#' @param name Data of data - optional, but helps produce informative
#'  error messages.
#' @export
#' @examples
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points()
#' NULL %>% ggvis(~mpg, ~wt) %>% add_data(mtcars) %>% layer_points()
add_data_ <- function(vis, data, name = deparse2(substitute(data))) {
  if (is.null(data)) return(vis)

  # Add static data as well. (Duplicative. Using to transition from reactive shiny data.) - GDB 171004
  if (!'data' %in% names(vis$vega)) vis$vega[["data"]] <- list()
  if (shiny::is.reactive(data)) {
    vis$vega$data[[length(vis$vega$data) + 1]] <- as.vega(shiny::isolate(data()), name=name)[[1]]
  } else {
    vis$vega$data[[length(vis$vega$data) + 1]] <- as.vega(data, name=name)[[1]]
  }
  static_names <- unlist(lapply(vis$vega$data, function(d) {d$name}))
  if (length(static_names) > length(unique(static_names))) warning("Static names contains duplicate names. This will cause a VEGA error.")
  rm(static_names)

  vis
}

#' Update a vega visualization
#'
#' https://vega.github.io/vega/docs/specification/
#'
#' @param vis Visualisation to modify.
#' @param schema URL The URL for the Vega schema.
#' @param description String  A text description of the visualization.
#' @param background  Color The background color of the entire view (defaults to transparent).
#' @param width Number  The width in pixels of the data rectangle.
#' @param height  Number  The height in pixels of the data rectangle.
#' @param padding Number | Object The padding in pixels to add around the visualization. If a number, specifies padding for all sides. If an object, the value should have the format {"left": 5, "top": 5, "right": 5, "bottom": 5}. Padding is applied after autosize layout completes.
#' @param autosize  String | Autosize Sets how the visualization size should be determined. If a string, should be one of pad (default), fit or none. Object values can additionally specify parameters for content sizing and automatic resizing. See the autosize section below for more.
#' @param title Title Title text to describe a visualization.
#' @return visualization object
#' @export
update_vega <- function(vis,
                 schema=NULL,
                 description=NULL,
                 background=NULL,
                 width=NULL,
                 height=NULL,
                 padding=NULL,
                 autosize="pad",
                 title=NULL) {

  args <- list(`$schema`=schema, description=description, background=background, width=width,
               height=height, padding=padding, autosize=autosize, title=title)
  args <- args[!unlist(lapply(args, is.null))]

  vis$vega <- utils::modifyList(vis$vega, args)

  vis
}

#' Render using plot using htmltools and vega standard process
#' @export
#' @param x Visualisation to modify.
#' @param launch If \code{TRUE}, will launch plot in a viewer/browser. If
#'   \code{FALSE} returns an object that you can \code{print()} to launch.
#' @param ... 'render', 'logLevel', and 'tooltop_opts' parameters for vega_render() function
#' @return printable object
#' @examples
#' p <- mtcars %>%
#'   dplyr::group_by(cyl) %>%
#'   dplyr::summarize(mpg = median(mpg)) %>%
#'   dplyr::mutate(cyl = as.factor(cyl)) %>%
#'   ggvis::ggvis(x=~cyl, y=~mpg) %>%
#'   ggvis::layer_bars(fill:="steelblue") %>%
#'   ggvis::add_title(text="MPG by Cylinder", orient="top", anchor="start") %>%
#'   ggvis::flip()
#' tooltips_opts <- list(
#'   showAllFields=FALSE,
#'   fields=list(
#'     list(
#'       field= "x_",
#'       title= "Cylinder",
#'       formatType= "string"
#'     ),
#'     list(
#'       field= "stack_upr_",
#'       title= "MPG",
#'       formatType= "number"
#'     )
#'   )
#' )
#' vega_render(dump_spec(p), tooltip_opts = tooltips_opts)
print.ggvega <- function(x, launch = interactive(), ...) {
  out <- vega_render(jsonlite::toJSON(x$vega, auto_unbox = TRUE), ...)

  if (launch) print(out)
  out
}
