#' create a vega voronoi transform object
#'
#' https://vega.github.io/vega/docs/transforms/voronoi/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **voronoi** transform computes a [voronoi diagram](https://en.wikipedia.org/wiki/Voronoi_diagram) for a set of input points and returns the computed cell paths. The Voronoi cells can then be used to identify the nearest point for a given value. For example, a Voronoi diagram can be used to automatically select the data point closest to the mouse cursor.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param x Field The data field for point x-coordinates.
#' @param y Field The data field for point y-coordinates.
#' @param extent Array[] The clip extent of the Voronoi cells. The extent bounds are specified as an array `[[x0, y0], [x1, y1]]`, where x0 is the left side of the extent, y0 is the top, x1 is the right and y1 is the bottom. For example, `[[-1e5, -1e5], [1e5, 1e5]]` will clip the voronoi diagram at 10,000 pixels in both the negative and positive directions.
#' @param size Number[] An alternative to `extent` that sets the clip extent to `[[0,0], size]`.
#' @param as String[] The output field for the Voronoi cell SVG path string. The default is `path`.
#' @return a transform object
#' @export
vega_voronoi_transform <- function(
  type,
  x=NULL,
  y=NULL,
  extent=NULL,
  size=NULL,
  as=NULL
) {
  args <- list(type=type, x=x, y=y, extent=extent, size=size, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
