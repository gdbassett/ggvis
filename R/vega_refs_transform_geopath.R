#' create a vega geopath transform object
#'
#' https://vega.github.io/vega/docs/transforms/geopath/index.html
#' 
#' The **geopath** transform maps [GeoJSON](https://en.wikipedia.org/wiki/GeoJSON) features to [SVG path strings](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) according to a provided cartographic [projection](../../projections). It is intended for use with the [path](../../marks/path) mark type. This transform is similar in functionality to the [geoshape](../geoshape) transform, but immediately generates SVG path strings, rather than producing a shape instance that delays projection until the rendering stage. The [geoshape](../geoshape) transform may have better performance for the case of canvas-rendered dynamic maps.
#' 
#' This transform uses the [d3-geo](https://github.com/d3/d3-geo) library.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param projection String (required) The name of the projection to use.
#' @param field Field The data field containing GeoJSON data. If unspecified, the full input data object will be used.
#' @param as String The output field to write. The default is `"path"`.
#' @return a transform object
#' @export
vega_geopath_transform <- function(
  type,
  projection=NULL,
  field=NULL,
  as=NULL
) {
  args <- list(projection=projection, field=field, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
