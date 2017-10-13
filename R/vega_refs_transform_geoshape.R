#' create a vega geoshape transform object
#'
#' https://vega.github.io/vega/docs/transforms/geoshape/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **geoshape** transform generates a renderer instance that maps [GeoJSON](https://en.wikipedia.org/wiki/GeoJSON) features to a shape instance that issues drawing commands. It is intended for use solely with the [shape](../../marks/shape) mark type. This transform is similar in functionality to the [geopath](../geopath) transform, but rather than generate intermediate SVG path strings, this transform produces a shape instance that directly generates drawing commands during rendering. This transform can result in improved performance when using canvas rendering for dynamic maps.
#' 
#' This transform uses the [d3-geo](https://github.com/d3/d3-geo) library.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param projection String (required) The name of the projection to use.
#' @param field Field The data field containing GeoJSON data. If unspecified, the full input data object will be used.
#' @param as String The output field at which to write the generated shape instance. The default is `"shape"`.
#' @return a transform object
#' @export
vega_geoshape_transform <- function(
  type,
  projection=NULL,
  field=NULL,
  as=NULL
) {
  args <- list(type=type, projection=projection, field=field, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
