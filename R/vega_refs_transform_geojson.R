#' create a vega geojson transform object
#'
#' https://vega.github.io/vega/docs/transforms/geojson/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **geojson** transform consolidates geographic data into a single [GeoJSON](https://en.wikipedia.org/wiki/GeoJSON) feature collection. The generated GeoJSON data can then be used to parameterize other parts of a Vega specification, namely the [projection `fit` parameter](../../projections/). This transform can process both latitude / longitude data and existing GeoJSON features.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param fields Field[] Data fields containing  longitude and latitude values, respectively.
#' @param geojson Field Data field containing  GeoJSON feature objects.
#' @return a transform object
#' @export
vega_geojson_transform <- function(
  type,
  fields=NULL,
  geojson=NULL
) {
  args <- list(type=type, fields=fields, geojson=geojson)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
