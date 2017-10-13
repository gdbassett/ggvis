#' create a vega geopoint transform object
#'
#' https://vega.github.io/vega/docs/transforms/geopoint/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **geopoint** transform projects (longitude, latitude) pairs to (x, y) coordinates according to a given cartographic [projection](../../projections).
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param projection String (required) The name of the projection to use.
#' @param fields Field[] (required) The data fields containing the longitude and latitude values, respectively.
#' @param as String[] The output fields to write. The default is `["x", "y"]`.
#' @return a transform object
#' @export
vega_geopoint_transform <- function(
  type,
  projection=NULL,
  fields=NULL,
  as=NULL
) {
  args <- list(type=type, projection=projection, fields=fields, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
