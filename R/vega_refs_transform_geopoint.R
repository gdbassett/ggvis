#' create a vega geopoint transform object
#'
#' https://vega.github.io/vega/docs/transforms/geopoint/index.html
#' 
#' The **geopoint** transform projects (longitude, latitude) pairs to (x, y) coordinates according to a given cartographic [projection](../../projections).
#' 
#' ## Example
#' 
#' {% include embed spec="geopoint" %}
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param projection String {% include required %} The name of the projection to use.
#' @param fields Field[] {% include required %} The data fields containing the longitude and latitude values, respectively.
#' @param as String[] The output fields to write. The default is `["x", "y"]`.
#' @return a {0} transform object
#' @export
vega_geopoint_transform <- function(
  type,
  projection=NULL,
  fields=NULL,
  as=NULL,
) {
  args <- list(projection=projection, fields=fields, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
