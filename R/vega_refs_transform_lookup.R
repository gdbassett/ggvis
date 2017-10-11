#' create a vega lookup transform object
#'
#' https://vega.github.io/vega/docs/transforms/lookup/index.html
#' 
#' The **lookup** transform extends a primary data stream by looking up values on a secondary data stream. Lookup accepts one or more key fields from the primary data stream, each of which are then searched for in a single key field of the secondary data stream.
#' 
#' If a match is found, by default the full data object in the secondary stream is added as a property of the data object in the primary stream. However, if the _values_ parameter is supplied, the provided field names will instead be copied from the matched object to the primary object, maintaining a "flat" record structure.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param from Data (required) The name of the secondary data stream upon which to perform the lookup.
#' @param key Field (required) The key field on the secondary stream.
#' @param values Field[] The data fields to copy from the secondary stream to the primary stream. If not specified, a reference to the full data record is copied.
#' @param fields Field[] (required) The data fields in the primary stream to lookup.
#' @param as String[] The output fields at which to write data found in the secondary stream. If not specified and a _values_ parameter is supplied, the names of the fields in the _values_ array will be used. This parameter is required if multiple _fields_ are provided or _values_ is unspecified.
#' @param default Any The default value to assign if lookup fails (default `null`).
#' @return a transform object
#' @export
vega_lookup_transform <- function(
  type,
  from=NULL,
  key=NULL,
  values=NULL,
  fields=NULL,
  as=NULL,
  default=NULL
) {
  args <- list(from=from, key=key, values=values, fields=fields, as=as, default=default)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
