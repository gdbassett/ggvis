#' create a vega crossfilter transform object
#'
#' https://vega.github.io/vega/docs/transforms/crossfilter/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **crossfilter** transform maintains a filter mask for multiple dimensional queries, using a set of sorted indices. This transform can be used in conjunction with the [resolvefilter](../resolvefilter) transform to enable fast interactive querying over large data sets. This transform is inspired by the [Crossfilter library](http://crossfilter.github.io/crossfilter/) developed by Mike Bostock and collaborators.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param fields Field[] (required) An array of data fields to filter. The same field may be included more than once to specify multiple queries.
#' @param queries Array[] (required) An array of per-field range queries. Each entry must resolve to a two-element number array, indicating the minimum (inclusive) and maximum (exclusive) values that should pass through the filter.
#' @param signal String If defined, binds the computed filter mask to a signal with the given name.
#' @return a transform object
#' @export
vega_crossfilter_transform <- function(
  type,
  fields=NULL,
  queries=NULL,
  signal=NULL
) {
  args <- list(type=type, fields=fields, queries=queries, signal=signal)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
