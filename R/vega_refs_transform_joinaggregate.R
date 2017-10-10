#' create a vega joinaggregate transform object
#'
#' https://vega.github.io/vega/docs/transforms/joinaggregate/index.html
#' 
#' The **joinaggregate** transform extends the input data objects with aggregate values. Aggregation is performed and the results are then joined with the input data. The parameters for this transform are nearly identical to the [`aggregate`](../aggregate) transform, but rather than creating new output objects, the results are written back to each of the input data objects. This transform can be helpful for creating derived values that combine both raw data and aggregate calculations, such as percentages of group totals.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param groupby Field[] The data fields to group by. If not specified, a single group containing all data objects will be used.
#' @param fields Field[] The data fields for which to compute aggregate functions. This array should align with the _ops_ and _as_ arrays. If no _fields_ and _ops_ are specified, a `count` aggregation will be used by default.
#' @param ops String[] The aggregation operations to apply to the _fields_, such as `sum`, `average` or `count`. See the [aggregate operation reference](#ops) for more.
#' @param as String[] The output field names to use for each aggregated field in _fields_. If not specified, names will be automatically generated based on the operation and field names (e.g., `sum_field`, `average_field`).
#' @return a {0} transform object
#' @export
vega_joinaggregate_transform <- function(
  type,
  groupby=NULL,
  fields=NULL,
  ops=NULL,
  as=NULL,
) {
  args <- list(groupby=groupby, fields=fields, ops=ops, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
