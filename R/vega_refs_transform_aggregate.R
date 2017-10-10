#' create a vega aggregate transform object
#'
#' https://vega.github.io/vega/docs/transforms/aggregate/index.html
#' 
#' The **aggregate** transform groups and summarizes an input data stream to produce a derived output stream. Aggregate transforms can be used to compute counts, sums, averages and other descriptive statistics over groups of data objects.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param groupby Field[] The data fields to group by. If not specified, a single group containing all data objects will be used.
#' @param fields Field[] The data fields for which to compute aggregate functions. This array should align with the _ops_ and _as_ arrays. If no _fields_ and _ops_ are specified, a `count` aggregation will be used by default.
#' @param ops String[] The aggregation operations to apply to the _fields_, such as `sum`, `average` or `count`. See the [aggregate operation reference](#ops) for more.
#' @param as String[] The output field names to use for each aggregated field in _fields_. If not specified, names will be automatically generated based on the operation and field names (e.g., `sum_field`, `average_field`).
#' @param cross Boolean Indicates if the full cross-product of all groupby values should be included in the aggregate output (default `false`). If set to `true`, all possible combinations of groupby field values will be considered and zero count groups will be generated and returned for combinations that do not occur in the data itself. Cross-product output act as if the _drop_ parameter is `false`. In the case of streaming updates, the number of output groups will increase if new groupby field values are observed; all prior groups will be retained. This parameter can be useful for generating facets that include groups for all possible partitions of the data.
#' @param drop Boolean Indicates if empty (zero count) groups should be dropped (default `true`). When a data stream updates (for example, in response to interactive filtering), aggregation groups may become empty. By default, the group is removed from the output. However, in some cases (such as histograms), one may wish to retain empty groups.
#' @return a {0} transform object
#' @export
vega_aggregate_transform <- function(
  type,
  groupby=NULL,
  fields=NULL,
  ops=NULL,
  as=NULL,
  cross=NULL,
  drop=NULL,
) {
  args <- list(groupby=groupby, fields=fields, ops=ops, as=as, cross=cross, drop=drop)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
