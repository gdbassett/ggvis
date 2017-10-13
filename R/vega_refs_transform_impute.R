#' create a vega impute transform object
#'
#' https://vega.github.io/vega/docs/transforms/impute/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **impute** transform performs imputation of missing data objects.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param field Field (required) The data field for which missing values should be imputed.
#' @param key Field (required) A key field that uniquely identifies data objects within a group. Missing _key_ values (those occuring in the data but not in the current group) will be imputed.
#' @param keyvals Any[] An optional array of key values that should be considered for imputation. If provided, this array will be used in addition to the key values observed within the input data.
#' @param method String The imputation method to use for the _field_ value of imputed data objects. One of `value` (default), `mean`, `median`, `max` or `min`.
#' @param groupby Field[] An optional array of fields by which to group the values. Imputation will then be performed on a per-group basis. For example, missing values may be imputed using the group mean rather than the global mean.
#' @param value Any The field value to use when the imputation method is `value`.
#' @return a transform object
#' @export
vega_impute_transform <- function(
  type,
  field=NULL,
  key=NULL,
  keyvals=NULL,
  method=NULL,
  groupby=NULL,
  value=NULL
) {
  args <- list(type=type, field=field, key=key, keyvals=keyvals, method=method, groupby=groupby, value=value)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
