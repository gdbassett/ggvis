#' create a vega fold transform object
#'
#' https://vega.github.io/vega/docs/transforms/fold/index.html
#' 
#' The **fold** transform collapses (or "folds") one or more data fields into two properties: a _key_ property (containing the original data field name) and a _value_ property (containing the data value). The fold transform is useful for mapping matrix or cross-tabulation data into a standardized format.
#' 
#' This transform generates a new data stream in which each data object consists of the _key_ and _value_ properties as well as all the original fields of the corresponding input data object.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param fields Field[] (required) An array of data fields indicating the properties to fold.
#' @param as String[] The output field names for the _key_ and _value_ properties produced by the fold transform. The default is `["key", "value"]`.
#' @return a transform object
#' @export
vega_fold_transform <- function(
  type,
  fields=NULL,
  as=NULL
) {
  args <- list(fields=fields, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
