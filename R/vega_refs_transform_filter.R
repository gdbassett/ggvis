#' create a vega filter transform object
#'
#' https://vega.github.io/vega/docs/transforms/filter/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **filter** transform removes objects from a data stream based on a provided filter expression.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param expr Expr (required) A predicate [expression](../../expressions) for filtering the data. If the expression evaluates to `false`, the data object will be filtered.
#' @return a transform object
#' @export
vega_filter_transform <- function(
  type,
  expr=NULL
) {
  args <- list(type=type, expr=expr)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
