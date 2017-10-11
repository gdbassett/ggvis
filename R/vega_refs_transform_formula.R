#' create a vega formula transform object
#'
#' https://vega.github.io/vega/docs/transforms/formula/index.html
#' 
#' The **formula** transform extends data objects with new values according to a calculation formula.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param expr Expr (required) The formula [expression](../../expressions) for calculating derived values.
#' @param as String (required) The output field at which to write the formula value.
#' @param initonly Boolean If `true`, the formula is evaluated only when a data object is first observed. The formula values will _not_ automatically update if data objects are modified. Th default is `false`.
#' @return a transform object
#' @export
vega_formula_transform <- function(
  type,
  expr=NULL,
  as=NULL,
  initonly=NULL
) {
  args <- list(expr=expr, as=as, initonly=initonly)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
