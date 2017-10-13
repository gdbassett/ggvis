#' create a vega cross transform object
#'
#' https://vega.github.io/vega/docs/transforms/cross/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **cross** transform compute the cross-product of a data stream with itself.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param filter Expr An optional filter expression for limiting the results of the cross-product.
#' @param as Array The output fields for the two data objects being crossed. The default is `["a", "b"]`.
#' @return a transform object
#' @export
vega_cross_transform <- function(
  type,
  filter=NULL,
  as=NULL
) {
  args <- list(type=type, filter=filter, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
