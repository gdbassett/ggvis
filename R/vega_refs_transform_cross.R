#' create a vega cross transform object
#'
#' https://vega.github.io/vega/docs/transforms/cross/index.html
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
  args <- list(filter=filter, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
