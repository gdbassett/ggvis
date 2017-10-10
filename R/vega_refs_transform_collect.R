#' create a vega collect transform object
#'
#' https://vega.github.io/vega/docs/transforms/collect/index.html
#' 
#' The **collect** transform collects all the objects in a data stream within a single array, allowing sorting by data field values.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param sort Compare A comparator definition for sorting data objects.
#' @return a {0} transform object
#' @export
vega_collect_transform <- function(
  type,
  sort=NULL,
) {
  args <- list(sort=sort)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
