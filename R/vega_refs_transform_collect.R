#' create a vega collect transform object
#'
#' https://vega.github.io/vega/docs/transforms/collect/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **collect** transform collects all the objects in a data stream within a single array, allowing sorting by data field values.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param sort Compare A comparator definition for sorting data objects.
#' @return a transform object
#' @export
vega_collect_transform <- function(
  type,
  sort=NULL
) {
  args <- list(type=type, sort=sort)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
