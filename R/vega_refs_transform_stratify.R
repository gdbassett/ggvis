#' create a vega stratify transform object
#'
#' https://vega.github.io/vega/docs/transforms/stratify/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **stratify** transform generates a hierarchical (tree) data structure from input data objects, based on key fields that match parent and children nodes. Internally, this transform generates a set of tree node objects that can then be processed by tree layout methods such as [tree](../tree), [treemap](../treemap), [pack](../pack), and [partition](../partition).
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param key Field (required) A data field containing a unique key (identifier) for each node.
#' @param parentKey Field (required) A data field containing the key value for each node's parent in the hierarchy.
#' @return a transform object
#' @export
vega_stratify_transform <- function(
  type,
  key=NULL,
  parentKey=NULL
) {
  args <- list(type=type, key=key, parentKey=parentKey)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
