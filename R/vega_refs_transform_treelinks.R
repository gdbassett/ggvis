#' create a vega treelinks transform object
#'
#' https://vega.github.io/vega/docs/transforms/treelinks/index.html
#' 
#' The **treelinks** transform generates a new stream of data objects representing links among nodes in a tree. This transform must occur downstream of a tree-generating transform such as [nest](../nest) or [stratify](../stratify). The generated link objects will have `source` and `target` fields that reference input data objects corresponding to parent (source) and child (target) nodes.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param key Field A data field containing a unique key (identifier) for each node. This must be the same field used by the upstream [nest](../nest) or [stratify](../stratify) transform.
#' @return a transform object
#' @export
vega_treelinks_transform <- function(
  type,
  key=NULL
) {
  args <- list(key=key)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
