#' create a vega linkpath transform object
#'
#' https://vega.github.io/vega/docs/transforms/linkpath/index.html
#' 
#' The **linkpath** transform is used to route a visual link between two nodes. The most common use case is to draw edges in a tree or network layout. By default links are simply straight lines between source and target nodes; however, with additional shape and orientation information, a variety of link paths can be expressed. This transform writes one property to each datum, providing an [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) for the link path.
#' 
#' ## Example
#' 
#' {% include embed spec="linkpath" %}
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param sourceX Field The data field for the source x-coordinate. The default is `source.x`.
#' @param sourceY Field The data field for the source y-coordinate. The default is `source.y`.
#' @param targetX Field The data field for the target x-coordinate. The default is `target.x`.
#' @param targetY Field The data field for the target y-coordinate. The default is `target.y`.
#' @param orient String The orientation of the link path. One of `vertical` (default), `horizontal` or `radial`. If a `radial` orientation is specified, x and y coordinate parameters will instead be interpreted as an angle (in radians) and radius, respectively.
#' @param shape String The shape of the link path. One of `line` (default), `arc`, `curve`, `diagonal`, or `orthogonal`.
#' @param as String The output field for the link path. The default is `"path"`.
#' @return a {0} transform object
#' @export
vega_linkpath_transform <- function(
  type,
  sourceX=NULL,
  sourceY=NULL,
  targetX=NULL,
  targetY=NULL,
  orient=NULL,
  shape=NULL,
  as=NULL,
) {
  args <- list(sourceX=sourceX, sourceY=sourceY, targetX=targetX, targetY=targetY, orient=orient, shape=shape, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
