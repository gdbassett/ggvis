#' create a vega force transform object
#'
#' https://vega.github.io/vega/docs/transforms/force/index.html
#' 
#' The **force** transform computes a force-directed layout. Force-directed layout uses a model in which data objects act as charged particles (or _nodes_), optionally connected by a set of edges (or _links_). A set of forces are used to drive a physics simulation that determines the node positions. This transform uses the [d3-force](https://github.com/d3/d3-force) module.
#' 
#' To fix a node at a given position, you may set two special fields on a node object:
#' 
#' - `fx` - the node's fixed x-position
#' - `fy` - the node's fixed y-position
#' 
#' The force transform modifies the input node data _only_. It does not modify any properties of  link data. Instead, use a [lookup transform](../lookup) to join the node data with the link data. Then, use a transform such as [linkpath](../linkpath) to layout the links.
#' 
#' ## Example
#' 
#' {% include embed spec="force" %}
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param static Boolean Indicates if the simulation should be computed in batch to produce a static layout (`true`) or should be animated (`false`). The default is `false`.
#' @param restart Boolean Indicates if the simulation should restart when node object fields are modified (default `false`).
#' @param iterations Number The number of iterations to run the simulation when in _static_ mode (default `300`).
#' @param alpha Number A value representing the current energy level or "temperature" of the simulation. Alpha values lie in the range [0, 1]. Internally, the simulation will decrease the alpha value over time, causing the magnitude of updates to diminish.
#' @param alphaMin Number The minimum amount by which to lower the alpha value on each simulation iteration (default `0.001`).
#' @param alphaTarget Number The target alpha value to which the simulation coverges (default `0`).
#' @param velocityDecay Number The velocity decay factor is akin to atmospheric friction; after the application of any forces during an iteration, each node's velocity is multiplied by _1 - velocityDecay_ (default `0.4`).
#' @param forces [Force](#forces) An array of objects defining the forces to include in the simulation. See the [forces reference](#forces) for more.
#' @param as String[] The output fields to which node positions and velocities are written. The default is `["x", "y", "vx", "vy"]`.
#' @return a {0} transform object
#' @export
vega_force_transform <- function(
  type,
  static=NULL,
  restart=NULL,
  iterations=NULL,
  alpha=NULL,
  alphaMin=NULL,
  alphaTarget=NULL,
  velocityDecay=NULL,
  forces=NULL,
  as=NULL,
) {
  args <- list(static=static, restart=restart, iterations=iterations, alpha=alpha, alphaMin=alphaMin, alphaTarget=alphaTarget, velocityDecay=velocityDecay, forces=forces, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
