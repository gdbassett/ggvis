#' create a vega extent transform object
#'
#' https://vega.github.io/vega/docs/transforms/extent/index.html
#' 
#' The **extent** transform computes the minimum and maximum values for a data field, producing a `[min, max]` array. This transform is useful for computing a value range and binding it to a signal name, for example to use as a parameter for a [bin](../bin) transform. This transform does not change the input data stream, it only computes the extent as a side effect.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param field Field {% include required %} The data field for which to compute the extent.
#' @param signal String If defined, binds the computed extent array to a signal with the given name.
#' @return a {0} transform object
#' @export
vega_extent_transform <- function(
  type,
  field=NULL,
  signal=NULL,
) {
  args <- list(field=field, signal=signal)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
