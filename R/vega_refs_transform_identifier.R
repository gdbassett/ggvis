#' create a vega identifier transform object
#'
#' https://vega.github.io/vega/docs/transforms/identifier/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **identifier** transform extends data objects with a globally unique key value. Identifier values are assigned using an internal counter. This counter is shared across all instances of this transform within a single Vega view, including different data sources. Note, however, that the counter is _not_ shared across different Vega views.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param as String (required) The output field at which to write the unique identifier value.
#' @return a transform object
#' @export
vega_identifier_transform <- function(
  type,
  as=NULL
) {
  args <- list(type=type, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
