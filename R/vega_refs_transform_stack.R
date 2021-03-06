#' create a vega stack transform object
#'
#' https://vega.github.io/vega/docs/transforms/stack/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **stack** transform computes a layout by stacking groups of values. The most common use case is to create stacked graphs, including stacked bar charts and stream graphs. This transform writes two properties to each datum, indicating the starting and ending stack values.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param field Field The data field that determines the stack heights.
#' @param groupby Field[] An array of fields by which to partition the data into separate stacks.
#' @param sort Compare Criteria for sorting values within each stack.
#' @param offset Number The baseline offset. One of "zero" (default), "center", or "normalize". The "center" offset will center the stacks. The "normalize" offset will compute percentage values for each stack point, with output values in the range [0,1].
#' @param as String[] The output fields for the computed start and end stack values. The default is `["y0", "y1"]`.
#' @return a transform object
#' @export
vega_stack_transform <- function(
  type,
  field=NULL,
  groupby=NULL,
  sort=NULL,
  offset=NULL,
  as=NULL
) {
  args <- list(type=type, field=field, groupby=groupby, sort=sort, offset=offset, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
