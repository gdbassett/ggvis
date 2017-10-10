#' create a vega pie transform object
#'
#' https://vega.github.io/vega/docs/transforms/pie/index.html
#' 
#' The **pie** transform calculates the angular extents of arc segments laid out in a circle. The most common use case is to create pie charts and donut charts. This transform writes two properties to each datum, indicating the starting and ending angles (in radians) of an arc.
#' 
#' ## Example
#' 
#' {% include embed spec="pie" %}
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param field Field The data values from this field will be encoded as angular spans. If omitted, all pie slices will have equal spans.
#' @param startAngle Number The starting angle of the pie in radians (default `0`).
#' @param endAngle Number The end angle of the pie in radians (default `2 * PI`).
#' @param sort Boolean If true, sorts the arcs according to field values (default `false`).
#' @param as String[] The output fields for the computed start and end angles for each arc. The default is `["startAngle", "endAngle"]`.
#' @return a {0} transform object
#' @export
vega_pie_transform <- function(
  type,
  field=NULL,
  startAngle=NULL,
  endAngle=NULL,
  sort=NULL,
  as=NULL,
) {
  args <- list(field=field, startAngle=startAngle, endAngle=endAngle, sort=sort, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
