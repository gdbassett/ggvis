#' Create vega encode object
#'
#' All visual mark property definitions are specified as name-value pairs in a
#'   property set (such as `update`, `enter`, or `exit`). The name is simply the
#'   name of the visual property. The value should be a Value or Production Rule, as
#'   defined below.
#'
#' The `enter` set is invoked when a mark item is first instantiated and also
#'   when a visualization is resized. Unless otherwise indicated, the `update` set
#'   is invoked whenever data or display properties update. The `exit` set is
#'   invoked when the data value backing a mark item is removed. If hover
#'   processing is requested on the Vega View instance, the `hover` set will be
#'   invoked upon mouse hover.
#'
#' All parameters must be appropriate for the mark they represent
#'
#' @section Potential Parameters:
#' x, x2, xc, width, y, y2, yc, height, opacity, fill,
#'   fillOpacity, stroke, strokeWidth, strokeOpacity, strokeDash, strokeDashOffset,
#'   cursor, clip, size, shape, path, innerRadius, outerRadius, startAngle,
#'   endAngle, interpolate, tension, orient, url, align, baseline, text, dir,
#'   ellipsis, limit, dx, dy, radius, theta, angle, font, fontSize, fontWeight,
#'   fontStyle
#'
#' @section Value References:
#' A value reference specifies the value for a given mark property. The value
#'   may be a constant or drawn from a data object. In addition, the value may be
#'   run through a scale transform and further modified. Examples include:

#' - `{"value": "left"}` - Literal value
#' - `{"field": "amount"}` - Data field value
#' - `{"scale": "yscale", "field": "amount"}` - Scale-transformed data field value
#' - `{"signal": "sqrt(pow(datum.a, 2) + pow(datum.b, 2))"}` - Signal expression value

#' For more, see the Value type documentation, including the specialized Color
#'   Value and Field Value types.
#'
#' @section Production Rule:
#' Visual properties can also be set by evaluating an `if-then-else` style chain
#'   of _production rules_. Rules consist of an array of _value reference_ objects,
#'   each of which must contain an additional `test` property. A single value
#'   reference, without a `test` property, can be specified as the final element
#'   within the rule to serve as the `else` condition. The value of this property
#'   should be a predicate expression, that evaluates to
#'   `true` or `false`. The visual property is set to the value reference
#'   corresponding to the first predicate that evaluates to `true` within the rule.
#'   If none do, the property is set to the final (predicate-less) value reference
#'   if one is specified. For example, the following specification sets a mark's
#'   fill colour using a production rule:
#'
#' "fill": [
#' {
#'   "test": "indata('selectedPoints', 'key', datum.key)",
#'   "scale": "c",
#'   "field": "species"
#' },
#'   {"value": "grey"}
#' ]
#'
#' @param enter a list of initial parameters
#' @param update a list of parameters applied every update
#' @param hover a list of parameters applied to the mark when the mark is hovered over.
#'   (This has nothing to do with tool tips.)
#' @param exit a list of parameters applied when the mark is removed
#' @export
vega_encode <- function(
  enter = NULL,
  update = NULL,
  hover = NULL,
  exit = NULL
) {
  args <- list(enter=enter, update=update, hover=hover, exit=exit)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_encode(args, error=TRUE)

  args
}
