#' Add a static vega legend
#'
#' https://vega.github.io/vega/docs/legends/
#'
#' Legends visualize scale mappings for visual values such as color, shape and size. Similar to scales and axes, legends can be defined either at the top-level visualization, or within the scope of a group mark.
#'
#' ### Legend Properties
#'
#' Properties for specifying a legend. Legends accept one or more scales as parameters. At least one of the size, shape, fill, stroke, strokeDash, or opacity properties must be specified. If multiple scales are provided, they must share the same domain of input vales. Otherwise, the behavior of the legend is undefined.
#'
#' ### Legend Orientation Reference
#'
#' Valid settings for the legend orient parameter.
#'
#' \tabular{rl}{
#' *Value* \tab *Description*\cr
#' left \tab Place the legend to the left of the chart.\cr
#' right \tab Place the legend to the right of the chart.\cr
#' top \tab Place the legend above the top of the chart.\cr
#' bottom \tab Place the legend below the bottom of the chart.\cr
#' top-left \tab Place the legend inside the upper left corner of the chart.\cr
#' top-right \tab Place the legend inside the upper right corner of the chart.\cr
#' bottom-left \tab Place the legend inside the lower left corner of the chart.\cr
#' bottom-right \tab Place the legend inside the lower right corner of the chart.\cr
#' none \tab Do not perform automatic layout. Allows custom layout by setting the x and y properties within a legend encoding block.\cr
#' }
#'
#' Multiple legends: If multiple legends have a left or right orientation, they will be vertically ordered. If multiple legends have a top or bottom orientation, they will be horizontally ordered. In all other cases, legends will be drawn on top of each other when placed in the same location.
#'
#' Legend offset: In the case of left, right, top and bottom orientation, the offset parameter determines how far away the legend is placed from the rest of the chart. If the orientation is none, the offset parameter is ignored. For all other settings, the offset determines the distance the legend is moved inward from a corner of the data rectangle.
#'
#' ### Custom Legend Encodings
#'
#' Custom mark properties can be set for all legend elements using the encode parameter. The addressable elements are:
#'
#' * legend - for the legend group mark,
#' * title - for the title text mark,
#' * labels - for label text marks,
#' * symbols - for legend symbol marks, and
#' * gradient - for a gradient-filled rect mark.
#' * Each - element accepts a set of visual encoding directives grouped into enter, update, exit, etc. objects as described in the Marks documentation. Mark properties can be styled using standard value references.
#'
#' In addition, each encode block may include a string-valued name property to assign a unique name to the mark set, a boolean-valued interactive property to enable input event handling, and a string-valued (or array-valued) style property to apply default property values. Unless otherwise specified, title elements use a default style of "guide-title" and labels elements use a default style of "guide-label".
#'
#' Each legend symbol and label instance is backed by a data object with the following fields, which may be accessed as part of a custom visual encoding rule:
#'
#' * index - an integer index
#' * label - the string label
#' * value - the data value
#' * size - the symbol size (for symbol legends only)
#'
#' The following example shows how to set custom fonts and a border on a legend for a fill color encoding. The labels encoding block also make legend labels responsive to input events, and changes the text color on mouse hover.
#'
#' "legends": [
#'   {
#'     "fill": "color",
#'     "encode": {
#'       "title": {
#'         "update": {
#'           "fontSize": {"value": 14}
#'         }
#'       },
#'       "labels": {
#'         "interactive": true,
#'         "update": {
#'           "fontSize": {"value": 12},
#'           "fill": {"value": "black"}
#'         },
#'         "hover": {
#'           "fill": {"value": "firebrick"}
#'         }
#'       },
#'       "symbols": {
#'         "update": {
#'           "stroke": {"value": "transparent"}
#'         }
#'       },
#'       "legend": {
#'         "update": {
#'           "stroke": {"value": "#ccc"},
#'           "strokeWidth": {"value": 1.5}
#'         }
#'       }
#'     }
#'   }
#' ]
#' Custom text can be defined using the text property for labels. For example, one could define an ordinal scale that serves as a lookup table from a backing value to legend label text. In addition, one can set the x and y properties for the legend to perform custom positioning when orient is none.
#'
#' @param type String The type of legend to include. One of symbol (the default) for discrete symbol legends, or gradient for a continuous color gradient. If gradient is used only the fill or stroke scale parameters are considered.
#' @param orient String The orientation of the legend, determining where the legend is placed relative to a chart’s data rectangle (default right). See the legend orientation reference.
#' @param fill String The name of a scale that maps to a fill color.
#' @param opacity String The name of a scale that maps to an opacity value.
#' @param shape String The name of a scale that maps to a shape value.
#' @param size String The name of a scale that maps to a size (area) value.
#' @param stroke String The name of a scale that maps to a stroke color.
#' @param strokeDash String The name of a scale that maps to a stroke dash value.
#' @param encode Object Optional mark encodings for custom legend styling. Supports encoding blocks for legend, title, labels, symbols and gradient. See custom legend encodings.
#' @param entryPadding Number | Value The padding between entries in a symbol legend.
#' @param format String The format specifier pattern for legend labels. For numerical values, must be a legal d3-format specifier. For date-time values, must be a legal d3-time-format specifier.
#' @param offset Number | Value The offset in pixels by which to displace the legend from the data rectangle and axes.
#' @param padding Number | Value The padding between the border and content of the legend group.
#' @param tickCount Number | String | Object The desired number of tick values for quantitative legends. For scales of type time or utc, the tick count can instead be a time interval specifier. Legal string values are "millisecond", "second", "minute", "hour", "day", "week", "month", and "year". Alternatively, an object-valued interval specifier of the form {"interval": "month", "step": 3} includes a desired number of interval steps. Here, ticks are generated for each quarter (Jan, Apr, Jul, Oct) boundary.
#' @param titlePadding Number | Value The padding between the legend title and entries.
#' @param title String The title for the legend (none by default).
#' @param values Array Explicitly set the visible legend values.
#' @param zindex Number The integer z-index indicating the layering of the legend group relative to other axis, mark and legend groups. The default value is 0.
#' @return a vega legend object
vega_legend <- function(
  type="symbol",
  orient="right",
  fill=NULL,
  opacity=NULL,
  shape=NULL,
  size=NULL,
  stroke=NULL,
  strokeDash=NULL,
  encode=NULL,
  entryPadding=NULL,
  format=NULL,
  offset=NULL,
  padding=NULL,
  tickCount=NULL,
  titlePadding=NULL,
  title=NULL,
  values=NULL,
  zindex=0
) {
  args <- list(type=type, orient=orient, fill=fill, opacity=opacity, shape=shape, size=size,
               stroke=stroke, strokeDash=strokeDash, encode=encode, entryPadding=entryPadding,
               format=format, offset=offset, padding=padding, tickCount=tickCount, titlePadding=titlePadding,
                title=title, values=values, zindex=zindex)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_legend(args, error=TRUE)

  args
}
