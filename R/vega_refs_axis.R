#' return a vega axis
#'
#' https://vega.github.io/vega/docs/axes/
#'
#' Axes visualize spatial scale mappings using ticks, grid lines and labels. Vega currently supports axes for Cartesian (rectangular) coordinates. Similar to scales, axes can be defined either at the top-level of the specification, or as part of a group mark.
#'
#' @param scale String  Required. The name of the scale backing the axis component.
#' @param orient  String  Required. The orientation of the axis. See the axis orientation reference.
#' @param domain  Boolean A boolean flag indicating if the domain (the axis baseline) should be included as part of the axis (default true).
#' @param encode  Object  Optional mark encodings for custom axis styling. Supports encoding blocks for axis, ticks, labels, grid, and domain. See custom axis encodings.
#' @param format  String  The format specifier pattern for axis labels. For numerical values, must be a legal d3-format specifier. For date-time values, must be a legal d3-time-format specifier.
#' @param grid  Boolean A boolean flag indicating if grid lines should be included as part of the axis (default false).
#' @param gridScale String  The name of the scale to use for including grid lines. By default grid lines are driven by the same scale as the ticks and labels.
#' @param labels  Boolean A boolean flag indicating if labels should be included as part of the axis (default true).
#' @param labelBound  Boolean | Number  Indicates if labels should be hidden if they exceed the axis range. If false (the default) no bounds overlap analysis is performed. If true, labels will be hidden if they exceed the axis range by more than 1 pixel. If this property is a number, it specifies the pixel tolerance: the maximum amount by which a label bounding box may exceed the axis range.
#' @param labelFlush  Boolean | Number  Indicates if the first and last axis labels should be aligned flush with the scale range. Flush alignment for a horizontal axis will left-align the first label and right-align the last label. For vertical axes, bottom and top text baselines are applied instead. If this property is a number, it also indicates the number of pixels by which to offset the first and last labels; for example, a value of 2 will flush-align the first and last labels and also push them 2 pixels outward from the center of the axis. The additional adjustment can sometimes help the labels better visually group with corresponding axis ticks.
#' @param labelPadding  Number  The padding in pixels between labels and ticks.
#' @param labelOverlap  Boolean | String  The strategy to use for resolving overlap of axis labels. If false (the default), no overlap reduction is attempted. If set to true or "parity", a strategy of removing every other label is used (this works well for standard linear axes). If set to "greedy", a linear scan of the labels is performed, removing any labels that overlaps with the last visible label (this often works better for log-scaled axes).
#' @param minExtent Number | Value  The minimum extent in pixels that axis ticks and labels should use. This determines a minimum offset value for axis titles.
#' @param maxExtent Number | Value  The maximum extent in pixels that axis ticks and labels should use. This determines a maximum offset value for axis titles.
#' @param offset  Number | Value  The orthogonal offset in pixels by which to displace the axis from its position along the edge of the chart.
#' @param position  Number | Value  The anchor position of the axis in pixels (default 0). For x-axes with top or bottom orientation, this sets the axis group x coordinate. For y-axes with left or right orientation, this sets the axis group y coordinate.
#' @param ticks Boolean A boolean flag indicating if ticks should be included as part of the axis (default true).
#' @param tickCount Number | String | Object  A desired number of ticks, for axes visualizing quantitative scales. The resulting number may be different so that values are “nice” (multiples of 2, 5, 10) and lie within the underlying scale’s range. For scales of type time or utc, the tick count can instead be a time interval specifier. Legal string values are "millisecond", "second", "minute", "hour", "day", "week", "month", and "year". Alternatively, an object-valued interval specifier of the form {"interval": "month", "step": 3} includes a desired number of interval steps. Here, ticks are generated for each quarter (Jan, Apr, Jul, Oct) boundary.
#' @param tickSize  Number  The size in pixels of axis ticks.
#' @param title String  A title for the axis (none by default).
#' @param titlePadding  Number | Value  The offset in pixels between the axis labels and axis title.
#' @param values  Array Explicitly set the visible axis tick and label values.
#' @param zindex  Number  The integer z-index indicating the layering of the axis group relative to other axis, mark and legend groups. The default value is 0 and axes and grid lines are drawn behind any marks defined in the same specification level. Higher values (1) will cause axes and grid lines to be drawn on top of marks.
#' @return a vega axis object
#' @export
vega_axis <- function(
  scale,
  orient,
  domain=TRUE,
  encode=NULL,
  format=NULL,
  grid=FALSE,
  gridScale=NULL,
  labels=TRUE,
  labelBound=FALSE,
  labelFlush=NULL,
  labelPadding=NULL,
  labelOverlap=FALSE,
  minExtent=NULL,
  maxExtent=NULL,
  offset=NULL,
  position=0,
  ticks=TRUE,
  tickCount=NULL,
  tickSize=NULL,
  title="",
  titlePadding=NULL,
  values=NULL,
  zindex=0
) {
  args <- list(scale=scale, orient=orient, domain=domain, encode=encode, format=format, grid=grid,
               gridScale=gridScale, labels=labels, labelBound=labelBound, labelPadding=labelPadding,
               labelOverlap=labelOverlap, minExtent=minExtent, maxExtent=maxExtent, offset=offset,
               position=position, ticks=ticks, tickCount=tickCount, tickSize=tickSize, title=title,
               titlePadding=titlePadding, values=values, zindex=zindex)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_axis(args, error=TRUE)

  args
}
