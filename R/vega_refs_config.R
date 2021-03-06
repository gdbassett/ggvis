#' Defines default visual values to set a visualization’s theme.
#'
#' https://vega.github.io/vega/docs/config/#title
#'
#'   The Vega parser accepts a JSON configuration file that defines default
#'   settings for a variety of visual encoding choices. Different configuration
#'   files can be used to “theme” charts with a customized look and feel. A
#'   configuration file is simply a JSON object with a set of named properties,
#'   grouped by type. To provide a configuration file at parse-time, simply pass
#'   an additional parameter to the parse method:
#'   `var runtime = vega.parse(spec, config);`
#'
#'   In addition, Vega JSON specifications may contain a single, top-level config
#'   property to override any configuration settings. Any configuration provided
#'   within the specification itself will take precedence over external configurations
#'   passed to the parser.
#'
#' @details
#' @section Axis Properties:
#' \tabular{rrl}{
#' Property \tab Type \tab Description\cr
#' bandPosition \tab  "Number"  \tab An interpolation fraction indicating where, for `band` scales, axis ticks should be positioned. A value of `0` places ticks at the left edge of their bands. A value of `0.5` places ticks in the middle of their bands.\cr
#' domain \tab  "Boolean"  \tab Boolean flag indicating if axis domain line should be included by default.\cr
#' domainColor \tab  "Color"  \tab Color of axis domain line.\cr
#' domainWidth \tab  "Number"  \tab Stroke width of axis domain line.\cr
#' grid \tab  "Boolean"  \tab Boolean flag indicating if axis grid lines should be included by default.\cr
#' gridColor \tab  "Color"  \tab Color of axis grid lines.\cr
#' gridDash \tab  "Number[]"  \tab Stroke dash of axis grid lines (or `[]` for solid lines).\cr
#' gridOpacity \tab  "Number"  \tab Opacity of axis grid lines.\cr
#' gridWidth \tab  "Number"  \tab Stroke width of axis grid lines.\cr
#' labels \tab  "Boolean"  \tab Boolean flag indicating if axis tick labels should be included by default.\cr
#' labelAngle \tab  "Number"  \tab Angle in degrees of axis tick labels.\cr
#' labelBound \tab  "Boolean|Number"  \tab Boolean flag or pixel tolerance value for removal of labels that exceed the axis range.\cr
#' labelColor \tab  "Color"  \tab Text color of axis tick labels.\cr
#' labelFlush \tab  "Boolean|Number"  \tab Boolean flag or offset value for flush alignment of first and last axis labels.\cr
#' labelFont \tab  "String"  \tab Font name for axis tick labels.\cr
#' labelFontSize \tab  "Number"  \tab Font size of axis tick labels.\cr
#' labelLimit \tab  "Number"  \tab The maximum allowed length in pixels of axis tick labels.\cr
#' labelPadding \tab  "Number"  \tab Padding in pixels betweem axis ticks and tick labels.\cr
#' maxExtent \tab  "Number"  \tab The maximum extent in pixels that axis ticks and labels should use. This determines a maximum offset value for axis titles.\cr
#' minExtent \tab  "Number"  \tab The minimum extent in pixels that axis ticks and labels should use. This determines a minimum offset value for axis titles.\cr
#' ticks \tab  "Boolean"  \tab Boolean flag indicating if axis tick marks should be included by default.\cr
#' tickColor \tab  "Color"  \tab Color of axis ticks.\cr
#' tickExtra \tab  "Boolean"  \tab Boolean flag indicating if an extra axis tick should be added for the initial position of the axis. This flag is useful for styling axes for `band` scales such that ticks are placed on band boundaries rather in the middle of a band. Use in conjunction with `"bandPostion": 1` and an axis `"padding"` value of `0`.\cr
#' tickOffset \tab  "Number"  \tab Position offset in pixels to apply to ticks, labels, and gridlines.\cr
#' tickRound \tab  "Boolean"  \tab Boolean flag indicating if pixel position values should be rounded to the nearest integer.\cr
#' tickSize \tab  "Number"  \tab Size, or length, in pixels of axis ticks.\cr
#' tickWidth \tab  "Number"  \tab Width in pixels of axis ticks.\cr
#' titleAlign \tab  "String"  \tab Horizontal text alignment of axis titles.\cr
#' titleAngle \tab  "Number"  \tab Angle in degrees of axis titles.\cr
#' titleBaseline \tab  "String"  \tab Vertical text baseline for axis titles.\cr
#' titleColor \tab  "Color"  \tab Text color of axis titles.\cr
#' titleFont \tab  "String"  \tab Font name for axis titles.\cr
#' titleFontSize \tab  "Number"  \tab Font size of axis titles.\cr
#' titleFontWeight \tab  "String"  \tab Font weight of axis titles.\cr
#' titleLimit \tab  "Number"  \tab The maximum allowed length in pixels of axis titles.\cr
#' titlePadding \tab  "Number"  \tab Padding in pixels between axis tick labels and titles.\cr
#' titleX \tab  "Number"  \tab X-coordinate of the axis title relative to the axis group.\cr
#' titleY \tab  "Number"  \tab Y-coordinate of the axis title relative to the axis group.\cr
#' }
#'
#' @section Legend Properties:
#' \tabular{rrl}{
#' Property \tab Type \tab Description   \cr
#' cornerRadius \tab "Number"  \tab Corner radius for the full legend.\cr
#' entryPadding \tab "Number"  \tab Padding in pixels between legend entries in a symbol legend.\cr
#' fillColor \tab "Color"  \tab Background fill color for the full legend.\cr
#' gradientWidth \tab "Number"  \tab Width in pixels of color ramp gradients.\cr
#' gradientHeight \tab "Number"  \tab Height in pixels of color ramp gradients.\cr
#' gradientStrokeColor \tab "Color"  \tab Stroke color for color ramp gradient borders.\cr
#' gradientStrokeWidth \tab "Number"  \tab Stroke width for color ramp gradient borders.\cr
#' gradientLabelBaseline \tab "String"  \tab Text baseline for color ramp gradient labels.\cr
#' gradientLabelLimit \tab "Number"  \tab The maximum allowed length in pixels of color ramp gradient labels.\cr
#' gradientLabelOffset \tab "Number"  \tab Vertical offset in pixels for color ramp gradient labels.\cr
#' labelAlign \tab "String"  \tab Horizontal text alignment for legend labels.\cr
#' labelBaseline \tab "String"  \tab Vertical text baseline for legend labels.\cr
#' labelColor \tab "Color"  \tab Text color for legend labels.\cr
#' labelFont \tab "String"  \tab Font name for legend labels.\cr
#' labelFontSize \tab "Number"  \tab Font size in pixels for legend labels.\cr
#' labelLimit \tab "Number"  \tab The maximum allowed length in pixels of legend labels.\cr
#' labelOffset \tab "Number"  \tab Horizontal offset in pixels between legend symbols and labels.\cr
#' offset \tab "Number"  \tab Offset in pixels of the legend from the chart body.\cr
#' orient \tab "String"  \tab Default legend orientation (e.g., `"right"` or `"left"`).\cr
#' padding \tab "Number"  \tab Padding in pixels between legend border and contents.\cr
#' titleAlign \tab "String"  \tab Horizontal text alignment for legend titles.\cr
#' titleBaseline \tab "String"  \tab Vertical text baseline for legend titles.\cr
#' titleColor \tab "Color"  \tab Text color for legend titles.\cr
#' titleFont \tab "String"  \tab Font name for legend titles.\cr
#' titleFontSize \tab "Number"  \tab Font size in pixels for legend titles.\cr
#' titleFontWeight \tab "String"  \tab Font weight for legend titles.\cr
#' titleLimit \tab "Number"  \tab The maximum allowed length in pixels of legend titles.\cr
#' titlePadding \tab "Number"  \tab Padding in pixels between the legend title and entries.\cr
#' strokeColor \tab "Color"  \tab Border stroke color for the full legend.\cr
#' strokeDash \tab "Number[]"  \tab Border stroke dash pattern for the full legend.\cr
#' strokeWidth \tab "Number"  \tab Border stroke width for the full legend.\cr
#' symbolType \tab "String"  \tab Default shape type (such as `"circle"`) for legend symbols.\cr
#' symbolSize \tab "Number"  \tab Default symbol area size (in pixels<sup>2</sup>).\cr
#' symbolFillColor \tab "Color"  \tab Default fill color for legend symbols. Only applied if there is no `"fill"` scale color encoding for the legend.\cr
#' symbolStrokeColor \tab "Color"  \tab Default stroke color for legend symbols. Only applied if there is no `"fill"` scale color encoding for the legend.\cr
#' symbolStrokeWidth \tab "Number"  \tab Default legend symbol stroke width.\cr
#' }
#'
#' @section Title properties:
#'\tabular{rrl}{
#' Property \tab Type \tab Description   \cr
#' anchor \tab  "String" \tab Title anchor position (`"start"`, `"middle"`, or `"end"`).\cr
#' angle \tab  "Number" \tab Angle in degrees of title text.\cr
#' baseline \tab  "String" \tab Vertical text baseline for title text.\cr
#' color \tab  "Color" \tab Text color for title text.\cr
#' font \tab  "String" \tab Font name for title text.\cr
#' fontSize \tab  "Number" \tab Font size in pixels for title text.\cr
#' fontWeight \tab  "String" \tab Font weight for title text.\cr
#' limit \tab  "Number" \tab The maximum allowed length in pixels of legend labels.\cr
#' offset \tab  "Number" \tab Offset in pixels of the title from the chart body and axes.\cr
#' orient \tab  "String" \tab Default title orientation (`"top"`, `"bottom"`, `"left"`, or `"right"`).\cr
#' }
#'
#' @section Scale Range properties:
#' \tabular{rrl}{
#' Property \tab Type \tab Description   \cr
#' category \tab color scheme \tab Default [color scheme](../schemes) for categorical data.\cr
#' diverging \tab color scheme \tab Default [color scheme](../schemes) for diverging quantitative ramps.\cr
#' heatmap \tab color scheme \tab Default [color scheme](../schemes) for quantitative heatmaps.\cr
#' ordinal \tab color scheme \tab Default [color scheme](../schemes) for rank-ordered data.\cr
#' ramp \tab color scheme \tab Default [color scheme](../schemes) for sequential quantitative ramps.\cr
#' symbol \tab "String[]" \tab Array of [symbol](../marks/symbol) names or paths for the default shape palette.\cr
#' }
#'
#' @param view.autosize string or resize object. Default automatic sizing setting.
#'   Valid string values are "pad", "fit" or "none". See the autosize documentation
#'   for more.
#' @param view.background color. Background color of the view component, or null for
#'   transparent.
#' @param view.group object. Default properties for the top-level group mark
#'   representing the data rectangle of a chart. Valid properties of this object are
#'   mark properties such as "fill", "stroke" and "strokeWidth".
#' @param events object. An object describing which events that originate within the
#'   Vega view should have their default behavior suppressed by invoking the
#'   event.preventDefault method. The defaults object should have a single
#'   property: either "prevent" (to indicate which events should have default behavior
#'   suppressed) or "allow" (to indicate only those events whose default behavior
#'   should be allowed). This property accepts either a boolean value (to prevent/allow
#'   all events) or an array of event type strings.
#' @param mark object. Properties defining default property values for each mark type.
#'   These properties are defined within blocks with names matching a valid mark type
#'   (e.g., "area", "line", "rect"). The valid properties within each block consist of
#'   the legal mark properties (e.g., "fill", "stroke", "size", "font"). Global defaults
#'   for all mark types can be set using the "mark" property.

#'   Defaults for fill or stroke color will be applied only if neither "fill" nor
#'   "stroke" are defined in the Vega spec.
#' @param style object. In addition to the default mark properties above, default
#'   values can be further customized using named styles defined under the style block
#'   in the config. Styles can then be invoked by including a style directive within a
#'   mark definition.
#'
#'   Style settings take precedence over default mark settings, but are overridden by
#'   the axis, legend, and title properties described below.
#' @param axis object. Properties defining default settings for axes. These properties
#'   are defined under the "axis" property in the config object, in which case the
#'   settings apply to all axes.

#'   Additional property blocks can target more specific axis types based on the
#'   orientation ("axisX", "axisY", "axisLeft", "axisTop", etc.) or band scale type
#'   ("axisBand"). For example, properties defined under the "axisBand" property will
#'   only apply to axes visualizing "band" scales. If multiple axis config blocks apply
#'   to a single axis, type-based options take precedence over orientation-based options,
#'   which in turn take precedence over general options.
#'
#'   See details for a list of params.
#' @param legend object. Properties defining default settings for legends. These
#'   properties are defined under the "legend" property within the config object.
#'
#'   See details for a list of params
#' @param title object. Properties defining default settings for titles. These properties
#'   are defined under the "title" property within the config object.
#'
#'   See details for a list of params
#' @param scale.range object. Properties defining named range arrays that can be used
#'   within scale range definitions (such as `{"type": "ordinal", "range": "category"}`).
#'   These properties are defined under the `"range"` property in the config object.
#'
#'   Object-valued properties must be legal [scale range](../scales/#range) definitions.
#'
#'   See details for a list of params
#' @return a config list object
#' @export
vega_config <- function(
  view.autosize = NULL,
  view.background = NULL,
  view.group = NULL,
  events = NULL,
  mark = NULL,
  style = NULL,
  axis=NULL,
  legend = NULL,
  title = NULL,
  scale.range = NULL
) {
  args <- list(autosize=view.autosize, background=view.background, group=view.group,
               events=list(defaults=events), mark=mark, style=style, axis=axis, legend=legend, title=title,
               range=scale.range)
  args <- args[!unlist(lapply(args, is.null))]

  # move mark properties into root.
  # if ("mark" %in% names(args)) {
  #   args <- c(args[names(args) != "mark"], args$mark)
  # }

  # move axes properties into root.
  # if ("axes" %in% names(args)) {
  #   args <- c(args[names(args) != "axes"], args$axes)
  # }

  is.vega_config(args)

  args
}
