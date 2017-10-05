#' return a vega title object
#'
#' https://vega.github.io/vega/docs/title/
#' The title directive adds a descriptive title to a chart. Similar to scales, axes,
#'   and legends, a title can be defined at the top-level of a specification or as part
#'   of a group mark.
#'
#'  To create themes, new default values for many title properties can be set using a
#'  config object.
#'
#' @param text string. Required. The title text.
#' @param orient string. The orientation of the title relative to the chart. One of top
#'   (the default), bottom, left, or right.
#' @param anchor string. The anchor position for placing the title. One of start,
#'   middle (the default), or end. For example, with an orientation of top these anchor
#'   positions map to a left-, center-, or right-aligned title.
#' @param encode list. The anchor position for placing the title. One of start, middle
#'   (the default), or end. For example, with an orientation of top these anchor
#'   positions map to a left-, center-, or right-aligned title.
#' @param interactive logical.  Optional mark encodings for custom title styling. This
#'   is a standard encode block for text marks, and may contain enter, exit, update, and
#'   hover property sets. To set a custom font, font size, etc. for a title, one can
#'   either use custom encode blocks or update the title config.
#' @param name string. A mark name property to apply to the title text mark.
#' @param style string. A mark style property to apply to the title text mark. If not
#'   specified, a default style of "group-title" is applied.
#' @param offset number.  The orthogonal offset in pixels by which to displace the title
#'   from its position along the edge of the chart.
#' @param zindex number. The integer z-index indicating the layering of the title group
#'   relative to other axis, mark and legend groups. The default value is 0.
#' @export
#' @examples
#'mtcars %>%
#'  dplyr::group_by(cyl) %>%
#'  dplyr::summarize(mpg = median(mpg)) %>%
#'  dplyr::mutate(cyl = as.factor(cyl)) %>%
#'  ggvis::ggvis(x=~cyl, y=~mpg) %>%
#'  ggvis::layer_bars() %>%
#'  ggvis::add_title(text="MPG by Cylinder")
#' mtcars %>%
#'  dplyr::group_by(cyl) %>%
#'  dplyr::summarize(mpg = median(mpg)) %>%
#'  dplyr::mutate(cyl = as.factor(cyl)) %>%
#'  ggvis::ggvis(x=~cyl, y=~mpg) %>%
#'  ggvis::layer_bars() %>%
#'  ggvis::add_title(text="MPG by Cylinder", orient="top", anchor="start")
vega_title <- function(text,
                       name=NULL,
                       orient=NULL,
                       anchor=NULL,
                       style=NULL,
                       zindex=NULL,
                       interactive=NULL,
                       offset=NULL,
                       encode=NULL)
{
  args <- list(text=text, name=name, orient=orient, anchor=anchor, style=style, zindex=zindex, interactive=interactive,
               offset=offset, encode=encode)
  args <- args[!unlist(lapply(args, is.null))]
  # schema allows string or object
  if (length(args) == 1) {
    if (inherits(text, "character") & length(text) == 1) {
      return(paste(text, collapse=))
    } else {
      stop("Title text must be a character string.")
    }
  } else {
    if (!is.vega_title(args)) {
      stop("Title arguments do not match the VEGA title spec: https://vega.github.io/vega/docs/title/")
    }
    return(args)
  }
}


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
#' @param event object. An object describing which events that originate within the
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
#' @param axes object. Properties defining default settings for axes. These properties
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
  event  = NULL,
  mark = NULL,
  style = NULL,
  axes=NULL,
  legend = NULL,
  title = NULL,
  scale.range = NULL
) {
  args <- list(autosize=view.autosize, background=view.background, group=view.group,
               events=list(defaults=event), mark=mark, style=style, axes=axes, legend=legend, title=title,
               range=scale.range)
  args <- args[!unlist(lapply(args, is.null))]

  # move mark properties into root.
  if ("mark" %in% names(args)) {
    args <- c(args[names(args) != "mark"], args$mark)
  }

  # move axes properties into root.
  if ("axes" %in% names(args)) {
    args <- c(args[names(args) != "axes"], args$axes)
  }

  if (!is.vega_config(args)) {
    stop("Config arguments do not match the VEGA config spec: https://vega.github.io/vega/docs/config/#title")
  }

  args
}

#' Add a static vega mark
#'
#' Graphical **marks** visually encode data using geometric primitives such as
#'   rectangles, lines, and plotting symbols. Marks are the basic visual building block
#'   of a visualization, providing basic shapes whose properties can be set according to
#'   backing data. Mark property definitions may be simple constants or data fields, or
#'   [scales](../scales) can be used to map data values to visual values.
#'
#' @section Mark Types
#'
#' The supported mark types are:
#'
#' * arc - Circular arcs, including pie and donut slices.
#' * area - Filled areas with horizontal or vertical alignment.
#' * image - Images, including icons or photographs.
#' * group - Containers for other marks, useful for sub-plots.
#' * line - Stroked lines, often used for showing change over time.
#' * path - Arbitrary paths or polygons, defined using SVG path syntax.
#' * rect - Rectangles, as in bar charts and timelines.
#' * rule - Rules are line segments, often used for axis ticks and grid lines.
#' * shape - A special variant of path marks for faster drawing of cartographic maps.
#' * symbol - Plotting symbols, including circles, squares and other shapes.
#' * text - Text labels with configurable fonts, alignment and angle.
#' * trail - Lines that can change size based on underlying data.
#'
#' @section Visual Encoding
#'
#' https://vega.github.io/vega/docs/marks/
#'
#' Each mark supports a set of visual encoding properties that determine the position and
#'   appearance of mark instances. Typically one mark instance is generated per input
#'   data element; the exceptions are the `line` and `area` mark types, which represent
#'   multiple data elements as a single line or area shape.
#'
#' There are three primary property sets: _enter_, _update_, _exit_. The _enter_
#'  properties are evaluated when data is processed for the first time and a mark
#' instance is newly added to a scene. The _update_ properties are evaluated for
#' all existing (non-exiting) mark instances. The _exit_ properties are evaluated
#' when the data backing a mark is removed, and so the mark is leaving the visual
#' scene. To better understand how enter, update, and exit sets work, take a look
#' at [Mike Bostock's Thinking with Joins](http://bost.ocks.org/mike/join/).
#'
#' In addition, an optional _hover_ set determines visual properties when the mouse
#'   cursor hovers over a mark instance. Upon mouse out, the _update_ set is applied.
#'
#' There is also a special group mark type (`group`) that can contain other marks,
#'   as well as local data, signal, scale, axis and legend definitions. Groups can
#'   be used to create visualizations consisting of grouped or repeated elements;
#'   examples include stacked graphs (each stack is a separate group containing a
#'   series of data values) and small multiples displays (each plot is contained in
#'   its own group). See the [Group Marks](../marks/group) page for more.
#'
#' @param type String Required. The graphical mark type. Must be one of the supported
#'   mark types.
#' @param clip Boolean Indicates if the marks should be clipped to the enclosing group’s
#'   width and height (default false).
#' @param description String An optional description of this mark. Can be used as a
#'   comment.
#' @param encode Encode An object containing a set of visual encoding rules for mark
#'   properties.
#' @param from From An object describing the data this mark set should visualize. If
#'   undefined, a single element data set containing an empty object is assumed. The from
#'   property can either specify a data set to use (e.g., {"data": "table"}) or provide a
#'   faceting directive to subdivide a data set across a set of group marks.
#' @param interactive Boolean A boolean flag (default true) indicating if the marks can
#'   serve as input event sources. If false, no mouse or touch events corresponding to
#'   the marks will be generated.
#' @param key Field A data field to use as a unique key for data binding. When a
#'   visualization’s data is updated, the key value will be used to match data elements
#'   to existing mark instances. Use a key field to enable object constancy for
#'   transitions over dynamic data.
#' @param name String A unique name for the mark. This name can be used to refer to these
#'   marks within an event stream definition. SVG renderers will add this name value as a
#'   CSS class name on the enclosing SVG group (g) element containing the mark instances.
#' @param on Trigger[] A set of triggers for modifying mark properties in response to
#'   signal changes.
#' @param sort Compare A comparator for sorting mark items. The sort order will determine
#'   the default rendering order. The comparator is defined over generated scenegraph
#'   items and sorting is performed after encodings are computed, allowing items to be
#'   sorted by size or position. To sort by underlying data properties in addition to
#'   mark item properties, append the prefix datum. to a field name
#'   (e.g., {"field": "datum.field"}).
#' @param transform Transform[] A set of post-encoding transforms, applied after any
#'   encode blocks, that operate directly on mark scenegraph items (not backing data
#'   objects). These can be useful for performing layout with transforms that can set x,
#'   y, width, height, etc. properties. Only data transforms that do not generate or
#'   filter data objects may be used.
#' @param role String A metadata string indicating the role of the mark. SVG renderers
#'   will add this role value (prepended with the prefix role-) as a CSS class name on
#'   the enclosing SVG group (g) element containing the mark instances. Roles are used
#'   internally by Vega to guide layout. Do not set this property unless you know which
#'   layout effect you are trying to achieve.
#' @param style String String[] A string or array of strings indicating the name of
#'   custom styles to apply to the mark. A style is a named collection of mark property
#'   defaults defined within the configuration. These properties will be applied to the
#'   mark’s enter encoding set, with later styles overriding earlier styles. Any
#'   properties explicitly defined within the mark’s encode block will override a style
#'   default.
#' @export
vega_mark <- function(
  type,
  clip=FALSE,
  description=NULL,
  encode=NULL,
  from=NULL,
  interactive=TRUE,
  key=NULL,
  name=rand_id(),
  on=NULL,
  sort=NULL,
  transform=NULL,
  role=NULL,
  style=NULL
) {
  # TODO
}

