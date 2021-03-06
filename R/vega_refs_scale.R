#' create a vega scale object
#'
#' https://vega.github.io/vega/docs/scales/
#'
#' **Scales** map data values (numbers, dates, categories, _etc._) to visual values (pixels, colors, sizes). Scales are a fundamental building block of data visualization, as they determine the nature of visual encodings. Vega includes a range of scales for both continuous and discrete input data, and supports mappings for position, shape, size and color encodings.
#'
#' To visualize scales, Vega specifications may include [axes](../axes) or [legends](../legends). For more about supported color encodings, see the [color scheme reference](../schemes). Internally, Vega uses the scales provided by the [d3-scale](https://github.com/d3/d3-scale) library; for more background see [Introducing d3-scale](https://medium.com/@mbostock/introducing-d3-scale-61980c51545f) by Mike Bostock.
#'
#' ## Documentation Overview
#'
#' Scale Properties
#' Scale Types
#' Scale Domains
#' Scale Ranges
#'
#' Vega can be extended at runtime with additional scales using the vega.scale method.
#'
#' ## Quantitative Scales
#'
#' A quantitative scale maps a continuous domain (numbers or dates) to a continuous output range (pixel locations, sizes, colors). The available quantitative scale type values are linear, pow, sqrt, log, time and utc. All quantitative scales except for time and utc use a default domain of [0, 1] and default unit range [0, 1].
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' clamp \tab Boolean \tab A boolean indicating if output values should be clamped to the range (default false). If clamping is disabled and the scale is passed a value outside the domain, the scale may return a value outside the range through extrapolation. If clamping is enabled, the output value of the scale is always within the scale’s range.\cr
#' interpolate \tab String | Object \tab The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in RGB space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include rgb, hsl, hsl-long, lab, hcl, hcl-long, cubehelix and cubehelix-long (‘-long’ variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued type property and an optional numeric gamma property applicable to rgb and cubehelix interpolators. For more, see the d3-interpolate documentation.\cr
#' padding \tab Number \tab Expands the scale domain to accommodate the specified number of pixels on each of the scale range. The scale range must represent pixels for this parameter to function as intended. Padding adjustment is performed prior to all other adjustments, including the effects of the zero, nice, domainMin, and domainMax properties.\cr
#' nice \tab Boolean | Number \tab Extends the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of [0.201479…, 0.996679…], a nice domain might be [0.2, 1.0]. Domain values set via domainMin and domainMax (but not domainRaw) are subject to nicing. Using a number value for this parameter (representing a desired tick count) allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.\cr
#' zero \tab Boolean \tab Boolean flag indicating if the scale domain should include zero. The default value is true for linear, sqrt and pow, and false otherwise.\cr
#' }
#'
#' ## Linear Scales
#'
#' Linear scales (linear) are quantitative scales scales that preserve proportional differences. Each range value y can be expressed as a linear function of the domain value x: y = mx + b.
#'
#' ## Power Scales
#'
#' Power scales (pow) are quantitative scales scales that apply an exponential transform to the input domain value before the output range value is computed. Each range value y can be expressed as a polynomial function of the domain value x: y = mx^k + b, where k is the exponent value. Power scales also support negative domain values, in which case the input value and the resulting output value are multiplied by -1.
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' exponent \tab Number \tab The exponent to use in the scale transform (default 1).\cr
#' }
#'
#' ## Square Root Scales
#'
#' Square root (sqrt) scales are a convenient shorthand for power scales with an exponent of 0.5, indicating a square root transform.
#'
#' ## Logarithmic Scales
#'
#' Log scales (log) are quantitative scales scales in which a logarithmic transform is applied to the input domain value before the output range value is computed. Log scales are particularly useful for plotting data that varies over multiple orders of magnitude. The mapping to the range value y can be expressed as a logarithmic function of the domain value x: y = m log(x) + b.
#'
#' As log(0) = -∞, a log scale domain must be strictly-positive or strictly-negative; the domain must not include or cross zero. A log scale with a positive domain has a well-defined behavior for positive values, and a log scale with a negative domain has a well-defined behavior for negative values. (For a negative domain, input and output values are implicitly multiplied by -1.) The behavior of the scale is undefined if you run a negative value through a log scale with a positive domain or vice versa.
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' base \tab Number \tab The base of the logarithm (default 10).\cr
#' }
#'
#' ## Time and UTC Scales
#'
#' Time scales (time and utc) are quantitative scales with a temporal domain: values in the input domain are assumed to be Date objects or timestamps. The time scale type uses the current local timezone setting. UTC scales (utc) instead use Coordinated Universal Time. Both time and utc scales use a default domain of [2000-01-01, 2000-01-02], and a default unit range [0, 1].
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' nice \tab String | Object | Number | Boolean \tab If specified, modifies the scale domain to use a more human-friendly value range. For time and utc scale types, the nice value can additionally be a string indicating the desired time interval. Legal values are "millisecond", "second", "minute", "hour", "day", "week", "month", and "year". Alternatively, time and utc scales can accept an object-valued interval specifier of the form {"interval": "month", "step": 3}, which includes a desired number of interval steps. Here, the domain would snap to quarter (Jan, Apr, Jul, Oct) boundaries.\cr
#' }
#'
#' ## Sequential Scales
#'
#' Sequential scales (sequential) are similar to linear scales, but use a fixed interpolator to determine the output range. The major use case for sequential scales is continuous quantitative color scales. Sequential scales default to a domain of [0, 1].
#'
#' Akin to quantitative scales, sequential scales support piecewise domain settings with more than two entries. In such cases, the output range is subdivided into equal-sized segments for each piecewise segment of the domain. For example, the domain [1, 4, 10] would lead to the interpolants 1 -> 0, 4 -> 0.5, and 10 -> 1. Piecewise domains are useful for parameterizing diverging color encodings, where a middle domain value corresponds to the mid-point of the color range.
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' clamp \tab Boolean \tab A boolean indicating if output values should be clamped to the range (default false). If clamping is disabled and the scale is passed a value outside the domain, the scale may return a value outside the range through extrapolation. If clamping is enabled, the output value of the scale is always within the scale’s range.\cr
#' domainMax \tab Number \tab Sets the maximum value in the scale domain, overriding the domain property.\cr
#' domainMin \tab Number \tab Sets the minimum value in the scale domain, overriding the domain property.\cr
#' domainMid \tab Number \tab Inserts a single mid-point value into a two-element domain. The mid-point value must lie between the domain minium and maximum values. This property can be useful for setting a midpoint for diverging color scales.\cr
#' range \tab Scheme | Color[] \tab Required. The range value should either be a color scheme object or an array of color strings. If an array of colors is provided, the array will be used to create a continuous interpolator via basis spline interpolation in the RGB color space.\cr
#' }
#'
#' ## Discrete Scales
#'
#' Discrete scales map values from a discrete domain to a discrete range. In the case of band and point scales, the range is determined by discretizing a continuous numeric range.
#'
#' ## Ordinal Scales
#'
#' Ordinal scales (ordinal) have a discrete domain and range. For example, an ordinal scale might map a set of named categories to a set of colors, or to a set of shapes. Ordinal scales function as a “lookup table” from a domain value to a range value.
#'
#' This example uses an ordinal scale for color-coded categories, with up to 20 unique colors:
#'
#' {
#'   "scales": [
#'     {
#'       "name": "color",
#'       "type": "ordinal",
#'       "domain": {"data": "table", "field": "category"},
#'       "range": {"scheme": "category20"}
#'     }
#'     ]
#' }
#' ## Band Scales
#'
#' Band scales (band) accept a discrete domain similar to ordinal scales, but map this domain to a continuous, numeric output range such as pixels. Discrete output values are automatically computed by the scale by dividing the continuous range into uniform bands. Band scales are typically used for bar charts with an ordinal or categorical dimension.
#'
#' In addition to a standard numerical range value (such as [0, 500]), band scales can be given a fixed step size for each band. The actual range is then determined by both the step size and the cardinality (element count) of the input domain. The step size is specified by an object with a step property that provides the step size in pixels, for example "range": {"step": 20}.
#'
#' This image from the d3-scale documentation illustrates how a band scale works:
#'
#' \figure{band.png}{options: width=100 alt="band spacing"}
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' align \tab Number \tab The alignment of elements within each band step, as a fraction of the step size (default 0.5). This value must lie in the range [0,1].\cr
#' padding \tab Number \tab Sets paddingInner and paddingOuter to the same padding value (default 0). This value must lie in the range [0,1].\cr
#' paddingInner \tab Number \tab The inner padding (spacing) within each band step, as a fraction of the step size (default 0). This value must lie in the range [0,1].\cr
#' paddingOuter \tab Number \tab The outer padding (spacing) at the ends of the scale range, as a fraction of the step size (default 0). This value must lie in the range [0,1].\cr
#' }
#'
#' ## Point Scales
#'
#' Point scales (point) are a variant of band scales where the internal band width is fixed to zero. Point scales are typically used for scatterplots with an ordinal or categorical dimension. Similar to band scales, point scale range values may be specified using either a numerical extent ([0, 500]) or a step size ({"step": 20}). As point scales do not have internal band widths (only step sizes between bands), they do not accept the paddingInner property.
#'
#' This image from the d3-scale documentation illustrates how a point scale works:
#'
#' \figure{point.png}{options: width=100 alt="point spacing"}
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' align \tab Number \tab The alignment of elements within each band step, as a fraction of the step size (default 0.5). This value must lie in the range [0,1].\cr
#' padding \tab Number \tab Sets paddingInner and paddingOuter to the same padding value (default 0). This value must lie in the range [0,1].\cr
#' paddingOuter \tab Number \tab The outer padding (spacing) at the ends of the scale range, as a fraction of the step size (default 0). This value must lie in the range [0,1].\cr
#' }
#'
#' ## Discretizing Scales
#'
#' Discretizing scales break up a continuous domain into discrete segments, and then map values in each segment to a range value.
#'
#' ## Quantile Scales
#'
#' Quantile scales (quantile) map a sample of input domain values to a discrete range based on computed quantile boundaries. The domain is considered continuous and thus the scale will accept any reasonable input value; however, the domain is specified as a discrete set of sample values. The number of values in (i.e., the cardinality of) the output range determines the number of quantiles that will be computed from the domain. To compute the quantiles, the domain is sorted, and treated as a population of discrete values. The resulting quantile boundaries segment the domain into groups with roughly equals numbers of sample points per group.
#'
#' Quantile scales are particularly useful for creating color or size encodings with a fixed number of output values. Using a discrete set of encoding levels (typically between 5-9 colors or sizes) sometimes supports more accurate perceptual comparison than a continuous range. For related functionality see quantize scales, which partition the domain into uniform domain extents, rather than groups with equal element counts. Quantile scales have the benefit of evenly distributing data points to encoded values. In contrast, quantize scales uniformly segment the input domain and provide no guarantee on how data points will be distributed among the output visual values.
#'
#' This example color-codes quantile values in five groups, using colors sampled from a continuous color scheme:
#'
#' {
#'   "name": "color",
#'   "scale": "quantile",
#'   "domain": {"data": "table", "field": "value"},
#'   "range": {"scheme": "plasma", "count": 5}
#' }
#'  ## Quantize Scales
#'
#' Quantize scales (quantize) are similar to linear scales, except they use a discrete rather than continuous range. The continuous input domain is divided into uniform segments based on the number of values in (i.e., the cardinality of) the output range. Each range value y can be expressed as a quantized linear function of the domain value x: y = m round(x) + b.
#'
#' Quantize scales are particularly useful for creating color or size encodings with a fixed number of output values. Using a discrete set of encoding levels (typically between 5-9 colors or sizes) sometimes supports more accurate perceptual comparison than a continuous range. For related functionality see quantile scales, which partition the domain into groups with equal element counts, rather than uniform domain extents.
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' nice \tab Boolean | Number \tab Extends the domain so that it starts and ends on nice round values. This method typically modifies the scale’s domain, and may only extend the bounds to the nearest round value. Nicing is useful if the domain is computed from data and may be irregular. For example, for a domain of [0.201479…, 0.996679…], a nice domain might be [0.2, 1.0]. Domain values set via domainMin and domainMax (but not domainRaw) are subject to nicing. Using a number value for this parameter (representing a desired tick count) allows greater control over the step size used to extend the bounds, guaranteeing that the returned ticks will exactly cover the domain.\cr
#' zero \tab Boolean \tab Boolean flag indicating if the scale domain should include zero (default false).\cr
#' }
#'
#' This example color-codes a quantized domain using a 7-point color scheme:
#'
#' {
#'   "name": "color",
#'   "scale": "quantize",
#'   "domain": {"data": "table", "field": "value"},
#'   "range": {"scheme": "blues", "count": 7}
#' }
#' ## Threshold Scales
#'
#' Threshold scales (threshold) are similar to quantize scales, except they allow mapping of arbitrary subsets of the domain (not uniform segments) to discrete values in the range. The input domain is still continuous, and divided into slices based on a set of threshold values provided to the domain property. The range property must have N+1 elements, where N is the number of threshold boundaries provided in the domain.
#'
#' Given the following scale definition,
#'
#' {
#'   "name": "threshold",
#'   "type": "threshold",
#'   "domain": [0, 1],
#'   "range": ["red", "white", "blue"]
#' }
#' the scale will map domain values to color strings as follows:
#'
#' -1   => "red"
#' 0    => "white"
#' 0.5  => "white"
#' 1.0  => "blue"
#' 1000 => "blue"
#'
#' ## Bin-Linear Scales
#'
#' Binned linear scales (bin-linear) are a special type of linear scale for use with data that has been subdivided into bins (for example, using Vega’s bin transform). The domain values for a binned linear scale must be the set of all bin boundaries, from the minimum bin start to maximum bin end. Input domain values are discretized to the appropriate bin, and then run through a standard linear scale mapping. The main benefit of using bin-linear scales is that they provide “bin-aware” routines for sampling values and generating labels for inclusion in legends. They are particularly useful for creating binned size encodings.
#'
#' The trickiest part of using binned linear scales is retrieving the correct set of bin boundaries for the domain property. Here is one way to do this in conjunction with a bin transform:
#'
#' {
#'   "data": [
#'     {
#'       "name": "input",
#'       "transform": [
#'         { "type": "extent", "field": "value", "signal": "extent" },
#'         { "type": "bin", "extent": {"signal": "extent"}, "signal": "bins" }
#'       ]
#'     }
#'   ],
#'   "scales": [
#'     {
#'       "name": "size",
#'       "type": "bin-linear",
#'       "domain": {"signal": "sequence(bins.start, bins.stop + bins.step, bins.step)"},
#'       "range": [1, 1000]
#'     }
#'   ]
#' }
#'
#' ## Bin-Ordinal Scales
#'
#' Binned ordinal scales (bin-ordinal) are a special type of ordinal scale for use with data that has been subdivided into bins (for example, using Vega’s bin transform). The domain values for a binned ordinal scale must be the set of all bin boundaries, from the minimum bin start to maximum bin end. Input domain values are discretized to the appropriate bin, which is then treated as a standard ordinal scale input. The main benefit of using bin-ordinal scales is that they provide “bin-aware” routines for sampling values and generating labels for inclusion in legends. They are particularly useful for creating binned color encodings.
#'
#' The trickiest part of using binned ordinal scales is retrieving the correct set of bin boundaries for the domain property. Here is one way to do this in conjunction with a bin transform:
#'
#' {
#'   "data": [
#'     {
#'       "name": "input",
#'       "transform": [
#'         { "type": "extent", "field": "value", "signal": "extent" },
#'         { "type": "bin", "extent": {"signal": "extent"}, "signal": "bins" }
#'       ]
#'     }
#'   ],
#'   "scales": [
#'     {
#'       "name": "color",
#'       "type": "bin-ordinal",
#'       "domain": {"signal": "sequence(bins.start, bins.stop + bins.step, bins.step)"},
#'       "range": {"scheme": "greens"}
#'     }
#'   ]
#' }
#'
#' ## Scale Domains
#'
#' Scale domains can be specified in multiple ways:
#'
#' * As an array literal of domain values. For example, [0, 500] or ['a', 'b', 'c']. Array literals may include signal references as elements.
#' * A signal reference that resolves to a domain value array. For example, {"signal": "myDomain"}.
#' * A data reference object that specifies field values in one or more data sets.
#'
#' ## Basic Data Reference
#'
#' A basic data reference indicates a data set, field name, and optional sorting for discrete scales:
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' data \tab String \tab Required. The name of the data set containing domain values.\cr
#' field \tab Field \tab Required. The name of the data field (e.g., "price").\cr
#' sort \tab Boolean | Sort \tab If a boolean true value, sort the domain values in ascending order. If object-valued, sort the domain according to the provided sort parameters. Sorting is only supported for discrete scale types.\cr
#' }
#'
#' For example, "domain": {"data": "table", "field": "value"}, derives a scale domain from the value field of data objects in the table data set. If the scale type is quantitative or a quantize, the derived domain will be a two-element [min, max] array. If the scale type is discrete, the derived domain will be an array of all distinct values. If the scale type is quantile, all values will be used to compute quantile boundaries.
#'
#' ## Multi-Field Data References
#'
#' Scale domains can also be derived using values from multiple fields. Multiple fields from the same data set can be specified by replacing the field property with a fields property that takes an array of field names:
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' data \tab String \tab Required. The name of the data set containing domain values.\cr
#' field \tab Field[] \tab Required. The names of the data field (e.g., ["price", "cost"]).\cr
#' sort \tab Boolean | Sort \tab If a boolean true value, sort the domain values in ascending order. If object-valued, sort the domain according to the provided sort parameters. Sorting is only supported for discrete scale types.\cr
#' }
#'
#' More generally, scale domains may also use values pulled from different data sets. In this case, the domain object should have a fields property, which is an array of basic data references:
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' fields \tab DataRef[] \tab Required. An array of basic data references indicating each data set and field value to include in the domain. In addition, array literals (e.g., [0, 100], ["a", "b", "c"]) may be included as elements of the fields array for inclusion in the domain determination.\cr
#' sort \tab Boolean | Sort \tab If a boolean true value, sort the domain values in ascending order. If object-valued, sort the domain according to the provided sort parameters. Sorting is only supported for discrete scale types.\cr
#' }
#'
#' Here is an example that constructs a domain using the fields price and cost drawn from two different data sets:
#'
#' "domain": {
#'   "fields": [
#'     {"data": "table1", "field": "price"},
#'     {"data": "table2", "field": "cost"}
#'     ]
#' }
#'
#' ## Sorting Domains
#'
#' The sort property of a domain data reference can accept, in addition to a simple boolean, an object-valued sort definition:'
#'
#' \tabular{rrl}{
#' *Property* \tab *Type* \tab *Description*\cr
#' field \tab Field \tab The data field to sort by. If unspecified, defaults to the field specified in the outer data reference.\cr
#' op \tab String \tab An aggregate operation to perform on the field prior to sorting. Examples include count, mean and median. This property is required in cases where the sort field and the data reference field do not match. The input data objects will be aggregated, grouped by data reference field values. For a full list of operations, see the aggregate transform.\cr
#' order \tab String \tab The sort order. One of ascending (default) or descending.\cr
#' }
#'
#' This example sorts distinct category field values in descending order by the associated median of the value field:
#'
#' {
#'   "domain": {
#'     "data": "table",
#'     "field": "category",
#'     "sort": {"op": "median", "field": "value", "order": "descending"}
#'   }
#' }
#' This example sorts a multi-field domain in descending order based on the counts of each of the domain values:
#'
#' {
#'   "domain": {
#'     "data": "table",
#'     "fields": ["category1", "category2"],
#'     "sort": {"op": "count", "order": "descending"}
#'   }
#' }
#' Note: For domains drawn from multiple fields, the sort.field property is not allowed and the only legal op is count.
#'
#' ## Scale Ranges
#'
#' Scale ranges can be specified in multiple ways:
#'
#' * As an array literal of range values. For example, [0, 500] or ['a', 'b', 'c']. Array literals may include signal references as elements.
#' * A signal reference that resolves to a range value array. For example, {"signal": "myRange"}.
#' * A color scheme reference for a color palette. For example, {"scheme": "blueorange"}.
#' * For ordinal scales only, a data reference for a set of distinct field values. For example, {"data": "table", "field": "value"}.
#' * For band and point scales only, a step size for each range band. For example, {"step": 20}.
#' * A string indicating a pre-defined scale range default. For example, "width", "symbol", or "diverging".
#'
#' ## Scale Range Defaults
#'
#' Scale ranges can also accept string literals that map to default values. Default values can be modified, and new named defaults can be added, by using custom config settings.
#'
#' \tabular{rrl}{
#' *Value \tab *Description*\cr
#' "width" \tab A spatial range determined by the value of the width signal.\cr
#' "height" \tab A spatial range determined by the value of the height signal. The direction of the range (top-to-bottom or bottom-to-top) is automatically determined according to the scale type.\cr
#' "symbol" \tab The default plotting symbol set to use for shape encodings.\cr
#' "category" \tab The default categorical color scheme to use for nominal data.\cr
#' "diverging" \tab The default diverging color scheme to use for quantitative data.\cr
#' "ordinal" \tab The default sequential color scheme to use for ordinal data.\cr
#' "ramp" \tab The default sequential color scheme to use for quantitative data.\cr
#' "heatmap" \tab The default sequential color scheme to use for quantitative heatmaps.\cr
#' }
#'
#' @param name  String  Required. A unique name for the scale. Scales and projections share the same namespace; names must be unique across both.
#' @param type  String  The type of scale (default linear). See the scale type reference for more.
#'   Quantitative Scales: linear, pow, sqrt, log, time, utc, sequential
#'   Discrete Scales: ordinal, band, point
#'   Discetizing Scales: quantile, quantize, threshold, bin-linear, bin-ordinal
#' @param align Number  The alignment of elements within each band step, as a fraction of the step size (default 0.5). This value must lie in the range [0,1].
#' @param base  Number  The base of the logarithm (default 10 for log scales).
#' @param clamp Boolean A boolean indicating if output values should be clamped to the range (default false). If clamping is disabled and the scale is passed a value outside the domain, the scale may return a value outside the range through extrapolation. If clamping is enabled, the output value of the scale is always within the scale’s range.
#' @param domain  Domain  The domain of input data values for the scale. For quantitative data, this can take the form of a two-element array with minimum and maximum values. For ordinal or categorical data, this may be an array of valid input values. The domain may also be specified as a reference to a data source. See the scale domain reference for more.
#' @param domainMax Number  Sets the maximum value in the scale domain, overriding the domain property. The domainMax property is only intended for use with scales having continuous domains.
#' @param domainMin Number  Sets the minimum value in the scale domain, overriding the domain property. The domainMin property is only intended for use with scales having continuous domains.
#' @param domainMid Number  Inserts a single mid-point value into a two-element domain. The mid-point value must lie between the domain minium and maximum values. This property can be useful for setting a midpoint for diverging color scales. The domainMid property is only intended for use with scales supporting continuous, piecewise domains.
#' @param domainRaw Array An array of raw values that, if non-null, directly overrides the domain property. This is useful for supporting interactions such as panning or zooming a scale. The scale may be initially determined using a data-driven domain, then modified in response to user input by setting the rawDomain value.
#' @param exponent  Number  The exponent to use in the scale transform (default 1 for power scales).
#' @param interpolate String | Object The interpolation method for range values. By default, a general interpolator for numbers, dates, strings and colors (in RGB space) is used. For color ranges, this property allows interpolation in alternative color spaces. Legal values include rgb, hsl, hsl-long, lab, hcl, hcl-long, cubehelix and cubehelix-long (‘-long’ variants use longer paths in polar coordinate spaces). If object-valued, this property accepts an object with a string-valued type property and an optional numeric gamma property applicable to rgb and cubehelix interpolators. For more, see the d3-interpolate documentation.
#' @param nice  String | Object | Number | Boolean  Multiple definitions based on scale type. See scale type documentation.
#' @param padding Number Multiple definitions based on scale type. See scale type documentation.
#' @param paddingInner  Number  The inner padding (spacing) within each band step, as a fraction of the step size (default 0). This value must lie in the range [0,1].
#' @param paddingOuter  Number  The outer padding (spacing) at the ends of the scale range, as a fraction of the step size (default 0). This value must lie in the range [0,1].
#' @param range | Scheme | Color[] Range The range of the scale, representing the set of visual values. For numeric values, the range can take the form of a two-element array with minimum and maximum values. For ordinal or quantized data, the range may be an array of desired output values, which are mapped to elements in the specified domain. See the scale range reference for more.
#' @param reverse Boolean If true, reverses the order of the scale range.
#' @param round Boolean If true, rounds numeric output values to integers. Helpful for snapping to the pixel grid.
#' @param zero  Boolean Boolean flag indicating if the scale domain should include zero. The default value is true for linear, sqrt and pow, and false otherwise.
#' @return a vega scale object
#' @export
vega_scale <- function(
  name=NULL,
  type="linear",
  align=NULL,
  base=NULL,
  clamp=NULL,
  domain=NULL,
  domainMax=NULL,
  domainMin=NULL,
  domainMid=NULL,
  domainRaw=NULL,
  exponent=NULL,
  interpolate=NULL,
  nice=NULL,
  padding=NULL,
  paddingInner=NULL,
  paddingOuter=NULL,
  range=NULL,
  reverse=NULL,
  round=NULL,
  zero=NULL
) {

  if (is.null(name)) {
    name <- paste0("scale_", rand_id())
    message(paste0("scale 'id' is ", name)) # may want to remove this later
  }

  args <- list(name=name, type=type, align=align, base=base, clamp=clamp, domain=domain,
               domainMax=domainMax, domainMin=domainMin, domainMid=domainMid, domainRaw=domainRaw,
               exponent=exponent, interpolate=interpolate, nice=nice, padding=padding,
               paddingInner=paddingInner, paddingOuter=paddingOuter, range=range, reverse=reverse,
               round=round, zero=zero)
  args <- args[!unlist(lapply(args, is.null))]


  is.vega_scale(args, error=TRUE)

  args
}
