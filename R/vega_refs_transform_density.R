#' create a vega density transform object
#'
#' https://vega.github.io/vega/docs/transforms/density/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **density** transform generates a new data stream of uniformly-spaced samples drawn from a one-dimensional [probability density function (pdf)](https://en.wikipedia.org/wiki/Probability_density_function) or [cumulative distribution function (cdf)](https://en.wikipedia.org/wiki/Cumulative_distribution_function). This transform is useful for representing probability distributions and generating continuous distributions from discrete samples using [kernel density estimation](https://en.wikipedia.org/wiki/Kernel_density_estimation).
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param distribution Distribution (required) An object describing the distribution type and parameters. See the  [distribution reference](#distributions) for more.
#' @param extent Number[] A _[min, max]_ domain from which to sample the distribution. This argument is required in most cases, but can be omitted in the case of distributions (namely, `kde`) that can deduce their own extent.
#' @param method String The type of distribution to generate. One of `pdf` (default) or `cdf`.
#' @param steps Number The number of uniformly spaced steps to take along the _extent_ domain (default `100`). A total of _steps + 1_ uniformly-spaced samples are drawn from the distribution.
#' @param as String[] The output fields for the sample value and associated probability. The default is `["value", "density"]`.
#' @return a transform object
#' @export
vega_density_transform <- function(
  type,
  distribution=NULL,
  extent=NULL,
  method=NULL,
  steps=NULL,
  as=NULL
) {
  args <- list(type=type, distribution=distribution, extent=extent, method=method, steps=steps, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
