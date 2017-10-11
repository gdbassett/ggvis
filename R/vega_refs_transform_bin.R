#' create a vega bin transform object
#'
#' https://vega.github.io/vega/docs/transforms/bin/index.html
#' 
#' The **bin** transform discretizes numeric values into a set of bins. A common use case is to create a histogram.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param field Field (required) The data field to bin.
#' @param extent Number[] (required) A two-element array with the minimum and maximum values of the bin range.
#' @param anchor Number A value in the binned domain at which to anchor the bins, shifting the bin boundaries if necessary to ensure that a boundary aligns with the anchor value. By default, the minimum bin extent value serves as the anchor.
#' @param maxbins Number The maximum number of bins to create (default `20`).
#' @param base Number The number base to use for automatic bin determination (default `10`).
#' @param step Number An exact step size to use between bins. If provided, options such as _maxbins_ will be ignored.
#' @param steps Number[] An array of allowable step sizes to choose from.
#' @param minstep Number The minimim allowed bin step size (default `0`).
#' @param divide Number[] Allowable bin step sub-divisions. The default value is `[5, 2]`, which indicates that for base 10 numbers (the default base) automatic bin determination can consider dividing bin step sizes by 5 and/or 2.
#' @param nice Boolean If `true` (the default), attempts to make the bin boundaries use human-friendly boundaries, such as multiples of ten.
#' @param signal String If defined, binds the computed binning specification (an object with _start_, _stop_ and _step_ properties) to a signal with the given name.
#' @param as String[] The output fields at which to write the start and end bin values. The default is `["bin0", "bin1"]`.
#' @return a transform object
#' @export
vega_bin_transform <- function(
  type,
  field=NULL,
  extent=NULL,
  anchor=NULL,
  maxbins=NULL,
  base=NULL,
  step=NULL,
  steps=NULL,
  minstep=NULL,
  divide=NULL,
  nice=NULL,
  signal=NULL,
  as=NULL
) {
  args <- list(field=field, extent=extent, anchor=anchor, maxbins=maxbins, base=base, step=step, steps=steps, minstep=minstep, divide=divide, nice=nice, signal=signal, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
