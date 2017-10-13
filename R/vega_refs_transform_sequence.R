#' create a vega sequence transform object
#'
#' https://vega.github.io/vega/docs/transforms/sequence/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#' 
#' The **sequence** transform generates a data stream containing a seqence of numeric values. See also the [sequence expression function](../../expressions/#sequence).
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param start Number (required) The starting value of the sequence.
#' @param stop Number (required) The ending value (exclusive) of the sequence.
#' @param step Number The step value between sequence entries (default `1`, or `-1` if _stop < start_).
#' @return a transform object
#' @export
vega_sequence_transform <- function(
  type,
  start=NULL,
  stop=NULL,
  step=NULL
) {
  args <- list(type=type, start=start, stop=stop, step=step)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
