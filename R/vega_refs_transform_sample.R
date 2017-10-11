#' create a vega sample transform object
#'
#' https://vega.github.io/vega/docs/transforms/sample/index.html
#' 
#' The **sample** transform randomly samples a data stream to create a smaller stream. As input data objects are added and removed, the sampled values may change in first-in, first-out manner. This transform uses [reservoir sampling](https://en.wikipedia.org/wiki/Reservoir_sampling) to maintain a representative sample of the stream.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param size Number The maximum number of data objects to include in the sample. The default value is `1000`.
#' @return a transform object
#' @export
vega_sample_transform <- function(
  type,
  size=NULL
) {
  args <- list(size=size)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
