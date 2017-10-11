#' create a vega graticule transform object
#'
#' https://vega.github.io/vega/docs/transforms/graticule/index.html
#' 
#' The **graticule** transform generates a reference grid for cartographic maps. A graticule is a uniform grid of meridians and parallels for showing projection distortion. The default graticule has meridians and parallels every 10° between ±80° latitude; for the polar regions, there are meridians every 90°.
#' 
#' This transform generates a new data stream containing a single [GeoJSON](https://en.wikipedia.org/wiki/GeoJSON) data object for the graticule, which can subsequently be drawn using the [geopath](../geopath) or [geoshape](../geoshape) transform. This transform uses the [d3-geo](https://github.com/d3/d3-geo) library.
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param field Field The data field to bin.
#' @param extentMajor Array[] The major extent of the graticule as a two-element array of coordinates.
#' @param extentMinor Array[] The minor extent of the graticule as a two-element array of coordinates.
#' @param extent Array[] Sets both the major and minor extents to the same values.
#' @param stepMajor Number[] The major step angles of the graticule (default `[90, 360]`).
#' @param stepMinor Number[] The minor step angles of the graticule (default `[10, 10]`).
#' @param step Number[] Sets both the major and minor step angles to the same values.
#' @param precision Number The precision of the graticule in degrees (default `2.5`).
#' @return a transform object
#' @export
vega_graticule_transform <- function(
  type,
  field=NULL,
  extentMajor=NULL,
  extentMinor=NULL,
  extent=NULL,
  stepMajor=NULL,
  stepMinor=NULL,
  step=NULL,
  precision=NULL
) {
  args <- list(field=field, extentMajor=extentMajor, extentMinor=extentMinor, extent=extent, stepMajor=stepMajor, stepMinor=stepMinor, step=step, precision=precision)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
