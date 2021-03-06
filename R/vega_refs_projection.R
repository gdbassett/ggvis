#' Create a vega projection object
#'
#' https://vega.github.io/vega/docs/projections/
#'
#' Cartographic projections map (longitude, latitude) pairs to projected (x, y) coordinates. Vega uses projections to layout both geographic points (such as locations on a map) for which longitude and latitude coordinates are part of the input data, and geographic regions (such as countries and states) represented using the GeoJSON format.
#'
#' @param name String Required. A unique name for the projection. Projections and scales share the same namespace; names must be unique across both.
#' @param type String The cartographic projection to use. The default is "winkel3". This value is case-insensitive, for example "albers" and "Albers" indicate the same projection type.
#' @param clipAngle Number Sets the projection’s clipping circle radius to the specified angle in degrees. If null, switches to antimeridian cutting rather than small-circle clipping.
#' @param clipExtent Array Sets the projection’s viewport clip extent to the specified bounds in pixels. The extent bounds are specified as an array [[x0, y0], [x1, y1]], where x0 is the left-side of the viewport, y0 is the top, x1 is the right and y1 is the bottom. If null, no viewport clipping is performed.
#' @param scale Number Sets the projection’s scale factor to the specified value. The default scale is projection-specific. The scale factor corresponds linearly to the distance between projected points; however, scale factor values are not equivalent across projections.
#' @param translate Number[] Sets the projection’s translation offset to the specified two-element array [tx, ty]. If translate is not specified, returns the current translation offset which defaults to [480, 250]. The translation offset determines the pixel coordinates of the projection’s center. The default translation offset places (0°,0°) at the center of a 960×500 area.
#' @param center Number[] Sets the projection’s center to the specified center, a two-element array of longitude and latitude in degrees. The default value is [0, 0].
#' @param rotate Number[] Sets the projection’s three-axis rotation to the specified angles, which must be a two- or three-element array of numbers [lambda, phi, gamma] specifying the rotation angles in degrees about each spherical axis. (These correspond to yaw, pitch and roll.) The default value is [0, 0, 0].
#' @param precision String Sets the threshold for the projection’s adaptive resampling to the specified value in pixels. This value corresponds to the Douglas–Peucker distance. If precision is not specified, returns the projection’s current resampling precision which defaults to √0.5 ≅ 0.70710…
#' @param fit Object | Array GeoJSON data to which the projection should attempt to automatically fit the translate and scale parameters. If object-valued, this parameter should be a GeoJSON Feature or FeatureCollection. If array-valued, each array member may be a GeoJSON Feature, FeatureCollection, or a sub-array of GeoJSON Features.
#' @param extent Array[] Used in conjunction with fit, provides the pixel area to which the projection should be automatically fit. The extent bounds are specified as an array [[x0, y0], [x1, y1]], where x0 is the left side of the extent, y0 is the top, x1 is the right and y1 is the bottom.
#' @param size Number[] Used in conjunction with fit, provides the width and height in pixels of the area to which the projection should be automatically fit. This parameter is equivalent to an extent of [[0,0], size].
#' @param coefficient Number (Not used in all projections. Default = 2 when used.) Hammer coefficient.
#' @param distance Number (Not used in all projections. Default = 2 when used.) Distance from the center of the sphere to the point of view, as a proportion of the sphere’s radius; defaults to 2.0. The recommended maximum clip angle for a given distance is acos(1 / distance) converted to degrees. If tilt is also applied, then more conservative clipping may be necessary. For exact clipping, the in-development geographic projection pipeline is needed; see the satellite example.
#' @param fraction Number (Not used in all projections. Default = 0.5 when used.) [0, 1] Used in Bottomley projection
#' @param lobes Number (Not used in all projections. Default = 5 when used.) If lobes is specified, sets the number of lobes in the resulting star, and returns this projection.
#' @param parallel Degrees as Number (Not used in all projections. Default = 20 when used.) The armadillo projection center parallel.
#' @param radius Degrees as Number(Not used in all projections. Default = 30 when used.) Used in the U.S.-centric Gingery world projection, as inspired by Cram’s Air Age.
#' @param ratio Number (Not used in all projections. Default = 1 when used.) Used in hill eucyclic projection is pseudoconic and equal-area.  With a ratio of 0, this projection becomes the Maurer No. 73. As it approaches ∞, the projection converges to the Eckert IV.
#' @param spacing Number (Not used in all projections. Default = 0.5 when used.) Used in The Lagrange conformal projection.
#' @param tilt Degrees as Number (Not used in all projections. Default = 0 when used.) Used in the satellite (tilted perspective) projection.
#' @return a vega projection object
#' @export
vega_projection <- function(
  name=NULL,
  type="winkel3",
  clipAngle=NULL,
  clipExtent=NULL,
  scale=NULL,
  translate=NULL,
  center=NULL,
  rotate=NULL,
  precision=NULL,
  fit=NULL,
  extent=NULL,
  size=NULL,
  coefficient=NULL,
  distance=NULL,
  fraction=NULL,
  lobes=NULL,
  parallel=NULL,
  radius=NULL,
  ratio=NULL,
  spacing=NULL,
  tilt=NULL
) {
  if (is.null(name)) {
    name <- paste0("projection_", rand_id())
    message(paste0("Projection name is ", name, "."))
  }

  args <- list(name-name, type=type, clipAngle=clipAngle, clipExtent=clipExtent, scale=scale,
               translate=translate, center=center, rotate=rotate, precision=precision, fit=fit,
               extent=extent, size=size, coefficient=coefficient, distance=distance, fraction=fraction,
               lobes=lobes, parallel=parallel, radius=radius, ratio=ratio, spacing=spacing, tilt=tilt)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_legend(args, error=TRUE)

  args
}
