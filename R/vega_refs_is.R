#' Validate if the object is a vega title object
#' @param obj an object
#' @return logical
is.vega_title <- function(obj) {
  ret <- TRUE
  if (inherits(obj, "character") & length(obj) == 1) {
    # pass, ret = TRUE
  } else if (inherits(obj, "list")) {
    additionalProperties <- setdiff(names(obj), c("name", "orient", "anchor", "style", "zindex", "interactive", "offset", "encode"))
    if (length(additionalProperties) == 0) {
      ret <- FALSE
    }
    if ("orient" %in% names(obj)) {
      if (!obj$orient %in% c("bottom", "left", "right", "top")) {
        ret <- FALSE
      }
    }
    if ("anchor" %in% names(obj)) {
      if (!obj$anchor %in% c("start", "middle", "end")) {
        ret <- FALSE
      }
    }
    if ("encode" %in% names(obj)) {
      #TODO: validate against encode object
    }
    if ("interactive" %in% names(obj)) {
      if (!inherits(obj$interactive, "logical") || length(obj) != 1) {
        ret <- FALSE
      }
    }
    if ("name" %in% names(obj)) {
      if (!inherits(obj$name, "character") || length(obj) != 1) {
        ret <- FALSE
      }
    }
    if ("style" %in% names(obj)) {
      if (!inherits(obj$style, "character")) {
        ret <- FALSE
      }
    }
    if ("offset" %in% names(obj)) {
      if (!inherits(obj$offset, "number") || length(obj) != 1) {
        ret <- FALSE
      }
    }
    if ("offset" %in% names(obj)) {
      if (!inherits(obj$zindex, "number") || length(obj) != 1) {
        ret <- FALSE
      }
    }
  }
  ret
}

#' Validate if the object is a vega config object
#' @param obj an object
#' @return logical
is.vega_config <- function(obj) {
  ret <- TRUE

  #TODO Go through all the  config properties and make sure they are correct.
  # most will probably be validating the properties as other types of objects.

  ret
}
