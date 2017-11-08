#' Add a title to a vega plot
#'
#' @rdname vega_title
#' @param vis a ggvis object
#' @inheritDotParams vega_title
#' @return a ggvis object
#' @export
add_title <- function(vis, ...) {
  vis$vega$title <- vega_title(...)
  vis
}

#' Add a config section to a vega plot
#'
#' @param vis a ggvis object
#' @inheritDotParams vega_config
#' @return a ggvis object
#' @export
add_config <- function(vis, ...) {
  vis$vega$config <- vega_config(...)
  vis
}

#' Add a mark
#'
#' @param vis a ggvis object
#' @param parent Place within a group_mark.  Default = FALSE.
#'   If 'TRUE', the mark will be placed within the first group mark.
#'   If an integer (N), the mark will be placed within the Nth group mark.
#'   If a name, will be placed in the group mark with the given name.
#' @inheritDotParams vega_mark
#' @return a ggvis object
#' @export
add_mark_ <- function(vis, parent=FALSE, ...) {
  if (parent == FALSE) {
    dest <- vis$vega
    group_mark <- FALSE
  } else if (parent == TRUE) {
    group_mark <- which("group" %in% unlist(purrr::map(vis$vega$marks, "type")))[1]
    dest <- vis$vega$marks[[group_mark]]
  } else if (is.integer(parent)) {
    group_mark <- which("group" %in% unlist(purrr::map(vis$vega$marks, "type")))[parent]
    dest <- vis$vega$marks[[group_mark]]
  } else if (is.character(parent)) {
    group_mark <- which(parent %in% unlist(purrr::map(vis$vega$marks, "name")))[1]
    dest <- vis$vega$marks[[group_mark]]
  } else {
    stop(paste0("Parent ", parent, " is not a valid value."))
  }

  mark <- vega_mark(...)

  #
  if (!"marks" %in% names(dest)) dest$marks <- list()

  # check if named marks exist in both the incoming mark and vis
  if ("name" %in% names(mark)) {
    locs <- which(mark$name %in% unlist(purrr::map(dest$marks, "name")))
  } else {
    locs <- c()
  }

  # if there's any overlap in names, replace the mark, otherwise add the mark
  if (length(locs) > 0) {
    dest$marks[locs] <- list(mark)
  } else {
    dest$marks[[length(dest$mark) + 1]] <- mark
  }

  # put dest back
  if (parent != FALSE) {
    vis$vega$marks[[group_mark]] <- dest
  } else {
    vis$vega <- dest
  }

  # return
  vis
}

#' Add a group mark
#'
#' @param group_mark a mark object object
#' @inheritDotParams vega_mark
#' @return a ggvis object
#' @export
add_group_mark <- function(group_mark, ...) {
  mark <- vega_mark(...)

  #
  if (!"marks" %in% names(group_mark)) group_mark$marks <- list()

  # check if named marks exist in both the incoming mark and group_mark
  if ("name" %in% names(mark)) {
    locs <- which(mark$name %in% unlist(purrr::map(group_mark$marks, "name")))
  } else {
    locs <- c()
  }

  # if there's any overlap in names, replace the mark, otherwise add the mark
  if (length(locs) > 0) {
    group_mark$marks[locs] <- list(mark)
  } else {
    group_mark$marks[[length(group_mark$marks) + 1]] <- mark
  }

  # return
  group_mark
}

#' Add a scale
#'
#' @param vis a ggvis object
#' @inheritDotParams vega_scale
#' @return a ggvis object
#' @export
add_scale_ <- function(vis, ...) {
  scale <- vega_scale(...)

  #
  if (!"scales" %in% names(vis$vega)) vis$vega$scales <- list()

  # check if named scales exist in both the incoming scale and vis
  if ("name" %in% names(scale)) {
    locs <- which(scale$name %in% unlist(purrr::map(vis$vega$scales, "name")))
  } else {
    locs <- c()
  }

  # if there's any overlap in names, replace the scale, otherwise add the scale
  if (length(locs) > 0) {
    vis$vega$scales[locs] <- list(scale)
  } else {
    vis$vega$scales[[length(vis$vega$scales) + 1]] <- scale
  }

  # return
  vis
}

#' Add a axis
#'
#' @param vis a ggvis object
#' @inheritDotParams vega_axis
#' @return a ggvis object
#' @export
add_axis_ <- function(vis, ...) {
  axis <- vega_axis(...)

  if (!"axes" %in% names(vis$vega)) vis$vega$axes <- list()

  vis$vega$axes[[length(vis$vega$axes) + 1]] <- axis

  # return
  vis
}

#' Add a vega signal to a vega vis
#'
#' @param vis a ggvis object
#' @inheritDotParams vega_signal
#' @return a ggvis object
#' @export
add_signal_ <- function(vis, ...) {
  signal <- vega_signal(...)

  if (!"signals" %in% names(vis$vega)) vis$vega$signals <- list()

  # check if named scales exist in both the incoming scale and vis
  if ("name" %in% names(signal)) {
    locs <- which(signal$name %in% unlist(purrr::map(vis$vega$signals, "name")))
  } else {
    locs <- c()
  }

  # if there's any overlap in names, replace the scale, otherwise add the scale
  lapply
  if (length(locs) > 0) {
    vis$vega$signals[locs] <- list(signal)
  } else {
    vis$vega$signals[[length(vis$vega$signals) + 1]] <- signal
  }

  # return
  vis
}


#' Update a vega signal in a vega vis
#'
#' if the signal name isn't found int he vis, it is added
#'
#' @param vis a ggvis object
#' @inheritDotParams vega_signal
#' @return a ggvis object
#' @export
update_signal_ <- function(vis, ...) {
  signal <- vega_signal(...)

  if (!"signals" %in% names(vis$vega)) vis$vega$signals <- list()

  # check if named scales exist in both the incoming scale and vis
  if ("name" %in% names(signal)) {
    locs <- which(signal$name %in% unlist(purrr::map(vis$vega$signals, "name")))
  } else {
    locs <- c()
  }

  # if there's any overlap in names, replace the scale, otherwise add the scale
  if (length(locs) == 0) {
    vis$vega$signals[[length(vis$vega$signals) + 1]] <- signal
  } else {
    lapply(locs, function(loc) {
      # on & bind are lists of lists, so update them individually
      lapply(c("on", "bind"), function(prop) {
        if (prop %in% names(signal)) {
          if (prop %in% names(vis$vega$signals[[loc]])) {
            # update the properties in the signal so we can just update the signal later
            singal[[prop]] <- c(vis$vega$signals[[loc]][[prop]], singal[[prop]])
          }
        }
      })
      vis$vega$signals[[loc]] <- utils::modifyList(vis$vega$signals[[loc]], signal)
    })
  }

  # return
  vis
}

#' Add a vega legend to a vega vis
#'
#' Note: legend is simply appended without trying to identify other legends for the scale the legend applies to
#'
#' @param vis a ggvis object
#' @inheritDotParams vega_legend
#' @return a ggvis object
#' @export
add_legend_ <- function(vis, ...) {
  legend <- vega_legend(...)

  if (!"legends" %in% names(vis$vega)) vis$vega$legends <- list()

  vis$vega$legends[[length(vis$vega$legends) + 1]] <- legend

  # return
  vis
}


#' Add a vega projection to a vega vis
#'
#' @param vis a ggvis object
#' @inheritDotParams vega_projection
#' @return a ggvis object
#' @export
add_projection <- function(vis, ...) {
  projection <- vega_projection(...)

  #
  if (!"projections" %in% names(vis$vega)) vis$vega$projections <- list()

  # check if named projections exist in both the incoming projection and vis
  if ("name" %in% names(projection)) {
    locs <- which(projection$name %in% unlist(purrr::map(vis$vega$projections, "name")))
  } else {
    locs <- c()
  }

  # if there's any overlap in names, replace the projection, otherwise add the projection
  if (length(locs) > 0) {
    vis$vega$projections[locs] <- list(projection)
  } else {
    vis$vega$projections[[length(vis$vega$projection) + 1]] <- projection
  }

  # return
  vis
}


#' Add a vega transform to a vega vis
#'
#' The transform is added to the first data then group mark found matching 'name'.  If none are found, no transform is added.
#'
#' @param vis a ggvis object
#' @param name string a data or mark object name
#' @param type string type of transform to add
#' @param ... arguments to transforms
#' @return a ggvis object
#' @export
add_transform <- function(vis, name, type, ...) {
  transforms <- c("aggregate", "bin", "collect", "contour", "countpattern", "cross", "crossfilter",
                  "density", "extent", "filter", "fold", "force", "formula", "geojson", "geopath",
                  "geopoint", "geoshape", "graticule", "identifier", "impute", "joinaggregate",
                  "linkpath", "lookup", "nest", "pack" , "partition", "pie", "resolvefilter", "sample",
                  "sequence", "stack", "stratify", "tree", "treelinks", "treemap", "voronoi", "window",
                  "wordcloud")
  if (!type %in% transforms) stop(paste0("Type ", type, " is not a valid transform."))

  transform <- eval(parse(text=paste0("vega_", type, "_transform(type, ...)")))


  locs <- c()

  if ("data" %in% names(vis$vega)) {
    locs <- which(name %in% unlist(purrr::map(vis$vega$data, "name")))
  }
  if (length(locs) > 0) {
    loc <- min(locs)
    if (!"transform" %in% names(vis$vega$data[[loc]])) vis$vega$data[[loc]]$transform <- list()
    vis$vega$data[[loc]]$transform[[length(vis$vega$data[[loc]]$transform) + 1]] <- transform
  } else { # no data match
    if ("marks" %in% names(vis$vega)) {
      locs <- which(name %in% unlist(purrr::map(vis$vega$marks, "name")))
      group_locs <- which("group" %in% unlist(purrr::map(vis$vega$marks, "type")))
      locs <- intersect(locs, group_locs)
      loc <- min(locs)
      if (!"transform" %in% names(vis$vega$marks[[loc]])) vis$vega$marks[[loc]]$transform <- list()
      vis$vega$marks[[loc]]$transform[[length(vis$vega$marks[[loc]]$transform) + 1]] <- transform
    }
  }

  # return
  vis
}


