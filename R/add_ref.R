#' Add a title to a vega plot
#'
#' @rdname vega_title
#' @param vis a ggvis object
#' @param ... vega title properties
#' @return a ggvis object
#' @export
add_title <- function(vis, ...) {
  vis$options$title <- vega_title(...)
  vis
}

#' Add a config section to a vega plot
#'
#' @param vis a ggvis object
#' @param ... vega config properties
#' @return a ggvis object
#' @export
add_config <- function(vis, ...) {
  vis$vega$config <- vega_config(...)
  vis
}

#' Add a mark
#'
#' @param vis a ggvis object
#' @param ... vega mark properties
#' @return a ggvis object
#' @export
add_mark_ <- function(vis, ...) {
  mark <- vega_mark(...)

  #
  if (!"marks" %in% names(vis$vega)) vis$vega$marks <- list()

  # check if named marks exist in both the incoming mark and vis
  if ("name" %in% names(mark)) {
    locs <- which(mark$name %in% unlist(purrr::map(vis$vega$marks, "name")))
  } else {
    locs <- c()
  }

  # if there's any overlap in names, replace the mark, otherwise add the mark
  if (length(locs) > 0) {
    vis$vega$marks[locs] <- list(mark)
  } else {
    vis$vega$marks[[length(vis$vega$mark) + 1]] <- mark
  }

  # return
  vis
}

#' Add a group mark
#'
#' @param group_mark a mark object object
#' @param ... vega mark properties
#' @return a ggvis object
#' @export
add_group_mark <- function(group_mark, ...) {
  mark <- vega_mark(...)

  #
  if (!"marks" %in% names(group_mark)) group_mark$marks <- list()

  # check if named marks exist in both the incoming mark and vis
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
#' @param ... vega scale properties
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
#' @param ... vega axis properties
#' @return a ggvis object
#' @export
add_axis_ <- function(vis, ...) {
  axis <- vega_axis(...)

  if (!"axes" %in% names(vis$vega)) vis$vega$axes <- list()

  vis$vega$axes[[length(vis$vega$axes) + 1]] <- axis

  # return
  vis
}

#' Add a signal
#'
#' @param vis a ggvis object
#' @param ... vega signal properties
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


#' Update a signal
#'
#' @param vis a ggvis object
#' @param ... vega signal properties
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
