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
#' @param ... vega title properties
#' @return a ggvis object
#' @export
add_config <- function(vis, ...) {
  vis$vega$config <- vega_config(...)
  vis
}

#' Add a mark
#'
#' @param vis a ggvis object
#' @param ... vega title properties
#' @return a ggvis object
#' @export
add_mark_ <- function(vis, ...) {
  mark <- vega_mark(...)

  #
  if (!"mark" %in% names(vis$vega)) vis$vega$mark <- list()

  # check if named marks exist in both the incoming mark and vis
  if ("name" %in% names(mark)) {
    locs <- which(mark$name %in% unlist(purrr::map(vis$vega$mark, "name")))
  } else {
    locs <- c()
  }

  # if there's any overlap in names, replace the mark, otherwise add the mark
  lapply
  if (length(locs) > 0) {
    vis$vega$mark[locs] <- list(mark)
  } else {
    vis$vega$mark[[length(vis$vega$mark) + 1]] <- mark
  }

  # return
  vis
}
