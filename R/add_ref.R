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
  vis$config <- vega_config(...)
  vis
}
