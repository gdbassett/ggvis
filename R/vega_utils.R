#' insert a deep value within a list of lists
#'
#' This can be useful for updating specific settings within the vega object
#'
#' @param l list to update
#' @param obj list with the object names to identifying the path to the object to change
#' @param value The value to store in the object to change
#' @return list l with the value updated
#' @examples
#' test <- list(a=1, b=2, c=list(e=list(g=4, h=5), f=3))
#' insert(test, list("c", "e", "g"), 7)
#' insert(test, list("c", "e", 2), 8)
#' @export
insert <- function(l, obj, value=NULL) {
  if (!is.vector(obj)) stop("Obj must be a vector of keys leading to the object to be changed.")
  if (length(obj) > 1) {
    l[[obj[[1]]]] <- insert(l[[obj[[1]]]], obj[-1], value=value)
    return(l)
  } else {
    if (is.null(value)) {
      return(l[[!which(obj[[1]] %in% l)]])
    } else {
      l[obj[[1]]] <- value
      return(l)
    }
  }
}
