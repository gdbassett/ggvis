#' Flip the x and y axis
#'
#' This is accomplished by updating the x & y marks, updating the flipping the
#' scales, and updating the axis labels.
#'
#' WARNING: This currently works for rectangular layer figures.  It may not work with
#' multiple-layer figures, other marks, or signals.
#'
#' WARNING: No tests currently exist for this function
#'
#' @param vis a ggvis object
#' @return a ggvis object
#' @export
#' @examples
#' p <- mtcars %>%
#' dplyr::group_by(cyl) %>%
#'   dplyr::summarize(mpg = median(mpg)) %>%
#'   ggvis::ggvis(x=~cyl, y=~mpg) %>%
#'   ggvis::layer_bars() %>%
#'   ggvis::add_axis("x", title="Cylinders") %>%
#'   ggvis::add_axis("y", title="Miles Per Gallon") %>%
#'   ggvis::flip()
#' p <- mtcars %>%
#'   dplyr::group_by(cyl) %>%
#'   dplyr::summarize(mpg = median(mpg)) %>%
#'   ggvis::ggvis(x=~cyl, y=~mpg) %>%
#'   ggvis::layer_bars() %>%
#'   ggvis::flip()
flip <- function(vis) {

  lookup <- c(x="y", x2="y2", y="x", y2="x2", height="width", width="height") # create lookup for the values we want to flip

  # Save current data and props
  # old_data  <- vis$cur_data
  old_props <- vis$cur_props

  # update properties
  new_props <- vis$props[[length(vis$props)]] # get props
  n <- t(as.data.frame(strsplit(names(new_props), "[.]"))) # split the names
  names(new_props) <- ifelse(n[, 1] %in% names(lookup),
                                  paste(lookup[n[, 1]], n[, 2], sep = "."),
                                  names(new_props)) # flip, rejoin and save names
  # loop through properties (to make sure I don't change the property object or lose any other information in it)
  for (var_name in grep("^(x|x2|y|y2|height|width)[.]", names(new_props), value=TRUE)) {
    for (name in grep("^property$|^scale$", names(new_props[[var_name]]), value=TRUE)) {
      # message(paste(var_name, name, new_props[[var_name]][[name]]))
      new_props[[var_name]][[name]] = lookup[new_props[[var_name]][[name]]]
    }
  }
  vis$props[[length(vis$props) + 1]] <- new_props
  vis$cur_props <- new_props

  # Update marks: (for 'enter', 'update', 'exit', 'hover'). We could update from properties, but I don't know enough
  #   to ensure that doesn't overwrite anything in marks
  #    Swap the x and y mark
  #    Swap width mark for hight and height for width
  vis$marks <- lapply(vis$marks, function(mark) {

    new_mark_props <- mark[["props"]] # get props
    n <- t(as.data.frame(strsplit(names(new_mark_props), "[.]"))) # split the names
    names(new_mark_props) <- ifelse(n[, 1] %in% names(lookup),
                                    paste(lookup[n[, 1]], n[, 2], sep = "."),
                                    names(new_mark_props)) # flip, rejoin and save names
    # loop through properties (to make sure I don't change the property object or lose any other information in it)
    for (var_name in grep("^(x|x2|y|y2|height|width)[.]", names(new_mark_props), value=TRUE)) {
      for (name in grep("^property$|^scale$", names(new_mark_props[[var_name]]), value=TRUE)) {
        # message(paste(var_name, name, new_props[[var_name]][[name]]))
        new_mark_props[[var_name]][[name]] = lookup[new_mark_props[[var_name]][[name]]]
      }
    }
    mark[["props"]] <- new_mark_props

    # return
    mark
  })

  # Update scales:
  #    Swap name ( tied to mark and axis)
  #    Swap hight/width for ranges
  vis$scales <- lapply(vis$scales, function(scale) {
    if (scale$property %in% names(lookup)) scale$property <- lookup[scale$property]
    if (scale$name %in% names(lookup)) scale$name <- lookup[scale$name]
    # this may need to flip range similar to how axis filps title
    scale
  })

  # Update axis names
  axes_titles <- unlist(lapply(vis$axes, function(axis) {
    title <- if ("title" %in% names(axis)) axis$title

    if (!is.null(title)) {
      names(title) <- axis$type
      title
    }
  }))
  vis$axes <- lapply(vis$axes, function(axis) {
    if (axis$type %in% names(lookup)) {
      if (lookup[axis$type] %in% names(axes_titles)) {
        axis$title <- axes_titles[lookup[axis$type]]
      } else {
        axis <- axis[names(axis) != "title"]
        attr(axis, "class") <- "ggvis_axis"
      }
      # do nothing.  The axis isn't in our interest list
    }
    axis
  })

  # return
  vis
}


#' Flip the x and y axis
#'
#' This is accomplished by updating the x & y marks, updating the flipping the
#' scales, and updating the axis labels.
#'
#' WARNING: This currently works for rectangular layer figures.  It may not work with
#' multiple-layer figures, other marks, or signals.
#'
#' WARNING: No tests currently exist for this function
#'
#' @param vis a ggvis object
#' @return a ggvis object
#' @export
#' @examples
#' p <- mtcars %>%
#' dplyr::group_by(cyl) %>%
#'   dplyr::summarize(mpg = median(mpg)) %>%
#'   ggvis::ggvis(x=~cyl, y=~mpg) %>%
#'   ggvis::layer_bars() %>%
#'   ggvis::add_axis("x", title="Cylinders") %>%
#'   ggvis::add_axis("y", title="Miles Per Gallon") %>%
#'   ggvis::flip()
#' p <- mtcars %>%
#'   dplyr::group_by(cyl) %>%
#'   dplyr::summarize(mpg = median(mpg)) %>%
#'   ggvis::ggvis(x=~cyl, y=~mpg) %>%
#'   ggvis::layer_bars() %>%
#'   ggvis::flip()
flip_ <- function(vis) {
  lookup <- c(x="y", x2="y2", y="x", y2="x2", height="width", width="height") # create lookup for the values we want to flip

  if ('type' %in% names(vis) && vis$type=='group') { # it's a grouped mark
    is_group_mark <- TRUE
    spec <- vis
  } else {
    is_group_mark <- FALSE
    # update properties (replace 'encode' column from lookup vector)
    vis$properties[vis$properties$encode %in% lookup, ]$encode <- lookup[vis$properties[vis$properties$encode %in% lookup, 'encode']]
    spec <- vis$vega
  }

  # Update marks: (for 'enter', 'update', 'exit', 'hover').
  #    Swap the x and y mark
  #    Swap width mark for hight and height for width
  spec$marks <- lapply(spec$marks, function(mark) {
    new_mark <- mark
    for (property in intersect(c("enter", "update", "exit", "hover"), names(mark$encode))) {
      encodings <- intersect(c("x", "x2", "y", "y2", "height", "width"), names(mark$encode[[property]]))
      scales_lookup <- purrr::map_chr(
        encodings,
        function(encoding) {ifelse('scale' %in% names(mark$encode[[property]][[encoding]]), mark$encode[[property]][[encoding]][["scale"]], NA)})
      names(scales_lookup) <- lookup[encodings]
      new_mark$encode[[property]] <- purrr::map(
        encodings,
        function(encoding) {
          e <- mark$encode[[property]][[encoding]]
          if ("scale" %in% names(new_mark$encode[[property]][[encoding]])) {
            e[["scale"]] <- scales_lookup[lookup]
          }
          # list(e, scales_lookup)
          e
        }
      )
      names(new_mark$encode[[property]]) <- lookup[encodings]
    }

    # return
    new_mark
  })

  # Update scales:
  #    Swap name ( tied to mark and axis)
  #    Swap hight/width for ranges
  spec$scales <- lapply(spec$scales, function(scale) {
    if (scale$name %in% names(scales_lookup)) scale$name <- lookup[scale$name]
    # this may need to flip range similar to how axis filps title
    scale
  })

  # Update axis names
  # map scale to title
  axes_titles <- unlist(lapply(spec$axes, function(axis) {
    title <- if ("title" %in% names(axis)) axis$title
    if (!is.null(title)) {
      names(title) <- axis$scale
      title
    }
  }))
  # move title to axis with new scale
  spec$axes <- lapply(spec$axes, function(axis) {
    if (axis$scale %in% scales_lookup) { # if the current axis has a scale in the new scale lookup
      if (names(scales_lookup[axis$scale]) %in% names(axes_titles)) { # if axis w/ the old scale lookup has a title...
        axis[['title']] <- axes_titles[names(scales_lookup[axis$scale])]
      } else { # no title to put in so remove the title
        axis <- axis[names(axis) != "title"]
      }
    }
    axis
  })

  # return
  if (is_group_mark) {
    vis <- spec
  } else {
    vis$vega <- spec
  }
  vis
}