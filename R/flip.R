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
  flipped_axes <- c() # tracked so we don't double-flip an axis
  gt_flip <- c(h="v", v="h")
  gt_lookup <- c(x="h", y="v", x2="h", y2="v", width="h", height="v") # ground truth lookup
  encoding_lookup <- c(x="y", x2="y2", y="x", y2="x2", height="width", width="height") # create lookup for the values we want to flip

  if ('type' %in% names(vis) && vis$type=='group') { # it's a grouped mark
    is_group_mark <- TRUE
    spec <- vis
  } else {
    is_group_mark <- FALSE
    # update properties (replace 'encode' column from lookup vector)
    vis$properties[vis$properties$encode %in% encoding_lookup, ]$encode <- encoding_lookup[vis$properties[vis$properties$encode %in% encoding_lookup, 'encode']]
    spec <- vis$vega
  }

  # run across all marks
  for (j in seq_along(spec$marks)) {
    # (Ultimately, everything is anchored on orientation.  Orientation stays the same and everything shifts around it. We have encoding mapping, but need it for scales and axes)
    # Step 1: iterate over everything to get scales and axis mappings.

    # get mark mapping: old scale - new scale
    scales_lookup <- c()
    for (property in intersect(c("enter", "update", "exit", "hover"), names(spec$marks[[j]]$encode))) {
      scales_lookup <- c(
        scales_lookup,
        purrr::map_chr(intersect(c("x", "x2", "y", "y2", "height", "width"), names(spec$marks[[j]]$encode[[property]])), function(encoding) {
          ifelse("scale" %in% names(spec$marks[[j]]$encode[[property]][[encoding]]), spec$marks[[j]]$encode[[property]][[encoding]]$scale, NA)
        })
      )
      names(scales_lookup) <- intersect(c("x", "x2", "y", "y2", "height", "width"), names(spec$marks[[j]]$encode[[property]]))
      # scales -> encoding_lookup -> gt_lookup: scale <-> h/v e.g. c(h="xscale", v="yscale")
      names(scales_lookup) <- gt_lookup[names(scales_lookup)]
      scales_lookup <- scales_lookup[!duplicated(names(scales_lookup))] # we could have repeated keys (names) so remove those.
      # below line is where the flip truly happens.  Scales are now mapped to their flip (e.g. c(xscale="yscale", yscale="xscale"))
      # now old_scale <-> new_scale
      names(scales_lookup) <- scales_lookup[gt_flip[names(scales_lookup)]]
      if (length(scales_lookup) > 2) warning("More than two scales per mark.  Flip may fail or provide inconsistent results.")
    }
    # create axis to orient lookup: name: new scale - orient
    orients_lookup <- purrr::map_chr(spec$axes, "orient")
    names(orients_lookup) <- purrr::map_chr(spec$axes, "scale")
    # orients_lookup <- orients_lookup[scales_lookup[names(orients_lookup)]]
    if (length(orients_lookup) > 2) warning("More than two axis orientations per mark.  Flip may fail or provide inconsistent results.")
    # create orient to title lookup
    titles_lookup <- purrr::map_chr(spec$axes, function(axis) {
      ifelse("title" %in% names(axis), axis$title, NA)
    })
    names(titles_lookup) <- purrr::map_chr(spec$axes, "orient")
    # name: orient, value=scale
    rev_orients_lookup <- names(orients_lookup)
    names(rev_orients_lookup) <- orients_lookup
    # old title -> old_orient -> old scale -> new scale -> new orient -> new title
    names(titles_lookup) <- titles_lookup[orients_lookup[scales_lookup[rev_orients_lookup[names(titles_lookup)]]]]
    if (length(titles_lookup) > 2) warning("More than two axis titles per mark.  Flip may fail or provide inconsistent results.")

    # Step 2: at this point we should have the mark, scale, and axis mappings we need to flip them

    # Swap marks
    for (property in intersect(c("enter", "update", "exit", "hover"), names(spec$marks[[j]]$encode))) {
      encoding_names <- names(spec$marks[[j]]$encode[[property]])
      encoding_names <- ifelse(encoding_names %in% names(encoding_lookup), encoding_lookup[encoding_names], encoding_names)
      names(spec$marks[[j]]$encode[[property]]) <- encoding_names
    }

    # swap scales
    spec$scales <- purrr::map(spec$scales, function(scale){
      if ("range" %in% names(scale) & scale$range %in% names(encoding_lookup)) scale$range <- encoding_lookup[scale$range] # swap height/width range
      scale
    })

    # swap axes
    for (i in seq_along(spec$axes)) {
      if (spec$axes[[i]]$scale %in% names(orients_lookup) && !i %in% flipped_axes) {
        spec$axes[[i]]$orient <- orients_lookup[spec$axes[[i]]$scale]
        spec$axes[[i]]$scale <- scales_lookup[spec$axes[[i]]$scale]
        spec$axes[[i]]$title <- titles_lookup[ifelse("title" %in% names(spec$axes[[i]]), spec$axes[[i]]$title, NA)]
        flipped_axes <- c(flipped_axes, i) # so we can skip it later
      }
    }

  }


  # return
  if (is_group_mark) {
    vis <- spec
  } else {
    vis$vega <- spec
  }
  vis
}
