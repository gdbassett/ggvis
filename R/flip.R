#' Flip the x and y axis
#'
#' This is accomplished by updating the x & y marks, updating the flipping the
#' scales, and updating the axis labels.
#'
#' This currently works for rectangular layer figures.  It may not work with
#' multiple-layer figures or other marks.

axis_flip <- function(vis) {

  lookup <- c(x="y", x2="y2", y="x", y2="x2", height="width", width="height") # create lookup for the values we want to flip

  # Save current data and props
  # old_data  <- vis$cur_data
  old_props <- vis$cur_props

  # update properties
  new_props <- vis$cur_props[[length(props)]] # get props
  names(new_props) <- paste(lookup[n[, 1]], n[, 2], sep = ".") # flip, rejoin and save names
  # loop through properties (to make sure I don't change the property object or lose any other information in it)
  for (var_name in grep("^(x|x2|y|y2|height|width)[.]", names(new_props), value=TRUE)) {
    for (name in grep("^property$|^scale$", names(new_props[[var_name]]), value=TRUE)) {
      # message(paste(var_name, name, new_props[[var_name]][[name]]))
      new_props[[var_name]][[name]] = lookup[new_props[[var_name]][[name]]]
    }
  }
  vis$props[[length(vis$props) + 1]] <- new_props
  vis$cur_props <- new_props

  # Update marks: (for 'enter', 'update', 'exit', 'hover')
  #    Swap the x and y mark
  #    Swap width mark for hight and height for width


  # Update scales:
  #    Swap name ( tied to mark and axis)
  #    Swap hight/width for ranges


  # Update axis names

  # remove old scales, marks, and axis (marks is not actually removed because it's overwritten)

  # add mark back in
  new_mark <- mark(type, props = cur_props(vis), data = vis$cur_data)
  vis <- append_ggvis(vis, "marks", new_mark)

  # add scales back in
  vis <- register_scales_from_props(vis, cur_props(vis))

  # add axes back in
  invisible(lapply(axes, function(axis) {
    if (axis$scale %in% scale_names) {
      new_axis <- create_axis(type, scale, orient, title, title_offset, format,
                          ticks, values, subdivide, tick_padding,
                          tick_size_major, tick_size_minor, tick_size_end,
                          offset, layer, grid, properties)

      append_ggvis(vis, "axes", new_axis)
    }
  }))
  vis <- add_axis(vis, )

  # Restore old data
  # vis$cur_data <- old_data
  vis$cur_props <- old_props
  vis
}
