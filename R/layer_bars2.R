#' Display data with bars (a barchart).
#'
#' This will add bars to a plot. The exact behavior is complicated because
#' the term bar chart is used to describe four important variations on a theme.
#' The action of \code{layer_bars} depends on two factors: whether or not a
#' \code{y} prop has been specified, and whether the \code{x} props is
#' continuous or categorical.
#'
#' @section Visualisations:
#'
#' If no y prop has been specified, then this will count the number of entries
#' at each unique x value. There will be one bar at each unique x value, and
#' the y value (or height) of each bar will represent the count at that x value.
#'
#' If a y prop has been specified, then those y values will be used as weights
#' for a weighted count at each unique x value. If no x values appear more than
#' once in the data, then the end result is a plot where the height of the bar
#' at each x value is simply the y value. However, if an x value appear more
#' than once in the data, then this will sum up the y values at each x.
#'
#' If the x variable is continuous, then a continuous x axis will be used, and
#' the width of each bar is by default equal to the resolution of the data --
#' that is, the smallest difference between any two x values.
#'
#' If the x variable is categorical, then a categorical x axis will be used. By
#' default, the width of each bar is 0.9 times the space between the items.
#'
#' @param vis Visualisation to modify
#' @param ... Visual properties used to override defaults.
#' @param data THe data to use for this figure. May be a dataframe or a name.
#' @param width Width of each bar. When x is continuous, this controls the width
#'   in the same units as x. When x is categorical, this controls the width as a
#'   proportion of the spacing between items (default is 0.9).
#' @param group Field or fields to group by for grouped bar chart
#' @param stack If there are multiple bars to be drawn at an x location, should
#'   the bars be stacked? If FALSE, the bars will be overplotted on each other.
#' @param id A string to append to names created within this layer, (for example
#'   data or transform fields)
#' @seealso \code{\link{layer_histograms}} For bar graphs of counts at each unique
#'   x value, in contrast to a histogram's bins along x ranges.
#' @seealso \code{\link{compute_count}} and \code{\link{compute_tabulate}} for
#'   more information on how data is transformed.
#' @export
#' @examples
#' # Discrete x: bar graph of counts at each x value
#' cocaine %>% ggvis(~state) %>% layer_bars()
#' # Continuous x: bar graph of counts at unique locations
#' cocaine %>% ggvis(~month) %>% layer_bars()
#'
#' # Use y prop to weight by additional variable. This is also useful
#' # if you have pretabulated data
#' cocaine %>% ggvis(~state, ~weight) %>% layer_bars()
#' cocaine %>% ggvis(~month, ~weight) %>% layer_bars()
#'
#' # For continuous x, layer_bars is useful when the variable has a few
#' # unique values that you want to preserve. If you have many unique
#' # values and you want to bin, use layer_histogram
#' cocaine %>% ggvis(~price) %>% layer_bars()
#' cocaine %>% ggvis(~price) %>% layer_histograms(width = 100)
#'
#' # If you have unique x values, you can use layer_bars() as an alternative
#' # to layer_points()
#' pressure %>% ggvis(~temperature, ~pressure) %>% layer_points()
#' pressure %>% ggvis(~temperature, ~pressure) %>% layer_bars()
#'
#' # When x is continuous, width controls the width in x units
#' pressure %>% ggvis(~temperature, ~pressure) %>% layer_bars(width = 10)
#' # When x is categorical, width is proportional to spacing between bars
#' pressure %>% ggvis(~factor(temperature), ~pressure) %>%
#'   layer_bars(width = 0.5)
#'
#' # Stacked bars
#' # If grouping var is continuous, you need to manually specify grouping
#' ToothGrowth %>% group_by(dose) %>%
#'   ggvis(x = ~supp, y = ~len, fill = ~dose) %>% layer_bars()
#' # If grouping var is categorical, grouping is done automatically
#' cocaine %>% ggvis(x = ~state, fill = ~as.factor(month)) %>%
#'   layer_bars()
layer_bars2 <- function(vis, ..., data=NULL, group=NULL, stack = TRUE, width = NULL, id=NULL) {
  new_props <- merge_props(cur_props(vis), props(...)) # turn input into props (so ~x > x.update, etc) and merge with existing props

  # added so that items generated in layer can be uniquely identified in schema object. - gdb 171005
  if (is.null(id)) {
    id <- rand_id()
    message(paste0("layer_bars2 'id' is ", id)) # may want to remove this later
  }

  # added so data can be from existing vis or new. gdb 171005
  if (is.null(data)) {
    if ("data" %in% vis$vega) {
      stop("Data not found. Have you added data?")
    } else if (length(vis$vega$data) == 0) {
      stop("Data not found. Have you added data?")
    } else if (length(vis$vega$data) == 1) {
      data_name <- vis$vega$data[[1]]$name
    } else {
      data_name <- vis$vega$data[[1]]$name
      warning(paste0("Data source to use is ambiguous. Guessing ", data_name, ".")) # guess the first one
    }
  } else if (is.character(data)) {
    data_name <- data
    if (!data_name %in% unlist(purrr::map(vis$static_data, "name")))
    { stop(paste0("Data ", data_name, " not found. Have you added data?")) }
  } else {
    data_name <- paste0(deparse2(substitute(data)), "_", id)
    vis <- add_data(vis, data, data_name)
  }


  check_unsupported_props(new_props, c("x", "y", "x2", "y2"),
                          c("enter", "exit", "hover"), "layer_bars")

  x_var <- find_prop_var(new_props, "x.update") # get the data column associated with the x axis
  discrete_x <- prop_countable(cur_data(vis), new_props$x.update)

  vis <- set_scale_label(vis, "x", prop_label(cur_props(vis)$x.update))


  if (!is.null(new_props$y.update)) {
    if (prop_countable(cur_data(vis), new_props$y.update)) {
      stop("y variable (weights) must be numeric.")
    }
    y_var <- find_prop_var(new_props, "y.update")
    vis <- set_scale_label(vis, "y", prop_label(cur_props(vis)$y.update))

  } else {
    y_var <- NULL
    vis <- set_scale_label(vis, "y", "count")
  }

  if (discrete_x) {
    if (is.null(width)) {
      width <- 0.9
    }

    if (stack || !is.null(group)) {
      # TODO: If a grouping variable exists, need to add a transform. use 'stacked'.  ~group should be an enquoted param from ...
      # transform <- list(
      #   type = "stack",
      #   groupby = group,
      #   field = y_var,
      #   sort = list(field = x_var, order="descending"),
      #   as = c(paste(y_var, "0_", id), paste(y_var, "1_", id))
      # )
      # Old layer rects replaced by add_mark_
      # v <- layer_rects(v,
      #                  x = y_var, width = band(),
      #                  y = 0, y2 = stats::formula(paste0("~", paste(y_var, "1_", id))))
      e <- vega_encode(update = list(
        y = list(scale="y", value=0), y2 = list(scale="y", field=as.character(y_var)[2]),
        x = list(scale="x", field=as.character(x_var)[2]), width = list(scale="x", band=width)
      ))
      vis <- add_mark_(vis, type="rect", from = list(data=data_name), encode=e, name=paste0("mark_", id))
    } else {
      # creat group mark
      s <- vega_scale() # TODO
      e <- vega_encode(update=list(y = list(scale="y", field=group)))
      g <- vega_mark(type="group", from=list(data=data_name), encode=e, scales=list(s), name=paste0("group_mark_", id))

      # add mark to marks in group mark
      e <- vega_encode(update=list(
        y = list(scale="y", value=0), y2 = list(scale="y", field=as.character(y_var)[2]),
        x = list(scale="x", field=as.character(x_var)[2]), width = list(scale="x", band=width)
      ))
      g <- add_group_mark(g, type="rect", from = list(data=data_name), encode = e, name=paste0("mark_", id))
      # add group mark to visualization
      vis <- add_mark_(vis, g)
      # v <- layer_rects(v,
      #                  x = x_var, width = band(),
      #                  y = 0, y2 = y_var)
    }
    vis <- add_scale_(vis, name="x", type="band", paddingOuter=0, range="width",
                      domain=list(data=data_name, field=as.character(x_var)[2]),
                      round=TRUE)
    vis <- add_axis_(vis, scale="x", orient="bottom", title=as.character(y_var)[2])

    # vis <- scale_nominal(vis, "x", padding = 1 - width, points = FALSE) # pass-through to ggvis_scale that sets an ordinal scale with a 'nominal' subclass. ('nominal' is set in the 'class' so might be used for specific nominal functions)

  } else {
    #TODO: EVERYTHING BELOW
    vis <- layer_f(vis, function(v) {
      # v <- add_props(v, .props = new_props)
      # v <- compute_count(v, x_var, y_var)
      # v <- compute_align(v, ~x_, length = width)
      if (stack) {
        # v <- compute_stack(v, stack_var = ~count_, group_var = ~x_) # OLD
        # TODO: Create aggregate-count transform
        transform <- list(
          type = "aggregate",
          groupby = group,
          op = "count",
          as = paste(y_var, "_", id)
        )
        v <- layer_rects(v, x = ~xmin_, x2 = ~xmax_, y = ~stack_upr_,
                         y2 = ~stack_lwr_)
      } else {
        v <- layer_rects(v, x = ~xmin_, x2 = ~xmax_, y = 0, y2 = ~count_)
      }
      v
    })
  }

  vis <- add_scale_(vis, name="y", type="linear", range="height",
                    domain=list(data=data_name, field=as.character(y_var)[2]),
                    round=TRUE)
  vis <- add_axis_(vis, scale="y", orient="left", title=as.character(y_var)[2])
  # vis <- scale_numeric(vis, "y", domain = c(0, NA), expand = c(0, 0.05))

  vis
}
