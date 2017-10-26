#' Generate a line chart
#'
#' Assumptions:
#'   - y is numeric
#'   - all columns beyond x, y, and grouping will be aggregated
#'
#' The only difference between a line and path chart is the line chart is ordered
#' @param vis Visualisation to modify
#' @inheritDotParams layer_path_ -arrange
#' @export
#'
layer_line_ <- function(vis, ...) {
  layer_path_(vis, arrange=TRUE, ...)
}

#' Generate a path chart
#'
#' @param vis Visualisation to modify
#' @inheritDotParams vega_mark
#' @param from The data or other source to use for this figure. May be a dataframe or a name.
#' @param arrange Default NULL.  Boolean or other 'sort' input. Setting 'TRUE' creates line layer
#' @param stack If there are multiple bars to be drawn at an x location, should
#'   the bars be stacked? If FALSE, the bars will be overplotted on each other.
#' @param id A string to append to names created within this layer, (for example
#'   data or transform fields)
#' @export
layer_path_ <- function(vis, ..., from=NULL, stack = FALSE, arrange=NULL, id=NULL) {


  # added so that items generated in layer can be uniquely identified in schema object. - gdb 171005
  if (is.null(id)) {
    id <- rand_id()
    message(paste0("layer_path_ 'id' is ", id)) # may want to remove this later
  }

  # added so data can be from existing vis or new. gdb 171005
  if (is.null(from)) {
    from <- vis$properties[!is.na(vis$properties$from), 'from']
    if (length(unique(from)) > 1) message(paste0("'from' missing for layer_path_ layer ", id, ".  Guessing ", from[1], "."))
    from <- from[1]
  } else if (is.character(from)) {
    if (!from %in% unlist(purrr::map(vis$vega$data, "name")))
    { stop(paste0("Data ", from, " not found. Have you added data?")) }
  } else {
    vis <- add_data(vis, from, paste0(deparse2(substitute(from)), "_", id))
  }

  # join in any passed features
  props <- update_props(vis$properties, from, ...)
  props <- rbind(props, data.frame(value=TRUE, encode="interpolate", from=NA, field="linear", stringsAsFactors = FALSE))

  # aggregate if necessary
  if ("group" %in% props$encode) {
    vis <- add_transform(vis, name = from, "aggregate", groupby=c(props[props$encode=='x', 'field'][1], props[props$encode=="group", "field"][1]),
                         ops=I("sum"), fields=I(props[props$encode=='y', 'field'][1]), as=I(paste0(props[props$encode=='y', 'field'][1], "_sum")))
    vis <- add_scale_(vis, name="color", type="ordinal", range="category",
                      domain=list(data=from, field=props[props$encode=="group", "field"][1]))
    props <- rbind(
      data.frame(value=FALSE, encode="stroke", from=from, field=props[props$encode=="group", "field"][1], stringsAsFactors = FALSE),
      data.frame(value=FALSE, encode="y", from=from, field=paste0(props[props$encode=='y', 'field'][1], "_sum"), stringsAsFactors = FALSE),
      props
    )
  } else {
    vis <- add_transform(vis, name = from, "aggregate", groupby=I(props[props$encode=='x', 'field'][1]), ops=I("sum"),
                         fields=I(props[props$encode=='y', 'field'][1]), as=I(paste0(props[props$encode=='y', 'field'][1], "_sum")))
    props <- rbind(
      data.frame(value=FALSE, encode="y", from=from, field=paste0(props[props$encode=='y', 'field'][1], "_sum"), stringsAsFactors = FALSE),
      props
    )
  }

  # If stacked, create a stacked transform
  if (stack) {
    vis <- add_transform(vis, name = from, "stack", groupby=I(props[props$encode=='x', 'field'][1]), field="y", as=I("stack0", "stack1"))
    props <- rbind(data.frame(value=FALSE, encode="y", from=from, field="stack1", stringsAsFactors = FALSE), props)
  }

  # deduplicate properties
  props <- props[!duplicated(props$encode), ]
  if (!"x" %in% props$encode) stop("Layer_path_ requires a 'x' encode be defined.")

  # filter properties that are duplicate
  props <- props[!props$encode %in% c("y2", "x2", "xc", "height", "width"), ]

  # build the encode
  scales <- c(x="x", y="y", stroke="color")
  enc <- lapply(grep("group", props$encode, invert = TRUE), function(i) {
    if (props$encode[i] != "group") {
      encode_prop(props[i, ], scale=scales[props[i, 'encode']])
    }
  })
  names(enc) <- grep("group", props$encode, invert = TRUE, value=TRUE)

  # e <- vega_encode(update = list(
  #   y = list(scale="y", value=0),
  #   y2 = encode_prop(props[props$encode=='y', ], scale='y'),
  #   x = encode_prop(props[props$encode=='x', ], scale="x"),
  #   width = list(scale="x", band=props[props$encode=='width', 'field'])
  # ))
  # Dynamically defining the encodes has the issue that it's possible it will in herit contradictory encodes. May need to filter enodes prior to running.
  e <- vega_encode(update=enc)

  # add the mark
  if ("group" %in% props$encode) {
    # add group mark to visualization
    g <- vega_mark(type="group", from = list(facet=list(name=paste0(from, "_facet"), data=from, groupby=props[props$encode == "group", 'field'])), name=paste0("group_mark_", id))
    g <- add_group_mark(g, type="line", from=I(list(data=paste0(from, "_facet"))), encode=e, name=paste0("mark_", id))
    vis <- add_mark_(vis, g)
  } else {
    vis <- add_mark_(vis, type="line", from = list(data=from), encode=e, name=paste0("mark_", id))
  }

  # add x scale/axis
  vis <- add_scale_(vis, name="x", type="point", range="width",
                    domain=list(data=from, field=props[props$encode=='x', 'field'], sort=arrange),
                    round=TRUE, paddingInner=0.1, paddingOuter=0)
  vis <- add_axis_(vis, scale="x", orient="bottom", title=props[props$encode=='x', 'field'])

  # add y scale/axis
  vis <- add_scale_(vis, name="y", type="linear", range="height",
                    domain=list(data=from, field=props[props$encode=='y', 'field']),
                    round=TRUE)
  vis <- add_axis_(vis, scale="y", orient="left", title=props[props$encode=='y', 'field'])

  vis

}

# TODO:
# - add color
# - fix data reference
# - fix 'group' property not being correctly filtered
# - x & y scales seem to be switched (no clue why)
# - interpolate value needs to be 'linear', not a number (hopefully fixed with strings-as-factors)
# {
#   "name": "color",
#   "type": "ordinal",
#   "range": "category",
#   "domain": {"data": ".", "field": "enum"}
# }
