# vega_validate <- jsonvalidate::json_validator(
#   readr::read_file(
#     system.file("www/lib/vega/v3.0.json", package="ggvis")
#   )
# e)

#' Validae that the vis includes a valid vega object
#'
#' @param vis a ggvis vis
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega <- function(vis, error=FALSE) {
  spec <- dump_spec(vis)
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE, error=error)
}

#' Validate if the object is a vega title object
#' @param obj an object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_title <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(title = obj), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^title", err$field)) # judge only title errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^title", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors')) # raise error rather than returning as attr
  ret
}

#' Validate if the object is a vega config object
#' @param obj a config object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_config <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(config = obj), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^config", err$field)) # judge only config errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^config", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors')) # raise error rather than returning as attr
  ret
}

#' Validate if the object is a vega mark object
#' @param obj a mark object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_mark <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(mark = list(obj)), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^mark", err$field)) # judge only mark errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^mark", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors'))  # raise error rather than returning as attr

  ret
}

#' Validate if the object is a vega encode object
#' @param obj an encode object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_encode <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(mark = list(list(encode=obj))), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  # TODO: The validation line below doesn't seem to be validating (or maybe the schema is too lax) - 171005
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("encode", err$field)) # judge only mark errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^encode", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors'))  # raise error rather than returning as attr

  ret
}

#' Validate if the object is a vega scale object
#' @param obj an scale object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_scale <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(scales = list(obj)), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^scales", err$field)) # judge only mark errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^scales", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors'))  # raise error rather than returning as attr

  ret
}

#' Validate if the object is a vega axis object
#' @param obj an axis object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_axis <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(axes = list(obj)), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^axes", err$field)) # judge only axis errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^axes", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors'))  # raise error rather than returning as attr

  ret
}


#' Validate if the object is a vega signal object
#' @param obj an signal object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_signal <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(signals = list(obj)), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^signals", err$field)) # judge only signals errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^signals", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors'))  # raise error rather than returning as attr

  ret
}


#' Validate if the object is a vega event object
#' @param obj an event object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_event <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(signals = list(name="test", on=list(obj))), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^signals[.]on", err$field)) # judge only event errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^signals[.]on", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors'))  # raise error rather than returning as attr

  ret
}


#' Validate if the object is a vega input object
#' @param obj an input object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_input <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(signals = list(name="test", bind=list(obj))), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^signals[.]bind", err$field)) # judge only input errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^signals[.]bind", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors'))  # raise error rather than returning as attr

  ret
}


#' Validate if the object is a vega legend object
#' @param obj a vega legend object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_legend <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(legends = list(obj)), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^legends", err$field)) # judge only legend errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^legends", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors'))  # raise error rather than returning as attr

  ret
}


#' Validate if the object is a vega projection object
#' @param obj a vega projection object
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_projection <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(projections = list(obj)), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^projections", err$field)) # judge only projection errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^projections", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors'))  # raise error rather than returning as attr

  ret
}


#' Validate if the object is a vega transform object
#' @param obj a vega transform object of any type
#' @param error Throw an error on parse failure? If TRUE, then the function returns NULL
#'   on success (i.e., call only for the side-effect of an error on failure, like
#'   stopifnot).
#' @return logical
#' @export
is.vega_transform <- function(obj, error=FALSE) {
  spec <- jsonlite::toJSON(list(data = list(name="test", transform=list(obj))), auto_unbox = TRUE, force = TRUE, null = "null") # easier to build full object than, subset spec
  schema <- readr::read_file( system.file("www/lib/vega/v3.0.json", package="ggvis") )
  err <- attr(jsonvalidate::json_validate(spec, schema, verbose=TRUE, greedy=TRUE), 'errors') # get errors
  ret <- !any(grepl("^data[.]transform", err$field)) # judge only transform errors
  if (!ret) attr(ret, 'errors') <- err[grepl("^data[.]transform", err$field), ] # add errors back in
  if (error) error_helper(attr(ret, 'errors'))  # raise error rather than returning as attr

  ret
}



#' parse the errors and raise them if they exist
#'
#' from jsonvalidate::json_validator
#'
#' @param errors dataframe of jsonvalidate errors
#' @return Null (if not stopped)
error_helper <- function(errors) {
  # from jsonvalidate::json_validator
  if (is.null(errors)) {
    return(NULL)
  }
  else {
    n <- nrow(errors)
    msg <- sprintf("%s %s validating json:\\n%s",
      n, ngettext(n, "error", "errors"), paste(sprintf("\\t- %s: %s",
        errors[[1]], errors[[2]]), collapse = "\\n"))
    stop(msg, call. = FALSE)
  }
}
