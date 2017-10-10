#' Add a vega signal
#'
#' Signals are dynamic variables that parameterize a visualization and can drive interactive behaviors. Signals can be used throughout a Vega specification, for example to define a mark property or data transform parameter.

#' Signal values are reactive: they can update in response to input event streams, external API calls, or changes to upstream signals. Event streams capture and sequence input events, such as mousedown or touchmove. When an event occurs, signals with associated event handlers are re-evaluated in their specification order. Updated signal values then propagate to the rest of the specification, and the visualization is re-rendered automatically.
#'
#' @param name String Required. A unique name for the signal. Signal names should be valid JavaScript identifiers: they should contain only alphanumeric characters (or “$”, or “_”) and may not start with a digit. Reserved keywords that may not be used as signal names are "datum", "event", and "parent".
#' @param bind Bind Binds the signal to an external input element such as a slider, selection list or radio button group.
#' @param description String A text description of the signal, useful for inline documentation.
#' @param on Handler[] An array of event stream handlers for updating the signal value in response to input events.
#' @param push	String	Required. To indicate an update to a signal defined in an outer scope, the push property must be set to "outer".
#' @param update Expression An update expression for the value of the signal. This expression may include other signals, in which case the signal will automatically update in response to upstream signal changes, so long as the react property is not false.
#' @param react Boolean A boolean flag (default true) indicating if the update expression should be automatically re-evaluated when any upstream signal dependencies update. If false, the update expression will only be run upon initialization.
#' @param value Any The initial value of the signal (default undefined).
#' @return a vega signal object
#' @export
vega_signal <- function (
  name=NULL,
  bind=NULL,
  description=NULL,
  on=NULL,
  push=NULL,
  update=NULL,
  react=TRUE,
  value=NULL
) {
  if (is.null(name)) {
    name <- past0("signal_", rand_id())
    message(paste0("Signal name is ", name, "."))
  }

  args <- list(name=name, bind=bind, description=description, on=on, push=push,
               update=update, react=react, value=value)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_signal(args, error=TRUE)

  args
}

#' return a vega event handler binding
#'
#' An event handler object includes an event stream definition indicating which events to respond to, and either an update expression for setting a new signal value, or an encode set for updating the mark being interacted with.
#'
#' This signal definition increments its value upon mouseover of rect items:
#'
#' {
#'   "name": "count",
#'   "value": 0,
#'   "on": [
#'     {"events": "rect:mouseover", "update": "count + 1"}
#'   ]
#' }
#' This signal definition invokes a custom encoding set upon mousedown and mouseup on mark items. The mark definition must include properties named "select" and "release" under the mark "encode" property.
#'
#' {
#'   "name": "clickEncode",
#'   "on": [
#'     {"events": "*:mousedown", "encode": "select"},
#'     {"events": "*:mouseup", "encode": "release"}
#'   ]
#' }
#'
#' @param events EventStream Required. The events to respond to.
#' @param update Expression An expression that is evaluated when events occur, the result then becomes the new signal value. This property is required if encode is not specified.
#' @param encode String The name of a mark property encoding set to re-evaluate for the the mark item that was the source of the input event. This property is required if update is not specified.
#' @param force Boolean A boolean flag (default false) indicating whether or not updates that do not change the signal value should propagate. For example, if set to true and an input stream update sets the signal to its current value, downstream signals will still be notified of an update.
#' @return a vega event object
#' @export
vega_event <- function(
  events,
  update=NULL,
  encode=NULL,
  force=FALSE
) {
  args <- list(input, element, debounce)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_event(args, error=TRUE)

  args
}

#' return a vega input binding object
#'
#' The bind property binds a signal to an input element defined outside of the visualization. Vega will generate new HTML form elements and set up a two-way binding: changes to the input element will update the signal, and vice versa. Vega includes dedicate support for checkbox (single boolean value), radio (group of radio buttons), select (drop-down menu), and range (slider) input types.
#'
#'
#' @section Radio and Select Input Properties:
#'
#' \tabular{rrl}{
#' Property \tab Type \tab Description\cr
#' options \tab Array \tab Required. An array of options to select from.\cr
#' debounce \tab Number \tab If defined, delays event handling until the specified milliseconds have elapsed since the last event was fired.\cr
#'}
#'
#' @section Range Input Properties:
#'
#' \tabular{rrl}{
#' Property \tab Type \tab Description\cr
#' max \tab Number \tab For range inputs, sets the maximum slider value. Defaults to the larger of the signal value and 100.\cr
#' min \tab Number \tab For range inputs, sets the minimum slider value. Defaults to the smaller of the signal value and 0.\cr
#' step \tab Number \tab For range inputs, sets the minimum slider increment. If undefined, the step size will be automatically determined based on the min and max values.\cr
#' debounce \tab Number \tab If defined, delays event handling until the specified milliseconds have elapsed since the last event was fired.\cr
#' }
#'
#' @section Other Input Types:
#'
#' In addition, any valid HTML input type may be used as the value of the type property. Examples include "text" (for single-line text entry), "color" (for a color picker), and "date" (for entering year, month and day). In these cases, any extra properties defined (e.g., placeholder for "text" input) will be added as attributes of the generated HTML form element.
#'
#' @param input String Required. The type of input element to use. The valid values are checkbox, radio, range, select, and any other legal HTML form input type.
#' @param element String An optional CSS selector string indicating the parent element to which the input element should be added. By default, all input elements are added within the parent container of the Vega view.
#' @param debounce Number If defined, delays event handling until the specified milliseconds have elapsed since the last event was fired.
#' @return a vega input object
#' @export
vega_input <- function(
  input,
  element=NULL,
  debounce=NULL
) {
  args <- list(input, element, debounce)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_input(args, error=TRUE)

  args
}
