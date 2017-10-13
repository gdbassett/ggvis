#' create a vega countpattern transform object
#'
#' https://vega.github.io/vega/docs/transforms/countpattern/index.html
#' NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
#'
#'
#' The **countpattern** transform counts the number of occurrences of a text pattern, as defined by a regular expression. This transform will iterate through each data object and count all unique pattern matches found within the designated text _field_.
#'
#' Both the _pattern_ and _stopwords_ parameters below are not "raw" regular expression patterns – they are embedded in a string. As a result, take care to make sure you use proper escape characters as needed. For example, to match digits, use `"\\\\d"`, not `"\\d"`.
#'
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param field Field (required) The data field containing the text data.
#' @param pattern String A string containing a well-formatted [regular expression](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions), defining a pattern to match in the text. All unique pattern matches will be separately counted. The default value is `[\\w\']+`, which will match sequences containing word characters and apostrophes, but no other characters.
#' @param case String A lower- or upper-case transformation to apply prior to pattern matching. One of `lower`, `upper` or `mixed` (the default).
#' @param stopwords String A string containing a well-formatted [regular expression](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions), defining a pattern of text to ignore. For example, the value `"(foo|bar|baz)"` will treat the words `"foo"`, `"bar"` and `"baz"` as stopwords that should be ignored. The default value is the empty string (`""`), indicating no stop words.
#' @param as String[] The output fields for the text pattern and occurrence count. The default is `["text", "count"]`.
#' @return a transform object
#' @export
vega_countpattern_transform <- function(
  type,
  field=NULL,
  pattern=NULL,
  case=NULL,
  stopwords=NULL,
  as=NULL
) {
  args <- list(type=type, field=field, pattern=pattern, case=case, stopwords=stopwords, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
