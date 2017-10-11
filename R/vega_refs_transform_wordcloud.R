#' create a vega wordcloud transform object
#'
#' https://vega.github.io/vega/docs/transforms/wordcloud/index.html
#' 
#' The **wordcloud** transform computes a word cloud layout, similar to [Wordle](http://www.wordle.net/). The wordcloud transform is intended for visualizing words or phrases with the [text mark type](../../marks/text). This transform uses [Jason Davies' wordcloud implementation](https://www.jasondavies.com/wordcloud/).
#' 
#'
#' @param type All transforms require a type property, specifying the name of the transform.
#' @param font String|Expr The font family to use for a word.
#' @param fontStyle String|Expr The font style to use for a word.
#' @param fontWeight String|Expr The font weight to use for a word.
#' @param fontSize Number|Expr The font size in pixels to use for a word.
#' @param fontSizeRange Number[] The range of font sizes to use for the words. If the range is specified and the _fontSize_ is not a numeric constant, the _fontSize_ values will automatically be scaled to lie in the range according to a square root scale.
#' @param padding Number|Expr The padding in pixels to place around a word.
#' @param rotate Number|Expr The angle in degrees to use for a word.
#' @param text Field The data field with the word text.
#' @param spiral String The spiral layout method used to place words. One of `archimedean` (the default) or `rectangular`.
#' @param as String[] The output fields written by the transform. The default is `["x", "y", "font", "fontSize", "fontStyle", "fontWeight", "angle"]`
#' @return a transform object
#' @export
vega_wordcloud_transform <- function(
  type,
  font=NULL,
  fontStyle=NULL,
  fontWeight=NULL,
  fontSize=NULL,
  fontSizeRange=NULL,
  padding=NULL,
  rotate=NULL,
  text=NULL,
  spiral=NULL,
  as=NULL
) {
  args <- list(font=font, fontStyle=fontStyle, fontWeight=fontWeight, fontSize=fontSize, fontSizeRange=fontSizeRange, padding=padding, rotate=rotate, text=text, spiral=spiral, as=as)
  args <- args[!unlist(lapply(args, is.null))]

  is.vega_transform(args, error=TRUE)

  args
}
