
#' @export
as_html = function(x, ...) {
  UseMethod('as_html', x)
}

#' @export
as_html.default = function(x, ...) {
  structure(
    html_escape(as.character(x)),
    dependencies = list(),
    html = TRUE,
    class = c("html", "character")
  )
}


