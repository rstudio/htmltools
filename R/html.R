
#' @export
html <- function(html, dependencies = NULL) {
  structure(
    html,
    dependencies = dependencies,
    html = TRUE,
    class = c("html", "character")
  )
}

#' @export
as_html = function(x, ...) {
  UseMethod('as_html', x)
}

#' @export
as_html.default = function(x, ...) {
  html(html_escape(as.character(x)))
}




