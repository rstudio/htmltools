
#' Implementation of the knit_print method for HTML
#'
#' Convenience method that provides an implementation of the
#' \code{\link[knitr:knit_print]{knit_print}} method for HTML content.
#'
#' @param html Object of class "html" to knit print
#'
#' @details HTML content that is inlined into R Markdown documents should not
#'   indent HTML tags and content (because that may cause content indented at 4
#'   spaces to be treated as a code block by pandoc).
#'
#'   See the documentation on
#'   \href{http://rmarkdown.rstudio.com/developer_html_widgets.html}{R Markdown
#'   HTML Widgets} for examples and additional details.
#'
#' @export
html_knit_print <- function(html) {
  structure(class = "knit_asis",
    html,
    knit_meta = attr(html, "dependencies")
  )
}
