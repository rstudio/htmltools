
#' Implementation of the print method for HTML
#'
#' Convenience method that provides an implementation of the
#' \code{\link[base:print]{print}} method for HTML content.
#'
#' @param html HTML content to print
#' @param dependencies List of HTML dependencies created using
#' the \code{\link{html_dependency}} function.
#'
#' @export
html_print <- function(html, dependencies) {

  # define temporary directory for output
  www_dir <- tempfile("viewhtml")
  dir.create(www_dir)

  # we want to re-use the html_dependencies_as_string function and also
  # ensure that the paths to dependencies are relative to the base
  # directory where the temp index.html is being built. to affect
  # this we setwd to the www_dir for the duration of this function
  # to a relative path
  oldwd <- setwd(www_dir)
  on.exit(setwd(oldwd), add = TRUE)

  # build the web-page
  html <- c("<!DOCTYPE html>",
            "<html>",
            "<head>",
            "<meta charset=\"utf-8\"/>",
            html_dependencies_as_character(dependencies, "lib"),
            "</head>",
            "<body>",
            html,
            "</body>",
            "</html>")

  # write it
  index_html <- file.path(www_dir, "index.html")
  writeLines(html, index_html, useBytes = TRUE)

  # show it
  viewer <- getOption("viewer")
  if (!is.null(viewer))
    viewer(index_html)
  else
    utils::browseURL(index_html)

  invisible(NULL)
}
