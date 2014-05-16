#' Make an HTML object browsable
#'
#' By default, HTML objects display their HTML markup at the console when
#' printed. \code{browsable} can be used to make specific objects render as HTML
#' by default when printed at the console.
#'
#' You can override the default browsability of an HTML object by explicitly
#' passing \code{browse = TRUE} (or \code{FALSE}) to the \code{print} function.
#'
#' @param x The object to make browsable or not.
#' @param value Whether the object should be considered browsable.
#' @return \code{browsable} returns \code{x} with an extra attribute to indicate
#'   that the value is browsable.
#' @export
browsable <- function(x, value = TRUE) {
  attr(x, "browsable_html") <- if (isTRUE(value)) TRUE else NULL
  return(x)
}

#' @return \code{is.browsable} returns \code{TRUE} if the value is browsable, or
#'   \code{FALSE} if not.
#' @rdname browsable
#' @export
is.browsable <- function(x) {
  return(isTRUE(attr(x, "browsable_html", exact=TRUE)))
}

#' Implementation of the print method for HTML
#'
#' Convenience method that provides an implementation of the
#' \code{\link[base:print]{print}} method for HTML content.
#'
#' @param html HTML content to print
#' @param dependencies List of HTML dependencies created using
#' the \code{\link{htmlDependency}} function.
#'
#' @export
html_print <- function(html) {

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

  rendered <- renderTags(html)

  deps <- lapply(rendered$dependencies, function(dep) {
    dep <- copyDependencyToDir(dep, "lib", FALSE)
    dep <- makeDependencyRelative(dep, www_dir)
    dep
  })

  # build the web-page
  html <- c("<!DOCTYPE html>",
            "<html>",
            "<head>",
            "<meta charset=\"utf-8\"/>",
            renderDependencies(deps, "file"),
            rendered$head,
            "</head>",
            "<body>",
            rendered$html,
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
