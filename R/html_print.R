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
#' @param background Background color for web page
#' @param viewer A function to be called with the URL or path to the generated
#'   HTML page. Can be \code{NULL}, in which case no viewer will be invoked.
#'
#' @return Invisibly returns the URL or path of the generated HTML page.
#'
#' @export
html_print <- function(html, background = "white", viewer = getOption("viewer", utils::browseURL)) {

  # define temporary directory for output
  www_dir <- tempfile("viewhtml")
  dir.create(www_dir)

  # define output file
  index_html <- file.path(www_dir, "index.html")

  # save file
  save_html(html, file = index_html, background = background, libdir = "lib")

  # show it
  if (!is.null(viewer))
    viewer(index_html)

  invisible(index_html)
}

#' Save an HTML object to a file
#'
#' Save the specified HTML object to a file, copying all of it's
#' dependencies to the directory specified via \code{libdir}.
#'
#' @param html HTML content to print
#' @param background Background color for web page
#' @param file File to write content to
#' @param libdir Directory to copy dependenies to
#'
#' @export
save_html <- function(html, file, background = "white", libdir = "lib") {

  # ensure that the paths to dependencies are relative to the base
  # directory where the webpage is being built.
  dir <- dirname(file)
  oldwd <- setwd(dir)
  on.exit(setwd(oldwd), add = TRUE)

  rendered <- renderTags(html)

  deps <- lapply(rendered$dependencies, function(dep) {
    dep <- copyDependencyToDir(dep, libdir, FALSE)
    dep <- makeDependencyRelative(dep, dir, FALSE)
    dep
  })

  # build the web-page
  html <- c("<!DOCTYPE html>",
            "<html>",
            "<head>",
            "<meta charset=\"utf-8\"/>",
            sprintf("<style>body{background-color:%s;}</style>", htmlEscape(background)),
            renderDependencies(deps, c("href", "file")),
            rendered$head,
            "</head>",
            "<body>",
            rendered$html,
            "</body>",
            "</html>")

  if (is.character(file)) {
    # Write to file in binary mode, so \r\n in input doesn't become \r\r\n
    con <- base::file(file, open = "w+b")
    on.exit(close(con))
  } else {
    con <- file
  }

  # write it
  writeLines(html, con, useBytes = TRUE)
}


