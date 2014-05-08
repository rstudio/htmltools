#' Define an HTML dependency
#'
#' Define an HTML dependency (e.g. CSS or Javascript and related library). HTML
#' dependency definitions are required for \code{\link{html_output}} that
#' require CSS or JavaScript within the document head to render correctly.
#'
#' @param name Library name
#' @param version Library version
#' @param path Full path to library
#' @param meta Named list of meta tags to insert into document head
#' @param script Script(s) to include within the document head (should be
#'   specified relative to the \code{path} parameter).
#' @param stylesheet Stylesheet(s) to include within the document (should be
#'   specified relative to the \code{path} parameter).
#' @param head Arbitrary lines of HTML to insert into the document head
#'
#' @return An object that can be included in the list of dependencies passed to
#'   \code{\link{html_print}} or \code{\link{html_knit_print}}.
#'
#' @details See the documentation on
#'   \href{http://rmarkdown.rstudio.com/developer_html_widgets.html}{R
#'   Markdown HTML Widgets} for examples and additional details.
#'
#' @export
#' @examples
#' d3 <- html_dependency(name = "d3", version = "3.4.1", path = "lib/d3",
#'   script = "d3.min.js")
#' cat(format(d3), "\n")
#'
html_dependency <- function(name,
                            version,
                            path,
                            meta = NULL,
                            script = NULL,
                            stylesheet = NULL,
                            head = NULL) {
  structure(class = "html_dependency", list(
    name = name,
    version = version,
    path = path,
    meta = meta,
    script = script,
    stylesheet = stylesheet,
    head = head
  ))
}

#' @export
format.html_dependency <- function(x, ...) {
  html <- list()

  # add meta content
  if (length(x$meta) > 0) {
    html$meta <- paste(
      "<meta name=\"", html_escape(names(x$meta)), "\" content=\"",
      html_escape(x$meta), "\" />",
      sep = ""
    )
  }

  # add stylesheets
  if (length(x$stylesheet) > 0) {
    html$stylesheet <- paste(
      "<link href=\"", html_escape(file.path(x$path, x$stylesheet)),
      "\" rel=\"stylesheet\" />",
      sep = ""
    )
  }

  # add scripts
  if (length(x$script) > 0) {
    html$script <- paste(
      "<script src=\"", file.path(x$path, x$script), "\"></script>",
      sep = ""
    )
  }

  # add raw head content
  html$head <- x$head

  unlist(html)
}

#' Copy dependencies to directory
#'
#' @param dependencies List of \code{\link{html_dependency}}s.
#' @param src Base directory to use for location of files.
#' @param dest Destination directory
#' @export
#' @examples
#' \dontrun{
#' d3 <- html_dependency(name = "d3", version = "3.4.1", path = "lib/d3",
#'   script = "d3.min.js")
#' copy_deps(list(d3), system.file("www", package = "ggvis"), "~/Desktop")
#' }
copy_deps <- function(dependencies, src = ".", dest = NULL) {
  stopifnot(is.list(dependencies))

  if (is.null(dest)) return()

  if (!file.exists(dest))
    dir.create(dest)

  paths <- unlist(lapply(dependencies, "[[", "path"))
  src_paths <- file.path(src, paths)
  dest_paths <- file.path(dest, dirname(paths))

  # Ugly code to get file.copy + recursive = TRUE to work
  dir.create(dest_paths, showWarnings = FALSE)
  Map(file.copy, src_paths, dest_paths, recursive = TRUE)
}
