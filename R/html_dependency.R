

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


# Given a list of HTML dependencies produce a character representation
# suitable for inclusion within the head of an HTML document
html_dependencies_as_character <- function(dependencies, lib_dir = NULL) {

  html <- c()

  for (dep in dependencies) {

    # copy library files if necessary
    if (!is.null(lib_dir)) {

      if (!file.exists(lib_dir))
        dir.create(lib_dir)

      target_dir <- file.path(lib_dir, basename(dep$path))
      if (!file.exists(target_dir))
        file.copy(from = dep$path, to = lib_dir, recursive = TRUE)

      dep$path <- file.path(basename(lib_dir), basename(target_dir))
    }

    # add meta content
    if (length(dep$meta) > 0) {
      html <- c(html, paste(
        "<meta name=\"", html_escape(names(dep$meta)), "\" content=\"",
        html_escape(dep$meta), "\" />",
        sep = ""
      ))
    }

    # add stylesheets
    if (length(dep$stylesheet) > 0) {
      html <- c(html, paste(
        "<link href=\"", html_escape(file.path(dep$path, dep$stylesheet)),
        "\" rel=\"stylesheet\" />",
        sep = ""
      ))
    }

    # add scripts
    if (length(dep$script) > 0) {
      html <- c(html, paste(
        "<script src=\"", file.path(dep$path, dep$script), "\"></script>",
        sep = ""
      ))
    }

    # add raw head content
    html <- c(html, dep$head)
  }

  html
}

