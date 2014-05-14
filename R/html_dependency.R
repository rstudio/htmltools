

#' Define an HTML dependency
#'
#' Define an HTML dependency (e.g. CSS or Javascript and related library). HTML
#' dependency definitions are required for \code{\link{html_output}} that
#' require CSS or JavaScript within the document head to render correctly.
#'
#' @param name Library name
#' @param version Library version
#' @param src Unnamed single-element character vector indicating the full path
#'   of the library directory. Alternatively, a named character string with one
#'   or more elements, indicating different places to find the library; see
#'   Details.
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
#' @details Each dependency can be located on the filesystem, at a relative or
#'   absolute URL, or both. The location types are indicated using the names of
#'   the \code{src} character vector: \code{file} for filesystem directory,
#'   \code{href} for URL. For example, a dependency that was both on disk and
#'   at a URL might use \code{src = c(file=filepath, href=url)}.
#'
#'   See the documentation on
#'   \href{http://rmarkdown.rstudio.com/developer_html_widgets.html}{R Markdown
#'   HTML Widgets} for examples and additional details.
#'
#' @export
htmlDependency <- function(name,
                           version,
                           src,
                           meta = NULL,
                           script = NULL,
                           stylesheet = NULL,
                           head = NULL) {
  srcNames <- names(src)
  if (is.null(srcNames))
    srcNames <- rep.int("", length(src))
  srcNames[!nzchar(srcNames)] <- "file"
  names(src) <- srcNames
  src <- as.list(src)

  structure(class = "html_dependency", list(
    name = name,
    version = version,
    src = src,
    meta = meta,
    script = script,
    stylesheet = stylesheet,
    head = head
  ))
}

#' @export
attachDependency <- function(x, dependency) {
  if (inherits(dependency, "html_dependency"))
    dependency <- list(dependency)
  structure(x, html_dependency = dependency)
}

dir_path <- function(dependency) {
  if ("dir" %in% names(dependency$src))
    return(dependency$src[["dir"]])

  if (length(names(dependency$src)) == 0 || all(!nzchar(dependency$src)))
    return(dependency$src[[1]])

  return(NULL)
}

href_path <- function(dependency) {
  if ("href" %in% names(dependency$src))
    return(dependency$src[["href"]])
  else
    return(NULL)
}

#' @export
urlEncode <- function(x) {
  gsub("%2[Ff]", "/", URLencode(x, TRUE))
}

#' @export
copyDependencyToDir <- function(dependency, outputDir, mustWork = TRUE) {

  dir <- dependency$src$file

  if (is.null(dir)) {
    if (mustWork) {
      stop("Dependency ", dependency$name, " ", dependency$version,
           " is not disk-based")
    } else {
      return(dependency)
    }
  }

  if (!file.exists(outputDir))
    dir.create(outputDir)

  target_dir <- file.path(outputDir,
    paste(dependency$name, dependency$version, sep = "-"))

  if (!file.exists(target_dir)) {
    file.copy(from = dir, to = outputDir, recursive = TRUE)
  }

  dir <- file.path(basename(outputDir), basename(target_dir))
  dependency$src$file <- dir

  dependency
}

# Given a list of HTML dependencies produce a character representation
# suitable for inclusion within the head of an HTML document
#' @export
renderDependencies <- function(dependencies,
  srcType = c("file", "href"),
  encodeFunc = urlEncode,
  pathFilter = identity) {

  html <- c()

  for (dep in dependencies) {

    dir <- dep$src[[srcType]]

    if (is.null(dir)) {
      stop("Dependency ", dep$name, " ", dep$version,
        " does not have a usable source")
    }

    srcpath <- if (srcType == "file") {
      encodeFunc(dir)
    } else {
      # Assume that href is already URL encoded
      href_path(dep)
    }

    # Drop trailing /
    srcpath <- sub("/$", "\\1", srcpath)

    # add meta content
    if (length(dep$meta) > 0) {
      html <- c(html, paste(
        "<meta name=\"", htmlEscape(names(dep$meta)), "\" content=\"",
        htmlEscape(dep$meta), "\" />",
        sep = ""
      ))
    }

    # add stylesheets
    if (length(dep$stylesheet) > 0) {
      html <- c(html, paste(
        "<link href=\"",
        htmlEscape(pathFilter(file.path(srcpath, encodeFunc(dep$stylesheet)))),
        "\" rel=\"stylesheet\" />",
        sep = ""
      ))
    }

    # add scripts
    if (length(dep$script) > 0) {
      html <- c(html, paste(
        "<script src=\"",
        htmlEscape(pathFilter(file.path(srcpath, encodeFunc(dep$script)))),
        "\"></script>",
        sep = ""
      ))
    }

    # add raw head content
    html <- c(html, dep$head)
  }

  HTML(paste(html, collapse = "\n"))
}

# html_dependencies_as_character(list(
#   htmlDependency("foo", "1.0",
#     c(href="http://foo.com/bar%20baz/"),
#     stylesheet="x y z.css"
#   )
# ))
# <link href=\"http://foo.com/bar%20baz/x%20y%20z.css\" rel=\"stylesheet\" />

# html_dependencies_as_character(list(
#   htmlDependency("foo", "1.0",
#     c(href="http://foo.com/bar%20baz"),
#     stylesheet="x y z.css"
#   )
# ))
# <link href=\"http://foo.com/bar%20baz/x%20y%20z.css\" rel=\"stylesheet\" />

# html_dependencies_as_character(list(
#   htmlDependency("foo", "1.0",
#     "foo bar/baz",
#     stylesheet="x y z.css"
#   )
# ))
# <link href=\"foo%20bar/baz/x%20y%20z.css\" rel=\"stylesheet\" />

# html_dependencies_as_character(list(
#   htmlDependency("foo", "1.0",
#     "foo bar/baz/",
#     stylesheet="x y z.css"
#   )
# ))
# <link href=\"foo%20bar/baz/x%20y%20z.css\" rel=\"stylesheet\" />
