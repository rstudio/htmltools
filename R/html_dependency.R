

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
    version = as.character(version),
    src = src,
    meta = meta,
    script = script,
    stylesheet = stylesheet,
    head = head
  ))
}

#' HTML dependency metadata
#'
#' Gets or sets the HTML dependencies associated with an object (such as a tag).
#'
#' \code{attachDependencies} provides an alternate syntax for setting
#' dependencies and is essentially equivalent to
#' \code{local(\{htmlDependencies(x) <- value; x\})}.
#'
#' @param x An object which has (or should have) HTML dependencies.
#' @param value An HTML dependency, or a list of HTML dependencies.
#'
#' @export
htmlDependencies <- function(x) {
  attr(x, "html_dependencies", TRUE)
}

#' @rdname htmlDependencies
#' @export
`htmlDependencies<-` <- function(x, value) {
  if (inherits(value, "html_dependency"))
    value <- list(value)
  attr(x, "html_dependencies") <- value
  x
}

#' @rdname htmlDependencies
#' @export
attachDependencies <- function(x, value) {
  htmlDependencies(x) <- value
  return(x)
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

#' Encode a URL path
#'
#' Encode characters in a URL path. This is the same as
#' \code{\link[utils]{URLencode}} with \code{reserved = TRUE} except that
#' \code{/} is preserved.
#'
#' @param x A character string.
#' @export
urlEncodePath <- function(x) {
  gsub("%2[Ff]", "/", URLencode(x, TRUE))
}

#' Copy an HTML dependency to a directory
#'
#' Copies an HTML dependency to a subdirectory of the given directory. The
#' subdirectory name will be \emph{name}-\emph{version} (for example,
#' "outputDir/jquery-1.11.0").
#'
#' In order for disk-based dependencies to work with static HTML files, it's
#' generally necessary to copy them to either the directory of the referencing
#' HTML file, or to a subdirectory of that directory. This function makes it
#' easier to perform that copy.
#'
#' If a subdirectory named \emph{name}-\emph{version} already exists in
#' \code{outputDir}, then copying is not performed; the existing contents are
#' assumed to be up-to-date.
#'
#' @param dependency A single HTML dependency object.
#' @param outputDir The directory in which a subdirectory should be created for
#'   this dependency.
#' @param mustWork If \code{TRUE} and \code{dependency} does not point to a
#'   directory on disk (but rather a URL location), an error is raised. If
#'   \code{FALSE} then non-disk dependencies are returned without modification.
#'
#' @return The dependency with its \code{src} value updated to the new
#'   location's absolute path.
#'
#' @seealso \code{\link{makeDependencyRelative}} can be used with the returned
#'   value to make the path relative to a specific directory.
#'
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
    dir.create(target_dir)

    srcfiles <- file.path(dir, list.files(dir))
    destfiles <- file.path(target_dir, list.files(dir))
    isdir <- file.info(srcfiles)$isdir
    destfiles <- ifelse(isdir, dirname(destfiles), destfiles)

    mapply(function(from, to, recursive) {
      if (recursive && !file.exists(to))
        dir.create(to)
      file.copy(from, to, recursive=recursive)
    }, srcfiles, destfiles, isdir)
  }

  dependency$src$file <- normalizePath(target_dir, "/", TRUE)

  dependency
}

# given a directory and a file, return a relative path from the directory to the
# file, or the unmodified file path if the file does not appear to be in the
# directory
relativeTo <- function(dir, file) {
  # ensure directory ends with a /
  if (!identical(substr(dir, nchar(dir), nchar(dir)), "/")) {
    dir <- paste(dir, "/", sep="")
  }

  # if the file is prefixed with the directory, return a relative path
  if (identical(substr(file, 1, nchar(dir)), dir))
    return(substr(file, nchar(dir) + 1, nchar(file)))
  else
    stop("The path ", file, " does not appear to be a descendant of ", dir)
}

#' Make an absolute dependency relative
#'
#' Change a dependency's absolute path to be relative to one of its parent
#' directories.
#'
#' @param dependency A single HTML dependency with an absolute path.
#' @param basepath The path to the directory that \code{dependency} should be
#'   made relative to.
#' @param mustWork If \code{TRUE} and \code{dependency} does not point to a
#'   directory on disk (but rather a URL location), an error is raised. If
#'   \code{FALSE} then non-disk dependencies are returned without modification.
#'
#' @return The dependency with its \code{src} value updated to the new
#' location's relative path.
#'
#' If \code{baspath} did not appear to be a parent directory of the dependency's
#' directory, an error is raised (regardless of the value of \code{mustWork}).
#'
#' @seealso \code{\link{copyDependencyToDir}}
#'
#' @export
makeDependencyRelative <- function(dependency, basepath, mustWork = TRUE) {
  basepath <- normalizePath(basepath, "/", TRUE)
  dir <- dependency$src$file
  if (is.null(dir)) {
    if (!mustWork)
      return(dependency)
    else
      stop("Could not make dependency ", dependency$name, " ",
           dependency$version, " relative; it is not file-based")
  }

  dependency$src <- c(file=relativeTo(basepath, dir))

  dependency
}

#' Create HTML for dependencies
#'
#' Create the appropriate HTML markup for including these dependencies in an
#' HTML document.
#'
#' @param dependencies A list of \code{htmlDependency} objects.
#' @param srcType The type of src paths to use; valid values are \code{file} or
#'   \code{href}.
#' @param encodeFunc The function to use to encode the path part of a URL. The
#'   default should generally be used.
#' @param hrefFilter A function used to transform the final, encoded URLs of
#'   script and stylsheet files. The default should generally be used.
#'
#' @export
renderDependencies <- function(dependencies,
  srcType = c("file", "href"),
  encodeFunc = urlEncodePath,
  hrefFilter = identity) {

  srcType <- match.arg(srcType)

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
        htmlEscape(hrefFilter(file.path(srcpath, encodeFunc(dep$stylesheet)))),
        "\" rel=\"stylesheet\" />",
        sep = ""
      ))
    }

    # add scripts
    if (length(dep$script) > 0) {
      html <- c(html, paste(
        "<script src=\"",
        htmlEscape(hrefFilter(file.path(srcpath, encodeFunc(dep$script)))),
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
