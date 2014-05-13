

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
html_dependency <- function(name,
                            version,
                            src,
                            meta = NULL,
                            script = NULL,
                            stylesheet = NULL,
                            head = NULL) {
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
attach_dependency <- function(x, dependency) {
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

# Given a list of dependencies, choose the latest versions and return them as a
# named list in the correct order.
get_newest_dependencies <- function(dependencies) {
  result <- list()
  for (dep in dependencies) {
    if (!is.null(dep)) {
      other <- result[[dep$name]]
      if (is.null(other) || compareVersion(dep$version, other$version) > 0) {
        # Note that if the dep was already in the result list, then this
        # assignment preserves its position in the list
        result[[dep$name]] <- dep
      }
    }
  }
  return(result)
}

# Remove `remove` from `dependencies` if the name matches.
# dependencies is a named list of dependencies.
# remove is a named list of dependencies that take priority.
# If warnOnConflict, then warn when a dependency is being removed because of an
# older version already being loaded.
remove_dependencies <- function(dependencies, remove, warnOnConflict = TRUE) {
  matches <- names(dependencies) %in% names(remove)
  if (warnOnConflict) {
    for (depname in names(dependencies)[matches]) {
      loser <- dependencies[[depname]]
      winner <- remove[[depname]]
      if (compareVersion(loser$version, winner$version) > 0) {
        warning(sprintf(paste("The dependency %s %s conflicts with",
          "version %s"), loser$name, loser$version, winner$version
        ))
      }
    }
  }

  # Return only deps that weren't in remove
  return(dependencies[!matches])
}

url_encode <- function(x) {
  gsub("%2[Ff]", "/", URLencode(x, TRUE))
}

# Given a list of HTML dependencies produce a character representation
# suitable for inclusion within the head of an HTML document
html_dependencies_as_character <- function(dependencies, lib_dir = NULL) {

  html <- c()

  for (dep in dependencies) {

    dir <- dir_path(dep)

    # copy library files if necessary
    if (!is.null(lib_dir) && !is.null(dir)) {

      if (!file.exists(lib_dir))
        dir.create(lib_dir)

      target_dir <- file.path(lib_dir, paste(dep$name, dep$version, sep = "-"))
      if (!file.exists(target_dir)) {
        file.copy(from = dir, to = lib_dir, recursive = TRUE)
      }

      dir <- file.path(basename(lib_dir), basename(target_dir))
    }

    srcpath <- if (!is.null(dir)) {
      # URL encode, then unencode /
      url_encode(dir)
    } else {
      # Assume that href is already URL encoded
      href_path(dep)
    }

    # Drop trailing /
    srcpath <- sub("/$", "\\1", srcpath)

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
        "<link href=\"",
        html_escape(file.path(srcpath, url_encode(dep$stylesheet))),
        "\" rel=\"stylesheet\" />",
        sep = ""
      ))
    }

    # add scripts
    if (length(dep$script) > 0) {
      html <- c(html, paste(
        "<script src=\"",
        html_escape(file.path(srcpath, url_encode(dep$script))),
        "\"></script>",
        sep = ""
      ))
    }

    # add raw head content
    html <- c(html, dep$head)
  }

  html
}

# html_dependencies_as_character(list(
#   html_dependency("foo", "1.0",
#     c(href="http://foo.com/bar%20baz/"),
#     stylesheet="x y z.css"
#   )
# ))
# [1] "<link href=\"http://foo.com/bar%20baz/x%20y%20z.css\" rel=\"stylesheet\" />"

# html_dependencies_as_character(list(
#   html_dependency("foo", "1.0",
#     c(href="http://foo.com/bar%20baz"),
#     stylesheet="x y z.css"
#   )
# ))
# [1] "<link href=\"http://foo.com/bar%20baz/x%20y%20z.css\" rel=\"stylesheet\" />"

# html_dependencies_as_character(list(
#   html_dependency("foo", "1.0",
#     "foo bar/baz",
#     stylesheet="x y z.css"
#   )
# ))
# [1] "<link href=\"foo%20bar/baz/x%20y%20z.css\" rel=\"stylesheet\" />"

# html_dependencies_as_character(list(
#   html_dependency("foo", "1.0",
#     "foo bar/baz/",
#     stylesheet="x y z.css"
#   )
# ))
# [1] "<link href=\"foo%20bar/baz/x%20y%20z.css\" rel=\"stylesheet\" />"
