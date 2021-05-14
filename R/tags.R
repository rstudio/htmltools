#' @import utils digest
NULL

# Like base::paste, but converts all string args to UTF-8 first.
paste8 <- function(..., sep = " ", collapse = NULL) {
  args <- c(
    lapply(list(...), enc2utf8),
    list(
      sep = if (is.null(sep)) sep else enc2utf8(sep),
      collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)
    )
  )

  do.call(paste, args)
}

# A special case of paste8 that employs paste0. Avoids the overhead of lapply.
concat8 <- function(...) {
  enc2utf8(paste0(...))
}

# Reusable function for registering a set of methods with S3 manually. The
# methods argument is a list of character vectors, each of which has the form
# c(package, genname, class).
registerMethods <- function(methods) {
  lapply(methods, function(method) {
    pkg <- method[[1]]
    generic <- method[[2]]
    class <- method[[3]]
    func <- get(paste(generic, class, sep="."))
    if (pkg %in% loadedNamespaces()) {
      registerS3method(generic, class, func, envir = asNamespace(pkg))
    }
    setHook(
      packageEvent(pkg, "onLoad"),
      function(...) {
        registerS3method(generic, class, func, envir = asNamespace(pkg))
      }
    )
  })
}

.onLoad <- function(...) {
  # htmltools provides methods for knitr::knit_print, but knitr isn't a Depends or
  # Imports of htmltools, only an Enhances. Therefore, the NAMESPACE file has to
  # declare it as an export, not an S3method. That means that R will only know to
  # use our methods if htmltools is actually attached, i.e., you have to use
  # library(htmltools) in a knitr document or else you'll get escaped HTML in your
  # document. This code snippet manually registers our methods with S3 once both
  # htmltools and knitr are loaded.
  registerMethods(list(
    # c(package, genname, class)
    c("knitr", "knit_print", "html"),
    c("knitr", "knit_print", "shiny.tag"),
    c("knitr", "knit_print", "shiny.tag.list")
  ))
}

depListToNamedDepList <- function(dependencies) {
  if (inherits(dependencies, "html_dependency"))
    dependencies <- list(dependencies)

  if (is.null(names(dependencies))) {
    names(dependencies) <- sapply(dependencies, `[[`, "name")
  }
  return(dependencies)
}

#' Resolve a list of dependencies
#'
#' Given a list of dependencies, removes any redundant dependencies (based on
#' name equality). If multiple versions of a dependency are found, the copy with
#' the latest version number is used.
#'
#' @param dependencies A list of [htmlDependency()] objects.
#' @param resolvePackageDir Whether to resolve the relative path to an absolute
#'   path via [system.file()] when the `package` attribute is
#'   present in a dependency object.
#' @return dependencies A list of [htmlDependency()] objects with
#'   redundancies removed.
#'
#' @export
resolveDependencies <- function(dependencies, resolvePackageDir = TRUE) {
  deps <- resolveFunctionalDependencies(dependencies)

  # Get names and numeric versions in vector/list form
  depnames <- vapply(deps, function(x) x$name, character(1))
  depvers <- numeric_version(vapply(deps, function(x) x$version, character(1)))

  # Get latest version of each dependency. `unique` uses the first occurrence of
  # each dependency name, which is important for inter-dependent libraries.
  return(lapply(unique(depnames), function(depname) {
    # Sort by depname equality, then by version. Since na.last=NA, all elements
    # whose names do not match will not be included in the sorted vector.
    sorted <- order(ifelse(depnames == depname, TRUE, NA), depvers,
      na.last = NA, decreasing = TRUE)
    # The first element in the list is the one with the largest version.
    dep <- deps[[sorted[[1]]]]
    if (resolvePackageDir && !is.null(dep$package)) {
      dir <- dep$src$file
      if (!is.null(dir)) dep$src$file <- system.file(dir, package = dep$package)
      dep$package <- NULL
    }
    dep
  }))
}

# Remove `remove` from `dependencies` if the name matches.
# dependencies is a named list of dependencies.
# remove is a named list of dependencies that take priority.
# If warnOnConflict, then warn when a dependency is being removed because of an
# older version already being loaded.

#' Subtract dependencies
#'
#' Remove a set of dependencies from another list of dependencies. The set of
#' dependencies to remove can be expressed as either a character vector or a
#' list; if the latter, a warning can be emitted if the version of the
#' dependency being removed is later than the version of the dependency object
#' that is causing the removal.
#'
#' @param dependencies A list of [htmlDependency()] objects from which
#'   dependencies should be removed.
#' @param remove A list of [htmlDependency()] objects indicating which
#'   dependencies should be removed, or a character vector indicating dependency
#'   names.
#' @param warnOnConflict If `TRUE`, a warning is emitted for each
#'   dependency that is removed if the corresponding dependency in `remove`
#'   has a lower version number. Has no effect if `remove` is provided as a
#'   character vector.
#'
#' @return A list of [htmlDependency()] objects that don't intersect
#'   with `remove`.
#'
#' @export
subtractDependencies <- function(dependencies, remove, warnOnConflict = TRUE) {
  dependencies <- resolveFunctionalDependencies(dependencies)
  depnames <- vapply(dependencies, function(x) x$name, character(1))

  if (is.character(remove)) {
    rmnames <- remove
  } else {
    remove <- resolveFunctionalDependencies(remove)
    rmnames <- vapply(remove, function(x) x$name, character(1))
  }

  matches <- depnames %in% rmnames
  if (warnOnConflict && !is.character(remove)) {
    for (loser in dependencies[matches]) {
      winner <- remove[[head(rmnames == loser$name, 1)]]
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


# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

nullOrEmpty <- function(x) {
  length(x) == 0
}

# Given a vector or list, drop all the NULL or length-0 items in it
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE=logical(1))]
}

isResolvedTag <- function(x) {
  inherits(x, "shiny.tag") && length(x$.renderHooks) == 0
}

isTag <- function(x) {
  inherits(x, "shiny.tag")
}

#' @rdname print.html
#' @export
print.shiny.tag <- function(x, browse = is.browsable(x), ...) {
  if (browse)
    html_print(x)
  else
    print(HTML(as.character(x)), ...)
  invisible(x)
}

# indent can be numeric to indicate an initial indent level,
# or FALSE to suppress
#' @export
format.shiny.tag <- function(x, ..., singletons = character(0), indent = 0) {
  as.character(renderTags(x, singletons = singletons, indent = indent)$html)
}

#' @export
as.character.shiny.tag <- function(x, ...) {
  as.character(renderTags(x)$html)
}

#' @export
as.character.html <- function(x, ...) {
  as.vector(enc2utf8(x))
}

#' @export
print.shiny.tag.list <- function(x, ...) {
  if (isTRUE(attr(x, "print.as.list", exact = TRUE))) {
    attr(x, "print.as.list") <- NULL
    class(x) <- setdiff(class(x), "shiny.tag.list")
    return(print(x))
  }

  print.shiny.tag(x, ...)
}

#' @export
format.shiny.tag.list <- format.shiny.tag

#' @export
as.character.shiny.tag.list <- as.character.shiny.tag

#' Print method for HTML/tags
#'
#' S3 method for printing HTML that prints markup or renders HTML in a web
#' browser.
#'
#' @param x The value to print.
#' @param browse If `TRUE`, the HTML will be rendered and displayed in a
#'   browser (or possibly another HTML viewer supplied by the environment via
#'   the `viewer` option). If `FALSE` then the HTML object's markup
#'   will be rendered at the console.
#' @param ... Additional arguments passed to print.
#'
#' @export
print.html <- function(x, ..., browse = is.browsable(x)) {
  if (browse)
    html_print(x)
  else
    cat(x, "\n", sep = "")
  invisible(x)
}

#' @export
format.html <- function(x, ...) {
  as.character(x)
}

normalizeText <- function(text) {
  if (!is.null(attr(text, "html", TRUE)))
    text
  else
    htmlEscape(text, attribute=FALSE)
}

#' Create a list of tags
#'
#' Create a `list()` of [tag]s with methods for [print()], [as.character()],
#' etc.
#'
#' @param ... A collection of [tag]s.
#' @export
#' @examples
#' tagList(
#'   h1("Title"),
#'   h2("Header text"),
#'   p("Text here")
#' )
tagList <- function(...) {
  lst <- dots_list(...)
  class(lst) <- c("shiny.tag.list", "list")
  return(lst)
}

#' Tag function
#'
#' Create 'lazily' rendered HTML [tags] (and/or [htmlDependencies()]).
#'
#' When possible, use [`tagAddRenderHook()`] to provide both a tag
#' structure and utilize a render function.
#'
#' @param func a function with no arguments that returns HTML tags and/or
#'   dependencies.
#' @seealso [`tagAddRenderHook()`]
#' @export
#' @examples
#' myDivDep <- tagFunction(function() {
#'   if (isTRUE(getOption("useDep", TRUE))) {
#'     htmlDependency(
#'       name = "lazy-dependency",
#'       version = "1.0", src = ""
#'     )
#'   }
#' })
#' myDiv <- attachDependencies(div(), myDivDep)
#' renderTags(myDiv)
#' withr::with_options(list(useDep = FALSE), renderTags(myDiv))
#'
tagFunction <- function(func) {
  if (!is.function(func) || length(formals(func)) != 0) {
    stop("`func` must be a function with no arguments")
  }
  structure(func, class = "shiny.tag.function")
}

#' Modify a tag prior to rendering
#'
#' Adds a hook to call on a [tag()] object when it is is rendered as HTML (with,
#' for example, [print()], [renderTags()], [as.tags()], etc).
#'
#' The primary motivation for [tagAddRenderHook()] is to create tags that can
#' change their attributes (e.g., change CSS classes) depending upon the context
#' in which they're rendered (e.g., use one set of CSS classes in one a page
#' layout, but a different set in another page layout). In this situation,
#' [tagAddRenderHook()] is preferable to [tagFunction()] since the latter is more a
#' "black box" in the sense that you don't know anything about the tag structure
#' until it's rendered.
#'
#' @param tag A [`tag()`] object.
#' @param func A function (_hook_) to call when the `tag` is rendered. This function
#'   should have at least one argument (the `tag`) and return anything that can
#'   be converted into tags via [as.tags()].
#' @param replace If `TRUE`, the previous hooks will be removed. If `FALSE`,
#'   `func` is appended to the previous hooks.
#' @return A [tag()] object with a `.renderHooks` field containing a list of functions
#'   (e.g. `func`). When the return value is _rendered_ (such as with [`as.tags()`]),
#'   these functions will be called just prior to writing the HTML.
#' @export
#' @seealso [tagFunction()]
#' @examples
#' # Have a place holder div and return a span instead
#' obj <- div("example", .renderHook = function(x) {
#'   x$name <- "span"
#'   x
#' })
#' obj$name # "div"
#' print(obj) # Prints as a `span`
#'
#' # Add a class to the tag
#' # Should print a `span` with class `"extra"`
#' spanExtra <- tagAddRenderHook(obj, function(x) {
#'   tagAppendAttributes(x, class = "extra")
#' })
#' spanExtra
#'
#' # Replace the previous render method
#' # Should print a `div` with class `"extra"`
#' divExtra <- tagAddRenderHook(obj, replace = TRUE, function(x) {
#'   tagAppendAttributes(x, class = "extra")
#' })
#' divExtra
#'
#' # Add more child tags
#' spanExtended <- tagAddRenderHook(obj, function(x) {
#'   tagAppendChildren(x, " ", tags$strong("bold text"))
#' })
#' spanExtended
#'
#' # Add a new html dependency
#' newDep <- tagAddRenderHook(obj, function(x) {
#'   fa <- htmlDependency(
#'     "font-awesome", "4.5.0", c(href="shared/font-awesome"),
#'     stylesheet = "css/font-awesome.min.css")
#'   attachDependencies(x, fa, append = TRUE)
#' })
#' # Also add a jqueryui html dependency
#' htmlDependencies(newDep) <- htmlDependency(
#'   "jqueryui", "1.11.4", c(href="shared/jqueryui"),
#'   script = "jquery-ui.min.js")
#' # At render time, both dependencies will be found
#' renderTags(newDep)$dependencies
#'
#' # Ignore the original tag and return something completely new.
#' newObj <- tagAddRenderHook(obj, function(x) {
#'   tags$p("Something else")
#' })
#' newObj
tagAddRenderHook <- function(tag, func, replace = FALSE) {
  if (!is.function(func) || length(formals(func)) == 0) {
    stop("`func` must be a function that accepts at least 1 argument")
  }

  tag$.renderHooks <-
    if (isTRUE(replace)) {
      list(func)
    } else {
      append(tag$.renderHooks, list(func))
    }

  tag
}


#' Append tag attributes
#'
#' Append (`tagAppendAttributes()`), check existence (`tagHasAttribute()`),
#' and obtain the value (`tagGetAttribute()`) of HTML attribute(s).
#'
#' @export
#' @param tag a [tag] object.
#' @param ... a collection of attributes.
#' @param .cssSelector A character string containing a [CSS
#'   selector](https://developer.mozilla.org/en-US/docs/Learn/CSS/Building_blocks/Selectors)
#'   for targeting particular (inner) tags of interest. At the moment, only a
#'   combination of
#'   [type](https://www.w3.org/TR/CSS22/selector.html#type-selectors) (e.g,
#'   `div`), [class](https://www.w3.org/TR/CSS22/selector.html#class-html)
#'   (e.g., `.my-class`),
#'   [id](https://www.w3.org/TR/CSS22/selector.html#id-selectors) (e.g.,
#'   `#myID`), and
#'   [universal](https://www.w3.org/TR/CSS22/selector.html#universal-selector)
#'   (`*`) selectors within a given [simple
#'   selector](https://www.w3.org/TR/CSS22/selector.html#selector-syntax) is
#'   supported. Note, if `.cssSelector` is used, the returned tags will have
#'   their `$children` fields flattened to a single `list()` via [`tagQuery()`].
#' @seealso [tagAppendChildren()], [tagQuery()]
#' @examples
#' html <- div(a())
#' tagAppendAttributes(html, class = "foo")
#' tagAppendAttributes(html, .cssSelector = "a", class = "bar")
#'
#' tagHasAttribute(div(foo = "bar"), "foo")
#' tagGetAttribute(div(foo = "bar"), "foo")
#'
tagAppendAttributes <- function(tag, ..., .cssSelector = NULL) {
  throw_if_tag_function(tag)

  if (!is.null(.cssSelector)) {
    return(
      tagQuery(tag)$
        find(.cssSelector)$
        addAttrs(...)$
        allTags()
    )
  }

  newAttribs <- dropNullsOrEmpty(dots_list(...))
  if (any(!nzchar(names2(newAttribs)))) {
    stop(
      "At least one of the new attribute values did not have a name.\n",
      "Did you forget to include an attribute name?"
    )
  }
  tag$attribs <- c(tag$attribs, newAttribs)
  tag
}

#' @rdname tagAppendAttributes
#' @param attr The name of an attribute.
#' @export
tagHasAttribute <- function(tag, attr) {
  throw_if_tag_function(tag)
  result <- attr %in% names(tag$attribs)
  result
}

#' @rdname tagAppendAttributes
#' @export
tagGetAttribute <- function(tag, attr) {
  throw_if_tag_function(tag)
  # Find out which positions in the attributes list correspond to the given attr
  attribs <- tag$attribs
  attrIdx <- which(attr == names(attribs))

  if (length(attrIdx) == 0) {
    return (NULL)
  }

  # Convert all attribs to chars explicitly; prevents us from messing up factors
  result <- lapply(attribs[attrIdx], as.character)
  # Separate multiple attributes with the same name
  result <- paste(result, collapse  = " ")
  result
}

#' Modify tag contents
#'
#' Modify the contents (aka children) of a [tag] object.
#'
#' @inheritParams tagAppendAttributes
#' @param child A child element to append to a parent tag.
#' @export
#' @seealso [tagAppendAttributes()], [tagQuery()]
#' @examples
#' html <- div(a(), h1())
#' tagAppendChild(html, span())
#' tagAppendChild(html, .cssSelector = "a", span())
#'
#' tagAppendChildren(html, span(), p())
#' tagAppendChildren(html, .cssSelector = "a", span(), p())
#'
#' tagSetChildren(html, span(), p())
#'
#' tagInsertChildren(html, after = 1, span(), p())
#'
tagAppendChild <- function(tag, child, .cssSelector = NULL) {
  throw_if_tag_function(tag)

  if (!is.null(.cssSelector)) {
    return(
      tagAppendChildren(tag, child, .cssSelector = .cssSelector)
    )
  }

  tag$children[[length(tag$children)+1]] <- child
  tag
}


#' @rdname tagAppendChild
#' @param ... a collection of `child` elements.
#' @param list Deprecated. Use `!!!` instead to splice into `...`.
#' @export
tagAppendChildren <- function(tag, ..., .cssSelector = NULL, list = NULL) {
  throw_if_tag_function(tag)

  children <- unname(c(dots_list(...), list))

  if (!is.null(.cssSelector)) {
    return(
      tagQuery(tag)$
        find(.cssSelector)$
        append(!!!children)$
        allTags()
    )
  }

  tag$children <- unname(c(tag$children, children))
  tag
}

#' @rdname tagAppendChild
#' @export
tagSetChildren <- function(tag, ..., .cssSelector = NULL, list = NULL) {
  throw_if_tag_function(tag)

  children <- unname(c(dots_list(...), list))

  if (!is.null(.cssSelector)) {
    return(
      tagQuery(tag)$
        find(.cssSelector)$
        empty()$
        append(!!!children)$
        allTags()
    )
  }

  tag$children <- children
  tag
}

#' @rdname tagAppendChild
#' @param after an integer value (i.e., subscript) referring to the child position to append after.
#' @export
tagInsertChildren <- function(tag, after, ..., .cssSelector = NULL, list = NULL) {
  throw_if_tag_function(tag)

  children <- unname(c(dots_list(...), list))

  if (!is.null(.cssSelector)) {
    return(
      tagQuery(tag)$
        find(.cssSelector)$
        each(function(x, i) {
          tagInsertChildren(x, after = after, !!!children)
        })$
        allTags()
    )
  }

  tag$children <- unname(append(tag$children, children, after))
  tag
}

throw_if_tag_function <- function(tag) {
  if (is_tag_function(tag))
    stop("`tag` can not be a `tagFunction()`")
}


# Use `known_tags` from `known_tags.R`
# Then remove `known_tags` once done creating tag functions
#' @include known_tags.R
names(known_tags) <- known_tags

#' Create HTML tags
#'
#' Create an R object that represents an HTML tag. For convenience, common HTML
#' tags (e.g., `<div>`) can be created by calling for their tag name directly
#' (e.g., `div()`). To create less common HTML5 (or SVG) tags (e.g.,
#' `<article>`), use the `tags` list collection (e.g., `tags$article()`). To
#' create other non HTML/SVG tags, use the lower-level `tag()` constructor.
#'
#' @name builder
#' @param ... Tag attributes (named arguments) and children (unnamed arguments).
#'   A named argument with an `NA` value is rendered as a boolean attributes
#'   (see example). Children may include any combination of:
#'   * Other tags objects
#'   * [HTML()] strings
#'   * [htmlDependency()]s
#'   * Single-element atomic vectors
#'   * `list()`s containing any combination of the above
#' @return A `list()` with a `shiny.tag` class that can be converted into an
#'   HTML string via `as.character()` and saved to a file with `save_html()`.
#' @seealso [tagList()], [withTags()], [tagAppendAttributes()], [tagQuery()]
#' @examples
#' tags$html(
#'   tags$head(
#'     tags$title('My first page')
#'   ),
#'   tags$body(
#'     h1('My first heading'),
#'     p('My first paragraph, with some ', strong('bold'), ' text.'),
#'     div(
#'       id = 'myDiv', class = 'simpleDiv',
#'       'Here is a div with some attributes.'
#'      )
#'   )
#' )
#'
#' # html5 <audio> with boolean control attribute
#' # https://www.w3.org/TR/html5/infrastructure.html#sec-boolean-attributes
#' tags$audio(
#'   controls = NA,
#'   tags$source(
#'     src = "myfile.wav",
#'     type = "audio/wav"
#'   )
#' )
#'
#' # suppress the whitespace between tags
#' tags$span(
#'   tags$strong("I'm strong", .noWS="outside")
#' )
#'
NULL

#' @rdname builder
#' @format NULL
#' @docType NULL
#' @keywords NULL
#' @import rlang
#' @export
tags <- lapply(known_tags, function(tagname) {
  # Overwrite the body with the `tagname` value injected into the body
  new_function(
    args = exprs(... = , .noWS = NULL, .renderHook = NULL),
    expr({
      validateNoWS(.noWS)
      contents <- dots_list(...)
      tag(!!tagname, contents, .noWS = .noWS, .renderHook = .renderHook)
    }),
    env = asNamespace("htmltools")
  )
})

# known_tags is no longer needed, so remove it.
rm(known_tags)


#' @rdname builder
#' @export
p <- tags$p

#' @rdname builder
#' @export
h1 <- tags$h1

#' @rdname builder
#' @export
h2 <- tags$h2

#' @rdname builder
#' @export
h3 <- tags$h3

#' @rdname builder
#' @export
h4 <- tags$h4

#' @rdname builder
#' @export
h5 <- tags$h5

#' @rdname builder
#' @export
h6 <- tags$h6

#' @rdname builder
#' @export
a <- tags$a

#' @rdname builder
#' @export
br <- tags$br

#' @rdname builder
#' @export
div <- tags$div

#' @rdname builder
#' @export
span <- tags$span

#' @rdname builder
#' @export
pre <- tags$pre

#' @rdname builder
#' @export
code <- tags$code

#' @rdname builder
#' @export
img <- tags$img

#' @rdname builder
#' @export
strong <- tags$strong

#' @rdname builder
#' @export
em <- tags$em

#' @rdname builder
#' @export
hr <- tags$hr


#' @rdname builder
#' @param _tag_name A character string to use for the tag name.
#' @param varArgs List of tag attributes and children.
#' @param .noWS Character vector used to omit some of the whitespace that would
#'   normally be written around this tag. Valid options include `before`,
#'   `after`, `outside`, `after-begin`, and `before-end`.
#'   Any number of these options can be specified.
#' @param .renderHook A function (or list of functions) to call when the `tag` is rendered. This
#'   function should have at least one argument (the `tag`) and return anything
#'   that can be converted into tags via [as.tags()]. Additional hooks may also be
#'   added to a particular `tag` via [tagAddRenderHook()].
#' @export
tag <- function(`_tag_name`, varArgs, .noWS = NULL, .renderHook = NULL) {
  validateNoWS(.noWS)
  # Get arg names; if not a named list, use vector of empty strings
  varArgsNames <- names2(varArgs)

  # Named arguments become attribs, dropping NULL and length-0 values
  named_idx <- nzchar(varArgsNames)
  attribs <- dropNullsOrEmpty(varArgs[named_idx])

  # Unnamed arguments are flattened and added as children.
  # Use unname() to remove the names attribute from the list, which would
  # consist of empty strings anyway.
  children <- unname(varArgs[!named_idx])

  st <- list(name = `_tag_name`,
      attribs = attribs,
      children = children)

  # Conditionally include the `.noWS` field.
  # We do this to avoid breaking the hashes of existing tags that weren't leveraging .noWS.
  if (!is.null(.noWS)) {
    st$.noWS <- .noWS
  }
  # Conditionally include the `.renderHooks` field.
  # We do this to avoid breaking the hashes of existing tags that weren't leveraging .renderHooks.
  if (!is.null(.renderHook)) {
    if (!is.list(.renderHook)) {
      .renderHook <- list(.renderHook)
    }
    st$.renderHooks <- .renderHook
  }

  # Return tag data structure
  structure(st, class = "shiny.tag")
}

isTagList <- function(x) {
  is.list(x) && (inherits(x, "shiny.tag.list") || identical(class(x), "list"))
}

noWSOptions <- c("before", "after", "after-begin", "before-end", "outside", "inside")
# Ensure that the provided `.noWS` string contains only valid options
validateNoWS <- function(.noWS) {
  if (!all(.noWS %in% noWSOptions)) {
    stop("Invalid .noWS option(s) '", paste(.noWS, collapse="', '") ,"' specified.")
  }
}

#' @include utils.R
tagWrite <- function(tag, textWriter, indent=0, eol = "\n") {

  if (length(tag) == 0)
    return (NULL)

  # optionally process a list of tags
  if (!isTag(tag) && isTagList(tag)) {
    tag <- dropNullsOrEmpty(flattenTags(tag))
    lapply(tag, tagWrite, textWriter, indent)
    return (NULL)
  }

  nextIndent <- if (is.numeric(indent)) indent + 1 else indent
  indent <- if (is.numeric(indent)) indent else 0

  # compute indent text
  indentText <- paste(rep(" ", indent*2), collapse="")
  textWriter$writeWS(indentText)

  # Check if it's just text (may either be plain-text or HTML)
  if (is.character(tag)) {
    .noWS <- attr(tag, "noWS", exact = TRUE)
    if ("before" %in% .noWS || "outside" %in% .noWS) {
      textWriter$eatWS()
    }
    textWriter$write(normalizeText(tag))
    if ("after" %in% .noWS || "outside" %in% .noWS) {
      textWriter$eatWS()
    }
    textWriter$writeWS(eol)
    return (NULL)
  }

  .noWS <- tag$.noWS

  if ("before" %in% .noWS || "outside" %in% .noWS) {
    textWriter$eatWS()
  }

  # write tag name
  textWriter$write(concat8("<", tag$name))

  attribs <- flattenTagAttribs(tag$attribs)
  attribNames <- names2(attribs)
  if (any(!nzchar(attribNames))) {
    # Can not display attrib without a key
    stop(
      "A tag's attribute value did not have a name.\n",
      "Did you forget to name all of your attribute values?"
    )
  }

  # write attributes
  for (attrib in attribNames) {
    attribValue <- attribs[[attrib]]
    if (!is.na(attribValue)) {
      if (is.logical(attribValue))
        attribValue <- tolower(attribValue)
      text <- htmlEscape(attribValue, attribute=TRUE)
      textWriter$write(concat8(" ", attrib,"=\"", text, "\""))
    }
    else {
      textWriter$write(concat8(" ", attrib))
    }
  }

  # write any children
  children <- dropNullsOrEmpty(flattenTags(tag$children))
  if (length(children) > 0) {
    textWriter$write(">")

    # special case for a single child text node (skip newlines and indentation)
    if ((length(children) == 1) && is.character(children[[1]]) ) {
      textWriter$write(concat8(normalizeText(children[[1]]), "</", tag$name, ">"))
    }
    else {
      if ("after-begin" %in% .noWS || "inside" %in% .noWS) {
        textWriter$eatWS()
      }
      textWriter$writeWS("\n")
      for (child in children)
        tagWrite(child, textWriter, nextIndent)
      textWriter$writeWS(indentText)
      if ("before-end" %in% .noWS || "inside" %in% .noWS) {
        textWriter$eatWS()
      }
      textWriter$write(concat8("</", tag$name, ">"))
    }
  }
  else {
    # only self-close void elements
    # (see: http://dev.w3.org/html5/spec/single-page.html#void-elements)
    if (tag$name %in% c("area", "base", "br", "col", "command", "embed", "hr",
      "img", "input", "keygen", "link", "meta", "param",
      "source", "track", "wbr")) {
      textWriter$write("/>")
    }
    else {
      textWriter$write(concat8("></", tag$name, ">"))
    }
  }
  if ("after" %in% .noWS || "outside" %in% .noWS) {
    textWriter$eatWS()
  }
  textWriter$writeWS(eol)
}

#' Render tags into HTML
#'
#' Renders tags (and objects that can be converted into tags using
#' [as.tags()]) into HTML. (Generally intended to be called from web
#' framework libraries, not directly by most users--see
#' [print.html()] for higher level rendering.)
#'
#' @param x Tag object(s) to render
#' @param singletons A list of [singleton] signatures to consider already
#'   rendered; any matching singletons will be dropped instead of rendered.
#'   (This is useful (only?) for incremental rendering.)
#' @param indent Initial indent level, or `FALSE` if no indentation should
#'   be used.
#'
#' @return `renderTags` returns a list with the following variables:
#'   * `head`: An [HTML()] string that should be included in `<head>`.
#'   * `singletons`: Character vector of singleton signatures that are
#'   known after rendering.
#'   * `dependencies`: A list of [resolved][resolveDependencies] [htmlDependency()] objects.
#'   * `html`: An [HTML()] string that represents the main HTML that was rendered.
#'
#' @export
renderTags <- function(x, singletons = character(0), indent = 0) {
  x <- tagify(x)
  # Do singleton and head processing before rendering
  singletonInfo <- takeSingletons(x, singletons)
  headInfo <- takeHeads(singletonInfo$ui)
  deps <- resolveDependencies(findDependencies(singletonInfo$ui, tagify = FALSE))

  headIndent <- if (is.numeric(indent)) indent + 1 else indent
  headHtml <- doRenderTags(headInfo$head, indent = headIndent)
  bodyHtml <- doRenderTags(headInfo$ui, indent = indent)

  return(list(head = headHtml,
    singletons = singletonInfo$singletons,
    dependencies = deps,
    html = bodyHtml))
}

#' @details `doRenderTags` is intended for very low-level use; it ignores
#'   singleton, head, and dependency handling, and simply renders the given tag
#'   objects as HTML.
#' @return `doRenderTags` returns a simple [HTML()] string.
#' @rdname renderTags
#' @export
doRenderTags <- function(x, indent = 0) {
  assertNotTagEnvLike(x, "doRenderTags")

  textWriter <- WSTextWriter()
  tagWrite(x, textWriter, indent)
  # Strip off trailing \n (if present?)
  textWriter$eatWS()
  HTML(textWriter$readAll())
}

# Walk a tree of tag objects, rewriting objects according to func.
# preorder=TRUE means preorder tree traversal, that is, an object
# should be rewritten before its children.
rewriteTags <- function(ui, func, preorder) {
  assertNotTagEnvLike(ui, "rewriteTags")

  if (preorder)
    ui <- func(ui)

  if (isTag(ui)) {
    ui$children[] <- lapply(ui$children, rewriteTags, func, preorder)
  } else if (isTagList(ui)) {
    ui[] <- lapply(ui, rewriteTags, func, preorder)
  }

  if (!preorder)
    ui <- func(ui)

  return(ui)
}

#' Singleton manipulation functions
#'
#' Functions for manipulating [singleton()] objects in tag
#' hierarchies. Intended for framework authors.
#'
#' @rdname singleton_tools
#' @name singleton_tools
NULL

#' @param ui Tag object or lists of tag objects. See [builder] topic.
#' @return `surroundSingletons` preprocesses a tag object by changing any
#'   singleton X into `<!--SHINY.SINGLETON[sig]-->X'<!--/SHINY.SINGLETON[sig]-->`
#'   where sig is the sha1 of X, and X' is X minus the singleton attribute.
#' @rdname singleton_tools
#' @export
surroundSingletons <- local({
  # In the case of nested singletons, outer singletons are processed
  # before inner singletons (otherwise the processing of inner
  # singletons would cause the sha1 of the outer singletons to be
  # different).
  surroundSingleton <- function(uiObj) {
    if (is.singleton(uiObj)) {
      sig <- digest(uiObj, "sha1")
      uiObj <- singleton(uiObj, FALSE)
      return(tagList(
        HTML(sprintf("<!--SHINY.SINGLETON[%s]-->", sig)),
        uiObj,
        HTML(sprintf("<!--/SHINY.SINGLETON[%s]-->", sig))
      ))
    } else {
      uiObj
    }
  }

  function(ui) {
    rewriteTags(ui, surroundSingleton, TRUE)
  }
})

#' @param singletons Character vector of singleton signatures that have already
#'   been encountered (i.e. returned from previous calls to
#'   `takeSingletons`).
#' @param desingleton Logical value indicating whether singletons that are
#'   encountered should have the singleton attribute removed.
#' @return `takeSingletons` returns a list with the elements `ui` (the
#'   processed tag objects with any duplicate singleton objects removed) and
#'   `singletons` (the list of known singleton signatures).
#' @rdname singleton_tools
#' @export
takeSingletons <- function(ui, singletons=character(0), desingleton=TRUE) {
  result <- rewriteTags(ui, function(uiObj) {
    if (is.singleton(uiObj)) {
      sig <- digest(uiObj, "sha1")
      if (sig %in% singletons)
        return(NULL)
      singletons <<- append(singletons, sig)
      if (desingleton)
        uiObj <- singleton(uiObj, FALSE)
      return(uiObj)
    } else {
      return(uiObj)
    }
  }, TRUE)

  return(list(ui=result, singletons=singletons))
}

# Given a tag object, extract out any children of tags$head
# and return them separate from the body.
takeHeads <- function(ui) {
  headItems <- list()
  result <- rewriteTags(ui, function(uiObj) {
    if (isTag(uiObj) && tolower(uiObj$name) == "head") {
      headItems <<- append(headItems, uiObj$children)
      return(NULL)
    }
    return(uiObj)
  }, FALSE)

  return(list(ui=result, head=headItems))
}

#' Collect attached dependencies from HTML tag object
#'
#' Walks a hierarchy of tags looking for attached dependencies.
#'
#' @param tags A tag-like object to search for dependencies.
#' @param tagify Whether to tagify the input before searching for dependencies.
#'
#' @return A list of [htmlDependency()] objects.
#'
#' @export
findDependencies <- function(tags, tagify = TRUE) {
  if (isTRUE(tagify)) {
    tags <- tagify(tags)
  }
  deps <- resolveFunctionalDependencies(htmlDependencies(tags))
  children <- if (is.list(tags)) {
    if (isTag(tags)) {
      tags$children
    } else {
      tags
    }
  }
  childDeps <- unlist(lapply(children, findDependencies, tagify = FALSE), recursive = FALSE, use.names = FALSE)
  c(childDeps, deps)
}


#' Resolves any [tagFunction()]s inside a list of [htmlDependencies()]. To
#' resolve [tagFunction()]s _and then_ remove redundant dependencies all at once,
#' use [resolveDependencies()] (which calls this function internally).
#' @noRd
resolveFunctionalDependencies <- function(dependencies) {
  if (!length(dependencies)) {
    return(dependencies)
  }
  dependencies <- asDependencies(dependencies)
  dependencies <- lapply(dependencies, function(dep) {
    if (is_tag_function(dep)) {
      dep <- dep()
    }
    if (isTag(dep) || inherits(dep, "shiny.tag.list")) {
      warning(
        "It appears attachDependencies() has been used to attach a tagFunction()",
        "that returns a shiny.tag/shiny.tag.list, which is considered poor practice",
        "since those tags will never actually get rendered", call. = FALSE
      )
      return(findDependencies(dep))
    }
    asDependencies(dep)
  })
  unlist(dependencies, recursive = FALSE, use.names = FALSE)
}


#' Mark Characters as HTML
#'
#' Marks the given text as HTML, which means the [tag] functions will know
#' not to perform HTML escaping on it.
#'
#' @param text The text value to mark with HTML
#' @param ... Any additional values to be converted to character and
#'   concatenated together
#' @param .noWS Character vector used to omit some of the whitespace that would
#'   normally be written around this HTML. Valid options include `before`,
#'   `after`, and `outside` (equivalent to `before` and
#'   `end`).
#' @return The input `text`, but marked as HTML.
#'
#' @examples
#' el <- div(HTML("I like <u>turtles</u>"))
#' cat(as.character(el))
#'
#' @export
HTML <- function(text, ..., .noWS = NULL) {
  htmlText <- c(text, as.character(dots_list(...)))
  htmlText <- paste8(htmlText, collapse=" ")
  attr(htmlText, "html") <- TRUE
  attr(htmlText, "noWS") <- .noWS
  class(htmlText) <- c("html", "character")
  htmlText
}

#' Evaluate an expression using `tags`
#'
#' This function makes it simpler to write HTML-generating code. Instead of
#' needing to specify `tags` each time a tag function is used, as in
#' `tags$div()` and `tags$p()`, code inside `withTags` is
#' evaluated with `tags` searched first, so you can simply use
#' `div()` and `p()`.
#'
#' If your code uses an object which happens to have the same name as an
#' HTML tag function, such as `source()` or `summary()`, it will call
#' the tag function. To call the intended (non-tags function), specify the
#' namespace, as in `base::source()` or `base::summary()`.
#'
#' @param code A set of tags.
#'
#' @examples
#' # Using tags$ each time
#' tags$div(class = "myclass",
#'   tags$h3("header"),
#'   tags$p("text")
#' )
#'
#' # Equivalent to above, but using withTags
#' withTags(
#'   div(class = "myclass",
#'     h3("header"),
#'     p("text")
#'   )
#' )
#'
#'
#' @export
withTags <- function(code) {
  eval(substitute(code), envir = as.list(tags), enclos = parent.frame())
}

# Make sure any objects in the tree that can be converted to tags, have been
tagify <- function(x) {
  rewriteTags(x, function(uiObj) {
    if (isResolvedTag(uiObj) || isTagList(uiObj) || is.character(uiObj))
      return(uiObj)
    else
      tagify(as.tags(uiObj))
  }, FALSE)
}

# Given a list of tags, lists, and other items, return a flat list, where the
# items from the inner, nested lists are pulled to the top level, recursively.
# Be sure to check for tagEnvLike objects and not allow them
flattenTags <- function(x) {
  assertNotTagEnvLike(x, "flattenTags")
  if (isTag(x)) {
    # For tags, wrap them into a list (which will be unwrapped by caller)
    list(x)
  } else if (isTagList(x)) {
    if (length(x) == 0) {
      # Empty lists are simply returned
      x
    } else {
      # For items that are lists (but not tags), recurse
      unlist(lapply(x, flattenTags), recursive = FALSE)
    }
  } else if (is.character(x)){
    # This will preserve attributes if x is a character with attribute,
    # like what HTML() produces
    list(x)

  } else {
    # For other items, coerce to character and wrap them into a list (which
    # will be unwrapped by caller). Note that this will strip attributes.
    flattenTags(as.tags(x))
  }
}
# This method should be just like `flattenTags()`, except the final `else` will
# return `list(x)`, rather than calling `flattenTags(as.tags(x))`.
#
# By not calling `as.tags(x)`, tagFunctions are not evaluated and other items
# are not converted.
flattenTagsRaw <- function(x) {
  if (isTag(x) || isTagEnv(x)) {
    # For tags, wrap them into a list (which will be unwrapped by caller)
    list(x)
  } else if (isTagList(x)) {
    if (length(x) == 0) {
      # Empty lists are simply returned
      x
    } else {
      # For items that are lists (but not tags), recurse
      unlist(lapply(x, flattenTagsRaw), recursive = FALSE)
    }
  } else {
    # This will preserve attributes if x is a character with attribute,
    # like what HTML() produces
    list(x)
  }
}

flattenTagAttribs <- function(attribs) {

  # Convert all attribs to chars explicitly; prevents us from messing up factors
  attribs <- lapply(attribs, as.character)
  # concatenate attributes
  # split() is very slow, so avoid it if possible
  if (anyDuplicated(names(attribs))) {
    attribs <- lapply(split(attribs, names(attribs)), function(x) {
      na_idx <- is.na(x)
      if (any(na_idx)) {
        if (all(na_idx)) {
          return(NA)
        }
        x <- x[!na_idx]
      }
      paste(x, collapse = " ")
    })
  }

  attribs

}

#' Convert a value to tags
#'
#' An S3 method for converting arbitrary values to a value that can be used as
#' the child of a tag or `tagList`. The default implementation simply calls
#' [as.character()].
#'
#' @param x Object to be converted.
#' @param ... Any additional parameters.
#'
#' @export
as.tags <- function(x, ...) {
  UseMethod("as.tags")
}

#' @export
as.tags.default <- function(x, ...) {
  # Plain (non-classed) lists will hit as.tags.list(), but lists with a class
  # will get here. (tagLists will already have been handled by
  # as.tags.shiny.tag.list)
  if (is.list(x)) {
    tagList(!!!unclass(x))
  } else {
    tagList(as.character(x))
  }
}

#' @export
as.tags.html <- function(x, ...) {
  x
}

#' @export
as.tags.shiny.tag <- function(x, ...) {
  if (isResolvedTag(x)) {
    return(x)
  }

  hook <- x$.renderHooks[[1]]
  # remove first hook
  x$.renderHooks[[1]] <- NULL
  # Recursively call as.tags on the updated object
  # (Perform in two lines to avoid lazy arg evaluation issues)
  y <- hook(x)
  as.tags(y)
}

#' @export
as.tags.shiny.tag.list <- function(x, ...) {
  x
}

#' @export
as.tags.shiny.tag.function <- function(x, ...) {
  x()
}

#' @export
as.tags.list <- function(x, ...) {
  # Only non-classed lists will hit this method
  # (classed lists will reach the default method)
  tagList(!!!x)
}

#' @export
as.tags.character <- function(x, ...) {
  # For printing as.tags("<strong>") directly at console, without dropping any
  # attached dependencies
  tagList(x)
}

#' @export
as.tags.html_dependency <- function(x, ...) {
  attachDependencies(tagList(), x)
}

#' Preserve HTML regions
#'
#' Use "magic" HTML comments to protect regions of HTML from being modified by
#' text processing tools.
#'
#' Text processing tools like markdown and pandoc are designed to turn
#' human-friendly markup into common output formats like HTML. This works well
#' for most prose, but components that generate their own HTML may break if
#' their markup is interpreted as the input language. The `htmlPreserve`
#' function is used to mark regions of an input document as containing pure HTML
#' that must not be modified. This is achieved by substituting each such region
#' with a benign but unique string before processing, and undoing those
#' substitutions after processing.
#'
#' @param x A character vector of HTML to be preserved.
#'
#' @return `htmlPreserve` returns a single-element character vector with
#'   "magic" HTML comments surrounding the original text (unless the original
#'   text was empty, in which case an empty string is returned).
#'
#' @examples
#' # htmlPreserve will prevent "<script>alert(10*2*3);</script>"
#' # from getting an <em> tag inserted in the middle
#' markup <- paste(sep = "\n",
#'   "This is *emphasized* text in markdown.",
#'   htmlPreserve("<script>alert(10*2*3);</script>"),
#'   "Here is some more *emphasized text*."
#' )
#' extracted <- extractPreserveChunks(markup)
#' markup <- extracted$value
#' # Just think of this next line as Markdown processing
#' output <- gsub("\\*(.*?)\\*", "<em>\\1</em>", markup)
#' output <- restorePreserveChunks(output, extracted$chunks)
#' output
#'
#' @export
htmlPreserve <- function(x) {
  raw = getOption("htmltools.preserve.raw", FALSE)
  x <- paste(x, collapse = "\n")
  if (nzchar(x))
    if (raw) {
      # use fenced code block if there are embedded newlines
      if (grepl("\n", x, fixed = TRUE))
        sprintf("\n```{=html}\n%s\n```\n", x)
      # otherwise use inline span
      else
        sprintf("`%s`{=html}", x)
    }
    else {
      sprintf("<!--html_preserve-->%s<!--/html_preserve-->", x)
    }
  else
    x
}

# Temporarily set x in env to value, evaluate expr, and
# then restore x to its original state
withTemporary <- function(env, x, value, expr, unset = FALSE) {

  if (exists(x, envir = env, inherits = FALSE)) {
    oldValue <- get(x, envir = env, inherits = FALSE)
    on.exit(
      assign(x, oldValue, envir = env, inherits = FALSE),
      add = TRUE)
  } else {
    on.exit(
      rm(list = x, envir = env, inherits = FALSE),
      add = TRUE
    )
  }

  if (!missing(value) && !isTRUE(unset))
    assign(x, value, envir = env, inherits = FALSE)
  else {
    if (exists(x, envir = env, inherits = FALSE))
      rm(list = x, envir = env, inherits = FALSE)
  }
  force(expr)
}

# Evaluate an expression using Shiny's own private stream of
# randomness (not affected by set.seed).
withPrivateSeed <- local({
  ownSeed <- NULL
  function(expr) {
    withTemporary(.GlobalEnv, ".Random.seed",
      ownSeed, unset=is.null(ownSeed), {
        tryCatch({
          expr
        }, finally = {ownSeed <<- .Random.seed})
      }
    )
  }
})

# extract_preserve_chunks looks for regions in strval marked by
# <!--html_preserve-->...<!--/html_preserve--> and replaces each such region
# with a long unique ID. The return value is a list with $value as the string
# with the regions replaced, and $chunks as a named character vector where the
# names are the IDs and the values are the regions that were extracted.
#
# Nested regions are handled appropriately; the outermost region is what's used
# and any inner regions simply have their boundaries removed before the values
# are stashed in $chunks.

#' @return `extractPreserveChunks` returns a list with two named elements:
#'   `value` is the string with the regions replaced, and `chunks` is
#'   a named character vector where the names are the IDs and the values are the
#'   regions that were extracted.
#' @rdname htmlPreserve
#' @export
extractPreserveChunks <- function(strval) {

  # Literal start/end marker text. Case sensitive.
  startmarker <- "<!--html_preserve-->"
  endmarker <- "<!--/html_preserve-->"
  # Start and end marker length MUST be different, it's how we tell them apart
  startmarker_len <- nchar(startmarker)
  endmarker_len <- nchar(endmarker)
  # Pattern must match both start and end markers
  pattern <- "<!--/?html_preserve-->"

  # It simplifies string handling greatly to collapse multiple char elements
  if (length(strval) != 1)
    strval <- paste(strval, collapse = "\n")

  # matches contains the index of all the start and end markers
  startmatches <- gregexpr(startmarker, strval, fixed = TRUE)[[1]]
  endmatches <- gregexpr(endmarker, strval, fixed = TRUE)[[1]]
  matches <- c(startmatches, endmatches)
  o <- order(matches)
  matches <- matches[o]
  lengths <- c(
    attr(startmatches, "match.length", TRUE),
    attr(endmatches, "match.length", TRUE)
  )[o]

  # No markers? Just return.
  if (unique(matches)[[1]] == -1)
    return(list(value = strval, chunks = character(0)))

  # If TRUE, it's a start; if FALSE, it's an end
  boundary_type <- lengths == startmarker_len

  # Positive number means we're inside a region, zero means we just exited to
  # the top-level, negative number means error (an end without matching start).
  # For example:
  # boundary_type - TRUE TRUE FALSE FALSE TRUE FALSE
  # preserve_level - 1 2 1 0 1 0
  preserve_level <- cumsum(ifelse(boundary_type, 1, -1))

  # Sanity check.
  if (any(preserve_level < 0) || tail(preserve_level, 1) != 0) {
    stop("Invalid nesting of html_preserve directives")
  }

  # Identify all the top-level boundary markers. We want to find all of the
  # elements of preserve_level whose value is 0 and preceding value is 1, or
  # whose value is 1 and preceding value is 0. Since we know that preserve_level
  # values can only go up or down by 1, we can simply shift preserve_level by
  # one element and add it to preserve_level; in the result, any value of 1 is a
  # match.
  is_top_level <- 1 == (preserve_level + c(0, preserve_level[-length(preserve_level)]))

  preserved <- character(0)

  top_level_matches <- matches[is_top_level]
  # Iterate backwards so string mutation doesn't screw up positions for future
  # iterations
  for (i in seq.int(length(top_level_matches) - 1, 1, by = -2)) {
    start_outer <- top_level_matches[[i]]
    start_inner <- start_outer + startmarker_len
    end_inner <- top_level_matches[[i+1]]
    end_outer <- end_inner + endmarker_len

    id <- withPrivateSeed(
      paste("preserve", paste(
        format(as.hexmode(sample(256, 8, replace = TRUE)-1), width=2),
        collapse = ""),
        sep = "")
    )

    preserved[id] <- gsub(pattern, "", substr(strval, start_inner, end_inner-1))

    strval <- paste(
      substr(strval, 1, start_outer - 1),
      id,
      substr(strval, end_outer, nchar(strval)),
      sep="")
    substr(strval, start_outer, end_outer-1) <- id
  }

  list(value = strval, chunks = preserved)
}

#' @param strval Input string from which to extract/restore chunks.
#' @param chunks The `chunks` element of the return value of
#'   `extractPreserveChunks`.
#' @return `restorePreserveChunks` returns a character vector with the
#'   chunk IDs replaced with their original values.
#' @rdname htmlPreserve
#' @export
restorePreserveChunks <- function(strval, chunks) {
  strval <- enc2utf8(strval)
  chunks <- enc2utf8(chunks)
  for (id in names(chunks))
    strval <- gsub(id, chunks[[id]], strval, fixed = TRUE, useBytes = TRUE)
  Encoding(strval) <- 'UTF-8'
  strval
}

#' Knitr S3 methods
#'
#' These S3 methods are necessary to allow HTML tags to print themselves in
#' knitr/rmarkdown documents.
#'
#' @name knitr_methods
#' @param x Object to knit_print
#' @param ... Additional knit_print arguments
NULL

#' @rdname knitr_methods
#' @export
knit_print.shiny.tag <- function(x, ...) {
  x <- tagify(x)
  output <- surroundSingletons(x)
  deps <- resolveDependencies(findDependencies(x, tagify = FALSE), resolvePackageDir = FALSE)
  content <- takeHeads(output)
  head_content <- doRenderTags(tagList(content$head))

  meta <- if (length(head_content) > 1 || head_content != "") {
    list(structure(head_content, class = "shiny_head"))
  }
  meta <- c(meta, deps)

  knitr::asis_output(
    htmlPreserve(format(content$ui, indent=FALSE)),
    meta = meta)
}

#' @rdname knitr_methods
#' @export
knit_print.html <- function(x, ...) {
  deps <- resolveDependencies(findDependencies(x, tagify = FALSE))
  knitr::asis_output(htmlPreserve(as.character(x)),
    meta = if (length(deps)) list(deps))
}

#' @rdname knitr_methods
#' @export
knit_print.shiny.tag.list <- knit_print.shiny.tag


#' Include Content From a File
#'
#' Load HTML, text, or rendered Markdown from a file and turn into HTML.
#'
#' These functions provide a convenient way to include an extensive amount of
#' HTML, textual, Markdown, CSS, or JavaScript content, rather than using a
#' large literal R string.
#'
#' @param path The path of the file to be included. It is highly recommended to
#'   use a relative path (the base path being the Shiny application directory),
#'   not an absolute path.
#'
#' @rdname include
#' @name include
#' @aliases includeHTML
#' @export
includeHTML <- function(path) {
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(HTML(paste8(lines, collapse='\n')))
}

#' @note `includeText` escapes its contents, but does no other processing.
#'   This means that hard breaks and multiple spaces will be rendered as they
#'   usually are in HTML: as a single space character. If you are looking for
#'   preformatted text, wrap the call with [pre()], or consider using
#'   `includeMarkdown` instead.
#'
#' @rdname include
#' @export
includeText <- function(path) {
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(paste8(lines, collapse='\n'))
}

#' @note The `includeMarkdown` function requires the `markdown`
#'   package.
#' @rdname include
#' @export
includeMarkdown <- function(path) {
  html <- markdown::markdownToHTML(path, fragment.only=TRUE)
  Encoding(html) <- 'UTF-8'
  return(HTML(html))
}

#' @param ... Any additional attributes to be applied to the generated tag.
#' @rdname include
#' @export
includeCSS <- function(path, ...) {
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  args <- dots_list(...)
  if (is.null(args$type))
    args$type <- 'text/css'
  return(do.call(tags$style,
    c(list(HTML(paste8(lines, collapse='\n'))), args)))
}

#' @rdname include
#' @export
includeScript <- function(path, ...) {
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(tags$script(HTML(paste8(lines, collapse='\n')), ...))
}

#' Include content only once
#'
#' Use `singleton` to wrap contents (tag, text, HTML, or lists) that should
#' be included in the generated document only once, yet may appear in the
#' document-generating code more than once. Only the first appearance of the
#' content (in document order) will be used.
#'
#' @param x A [tag()], text, [HTML()], or list.
#' @param value Whether the object should be a singleton.
#'
#' @export
singleton <- function(x, value = TRUE) {
  attr(x, "htmltools.singleton") <- if (isTRUE(value)) TRUE else NULL
  return(x)
}

#' @rdname singleton
#' @export
is.singleton <- function(x) {
  isTRUE(attr(x, "htmltools.singleton"))
}


#' Validate proper CSS formatting of a unit
#'
#' Checks that the argument is valid for use as a CSS unit of length.
#'
#' `NULL` and `NA` are returned unchanged.
#'
#' Single element numeric vectors are returned as a character vector with the
#' number plus a suffix of `"px"`.
#'
#' Single element character vectors must be `"auto"`, `"fit-content"`
#' or `"inherit"`, a number, or a length calculated by the `"calc"`
#' CSS function. If the number has a suffix, it must be valid: `px`,
#' `\%`, `ch`, `em`, `rem`, `pt`, `in`, `cm`,
#' `mm`, `ex`, `pc`, `vh`, `vw`, `vmin`, or
#' `vmax`.
#' If the number has no suffix, the suffix `"px"` is appended.
#'
#'
#' Any other value will cause an error to be thrown.
#'
#' @param x The unit to validate. Will be treated as a number of pixels if a
#'   unit is not specified.
#' @return A properly formatted CSS unit of length, if possible. Otherwise, will
#'   throw an error.
#' @examples
#' validateCssUnit("10%")
#' validateCssUnit(400)  #treated as '400px'
#' @export
validateCssUnit <- function(x) {
  if (is.null(x) || is.na(x))
    return(x)

  if (length(x) > 1 || (!is.character(x) && !is.numeric(x)))
    stop('CSS units must be a single-element numeric or character vector')

  # if the input is a character vector consisting only of digits (e.g. "960"),
  # coerce it to a numeric value
  if (is.character(x) && nchar(x) > 0 && gsub("\\d*", "", x) == "")
    x <- as.numeric(x)

  pattern <-
    "^(auto|inherit|fit-content|calc\\(.*\\)|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|ch|em|ex|rem|pt|pc|px|vh|vw|vmin|vmax))$"

  if (is.character(x) &&
      !grepl(pattern, x)) {
    stop('"', x, '" is not a valid CSS unit (e.g., "100%", "400px", "auto")')
  } else if (is.numeric(x)) {
    x <- paste(x, "px", sep = "")
  }
  x
}

#' CSS string helper
#'
#' Convenience function for building CSS style declarations (i.e. the string
#' that goes into a style attribute, or the parts that go inside curly braces in
#' a full stylesheet).
#'
#' CSS uses `'-'` (minus) as a separator character in property names, but
#' this is an inconvenient character to use in an R function argument name.
#' Instead, you can use `'.'` (period) and/or `'_'` (underscore) as
#' separator characters. For example, `css(font.size = "12px")` yields
#' `"font-size:12px;"`.
#'
#' To mark a property as `!important`, add a `'!'` character to the end
#' of the property name. (Since `'!'` is not normally a character that can be
#' used in an identifier in R, you'll need to put the name in double quotes or
#' backticks.)
#'
#' Argument values will be converted to strings using
#' `paste(collapse = " ")`. Any property with a value of `NULL` or
#' `""` (after paste) will be dropped.
#'
#' @param ... Named style properties, where the name is the property name and
#'   the argument is the property value. See Details for conversion rules.
#' @param collapse_ (Note that the parameter name has a trailing underscore
#'   character.) Character to use to collapse properties into a single string;
#'   likely `""` (the default) for style attributes, and either `"\n"`
#'   or `NULL` for style blocks.
#'
#' @examples
#' padding <- 6
#' css(
#'   font.family = "Helvetica, sans-serif",
#'   margin = paste0(c(10, 20, 10, 20), "px"),
#'   "padding!" = if (!is.null(padding)) padding
#' )
#'
#' @export
css <- function(..., collapse_ = "") {
  props <- dots_list(...)
  if (length(props) == 0) {
    return(NULL)
  }

  if (is.null(names(props)) || any(names(props) == "")) {
    stop("cssList expects all arguments to be named")
  }

  # Necessary to make factors show up as level names, not numbers
  props[] <- lapply(props, paste, collapse = " ")

  # Drop null args
  props <- props[!sapply(props, empty)]
  if (length(props) == 0) {
    return(NULL)
  }

  # Replace all '.' and '_' in property names to '-'
  names(props) <- gsub("[._]", "-", tolower(gsub("([A-Z])", "-\\1", names(props))))

  # Create "!important" suffix for each property whose name ends with !, then
  # remove the ! from the property name
  important <- ifelse(grepl("!$", names(props), perl = TRUE), " !important", "")
  names(props) <- sub("!$", "", names(props), perl = TRUE)

  paste0(names(props), ":", props, important, ";", collapse = collapse_)
}

empty <- function(x) {
  length(x) == 0 || (is.character(x) && !any(nzchar(x)))
}
