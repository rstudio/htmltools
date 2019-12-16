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
#' @param dependencies A list of \code{\link{htmlDependency}} objects.
#' @param resolvePackageDir Whether to resolve the relative path to an absolute
#'   path via \code{\link{system.file}} when the \code{package} attribute is
#'   present in a dependency object.
#' @return dependencies A list of \code{\link{htmlDependency}} objects with
#'   redundancies removed.
#'
#' @export
resolveDependencies <- function(dependencies, resolvePackageDir = TRUE) {
  # Remove nulls
  deps <- dependencies[!sapply(dependencies, is.null)]

  # Get names and numeric versions in vector/list form
  depnames <- sapply(deps, `[[`, "name")
  depvers <- numeric_version(sapply(deps, `[[`, "version"))

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
#' @param dependencies A list of \code{\link{htmlDependency}} objects from which
#'   dependencies should be removed.
#' @param remove A list of \code{\link{htmlDependency}} objects indicating which
#'   dependencies should be removed, or a character vector indicating dependency
#'   names.
#' @param warnOnConflict If \code{TRUE}, a warning is emitted for each
#'   dependency that is removed if the corresponding dependency in \code{remove}
#'   has a lower version number. Has no effect if \code{remove} is provided as a
#'   character vector.
#'
#' @return A list of \code{\link{htmlDependency}} objects that don't intersect
#'   with \code{remove}.
#'
#' @export
subtractDependencies <- function(dependencies, remove, warnOnConflict = TRUE) {
  depnames <- sapply(dependencies, `[[`, "name")
  rmnames <- if (is.character(remove))
    remove
  else
    sapply(remove, `[[`, "name")

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
print.shiny.tag.list <- print.shiny.tag

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
#' @param browse If \code{TRUE}, the HTML will be rendered and displayed in a
#'   browser (or possibly another HTML viewer supplied by the environment via
#'   the \code{viewer} option). If \code{FALSE} then the HTML object's markup
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

#' @name tag
#' @rdname tag
#' @import rlang
#' @export
tagList <- function(...) {
  lst <- dots_list(...)
  class(lst) <- c("shiny.tag.list", "list")
  return(lst)
}

#' @rdname tag
#' @export
tagAppendAttributes <- function(tag, ...) {
  tag$attribs <- c(tag$attribs, dropNullsOrEmpty(dots_list(...)))
  tag
}

#' @param attr The name of an attribute.
#' @rdname tag
#' @export
tagHasAttribute <- function(tag, attr) {
  result <- attr %in% names(tag$attribs)
  result
}

#' @rdname tag
#' @export
tagGetAttribute <- function(tag, attr) {
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

#' @rdname tag
#' @export
tagAppendChild <- function(tag, child) {
  tag$children[[length(tag$children)+1]] <- child
  tag
}

#' @rdname tag
#' @export
tagAppendChildren <- function(tag, ..., list = NULL) {
  tag$children <- unname(c(tag$children, c(dots_list(...), list)))
  tag
}

#' @rdname tag
#' @export
tagSetChildren <- function(tag, ..., list = NULL) {
  tag$children <- unname(c(dots_list(...), list))
  tag
}

#' HTML Tag Object
#'
#' \code{tag()} creates an HTML tag definition. Note that all of the valid HTML5
#' tags are already defined in the \code{\link{tags}} environment so these
#' functions should only be used to generate additional tags.
#' \code{tagAppendChild()} and \code{tagList()} are for supporting package
#' authors who wish to create their own sets of tags; see the contents of
#' bootstrap.R for examples.
#' @param _tag_name HTML tag name
#' @param varArgs List of attributes and children of the element. Named list
#'   items become attributes, and unnamed list items become children. Valid
#'   children are tags, single-character character vectors (which become text
#'   nodes), and raw HTML (see \code{\link{HTML}}). You can also pass lists that
#'   contain tags, text nodes, and HTML.
#' @param tag A tag to append child elements to.
#' @param child A child element to append to a parent tag.
#' @param ...  Unnamed items that comprise this list of tags.
#' @param list An optional list of elements. Can be used with or instead of the
#'   \code{...} items.
#' @param .noWS Character vector used to omit some of the whitespace that would
#'   normally be written around this tag. Valid options include \code{before},
#'   \code{after}, \code{outside}, \code{after-begin}, and \code{before-end}.
#'   Any number of these options can be specified.
#' @return An HTML tag object that can be rendered as HTML using
#'   \code{\link{as.character}()}.
#' @export
#' @examples
#' tagList(tags$h1("Title"),
#'         tags$h2("Header text"),
#'         tags$p("Text here"))
#'
#' # Can also convert a regular list to a tagList (internal data structure isn't
#' # exactly the same, but when rendered to HTML, the output is the same).
#' x <- list(tags$h1("Title"),
#'           tags$h2("Header text"),
#'           tags$p("Text here"))
#' tagList(x)
#'
#' # suppress the whitespace between tags
#' oneline <- tag("span",
#'   tag("strong", "Super strong", .noWS="outside")
#' )
#' cat(as.character(oneline))
tag <- function(`_tag_name`, varArgs, .noWS=NULL) {
  validateNoWS(.noWS)
  # Get arg names; if not a named list, use vector of empty strings
  varArgsNames <- names(varArgs)
  if (is.null(varArgsNames))
    varArgsNames <- character(length=length(varArgs))

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

  # Conditionally include the .noWS element. We do this to avoid breaking the hashes
  # of existing tags that weren't leveraging .noWS.
  if (!is.null(.noWS)){
    st$.noWS <- .noWS
  }

  # Return tag data structure
  structure(st, class = "shiny.tag")
}

isTagList <- function(x) {
  is.list(x) && (inherits(x, "shiny.tag.list") || identical(class(x), "list"))
}

noWSOptions <- c("before", "after", "after-begin", "before-end", "outside")
# Ensure that the provided `.noWS` string contains only valid options
validateNoWS <- function(.noWS){
  if (!all(.noWS %in% noWSOptions)){
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
    textWriter$write(normalizeText(tag))
    textWriter$writeWS(eol)
    return (NULL)
  }

  .noWS <- tag$.noWS

  if ("before" %in% .noWS || "outside" %in% .noWS) {
    textWriter$eatWS()
  }

  # write tag name
  textWriter$write(concat8("<", tag$name))

  # Convert all attribs to chars explicitly; prevents us from messing up factors
  attribs <- lapply(tag$attribs, as.character)
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

  # write attributes
  for (attrib in names(attribs)) {
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
#' \code{\link{as.tags}}) into HTML. (Generally intended to be called from web
#' framework libraries, not directly by most users--see
#' \code{\link{print.html}(browse=TRUE)} for higher level rendering.)
#'
#' @param x Tag object(s) to render
#' @param singletons A list of \link{singleton} signatures to consider already
#'   rendered; any matching singletons will be dropped instead of rendered.
#'   (This is useful (only?) for incremental rendering.)
#' @param indent Initial indent level, or \code{FALSE} if no indentation should
#'   be used.
#'
#' @return \code{renderTags} returns a list with the following variables:
#' \describe{
#'   \item{\code{head}}{An \code{\link{HTML}} string that should be included in
#'     \code{<head>}.
#'   }
#'   \item{\code{singletons}}{Character vector of singleton signatures that are
#'     known after rendering.
#'   }
#'   \item{\code{dependencies}}{A list of \link[=resolveDependencies]{resolved}
#'     \code{\link{htmlDependency}} objects.
#'   }
#'   \item{\code{html}}{An \code{\link{HTML}} string that represents the main
#'     HTML that was rendered.
#'   }
#' }
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

#' @details \code{doRenderTags} is intended for very low-level use; it ignores
#'   singleton, head, and dependency handling, and simply renders the given tag
#'   objects as HTML.
#' @return \code{doRenderTags} returns a simple \code{\link{HTML}} string.
#' @rdname renderTags
#' @export
doRenderTags <- function(x, indent = 0) {
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
#' Functions for manipulating \code{\link{singleton}} objects in tag
#' hierarchies. Intended for framework authors.
#'
#' @rdname singleton_tools
#' @name singleton_tools
NULL

#' @param ui Tag object or lists of tag objects. See \link{builder} topic.
#' @return \code{surroundSingletons} preprocesses a tag object by changing any
#'   singleton X into <!--SHINY.SINGLETON[sig]-->X'<!--/SHINY.SINGLETON[sig]-->
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
#'   \code{takeSingletons}).
#' @param desingleton Logical value indicating whether singletons that are
#'   encountered should have the singleton attribute removed.
#' @return \code{takeSingletons} returns a list with the elements \code{ui} (the
#'   processed tag objects with any duplicate singleton objects removed) and
#'   \code{singletons} (the list of known singleton signatures).
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
#' @return A list of \code{\link{htmlDependency}} objects.
#'
#' @export
findDependencies <- function(tags, tagify = TRUE) {
  if (isTRUE(tagify)) {
    tags <- tagify(tags)
  }
  dep <- htmlDependencies(tags)
  if (!is.null(dep) && inherits(dep, "html_dependency"))
    dep <- list(dep)
  children <- if (is.list(tags)) {
    if (isTag(tags)) {
      tags$children
    } else {
      tags
    }
  }
  childDeps <- unlist(lapply(children, findDependencies, tagify = FALSE), recursive = FALSE)
  c(childDeps, if (!is.null(dep)) dep)
}

#' HTML Builder Functions
#'
#' Simple functions for constructing HTML documents.
#'
#' The \code{tags} environment contains convenience functions for all valid
#' HTML5 tags. To generate tags that are not part of the HTML5 specification,
#' you can use the \code{\link{tag}()} function.
#'
#' Dedicated functions are available for the most common HTML tags that do not
#' conflict with common R functions.
#'
#' The result from these functions is a tag object, which can be converted using
#' \code{\link{as.character}()}.
#'
#' @name builder
#' @param ... Attributes and children of the element. Named arguments become
#'   attributes, and positional arguments become children. Valid children are
#'   tags, single-character character vectors (which become text nodes), raw
#'   HTML (see \code{\link{HTML}}), and \code{html_dependency} objects. You can
#'   also pass lists that contain tags, text nodes, or HTML. To use boolean
#'   attributes, use a named argument with a \code{NA} value. (see example)
#' @param .noWS A character vector used to omit some of the whitespace that
#'   would normally be written around this tag. Valid options include
#'   \code{before}, \code{after}, \code{outside}, \code{after-begin}, and
#'   \code{before-end}. Any number of these options can be specified.
#' @references \itemize{
#'    \item W3C html specification about boolean attributes
#'    \url{https://www.w3.org/TR/html5/infrastructure.html#sec-boolean-attributes}
#'  }
#' @export tags
#' @examples
#' doc <- tags$html(
#'   tags$head(
#'     tags$title('My first page')
#'   ),
#'   tags$body(
#'     h1('My first heading'),
#'     p('My first paragraph, with some ',
#'       strong('bold'),
#'       ' text.'),
#'     div(id='myDiv', class='simpleDiv',
#'         'Here is a div with some attributes.')
#'   )
#' )
#' cat(as.character(doc))
#'
#' # create an html5 audio tag with controls.
#' # controls is a boolean attributes
#' audio_tag <- tags$audio(
#'   controls = NA,
#'   tags$source(
#'     src = "myfile.wav",
#'     type = "audio/wav"
#'   )
#' )
#' cat(as.character(audio_tag))
#'
#' # suppress the whitespace between tags
#' oneline <- tags$span(
#'   tags$strong("I'm strong", .noWS="outside")
#' )
#' cat(as.character(oneline))
NULL


known_tags <- c(
  "a",
  "abbr",
  "address",
  "area",
  "article",
  "aside",
  "audio",
  "b",
  "base",
  "bdi",
  "bdo",
  "blockquote",
  "body",
  "br",
  "button",
  "canvas",
  "caption",
  "cite",
  "code",
  "col",
  "colgroup",
  "command",
  "data",
  "datalist",
  "dd",
  "del",
  "details",
  "dfn",
  "dialog",
  "div",
  "dl",
  "dt",
  "em",
  "embed",
  "eventsource",
  "fieldset",
  "figcaption",
  "figure",
  "footer",
  "form",
  "h1",
  "h2",
  "h3",
  "h4",
  "h5",
  "h6",
  "head",
  "header",
  "hgroup",
  "hr",
  "html",
  "i",
  "iframe",
  "img",
  "input",
  "ins",
  "kbd",
  "keygen",
  "label",
  "legend",
  "li",
  "link",
  "main",
  "mark",
  "map",
  "menu",
  "meta",
  "meter",
  "nav",
  "noscript",
  "object",
  "ol",
  "optgroup",
  "option",
  "output",
  "p",
  "param",
  "picture",
  "pre",
  "progress",
  "q",
  "rp",
  "rt",
  "ruby",
  "s",
  "samp",
  "script",
  "section",
  "select",
  "small",
  "source",
  "span",
  "strong",
  "style",
  "sub",
  "summary",
  "sup",
  "table",
  "tbody",
  "td",
  "template",
  "textarea",
  "tfoot",
  "th",
  "thead",
  "time",
  "title",
  "tr",
  "track",
  "u",
  "ul",
  "var",
  "video",
  "wbr"
)
names(known_tags) <- known_tags

#' @rdname builder
#' @format NULL
#' @docType NULL
#' @keywords NULL
#' @import rlang
tags <- lapply(known_tags, function(tagname) {
  function(..., .noWS=NULL) {
    validateNoWS(.noWS)
    contents <- dots_list(...)
    tag(tagname, contents, .noWS=.noWS)
  }
})

# known_tags is no longer needed, so remove it.
rm(known_tags)


#' Mark Characters as HTML
#'
#' Marks the given text as HTML, which means the \link{tag} functions will know
#' not to perform HTML escaping on it.
#'
#' @param text The text value to mark with HTML
#' @param ... Any additional values to be converted to character and
#'   concatenated together
#' @return The same value, but marked as HTML.
#'
#' @examples
#' el <- div(HTML("I like <u>turtles</u>"))
#' cat(as.character(el))
#'
#' @export
HTML <- function(text, ...) {
  htmlText <- c(text, as.character(dots_list(...)))
  htmlText <- paste8(htmlText, collapse=" ")
  attr(htmlText, "html") <- TRUE
  class(htmlText) <- c("html", "character")
  htmlText
}

#' Evaluate an expression using \code{tags}
#'
#' This function makes it simpler to write HTML-generating code. Instead of
#' needing to specify \code{tags} each time a tag function is used, as in
#' \code{tags$div()} and \code{tags$p()}, code inside \code{withTags} is
#' evaluated with \code{tags} searched first, so you can simply use
#' \code{div()} and \code{p()}.
#'
#' If your code uses an object which happens to have the same name as an
#' HTML tag function, such as \code{source()} or \code{summary()}, it will call
#' the tag function. To call the intended (non-tags function), specify the
#' namespace, as in \code{base::source()} or \code{base::summary()}.
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
    if (isTag(uiObj) || isTagList(uiObj) || is.character(uiObj))
      return(uiObj)
    else
      return(tagify(as.tags(uiObj)))
  }, FALSE)
}

# Given a list of tags, lists, and other items, return a flat list, where the
# items from the inner, nested lists are pulled to the top level, recursively.
flattenTags <- function(x) {
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

#' Convert a value to tags
#'
#' An S3 method for converting arbitrary values to a value that can be used as
#' the child of a tag or \code{tagList}. The default implementation simply calls
#' \code{\link[base]{as.character}}.
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
  if (is.list(x) && !isTagList(x))
    unclass(x)
  else
    tagList(as.character(x))
}

#' @export
as.tags.html <- function(x, ...) {
  x
}

#' @export
as.tags.shiny.tag <- function(x, ...) {
  x
}

#' @export
as.tags.shiny.tag.list <- function(x, ...) {
  x
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
#' their markup is interpreted as the input language. The \code{htmlPreserve}
#' function is used to mark regions of an input document as containing pure HTML
#' that must not be modified. This is achieved by substituting each such region
#' with a benign but unique string before processing, and undoing those
#' substitutions after processing.
#'
#' @param x A character vector of HTML to be preserved.
#'
#' @return \code{htmlPreserve} returns a single-element character vector with
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
  x <- paste(x, collapse = "\n")
  if (nzchar(x))
    sprintf("<!--html_preserve-->%s<!--/html_preserve-->", x)
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

#' @return \code{extractPreserveChunks} returns a list with two named elements:
#'   \code{value} is the string with the regions replaced, and \code{chunks} is
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
  matches <- gregexpr(pattern, strval)[[1]]
  lengths <- attr(matches, "match.length", TRUE)

  # No markers? Just return.
  if (matches[[1]] == -1)
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
#' @param chunks The \code{chunks} element of the return value of
#'   \code{extractPreserveChunks}.
#' @return \code{restorePreserveChunks} returns a character vector with the
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

#' @note \code{includeText} escapes its contents, but does no other processing.
#'   This means that hard breaks and multiple spaces will be rendered as they
#'   usually are in HTML: as a single space character. If you are looking for
#'   preformatted text, wrap the call with \code{\link{pre}}, or consider using
#'   \code{includeMarkdown} instead.
#'
#' @rdname include
#' @export
includeText <- function(path) {
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(paste8(lines, collapse='\n'))
}

#' @note The \code{includeMarkdown} function requires the \code{markdown}
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
#' Use \code{singleton} to wrap contents (tag, text, HTML, or lists) that should
#' be included in the generated document only once, yet may appear in the
#' document-generating code more than once. Only the first appearance of the
#' content (in document order) will be used.
#'
#' @param x A \code{\link{tag}}, text, \code{\link{HTML}}, or list.
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
#' \code{NULL} and \code{NA} are returned unchanged.
#'
#' Single element numeric vectors are returned as a character vector with the
#' number plus a suffix of \code{"px"}.
#'
#' Single element character vectors must be \code{"auto"} or \code{"inherit"},
#' a number, or a length calculated by the \code{"calc"} CSS function.
#' If the number has a suffix, it must be valid: \code{px},
#' \code{\%}, \code{ch}, \code{em}, \code{rem}, \code{pt}, \code{in}, \code{cm},
#' \code{mm}, \code{ex}, \code{pc}, \code{vh}, \code{vw}, \code{vmin}, or
#' \code{vmax}.
#' If the number has no suffix, the suffix \code{"px"} is appended.
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
    "^(auto|inherit|calc\\(.*\\)|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|ch|em|ex|rem|pt|pc|px|vh|vw|vmin|vmax))$"

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
#' CSS uses \code{'-'} (minus) as a separator character in property names, but
#' this is an inconvenient character to use in an R function argument name.
#' Instead, you can use \code{'.'} (period) and/or \code{'_'} (underscore) as
#' separator characters. For example, \code{css(font.size = "12px")} yields
#' \code{"font-size:12px;"}.
#'
#' To mark a property as \code{!important}, add a \code{'!'} character to the end
#' of the property name. (Since \code{'!'} is not normally a character that can be
#' used in an identifier in R, you'll need to put the name in double quotes or
#' backticks.)
#'
#' Argument values will be converted to strings using
#' \code{paste(collapse = " ")}. Any property with a value of \code{NULL} or
#' \code{""} (after paste) will be dropped.
#'
#' @param ... Named style properties, where the name is the property name and
#'   the argument is the property value. See Details for conversion rules.
#' @param collapse_ (Note that the parameter name has a trailing underscore
#'   character.) Character to use to collapse properties into a single string;
#'   likely \code{""} (the default) for style attributes, and either \code{"\n"}
#'   or \code{NULL} for style blocks.
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
