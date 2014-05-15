#' @import digest
NULL

depListToNamedDepList <- function(dependencies) {
  if (inherits(dependencies, "html_dependency"))
    dependencies <- list(dependencies)

  if (is.null(names(dependencies))) {
    names(dependencies) <- sapply(dependencies, `[[`, "name")
  }
  return(dependencies)
}

# Given a list of dependencies, choose only the latest versions.
#' @export
resolveDependencies <- function(dependencies) {
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
    deps[[sorted[[1]]]]
  }))
}

# Remove `remove` from `dependencies` if the name matches.
# dependencies is a named list of dependencies.
# remove is a named list of dependencies that take priority.
# If warnOnConflict, then warn when a dependency is being removed because of an
# older version already being loaded.
#' @export
subtractDependencies <- function(dependencies, remove, warnOnConflict = TRUE) {
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


# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0
}
# Given a vector or list, drop all the NULL items in it
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE=logical(1))]
}

isTag <- function(x) {
  inherits(x, "shiny.tag")
}

#' @export
print.shiny.tag <- function(x, ...) {
  print(as.character(x), ...)
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
  renderTags(x)$html
}

#' @export
print.shiny.tag.list <- print.shiny.tag

#' @export
format.shiny.tag.list <- format.shiny.tag

#' @export
as.character.shiny.tag.list <- as.character.shiny.tag

#' @export
print.html <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}

#' @export
format.html <- function(x, ...) {
  as.character(x)
}

normalizeText <- function(text) {
  if (!is.null(attr(text, "html")))
    text
  else
    htmlEscape(text, attribute=FALSE)

}

#' @rdname tag
#' @export
tagList <- function(...) {
  lst <- list(...)
  class(lst) <- c("shiny.tag.list", "list")
  return(lst)
}

#' @rdname tag
#' @export
tagAppendAttributes <- function(tag, ...) {
  tag$attribs <- c(tag$attribs, list(...))
  tag
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
  tag$children <- c(tag$children, c(list(...), list))
  tag
}

#' @rdname tag
#' @export
tagSetChildren <- function(tag, ..., list = NULL) {
  tag$children <- c(list(...), list)
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
tag <- function(`_tag_name`, varArgs) {
  # Get arg names; if not a named list, use vector of empty strings
  varArgsNames <- names(varArgs)
  if (is.null(varArgsNames))
    varArgsNames <- character(length=length(varArgs))

  # Named arguments become attribs, dropping NULL values
  named_idx <- nzchar(varArgsNames)
  attribs <- dropNulls(varArgs[named_idx])

  # Unnamed arguments are flattened and added as children.
  # Use unname() to remove the names attribute from the list, which would
  # consist of empty strings anyway.
  children <- unname(varArgs[!named_idx])

  # Return tag data structure
  structure(
    list(name = `_tag_name`,
      attribs = attribs,
      children = children),
    class = "shiny.tag"
  )
}

tagWrite <- function(tag, textWriter, indent=0, eol = "\n") {

  if (length(tag) == 0)
    return (NULL)

  # optionally process a list of tags
  if (!isTag(tag) && is.list(tag)) {
    tag <- dropNullsOrEmpty(flattenTags(tag))
    lapply(tag, tagWrite, textWriter, indent)
    return (NULL)
  }

  nextIndent <- if (is.numeric(indent)) indent + 1 else indent
  indent <- if (is.numeric(indent)) indent else 0

  # compute indent text
  indentText <- paste(rep(" ", indent*2), collapse="")

  # Check if it's just text (may either be plain-text or HTML)
  if (is.character(tag)) {
    textWriter(paste(indentText, normalizeText(tag), eol, sep=""))
    return (NULL)
  }

  # write tag name
  textWriter(paste(indentText, "<", tag$name, sep=""))

  # Convert all attribs to chars explicitly; prevents us from messing up factors
  attribs <- lapply(tag$attribs, as.character)
  # concatenate attributes
  # split() is very slow, so avoid it if possible
  if (anyDuplicated(names(attribs)))
    attribs <- lapply(split(attribs, names(attribs)), paste, collapse = " ")

  # write attributes
  for (attrib in names(attribs)) {
    attribValue <- attribs[[attrib]]
    if (!is.na(attribValue)) {
      if (is.logical(attribValue))
        attribValue <- tolower(attribValue)
      text <- htmlEscape(attribValue, attribute=TRUE)
      textWriter(paste(" ", attrib,"=\"", text, "\"", sep=""))
    }
    else {
      textWriter(paste(" ", attrib, sep=""))
    }
  }

  # write any children
  children <- dropNullsOrEmpty(flattenTags(tag$children))
  if (length(children) > 0) {
    textWriter(">")

    # special case for a single child text node (skip newlines and indentation)
    if ((length(children) == 1) && is.character(children[[1]]) ) {
      textWriter(paste(normalizeText(children[[1]]), "</", tag$name, ">", eol,
        sep=""))
    }
    else {
      textWriter("\n")
      for (child in children)
        tagWrite(child, textWriter, nextIndent)
      textWriter(paste(indentText, "</", tag$name, ">", eol, sep=""))
    }
  }
  else {
    # only self-close void elements
    # (see: http://dev.w3.org/html5/spec/single-page.html#void-elements)
    if (tag$name %in% c("area", "base", "br", "col", "command", "embed", "hr",
      "img", "input", "keygen", "link", "meta", "param",
      "source", "track", "wbr")) {
      textWriter(paste("/>", eol, sep=""))
    }
    else {
      textWriter(paste("></", tag$name, ">", eol, sep=""))
    }
  }
}

#' @export
doRenderTags <- function(ui, indent = 0) {
  # Render the body--the bodyHtml variable will be created
  conn <- file(open="w+")
  connWriter <- function(text) writeChar(text, conn, eos = NULL)
  htmlResult <- tryCatch({
    tagWrite(ui, connWriter, indent)
    flush(conn)
    readLines(conn)
  },
    finally = close(conn)
  )
  return(HTML(paste(htmlResult, collapse = "\n")))
}

#' @export
renderTags <- function(ui, singletons = character(0), indent = 0) {
  # Do singleton and head processing before rendering
  singletonInfo <- takeSingletons(ui, singletons)
  headInfo <- takeHeads(singletonInfo$ui)
  deps <- resolveDependencies(findDependencies(singletonInfo$ui))

  headIndent <- if (is.numeric(indent)) indent + 1 else indent
  headHtml <- doRenderTags(headInfo$head, indent = headIndent)
  bodyHtml <- doRenderTags(headInfo$ui, indent = indent)

  return(list(head = headHtml,
    singletons = singletonInfo$singletons,
    dependencies = deps,
    html = bodyHtml))
}

# Walk a tree of tag objects, rewriting objects according to func.
# preorder=TRUE means preorder tree traversal, that is, an object
# should be rewritten before its children.
rewriteTags <- function(ui, func, preorder) {
  if (preorder)
    ui <- func(ui)

  if (isTag(ui)) {
    ui$children[] <- lapply(ui$children, rewriteTags, func, preorder)
  } else if (is.list(ui)) {
    ui[] <- lapply(ui, rewriteTags, func, preorder)
  }

  if (!preorder)
    ui <- func(ui)

  return(ui)
}

# Preprocess a tag object by changing any singleton X into
# <!--SHINY.SINGLETON[sig]-->X'<!--/SHINY.SINGLETON[sig]-->
# where sig is the sha1 of X, and X' is X minus the singleton
# attribute.
#
# In the case of nested singletons, outer singletons are processed
# before inner singletons (otherwise the processing of inner
# singletons would cause the sha1 of the outer singletons to be
# different).
#' Singleton manipulation functions
#'
#' @rdname singleton-manipulation
#' @export
surroundSingletons <- local({
  surroundSingleton <- function(uiObj) {
    if (inherits(uiObj, "shiny.singleton")) {
      sig <- digest(uiObj, "sha1")
      class(uiObj) <- class(uiObj)[class(uiObj) != "shiny.singleton"]
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

# Given a tag object, apply singleton logic (allow singleton objects
# to appear no more than once per signature) and return the processed
# HTML objects and also the list of known singletons.
#' @rdname singleton-manipulation
#' @export
takeSingletons <- function(ui, singletons=character(0), desingleton=TRUE) {
  result <- rewriteTags(ui, function(uiObj) {
    if (inherits(uiObj, "shiny.singleton")) {
      sig <- digest(uiObj, "sha1")
      if (sig %in% singletons)
        return(NULL)
      singletons <<- append(singletons, sig)
      if (desingleton)
        class(uiObj) <- class(uiObj)[class(uiObj) != "shiny.singleton"]
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

#' @export
findDependencies <- function(ui) {
  dep <- attr(ui, "html_dependencies")
  if (!is.null(dep) && inherits(dep, "html_dependency"))
    dep <- list(dep)
  children <- if (is.list(ui)) {
    if (isTag(ui)) {
      ui$children
    } else {
      ui
    }
  }
  childDeps <- unlist(lapply(children, findDependencies), recursive = FALSE)
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
#'   tags, single-character character vectors (which become text nodes), and raw
#'   HTML (see \code{\link{HTML}}). You can also pass lists that contain tags,
#'   text nodes, and HTML.
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
NULL

#' @rdname builder
#' @format NULL
#' @docType NULL
#' @keywords NULL
tags <- list(
  a = function(...) tag("a", list(...)),
  abbr = function(...) tag("abbr", list(...)),
  address = function(...) tag("address", list(...)),
  area = function(...) tag("area", list(...)),
  article = function(...) tag("article", list(...)),
  aside = function(...) tag("aside", list(...)),
  audio = function(...) tag("audio", list(...)),
  b = function(...) tag("b", list(...)),
  base = function(...) tag("base", list(...)),
  bdi = function(...) tag("bdi", list(...)),
  bdo = function(...) tag("bdo", list(...)),
  blockquote = function(...) tag("blockquote", list(...)),
  body = function(...) tag("body", list(...)),
  br = function(...) tag("br", list(...)),
  button = function(...) tag("button", list(...)),
  canvas = function(...) tag("canvas", list(...)),
  caption = function(...) tag("caption", list(...)),
  cite = function(...) tag("cite", list(...)),
  code = function(...) tag("code", list(...)),
  col = function(...) tag("col", list(...)),
  colgroup = function(...) tag("colgroup", list(...)),
  command = function(...) tag("command", list(...)),
  data = function(...) tag("data", list(...)),
  datalist = function(...) tag("datalist", list(...)),
  dd = function(...) tag("dd", list(...)),
  del = function(...) tag("del", list(...)),
  details = function(...) tag("details", list(...)),
  dfn = function(...) tag("dfn", list(...)),
  div = function(...) tag("div", list(...)),
  dl = function(...) tag("dl", list(...)),
  dt = function(...) tag("dt", list(...)),
  em = function(...) tag("em", list(...)),
  embed = function(...) tag("embed", list(...)),
  eventsource = function(...) tag("eventsource", list(...)),
  fieldset = function(...) tag("fieldset", list(...)),
  figcaption = function(...) tag("figcaption", list(...)),
  figure = function(...) tag("figure", list(...)),
  footer = function(...) tag("footer", list(...)),
  form = function(...) tag("form", list(...)),
  h1 = function(...) tag("h1", list(...)),
  h2 = function(...) tag("h2", list(...)),
  h3 = function(...) tag("h3", list(...)),
  h4 = function(...) tag("h4", list(...)),
  h5 = function(...) tag("h5", list(...)),
  h6 = function(...) tag("h6", list(...)),
  head = function(...) tag("head", list(...)),
  header = function(...) tag("header", list(...)),
  hgroup = function(...) tag("hgroup", list(...)),
  hr = function(...) tag("hr", list(...)),
  html = function(...) tag("html", list(...)),
  i = function(...) tag("i", list(...)),
  iframe = function(...) tag("iframe", list(...)),
  img = function(...) tag("img", list(...)),
  input = function(...) tag("input", list(...)),
  ins = function(...) tag("ins", list(...)),
  kbd = function(...) tag("kbd", list(...)),
  keygen = function(...) tag("keygen", list(...)),
  label = function(...) tag("label", list(...)),
  legend = function(...) tag("legend", list(...)),
  li = function(...) tag("li", list(...)),
  link = function(...) tag("link", list(...)),
  mark = function(...) tag("mark", list(...)),
  map = function(...) tag("map", list(...)),
  menu = function(...) tag("menu", list(...)),
  meta = function(...) tag("meta", list(...)),
  meter = function(...) tag("meter", list(...)),
  nav = function(...) tag("nav", list(...)),
  noscript = function(...) tag("noscript", list(...)),
  object = function(...) tag("object", list(...)),
  ol = function(...) tag("ol", list(...)),
  optgroup = function(...) tag("optgroup", list(...)),
  option = function(...) tag("option", list(...)),
  output = function(...) tag("output", list(...)),
  p = function(...) tag("p", list(...)),
  param = function(...) tag("param", list(...)),
  pre = function(...) tag("pre", list(...)),
  progress = function(...) tag("progress", list(...)),
  q = function(...) tag("q", list(...)),
  ruby = function(...) tag("ruby", list(...)),
  rp = function(...) tag("rp", list(...)),
  rt = function(...) tag("rt", list(...)),
  s = function(...) tag("s", list(...)),
  samp = function(...) tag("samp", list(...)),
  script = function(...) tag("script", list(...)),
  section = function(...) tag("section", list(...)),
  select = function(...) tag("select", list(...)),
  small = function(...) tag("small", list(...)),
  source = function(...) tag("source", list(...)),
  span = function(...) tag("span", list(...)),
  strong = function(...) tag("strong", list(...)),
  style = function(...) tag("style", list(...)),
  sub = function(...) tag("sub", list(...)),
  summary = function(...) tag("summary", list(...)),
  sup = function(...) tag("sup", list(...)),
  table = function(...) tag("table", list(...)),
  tbody = function(...) tag("tbody", list(...)),
  td = function(...) tag("td", list(...)),
  textarea = function(...) tag("textarea", list(...)),
  tfoot = function(...) tag("tfoot", list(...)),
  th = function(...) tag("th", list(...)),
  thead = function(...) tag("thead", list(...)),
  time = function(...) tag("time", list(...)),
  title = function(...) tag("title", list(...)),
  tr = function(...) tag("tr", list(...)),
  track = function(...) tag("track", list(...)),
  u = function(...) tag("u", list(...)),
  ul = function(...) tag("ul", list(...)),
  var = function(...) tag("var", list(...)),
  video = function(...) tag("video", list(...)),
  wbr = function(...) tag("wbr", list(...))
)

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
  htmlText <- c(text, as.character(list(...)))
  htmlText <- paste(htmlText, collapse=" ")
  attr(htmlText, "html") <- TRUE
  class(htmlText) <- c("html", "character")
  htmlText
}

#' Evaluate an expression using the \code{tags}
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


# Given a list of tags, lists, and other items, return a flat list, where the
# items from the inner, nested lists are pulled to the top level, recursively.
flattenTags <- function(x) {
  if (isTag(x)) {
    # For tags, wrap them into a list (which will be unwrapped by caller)
    list(x)
  } else if (is.list(x)) {
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
#'
#' @export
as.tags <- function(x) {
  UseMethod("as.tags")
}

#' @export
as.tags.default <- function(x) {
  as.character(x)
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
  x <- paste(x, collapse = "\r\n")
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
  lengths <- attr(matches, "match.length")

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
  for (id in names(chunks))
    strval <- gsub(id, chunks[[id]], strval, fixed = TRUE, useBytes = TRUE)
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
  output <- surroundSingletons(x)
  deps <- resolveDependencies(findDependencies(x))
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

knit_print.html <- function(x, ...) {
  deps <- resolveDependencies(findDependencies(x))
  knitr::asis_output(htmlPreserve(as.character(x)),
    meta = if (length(deps)) list(deps))
}

#' @rdname knitr_methods
#' @export
knit_print.shiny.tag.list <- knit_print.shiny.tag



#' @rdname builder
#' @export
p <- function(...) tags$p(...)

#' @rdname builder
#' @export
h1 <- function(...) tags$h1(...)

#' @rdname builder
#' @export
h2 <- function(...) tags$h2(...)

#' @rdname builder
#' @export
h3 <- function(...) tags$h3(...)

#' @rdname builder
#' @export
h4 <- function(...) tags$h4(...)

#' @rdname builder
#' @export
h5 <- function(...) tags$h5(...)

#' @rdname builder
#' @export
h6 <- function(...) tags$h6(...)

#' @rdname builder
#' @export
a <- function(...) tags$a(...)

#' @rdname builder
#' @export
br <- function(...) tags$br(...)

#' @rdname builder
#' @export
div <- function(...) tags$div(...)

#' @rdname builder
#' @export
span <- function(...) tags$span(...)

#' @rdname builder
#' @export
pre <- function(...) tags$pre(...)

#' @rdname builder
#' @export
code <- function(...) tags$code(...)

#' @rdname builder
#' @export
img <- function(...) tags$img(...)

#' @rdname builder
#' @export
strong <- function(...) tags$strong(...)

#' @rdname builder
#' @export
em <- function(...) tags$em(...)

#' @rdname builder
#' @export
hr <- function(...) tags$hr(...)

#' Include Content From a File
#'
#' Include HTML, text, or rendered Markdown into a \link[=shinyUI]{Shiny UI}.
#'
#' These functions provide a convenient way to include an extensive amount of
#' HTML, textual, Markdown, CSS, or JavaScript content, rather than using a
#' large literal R string.
#'
#' @note \code{includeText} escapes its contents, but does no other processing.
#'   This means that hard breaks and multiple spaces will be rendered as they
#'   usually are in HTML: as a single space character. If you are looking for
#'   preformatted text, wrap the call with \code{\link{pre}}, or consider using
#'   \code{includeMarkdown} instead.
#'
#' @note The \code{includeMarkdown} function requires the \code{markdown}
#'   package.
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
  return(HTML(paste(lines, collapse='\r\n')))
}

#' @rdname include
#' @export
includeText <- function(path) {
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(paste(lines, collapse='\r\n'))
}

#' @rdname include
#' @export
includeMarkdown <- function(path) {
  library(markdown)

  html <- markdown::markdownToHTML(path, fragment.only=TRUE)
  Encoding(html) <- 'UTF-8'
  return(HTML(html))
}

#' @param ... Any additional attributes to be applied to the generated tag.
#' @rdname include
#' @export
includeCSS <- function(path, ...) {
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  args <- list(...)
  if (is.null(args$type))
    args$type <- 'text/css'
  return(do.call(tags$style,
    c(list(HTML(paste(lines, collapse='\r\n'))), args)))
}

#' @rdname include
#' @export
includeScript <- function(path, ...) {
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(tags$script(HTML(paste(lines, collapse='\r\n')), ...))
}

#' Include Content Only Once
#'
#' Use \code{singleton} to wrap contents (tag, text, HTML, or lists) that should
#' be included in the generated document only once, yet may appear in the
#' document-generating code more than once. Only the first appearance of the
#' content (in document order) will be used. Useful for custom components that
#' have JavaScript files or stylesheets.
#'
#' @param x A \code{\link{tag}}, text, \code{\link{HTML}}, or list.
#'
#' @export
singleton <- function(x) {
  class(x) <- c(class(x), 'shiny.singleton')
  return(x)
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
#' or a number. If the number has a suffix, it must be valid: \code{px},
#' \code{\%}, \code{em}, \code{pt}, \code{in}, \code{cm}, \code{mm}, \code{ex},
#' or \code{pc}. If the number has no suffix, the suffix \code{"px"} is
#' appended.
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
    "^(auto|inherit|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|em|ex|pt|pc|px))$"

  if (is.character(x) &&
      !grepl(pattern, x)) {
    stop('"', x, '" is not a valid CSS unit (e.g., "100%", "400px", "auto")')
  } else if (is.numeric(x)) {
    x <- paste(x, "px", sep = "")
  }
  x
}
