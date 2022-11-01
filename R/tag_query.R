#' @importFrom fastmap fastmap faststack
NULL

# TODO-barret followup PR
# * onRender(x, fn) - tagFunction(x, fn)

## Methods not implemented
# * `$set_selected(selected)` & `$set(selected_item, pos)` - These methods are
# not available in jQuery and is very brittle in implementation. Do not pursue!
# * With `$set(selected, pos)` not implemented, `[[<-.tagQuery` should not be
# implemented
# * With `$set(selected, pos_vector)` not implemented, `[<-.tagQuery` should not
# be implemented
# * If not doing, `[[<-.tagQuery` or `[<-.tagQuery`, then `[[.tagQuery` and
# `[.tagQuery` should not be implemented. Same with `length.tagQuery`
# * `$set_children(...)` - jQuery does not have this. Instead, you can call
# `$empty()$append(...)`
# * jQuery.val() - Get the current value of the first element in the set of
# matched elements or set the value of every matched element.
# * jQuery.text() - Get the combined text contents of each element in the set of
# matched elements, including their descendants, or set the text contents of the
# matched elements.
# * jQuery.css() - Get the value of a computed style property for the first
# element in the set of matched elements or set one or more CSS properties for
# every matched element.
# * jQuery.prop() - Get the value of a property for the first element in the set
# of matched elements or set one or more properties for every matched element.




## Skip these implementations for now as the tagQuery methods are small and composable.
## Instead write them where they are needed since they are small.
## (Just like we don't wrap dplyr code)
# tagAppendAttributesAt <- function(tag, cssSelector, ...) {
#   tagQuery(tag)$find(cssSelector)$addAttrs(...)$allTags()
# }
# tagAddClassAt <- function(tag, cssSelector, class) {
#   tagQuery(tag)$find(cssSelector)$addClass(class)$allTags()
# }
# tagMutateAt <- function(x, cssSelector, fn) {
#   tagQuery(tag)$find(cssSelector)$each(fn)$allTags()
# }
# tagFindAt <- function(x, cssSelector) {
#   tagQuery(tag)$find(cssSelector)$selectedTags()
# }


# # Design notes for tagQuery:
# ## Using stock R objects
#
# Advantages of standard R objects recursion
# * Environments must be handled with care as they are pass by reference
# * It is easy to alter the current object in place
# * Difficult to create a new search path while altering in place
#
# Disadvantages of standard R objects recursion
# * Asking for a grandparent element is difficult.
#   * Altering a grandparent element and having the change stay is impossible
# * Searching would need to be done at each stage, every time

# ## Using environments elements
#
# Advantages
# * Fast to convert to a "linked list" of tag environments
# * Access to parents
# * Calculations on siblings can now be done, even after alterations have been completed
# * Once a `find(".x")` has been completed, a set of element environment pointers can be stored.
#   * This makes followup alterations have the minimal O(k) complexity (where k
#   is _found_ elements), not O(n) + O(k) graph search + reconstruction and k
#   _found_ element alterations
#
# Disadvantages
# * MUST be careful not alter the environment object before converting back to a
# list. (Ex: Do not remove the element environment's children)
# * The item returned is a set of environments that will alter in place. We will
# need to be careful about documenting and/or safeguarding this


# ## Final design choice:
# Use environment elements
# * Being able to search and have a list of eles to immediately look at and
# alter in place is AMAZING!
# * Being able to ask for a grandparent (or obj$parent$parent) and be able to
# alter it in place is AMAZING! This has a strongly influenced by jquery.

# ----------

# # Current design decisions
# * tagQuery objects or tag environments can NOT be used in UI. These objects
# MUST be converted back to standard tag objects.
# * tagFunctions will not be altered in place
#   * To alter tagFunction()s, use the `onRender(x)` method to register a method
#   to be called after `as.tags(x)` is called.
#   * `onRender(x, expr)` will wrap create a tag function that will resolve the
#   tags before running the expr.


## rlang::obj_address()
# Use to get a unique key for stacks
# Use `env$envKey` over `rlang::obj_address()`; 10x speed improvement

# Use for `has()` functionality
envirMap <- function() {
  map <- fastmap()
  list(
    keys = map$keys,
    asList = function() {
      unname(map$as_list())
    },
    has = function(envir) {
      map$has(envir$envKey)
    },
    add = function(envir) {
      map$set(envir$envKey, TRUE)
    },
    remove = function(envir) {
      map$remove(envir$envKey)
    }
  )
}
# Use for consistent `asList()` order
envirStack <- function() {
  stack <- faststack()
  list(
    push = stack$push,
    asList = stack$as_list,
    uniqueList = function() {
      unique(stack$as_list())
    }
  )
}

# (Used for `unique_envirStack()` only. Do not use directly!)
# Provides same interface as `envirStack()`, but checks for duplicates when
# when items are on their way in (with `push()`) instead of on the way out
# (with `uniqueList()`). This is faster when size is ~500 and above.
envirStackUni_ <- function() {
  map <- fastmap()
  stack <- faststack()
  list(
    push = function(env) {
      key <- env$envKey
      if (!map$has(key)) {
        # mark the key as _seen_
        map$set(key, TRUE)
        # add the env
        stack$push(env)
      }
    },
    uniqueList = stack$as_list
  )
}
# Use to retrieve unique environments (eg: `tq$parent()`)
# Provides same interface as `envirStack()`, but switches to the faster
# `envirStackUni_()` implementation when size hits 500.
envirStackUnique <- function() {
  stack <- envirStack()
  count <- 0
  list(
    push = function(env) {
      count <<- count + 1
      if (count == 500) {
        # convert the current stack to a `envirStackUni_()`
        newStack <- envirStackUni_()
        walk(stack$asList(), newStack$push)
        stack <<- newStack
      }
      stack$push(env)
    },
    uniqueList = function() {stack$uniqueList()}
  )
}




# Copy all attributes that can be manually set
# ?attr
# Note that some attributes (namely ‘class’, ‘comment’, ‘dim’,
# ‘dimnames’, ‘names’, ‘row.names’ and ‘tsp’) are treated specially
# and have restrictions on the values which can be set.
copyAttributes <- function(from, to) {
  attrVals <- attributes(from)
  attrNames <- names(attrVals)
  for (i in seq_along(attrNames)) {
    attrName <- attrNames[i]
    switch(
      attrName,
      class = , comment =, dim =, dimnames =, names =, row.names =, tsp = NULL,
      {
        # Copy over the attribute
        attr(to, attrName) <- attrVals[[i]]
      }
    )
  }

  to
}

# Convert a list to an environment and keep class and attribute information
safeListToEnv <- function(x, classToAdd = NULL) {
  xList <- x
  ret <- list2env(xList, new.env(parent = emptyenv()))
  ret <- copyAttributes(from = xList, to = ret)
  oldClass(ret) <- c(classToAdd, oldClass(xList))
  ret
}


# Convert any mixture of standard tag structures and tag environments into just
# tag environments.
#
# This method is heavily used within `tagQuery()$rebuild()` to enforce all
# standard tag objects are upgraded to tag environments.
#
# If the object is already a tag environment, it will recurse the conversion for
# each of the children
#
# Extras done:
# * Flatten all attributes by combining duplicate keys
# * Flatten the tag's children to a single list * Check for circular
# dependencies of tag environments
#
# (Do not export to encourage direct use of `tagQuery()`)
asTagEnv <- function(x) {
  if (isTagQuery(x)) {
    stop("`tagQuery()` object can not be provided to `asTagEnv()`")
  }

  if (!isTagEnv(x) && !isTag(x)) {
    # force all methods to send in tags, lists / tagLists are not allowed
    stop("`asTagEnv()` can only accept tag envs or tag objects. It does not accept `lists()` or `tagLists()`")
  }
  asTagEnv_(x, parent = x$parent)
}
# Checking for cycles is not performed as it is slow. With tagQuery methods not really
# opening the door for cycles to occur, it would be the user doing dangerous things.
# At this point, they should understand when a stack overflow occurs.
asTagEnv_ <- function(x, parent = NULL) {
  isTagVal <- isTag(x)
  isTagEnvVal <- isTagEnv(x)

  if (isTagVal || isTagEnvVal) {
    if (!isTagEnvVal) {
      xList <- x
      x <- safeListToEnv(xList, "shiny.tag.env")
      # add parent env and key
      x$parent <- parent
      x$envKey <- obj_address(x)
    }

    if (!is.character(x[["name"]])) {
      stop("A tag environment has lost its `$name`. Did you remove it?")
    }
    # This alters the env, but these fields should exist!
    if (is.null(x[["attribs"]])) x$attribs <- list(placeholder = NULL)[0] # Empty named list
    if (is.null(x[["children"]])) x$children <- list()

    # Recurse through children
    if (length(x$children) != 0) {
      # Possible optimization... name the children tags to the formatted values.
      # * Allows for faster child look up later.
      # * Comes with the cost of always formatting the env values even if children names are not needed.
      # Attributes may be dropped
      # * Could replace with `x$children[] <- ....`
      # * Leaving as is to see if people mis-use the children field

      # Simplify the structures by flatting the tags
      # Does NOT recurse to grand-children etc.
      children <- flattenTagsRaw(x$children)
      # Use a `for-loop` over `lapply` to avoid `lapply` overhead
      for (i in seq_along(children)) {
        child <- children[[i]]
        if (!is.null(child)) {
          children[[i]] <- asTagEnv_(child, parent = x)
        }
      }
      x$children <- children
    }
  }
  x
}

# This method MUST undo everything done in `asTagEnv(x)`
# Do not export to encourage direct use of `tagQuery()$selectedTags()`
# Only allow for tag environments to be passed in.
tagEnvToTags <- function(x) {
  if (!isTagEnv(x)) {
    stop("`tagEnvToTags(x)` must start with a tag environment")
  }
  tagEnvToTags_(x)
}
# Allows for all types of objects that can be put in a tag environment's `$children` field.
# Ex: tag environment, "text", 5, tagFunctions, etc.
tagEnvToTags_ <- function(x) {
  if (isTagEnv(x)) {

    xEl <- x

    # Pull the names `name`, `attribs`, and `children` first to match `tag()` name order
    envNames <- ls(envir = xEl, all.names = TRUE, sorted = FALSE)
    newNames <- c(
      "name", "attribs", "children",
      if (length(envNames) > 5) {
        # Pull remaining names if they exist
        removeFromSet(envNames, c("name", "attribs", "children", "parent", "envKey"))
      }
    )

    # Use mget to pull names in order to avoid always shuffling the values
    x <- mget(newNames, xEl)
    x <- copyAttributes(from = xEl, to = x)
    oldClass(x) <- removeFromSet(oldClass(xEl), "shiny.tag.env")

    # Recurse through children
    children <- x$children
    # Use a `for-loop` over `lapply` to avoid overhead
    for (i in seq_along(children)) {
      child <- children[[i]]
      if (!is.null(child)) {
        children[[i]] <- tagEnvToTags_(child)
      }
    }
    x$children <- children
  }
  x
}


isTagEnv <- function(x) {
  inherits(x, "shiny.tag.env")
}
isTagQuery <- function(x) {
  inherits(x, "shiny.tag.query")
}
assertNotTagEnvLike <- function(x, fnName) {
  if (isTagEnv(x)) {
    stop("Tag environment objects (i.e., `tagQuery()`'s tag structure) are not allowed to be used as if they are regular `tag()` objects. Did you forget to call `$root()` or `$selected()`?", call. = FALSE)
  }
  if (isTagQuery(x)) {
    stop("`tagQuery()` objects are not allowed to be used as if they are regular `tag()` objects. Did you forget to call `$root()` or `$selected()`?", call. = FALSE)
  }
  invisible()
}


shinyTagEnvStr <- "<!-- shiny.tag.env -->"

#' @export
as.tags.shiny.tag.env <- function(x, ...) {
  stop("Method not allowed", call. = TRUE)
  # as.tags(tagEnvToTags(x), ...)
}
#' @export
print.shiny.tag.env <- function(x, ...) {
  cat(shinyTagEnvStr, "\n")
  print(tagEnvToTags(x), ...)
}
#' @export
format.shiny.tag.env <- function(x, ...) {
  format(tagEnvToTags(x), ...)
}
#' @export
as.character.shiny.tag.env <- function(x, ...) {
  as.character(tagEnvToTags(x), ...)
}
#' @export
str.shiny.tag.env <- function(object, ...) {
  cat(shinyTagEnvStr, "\n")
  str(tagEnvToTags(object), ...)
}

#' @export
as.tags.shiny.tag.query <- function(x, ...) {
  tagQueryAsTagErr()
}
#' @export
print.shiny.tag.query <- function(x, ...) {
  tagQ <- x
  cat("`$allTags()`:\n")
  allTags <- tagQ$allTags()
  print(allTags)

  selectedTags <- tagQ$selectedTags()

  cat("\n`$selectedTags()`:")

  if (length(selectedTags) == 0) {
    cat(" (Empty selection)\n")
  } else {
    # Convert allTags to same style of object as selected tags
    if (!isTagList(allTags)) allTags <- tagList(allTags)
    allTags <- tagListPrintAsList(!!!allTags)

    if (identical(allTags, selectedTags)) {
      cat(" `$allTags()`\n")
    } else {
      cat("\n")
      print(selectedTags)
    }
  }

  invisible(x)
}
#' @export
format.shiny.tag.query <- function(x, ...) {
  tagQueryAsTagErr()
}
#' @export
as.character.shiny.tag.query <- function(x, ...) {
  tagQueryAsTagErr()
}

tagQueryAsTagErr <- function() {
  stop(
    "`tagQuery()` objects can not be written directly as HTML tags.",
    "Call either `$allTags()` or `$selectedTags()` to extract the tags of interest.",
    call. = FALSE
  )
}


#' Query and modify HTML tags
#'
#' `r lifecycle::badge("experimental")`\cr\cr `tagQuery()` provides a
#' [`jQuery`](https://jquery.com/) inspired interface for querying and modifying
#' [tag()] (and [tagList()]) objects.
#'
#' @section Altered Tag structure:
#'
#' For performance reasons, the input tag structure to `tagQuery()` will be
#' altered into a consistently expected shape.
#'
#' Some alterations include:
#' * tags flattening their `$children` fields into a single `list()`
#' * tags relocating any attribute `html_dependency() to be located in `$children`
#' * `tagList()`-like structures relocating any attribute html dependency to
#'   be a entry in its list structure.
#'
#' While the resulting tag shape has possibly changed,
#' `tagQuery()`'s' resulting tags will still render
#' to the same HTML value (ex: [`renderTags()`]) and
#' HTML dependencies (ex: [`findDependencies()`]).
#'
#' @param tags A [tag()], [tagList()], or [list()] of tags.
#' @return A class with methods that are described below. This class can't be
#'   used directly inside other [tag()] or a [renderTags()] context, but
#'   underlying HTML tags may be extracted via `$allTags()` or
#'   `$selectedTags()`.
#' @export
tagQuery <- function(tags) {

  if (isTagQuery(tags)) {
    # Return tag query object as is
    return(tags)
  }

  # Make a new tag query object from the root element of `tags`
  # * Set the selected to `list(tags)`
  if (isTagEnv(tags)) {
    # Rebuild pseudo root tag
    pseudoRoot <- asTagEnv(
      findPseudoRootTag(tags)
    )
    return(
      tagQuery_(pseudoRoot, list(tags))
    )
  }

  # If `tags` is a list of tagEnvs...
  # * Make sure they share the same root element and
  # * Set the selected elements to `tags`
  if (!isTag(tags) && (is.list(tags) || isTagList(tags))) {
    # If it is a list, flatten them for easier/consisten inspection
    tags <- flattenTagsRaw(tags)
    tagsIsTagEnv <- vapply(tags, isTagEnv, logical(1))

    # If one of the elements is a tag env, verify that all tagEnvs share the same root.
    if (any(tagsIsTagEnv)) {
      if (any(!tagsIsTagEnv)) {
        notTagEnvPos <- which(!tagsIsTagEnv)
        # It is not known how a middle of the tree tagEnv should be combined with a standard tag
        stop(
          "`tagQuery(tags=)` can not be a mix of tag environments and standard tag objects.\n",
          "Items at positions `c(", paste0(notTagEnvPos, collapse = ", "), ")` ",
          "are not tag environments."
        )
      }
      pseudoRootStack <- envirStackUnique()
      walk(tags, function(el) {
        pseudoRootStack$push(findPseudoRootTag(el))
      })
      pseudoRoots <- pseudoRootStack$uniqueList()
      if (length(pseudoRoots) != 1) {
        stop("All tag environments supplied to `tagQuery()` must share the same root element.")
      }
      # Rebuild pseudo root tag
      pseudoRoot <- asTagEnv(pseudoRoots[[1]])
      return(
        tagQuery_(pseudoRoot, tags)
      )
    }
  }

  # Convert standard tags to tag envs
  root <- asTagEnv(
    wrapWithPseudoRootTag(tags)
  )
  # Select the top level tags
  selected <- tagQueryFindResetSelected(root)
  if (length(selected) == 0) {
    stop(
      "The initial set of tags supplied to `tagQuery()` must have at least 1 standard tag object.",
      " Ex: `div()`"
    )
  }
  tagQuery_(root, selected)
}

#' @rdname tagQuery
#' @aliases NULL
#' @usage NULL
tagQuery_ <- function(
  pseudoRoot,
  # Using a trailing `_` to avoid name collisions
  selected_
) {
  if (!isPseudoRootTag(pseudoRoot)) {
    stop("`tagQuery_(pseudoRoot=)` must be a pseudoRoot tag environment")
  }

  # Use `var_` names to avoid namespace collision
  # Make sure all elements are tag envs
  rebuild_ <- function() {
    # safe to do as `pseudoRoot` will never be turned into a standard list
    asTagEnv(pseudoRoot)
  }
  newTagQuery <- function(selected) {
    tagQuery_(pseudoRoot, selected)
  }

  setSelected <- function(selected) {
    selected <- selected %||% list()
    if (!is.list(selected)) {
      stop("`selected` must be a `list()`")
    }
    selected <- FilterI(selected, f = function(el, i) {
      if (!isTagEnv(el)) {
        stop(
          "`setSelected(selected=)` received a list item at position `", i, "`",
          " that was not a tag environment"
        )
      }
      !isPseudoRootTag(el)
    })
    selected
  }
  selected_ <- setSelected(selected_)

  self <-
    structure(
      class = "shiny.tag.query",
      list(
        #' @details
        #'
        #' # Vignette
        #'
        #' To get started with using `tagQuery()`, visit
        #' <https://rstudio.github.io/htmltools/articles/tagQuery.html>.
        #'
        #' # Methods
        #'
        #' Unless otherwise stated, `tagQuery()` methods accept a character
        #' vector as input.
        #'
        #' ## Query methods
        #'
        #' Query methods identify particular subsets of the root tag using CSS
        #' selectors (or R functions).
        #'
        #' ### Children
        #'
        #' * `$find(cssSelector)`: Get the descendants of
        #' each selected tag, filtered by a `cssSelector`.
        find = function(cssSelector) {
          newTagQuery(
            tagQueryFindAll(selected_, cssSelector)
          )
        },
        #' * `$children(cssSelector = NULL)`: Get the direct
        #' children of each selected tag, optionally filtered by a
        #' `cssSelector`.
        children = function(cssSelector = NULL) {
          newTagQuery(
            tagQueryFindChildren(selected_, cssSelector)
          )
        },
        #' ### Siblings
        #'
        #' * `siblings(cssSelector = NULL)`: Get the
        #' siblings of each selected tag, optionally filtered by a
        #' `cssSelector`.
        siblings = function(cssSelector = NULL) {
          newTagQuery(
            tagQueryFindSiblings(selected_, cssSelector)
          )
        },
        #' ### Parents
        #'
        #' * `$parent(cssSelector = NULL)`: Get the parent
        #' of each selected tag, optionally filtered by a `cssSelector`.
        parent = function(cssSelector = NULL) {
          newTagQuery(
            tagQueryFindParent(selected_, cssSelector)
          )
        },
        #' * `$parents(cssSelector = NULL)`: Get the
        #' ancestors of each selected tag, optionally filtered by a
        #' `cssSelector`.
        parents = function(cssSelector = NULL) {
          newTagQuery(
            tagQueryFindParents(selected_, cssSelector)
          )
        },
        #' * `$closest(cssSelector = NULL)`: For each selected tag, get the closest
        #' ancestor tag (including itself) satisfying a `cssSelector`. If
        #' `cssSelector = NULL`, it is equivalent to calling `$selectedTags()`.
        closest = function(cssSelector = NULL) {
          newTagQuery(
            tagQueryFindClosest(selected_, cssSelector)
          )
        },
        #' ### Filter
        #'
        #' * `$matches(fn)`: For each of the selected tags, return `TRUE` if
        #'  `fn(el)` returns `TRUE`. In addition to an R function with two
        #'   arguments (the selected tag `x` and the index `i`), `fn` may also
        #'   be a valid CSS selector.
        matches = function(fn) {
          tagQueryMatches(selected_, fn)
        },
        #' * `$filter(fn)`: Filter the selected tags to those for which `fn(x,
        #' i)` returns `TRUE`. In addition to an R function with two arguments
        #' (the selected tag `x` and the index `i`), `fn` may also be a valid
        #' CSS selector.
        filter = function(fn) {
          newSelected <- tagQueryFilter(selected_, fn)
          rebuild_()
          newTagQuery(newSelected)
        },
        #' ### Length
        #'
        #' * `$length()`: Number of tags that have been selected.
        length = function() {
          length(selected_)
        },
        #' ### Reset
        #'
        #' * `$resetSelected()`: Reset selected tags to the `$root()` tag. Useful
        #' in combination with `$replaceWith()` since it empties the selection.
        resetSelected = function() {
          newTagQuery(
            tagQueryFindResetSelected(pseudoRoot)
          )
        },

        #' ## Modify methods
        #'
        #' Unlike query methods, modify methods modify the `tagQuery()` object.
        #'
        #' ### Attributes
        #'
        #' * `$addClass(class)`: Adds class(es) to each selected tag.
        addClass = function(class) {
          tagQueryClassAdd(selected_, class)
          self
        },
        #' * `$removeClass(class)`: Removes class(es) to each selected tag.
        removeClass = function(class) {
          tagQueryClassRemove(selected_, class)
          self
        },
        #' * `$toggleClass(class)`: Adds class(es) that don't already exist and
        #' removes class(es) that do already exist (for each selected tag).
        toggleClass = function(class) {
          tagQueryClassToggle(selected_, class)
          self
        },
        #' * `$hasClass(class)`: Does each selected tag have all the provided
        #' class(es)?
        hasClass = function(class) {
          tagQueryClassHas(selected_, class)
        },
        #' * `$addAttrs(...)`: Add a set of attributes to each selected tag.
        addAttrs = function(...) {
          tagQueryAttrsAdd(selected_, ...)
          self
        },
        #' * `$removeAttrs(attrs)`: Remove a set of attributes from each
        #' selected tag.
        removeAttrs = function(attrs) {
          tagQueryAttrsRemove(selected_, attrs)
          self
        },
        #' * `$hasAttrs(attr)`: Do each selected tags have all of the attributes?
        hasAttrs = function(attrs) {
          tagQueryAttrsHas(selected_, attrs)
        },
        #' ### Children
        #'
        #' * `$append(...)`: For each selected tag, insert `...` **after** any
        #' existing children.
        append = function(...) {
          tagQueryChildrenAppend(selected_, ...)
          self
        },
        #' * `$prepend(...)`: For each selected tag, insert `...` **before** any
        #' existing children.
        prepend = function(...) {
          tagQueryChildrenPrepend(selected_, ...)
          self
        },
        #' ### Siblings
        #'
        #' * `$after(...)`: Add all `...` objects as siblings after each of the
        #' selected tags.
        after = function(...) {
          tagQuerySiblingAfter(selected_, ...)
          self
        },
        #' * `$before(...)`: Add all `...` objects as siblings before each of
        #' the selected tags.
        before = function(...) {
          tagQuerySiblingBefore(selected_, ...)
          self
        },
        #' ### Custom
        #'
        #' * `$each(fn)`: Modify each selected tag with a function `fn`. `fn`
        #' should accept two arguments: the first is the selected tag and second
        #' is the selected tags position index. Since the selected tag is a
        #' reference, any modifications to it will also modify the `tagQuery()`
        #' object.
        each = function(fn) {
          if (length(selected_) > 0) {
            tagQueryEach(selected_, fn)
            rebuild_()
          }
          self
        },

        #' ## Replace methods
        #'
        #' * `$replaceWith(...)`: Replace all selected tags with `...` in the
        #' root tag and clear the selection.
        replaceWith = function(...) {
          tagQuerySiblingReplaceWith(selected_, ...)
          newTagQuery(list())
        },
        #' * `$remove(...)`: Remove all selected tags from the root tag and
        #' clear the current selection.
        remove = function() {
          tagQuerySiblingRemove(selected_)
          # Remove items from selected info
          newTagQuery(list())
        },
        #' * `$empty()`: Remove any children of each selected tag. Use this
        #' method before calling `$append(...)` to replace the children of
        #' each selected tag, with other content.
        empty = function() {
          tagQueryChildrenEmpty(selected_)
          self
        },

        #' ## Extract HTML tags
        #'
        #' * `$allTags()`: Return the (possibly modified) root `tags`.
        allTags = function() {
          tagQueryTopLevelTags(pseudoRoot)
        },
        #' * `$selectedTags()`: Return a [tagList()] of the currently selected
        #' tags.
        selectedTags = function() {
          tagQuerySelectedAsTags(selected_)
        }
        #' @examples
        #' tagQ <- tagQuery(div(a()))
        #' tagQ$find("a")$addClass("foo")
        #' tagQ
        #'
        #' # To learn more, visit https://rstudio.github.io/htmltools/articles/tagQuery.html
      )
    )
  self
}


validatePosition <- function(position, selected) {
  if (!is.numeric(position)) {
    stop("`position` must be a numeric value")
  }
  if (length(position) != 1) {
    stop("`position` must have a length equal to 1")
  }
  if (position <= 0) {
    stop("`position` must be greater than 0")
  }
  if (position > length(selected)) {
    stop(
      "`position` must be less than or equal to the length of the selected elements: ",
      length(selected)
    )
  }
}

validateFnCanIterate <- function(fn) {
  if (!is.function(fn)) {
    stop("`fn` must be a function")
  }
  fnFormals <- formals(fn)
  if (! ("..." %in% names(fnFormals))) {
    if (length(fnFormals) < 2) {
      stop(
        "`fn(selected_i, i)` must be a function that accepts at least two arguments: ",
        "`selected[[i]]` and `i` "
      )
    }
  }
}

isPseudoRootTag <- function(x) {
  name <- x$name
  isTag(x) && !is.null(name) && isTRUE(name == "TagQueryPseudoRoot")
}

findPseudoRootTag <- function(el) {
  while (!is.null(el$parent)) {
    el <- el$parent
  }
  el
}

# Wrap the top level tags in the tagQuery() in a `tagQuery` tag object.
# This allows for appending and prepending elements to the top level tags.
# (Don't fight the structures... embrace them!)
wrapWithPseudoRootTag <- function(x) {
  tagSetChildren(
    tag("TagQueryPseudoRoot", list()),
    x
  )
}


# Return a tag env, tagList(tag envs), or NULL
tagQueryGetRoot <- function(root) {
  children <- root$children
  len <- length(children)
  if (len == 1) {
    children[[1]]
  } else if (len > 1) {
    tagList(!!!children)
  } else {
    # no children?
    NULL
  }
}

# Return a list of the manually selected elements
tagQuerySelected <- function(selected) {
  if (length(selected) == 1 && isPseudoRootTag(selected[[1]])) {
    list()
  } else {
    selected
  }
}

# # Return the `i`th position of the manually selected elements
# tagQueryGet <- function(selected, position) {
#   selected <- tagQuerySelected(selected)
#   validatePosition(position, selected)

#   selected[[position]]
# }

# Return the top level tags as a tagList or a single tag
tagQueryTopLevelTags <- function(pseudoRoot) {
  children <- tagEnvToTags(pseudoRoot)$children
  len <- length(children)
  if (len == 1) {
    # single top level tag
    children[[1]]
  } else {
    # 0 or >1 top leve tags
    tagList(!!!children)
  }
}

tagListPrintAsList <- function(...) {
  x <- tagList(...)
  attr(x, "print.as.list") <- TRUE
  x
}
tagQuerySelectedAsTags <- function(selected) {
  # return as a `tagList()` with a special attr that will cause it to print like a list
  tagListPrintAsList(!!!lapply(selected, tagEnvToTags))
}


as_character2 <- function(...) {
  as.character(
    # MUST call `unlist()` to allow for vector items in `list2()`
    unlist(
      list2(...),
      use.names = FALSE
    )
  )
}
FilterI <- function (f, x) {
  ind <- as.logical(
    Map(x, seq_along(x), f = f)
  )
  x[which(ind)]
}
# Call `.f(x[[i]], ...)` for all values of i
walk <- function(.x, .f, ...) {
  for (i in seq_along(.x)) {
    .f(.x[[i]], ...)
  }
  NULL
}
walk2 <- function(.x, .y, .f, ...) {
  if (length(.x) != length(.y)) {
    stop(".x and .y must be the same length.")
  }
  for (i in seq_along(.x)) {
    .f(.x[[i]], .y[[i]], ...)
  }
  NULL
}
# Call `.f(x[[i]])` in reverse order
# walk_rev <- function(.x, .f, ...) {
#   for (i in rev(seq_along(.x))) {
#     .f(.x[[i]], ...)
#   }
#   NULL
# }
# Calls `.f(x[[i]], i, ...)`
walkI <- function(.x, .f, ...) {
  for (i in seq_along(.x)) {
    .f(.x[[i]], i, ...)
  }
  NULL
}
# Calls `.f(x[[i]], i, ...)` in reverse order
walkIRev <- function(.x, .f, ...) {
  for (i in rev(seq_along(.x))) {
    .f(.x[[i]], i, ...)
  }
  NULL
}

# Actually return the iterated results
MapI <- function(.x, .f, ..., USE.NAMES = FALSE) {
  Map(.x, seq_along(.x), f = .f, ..., USE.NAMES = USE.NAMES)
}


# Return function that will verify elements before performing `func(els, fn)`
selectedWalkGen <- function(func) {
  force(func)
  function(els, fn) {
    if (is.null(els)) return(list())
    if (!is.list(els)) {
      stop("A list() must be supplied")
    }
    if (!is.function(fn)) {
      stop("`fn` must be a function")
    }

    # Make sure each item in list is a tag env
    walkI(els, function(el, i) {
      if (!is.null(el)) {
        if (isTag(el) && !isTagEnv(el)) {
          str(el)
          stop(
            "Object in position `", i, "` is a regular `tag()` and not a tag environment.",
            "\nDid you forget to call `$rebuild()`?"
          )
        }
      }
    })

    func(els, fn)
  }
}
tagQueryWalk <- selectedWalkGen(walk)
# selectedWalkRev <- selectedWalkGen(walkRev)
selectedWalkI <- selectedWalkGen(walkI)
selectedWalkIRev <- selectedWalkGen(walkIRev)
selectedMapI <- selectedWalkGen(MapI)
tagQueryLapply <- selectedWalkGen(lapply)


# Perform `fn` on each el in els
tagQueryEach <- function(els, fn) {
  validateFnCanIterate(fn)
  selectedWalkI(els, fn)
}


# For each el in els, go to el parent and find el's position
# Then call `fn(parent, el, elPos)`
# Perform this matching in reverse order
tagQueryMatchChildRev <- function(els, func) {
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    elKey <- el$envKey
    elParent <- el$parent
    # Walk in reverse to be able to remove all matches in a single pass
    selectedWalkIRev(elParent$children, function(child, childPos) {
      if (!isTagEnv(child)) return()
      childKey <- child$envKey
      if (elKey == childKey) {
        func(elParent, el, childPos)
        # Make sure to rebuild the parent tag into tag envs
        # Their internal structures will have changed
        asTagEnv(elParent)
      }
    })
  })
}
# Remove each el in els from their parent.
# Also remove parent pointer from within el
tagQuerySiblingRemove <- function(els) {
  tagQueryMatchChildRev(els, function(elParent, el, childPos) {
    # remove parent / child relationship
    el$parent <- NULL
    elParent$children[[childPos]] <- NULL
  })
}
# Add siblings after each el
tagQuerySiblingAfter <- function(els, ...) {
  tagQueryMatchChildRev(els, function(elParent, el, childPos) {
    tagInsertChildren(elParent, after = childPos, ...)
  })
}
# Add siblings before each el
tagQuerySiblingBefore <- function(els, ...) {
  tagQueryMatchChildRev(els, function(elParent, el, childPos) {
    tagInsertChildren(elParent, after = childPos - 1, ...)
  })
}
# Replace all `el` objects with `...`
tagQuerySiblingReplaceWith <- function(els, ...) {
  tagQueryMatchChildRev(els, function(elParent, el, childPos) {
    # Remove the current element
    el$parent <- NULL
    elParent$children[[childPos]] <- NULL
    # Replace with ... content where the child was
    tagInsertChildren(elParent, after = childPos - 1, ...)
  })
}


tagQueryChildrenSet <- function(els, ...) {
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    tagSetChildren(el, ...)
    # Make sure to rebuild the el and its children
    asTagEnv(el)
  })
}
tagQueryChildrenEmpty <- function(els) {
  # Do not include any arguments.
  # `dots_list()` returns an empty named list()
  tagQueryChildrenSet(els)
}
tagQueryChildrenAppend <- function(els, ...) {
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    tagInsertChildren(el, after = length(el$children), ...)
    # Make sure to rebuild the el and its children
    asTagEnv(el)
  })
}
tagQueryChildrenPrepend <- function(els, ...) {
  tagQueryChildrenInsert(els, after = 0, ...)
}
tagQueryChildrenInsert <- function(els, after, ...) {
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    tagInsertChildren(el, after = after, ...)
    # Make sure to rebuild the el and its children
    asTagEnv(el)
  })
}


tagEnvRemoveAttribs <- function(el, attrs) {
  el$attribs[names(el$attribs) %in% attrs] <- NULL
  el
}
# Add attribute values
tagQueryAttrsAdd <- function(els, ...) {
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    tagAppendAttributes(el, ...)
  })
}
# Remove attribute values
tagQueryAttrsRemove <- function(els, attrs) {
  attrs <- as_character2(attrs)
  if (length(attrs) < 1) return()
  if (!is.character(attrs)) {
    stop("`attrs` must be a charcter vector of attributes to remove")
  }
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    tagEnvRemoveAttribs(el, attrs)
  })
}
# Check if els have attributes
tagQueryAttrsHas <- function(els, attrs) {
  attrs <- as_character2(attrs)
  if ((length(attrs) == 0) || (!is.character(attrs))) {
    stop("`attrs` must be a character vector", call. = FALSE)
  }
  unlist(
    tagQueryLapply(els, function(el) {
      if (!isTagEnv(el)) return(FALSE)

      for (attr in attrs) {
        if (!tagHasAttribute(el, attr)) {
          return(FALSE)
        }
      }
      # All attrs found
      return(TRUE)
    }),
    use.names = FALSE
  )
}

prepCssClass <- function(class) {
  class <- as_character2(class)
  if (length(class) == 0 || !is.character(class)) {
    stop("`class` must resolve to a character value with a length of at least 1")
  }
  class
}
getCssClass <- function(class) {
  splitCssClass(prepCssClass(class))
}
splitCssClass <- function(class) {
  if (!is.character(class)) {
    stop("tagGetAttribute(x, \"class\") did not return a character value")
  }
  if (length(class) > 1) {
    class <- paste0(class, collapse = " ")
  }
  strsplit(class, "\\s+")[[1]]
}
joinCssClass <- function(classes) {
  if (length(classes) == 0) {
    NULL
  } else {
    paste0(classes, collapse = " ")
  }
}
# return list of logical values telling if the classes exists
tagQueryClassHas <- function(els, class) {
  # Quit early if class == NULL | character(0)
  if (length(class) == 0) {
    return(rep(FALSE, length(els)))
  }

  classes <- getCssClass(class)
  unlist(
    tagQueryLapply(els, function(el) {
      if (!isTagEnv(el)) return(FALSE)
      classVal <- tagGetAttribute(el, "class")
      if (isNonConformClassValue(classVal)) {
        return(FALSE)
      }
      elClasses <- splitCssClass(classVal)
      all(classes %in% elClasses)
    }),
    use.names = FALSE
  )
}
removeFromSet <- function(set, vals) {
  # removes the call to `unique()` with `setdiff`
  set[match(set, vals, 0L) == 0L]
}
isNonConformClassValue <- function(classVal) {
  length(classVal) == 0 ||
  (!is.character(classVal)) ||
  anyNA(classVal)
}
tagEnvSetClassAttrib <- function(el, classes) {
  class <- joinCssClass(classes)

  classAttribPos <- which(names(el$attribs) == "class")
  isClassLen <- length(classAttribPos)

  if (isClassLen == 0) {
    # Store new class value
    return(
      tagAppendAttributes(el, class = class)
    )
  }

  # isClassLen > 0
  if (isClassLen > 1) {
    # Remove other occurrences of class
    el$attribs[classAttribPos[-1]] <- NULL
  }
  # Overwrite "class" attrib
  el$attribs[[classAttribPos[1]]] <- class
  el
}
# add classes that don't already exist
tagQueryClassAdd <- function(els, class) {
  # Quit early if class == NULL | character(0)
  if (length(class) == 0) return()

  classes <- getCssClass(class)
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    classVal <- tagGetAttribute(el, "class")
    if (isNonConformClassValue(classVal)) {
      tagAppendAttributes(el, class = joinCssClass(classes))
    } else {
      elClasses <- splitCssClass(classVal)
      newClasses <- c(elClasses, removeFromSet(classes, elClasses))
      tagEnvSetClassAttrib(el, newClasses)
    }
  })
}
# remove classes that exist
tagQueryClassRemove <- function(els, class) {
  # Quit early if class == NULL | character(0)
  if (length(class) == 0) return()

  classes <- getCssClass(class)
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    classVal <- tagGetAttribute(el, "class")
    if (isNonConformClassValue(classVal)) return()
    elClasses <- splitCssClass(classVal)
    newClasses <- removeFromSet(elClasses, classes)
    tagEnvSetClassAttrib(el, newClasses)
  })
}
# toggle class existence depending on if they already exist or not
tagQueryClassToggle <- function(els, class) {
  # Quit early if class == NULL | character(0)
  if (length(class) == 0) return()

  classes <- getCssClass(class)
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    classVal <- tagGetAttribute(el, "class")
    if (isNonConformClassValue(classVal)) return()

    elClasses <- splitCssClass(classVal)
    hasClass <- (classes %in% elClasses)
    if (any(hasClass)) {
      elClasses <- removeFromSet(elClasses, classes)
    }
    if (any(!hasClass)) {
      elClasses <- c(elClasses, classes[!hasClass])
    }
    tagEnvSetClassAttrib(el, elClasses)
  })
}


# Return a list of `root$children`.
# This may change if root ends up becoming a list of elements
tagQueryFindResetSelected <- function(pseudoRoot) {
  if (!isTagEnv(pseudoRoot)) {
    stop("`pseudoRoot` must be a tag environment")
  }
  Filter(pseudoRoot$children, f = isTagEnv)
}
# Return a list of the unique set of parent elements
tagQueryFindParent <- function(els, cssSelector = NULL) {
  parentStack <- envirStackUnique()
  pushFn <- pushFnWrapper(parentStack, cssSelector)
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    pushFn(el$parent)
  })
  parentStack$uniqueList()
}
# Return a list of the unique set of ancestor elements

# * By only looking for elements that have not been seen before, searching is as
# lazy as possible
# * Must traverse all parents; If cssSelector exists, only return found parents
# that match selector.
# * Search using depth-first. This does not match jQuery's implementation.
tagQueryFindParents <- function(els, cssSelector = NULL) {
  # Use the map for `has()` and stack for `values()`
  ancestorsMap <- envirMap()
  ancestorsStack <- envirStackUnique()

  # func to add to the ancestor stack
  pushFn <- pushFnWrapper(ancestorsStack, cssSelector)

  # For every element
  tagQueryWalk(els, function(el) {
    # Make sure it is a tag environment
    if (!isTagEnv(el)) return()

    # While traversing up the parents...
    while (!is.null(el <- el$parent)) {
      # If the element has been seen before...
      if (ancestorsMap$has(el)) {
        # Stop traversing, as any matching parent found would be removed
        # (unique info only)
        return()
      }
      # Mark the ancestor as visited
      ancestorsMap$add(el)
      # Add the element to the return set
      pushFn(el)
    }
  })
  ancestorsStack$uniqueList()
}
# Return a unique list of the closest ancestor elements that match the css selector
# Should behave VERY similarly to $parents()
tagQueryFindClosest <- function(els, cssSelector = NULL) {
  if (is.null(cssSelector)) {
    return(els)
  }
  selector <- cssSelectorToSelector(cssSelector)
  # use the map for `has()` and stack for `values()`
  ancestorsMap <- envirMap()
  closestStack <- envirStackUnique()

  # For every element
  tagQueryWalk(els, function(el) {
    # Make sure it is a tag environment
    if (!isTagEnv(el)) return()

    # While traversing up the parents...
    while (!is.null(el)) {
      # If the element has been seen before...
      if (ancestorsMap$has(el)) {
        # Stop traversing, as any matching parent found would be removed
        # (unique info only)
        return()
      }
      # Mark the ancestor as visited
      ancestorsMap$add(el)
      # If it is a match...
      if (elMatchesSelector(el, selector)) {
        # Add to return value
        closestStack$push(el)
        return()
      }
      # set to parent element and repeat
      el <- el$parent
    }
  })

  closestStack$uniqueList()
}
# Get all unique children tag envs
tagQueryFindChildren <- function(els, cssSelector = NULL) {
  childrenStack <- envirStackUnique()
  pushFn <- pushFnWrapper(childrenStack, cssSelector)
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    tagQueryWalk(el$children, pushFn)
  })
  childrenStack$uniqueList()
}

# Return all unique siblings of each el in els
tagQueryFindSiblings <- function(els, cssSelector = NULL) {
  siblingStack <- envirStackUnique()
  pushFn <- pushFnWrapper(siblingStack, cssSelector)
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    elKey <- el$envKey
    tagQueryWalk(el$parent$children, function(sibling) {
      if (!isTagEnv(sibling)) return()
      siblingKey <- sibling$envKey
      if (elKey != siblingKey) {
        pushFn(sibling)
      }
    })
  })
  siblingStack$uniqueList()
}

# Filter the selected elements using a function
# The answer of `fn(el, i)` should work in an `if` block
tagQueryMatches <- function(els, fn) {
  if (is.character(fn)) {
    selector <- cssSelectorToSelector(fn)
    fn <- function(el, i) {
      elMatchesSelector(el, selector)
    }
  }
  validateFnCanIterate(fn)
  vapply(selectedMapI(els, fn), isTRUE, logical(1))
}
# Filter the selected elements using a function
# The answer of `fn(el, i)` should work in an `if` block
tagQueryFilter <- function(els, fn) {
  if (is.character(fn)) {
    selector <- cssSelectorToSelector(fn)
    fn <- function(el, i) {
      elMatchesSelector(el, selector)
    }
  }
  validateFnCanIterate(fn)

  filterStack <- envirStackUnique()
  selectedWalkI(els, function(el, i) {
    if (isTRUE(fn(el, i))) {
      filterStack$push(el)
    }
  })

  filterStack$uniqueList()
}


# Convert a CSS selection character value to a selector object
# @param cssSelector A character value representing a CSS search pattern
# @return A single item of a selector list. (See `asSelectorList()`).
#   A single-element CSS selector object with full CSS element match information.
#   (Child selectors are not allowed in single-element selectors)
cssSelectorToSelector <- function(cssSelector) {
  selector <-
    if (isSelector(cssSelector)) {
      cssSelector
    } else {
      selectorList <- asSelectorList(cssSelector)
      if (length(selectorList) > 1) {
        stop(
          "Can only match using a simple CSS selector. ",
          "Looking for descendant elements is not allowed."
        )
      }
      selectorList[[1]]
    }

  selector
}

pushFnWrapper <- function(stack, cssSelector) {
  if (is.null(cssSelector)) {
    stack$push
  } else {
    selector <- cssSelectorToSelector(cssSelector)
    function(el) {
      if (elMatchesSelector(el, selector)) {
        stack$push(el)
      }
    }
  }
}


elMatchesSelector <- function(el, selector) {
  if (!isTagEnv(el)) return(FALSE)

  if (!isSelector(selector)) {
    stop("`elMatchesSelector(selector=)` must be an object of class `\"shinySelector\"`")
  }

  if (selector$type == SELECTOR_EVERYTHING) {
    return(TRUE)
  }

  # match on element
  if (!is.null(selector$element)) {
    # bad element match
    if (el$name != selector$element) {
      return(FALSE)
    }
  }

  # match on id
  if (!is.null(selector$id)) {
    # bad id match
    if ( !identical(tagGetAttribute(el, "id"), selector$id)) {
      return(FALSE)
    }
  }

  # match on class values
  if (!is.null(selector$classes)) {
    elClass <- tagGetAttribute(el, "class")
    if (
      isNonConformClassValue(elClass) ||
      # missing a class value in tag
      ! all(
        selector$classes %in% splitCssClass(elClass)
      )
    ) {
      return(FALSE)
    }
  }

  # No other matches fail. Mark as a match
  TRUE
}


tagQueryFindDescendants <- function(els, selector) {
  if (!isSelector(selector)) {
    selector <- cssSelectorToSelector(selector)
  }

  foundStack <- envirStackUnique()
  # For every element...
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    # Ignore the element and
    # Walk through each child...
    tagQueryWalk(el$children, function(child) {
      # Find descendant matching the `selector`
      tagQueryFindDescendants_(child, selector, foundStack$push)
    })
  })
  foundStack$uniqueList()
}

tagQueryFindDescendants_ <- function(el, selector, fn) {
  if (isTagEnv(el)) {

    isMatch <- elMatchesSelector(el, selector)

    # If it was a match
    if (isMatch) {
      fn(el)
    }

    # If there are children and remaining selectors,
    # Recurse through without matching
    # (Only allowed if `>` is not found)
    if (length(el$children) > 0) {
      walk(
        el$children,
        tagQueryFindDescendants_,
        fn = fn,
        selector = selector
      )
    }

  } else if (is.list(el)) {
    # For each item in the list like object, recurse through
    walk(el, tagQueryFindDescendants_, fn = fn, selector = selector)
  } else if (is.atomic(el) || is.function(el)) {
    # Can not match on atomics or functions
    return()
  } else {
    message("tagQueryFindDescendants_() - Unknown Type! This has not happened before:")
    str(el)
    stop("Unknown type in tagQueryFindDescendants_()")
  }

  invisible()
}

# Find all elements within `els` that match the `selector`
tagQueryFindAll <- function(els, selector) {
  selectorList <- asSelectorList(selector)

  curEls <- els
  walk(selectorList, function(selector) {
    curEls <<-
      if (selector$traversal == SELECTOR_CHILD) {
        tagQueryFindChildren(curEls, selector)
      } else {
        # any descendant traversal
        tagQueryFindDescendants(curEls, selector)
      }
  })

  curEls
}
