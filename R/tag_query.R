#' @importFrom fastmap fastmap faststack
#' @importFrom rlang obj_address
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
    if (!is.character(xEl[["name"]])) {
      stop("A tag environment has lost its `$name`. Did you remove it?")
    }
    # This alters the env, but these fields should exist!
    if (is.null(xEl[["attribs"]])) xEl$attribs <- setNames(list(), character(0))
    if (is.null(xEl[["children"]])) xEl$children <- list()

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
    stop("Tag environment objects (which inherit `shiny.tag.env`) are not allowed to be processed in `", fnName, "()`")
  }
  if (isTagQuery(x)) {
    stop("`tagQuery()` structures (which inherit `shiny.tag.query`) are not allowed to be processed in `", fnName, "()`")
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
  stop("Method not allowed", call. = TRUE)
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
  stop(
    "`tagQuery()` objects can not be written directly as HTML tags.\n",
    "Call either `$allTags()` or `$selectedTags()` to extract the tags of interest."
  )
}
#' @export
as.character.shiny.tag.query <- function(x, ...) {
  stop(
    "`tagQuery()` objects can not be written directly as HTML tags.\n",
    "Call either `$allTags()` or `$selectedTags()` to extract the tags of interest."
  )
}



#' Perform jQuery-like alterations on tags
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function is VERY experimental. The result api will most likely change.
#' **Use at your own risk.**
#'
#' `tagQuery()` is modeled after [`jQuery`](https://jquery.com/)'s [DOM
#' maninipulation methods](https://api.jquery.com/category/manipulation/) and
#' [Tree Traversal](https://api.jquery.com/category/traversing/tree-traversal/)
#' categories with some alterations. One of the main differences is that there
#' is no centralized `window.document` object to use or reference. Instead, only
#' the original tags provided to `tagQuery(tags=)` are fully search-able. This
#' difference requires a call to `$find(cssSelector)` to make any meaningful
#' alterations to the `tagQuery()` object.
#'
#' `tagQuery()` is built to perform complex alterations within a set of tags.
#' For example, it is difficult to find a set of tags and alter the parent tag
#' when working with standard [`tag`] objects. With `tagQuery()`, it is possible
#' to find all `<span>` tags that match the css selector `div .inner span`, then
#' ask for the grandparent tag objects, then add a class to these grandparent
#' tag elements.  This could be accomplished using code similar to
#'
#' ```r tagQuery(ex_tags)$find("div .inner
#' span")$parent()$parent()$addClass("custom-class")$allTags()
#' ```
#'
#' This style of alteration is not easily achieved when using typical "pass by
#' value" R objects or standard tag objects.
#'
#' # Tag Query components
#'
#' ## Tag environments
#'
#' "Tag environments" are the building blocks to `tagQuery()`. When creating a
#' `tagQuery()`, each tag object is converted to a tag environment. This
#' conversion allows for element alterations to happen in place (pass by
#' reference). Meaning that if a css class is added to each selected tag
#' environment using `$addClass()` and the result of the method call is ignored,
#' the selected tag environments within the tag query object will still contain
#' the class addition.  The added class will exist when the tag query tag
#' environment are converted back to standard tags objects with `$selectedTags()`.
#'
#' Tag environments also contain an extra field, `.$parent`. The `.$parent`
#' value contains their parent tag environment. The top level tags supplied to
#' `tagQuery()` will also have a shared parent element. (The shared parent
#' element will have a `NULL` `.$parent` value.) This allows for performing
#' sibling alterations at the top level of the tag query object.
#'
#' The set of tag environments in a pointing to each other within a tag query
#' object can be thought of as a linked list while allowing for a "1 to many"
#' parent to child relationship and up to 1 parent per child.
#'
#' ## Tag Query
#'
#' A `tagQuery()` behaves simliar to an R6 object in that internal values are
#' altered in place. (but a `tagQuery()` object is not implemented with `R6`).
#' The `tagQuery()`'s methods will return itself as much as possible, unless the
#' method is directly asking for information, e.g. `$allTags()` or `$selectedTags()`.
#'
#' Internally, two important pieces of information are maintained: the root
#' elements and the selected elements. The root tag environment will always
#' point (after upgrading to a tag environment) to the original tag object
#' provided to `tagQuery(tag=)`. However, the selected elements are a list of
#' tag environments that update for every `$find(cssSelector)` call.  The
#' selected elements are initialized to a list containing the `root` tag
#' environment. All `tagQuery()` methods will act on the selected elements
#' unless declared otherwise.
#'
#' Tag query objects can be created from other tag query objects. Note, unless
#' there is an intermediate call to `$selectedTags()`, the original and new tag query
#' objects will share the same tag environments. The new tag query object will
#' have its selected elements reset. For example:
#'
#' ```r
#' x <- tagQuery(div())
#' y <- tagQuery(x)
#' z <- tagQuery(x$allTags())
#'
#' # Add an example class
#' y$addClass("example")
#'
#' # Show `x` and `y` both have the new class
#' x$selectedTags()
#' #> <div class="example"></div>
#' y$selectedTags()
#' #> <div class="example"></div>
#'
#' # `z` is isolated from the changes in `x` and `y` due to the `$selectedTags()`
#' z$selectedTags()
#' #> <div></div>
#' ```
#'
#'
#' @section Limitations:
#'
#'   `tagQuery()`s can **not** be used directly within typical `tag` locations.
#'   An error should be thrown. Instead, please call `$selectedTags()` to retrieve the
#'   tag structures of the selected tag elements or root element respectively.
#'
#' @param tags Any standard tag object or `tagList()`.
#' @return A `tagQuery()` object. The `tag` supplied will be considered the
#'   `root` object. At the time of initialization, the `root` is also considered
#'   the single selected item. If any selections are made, the selected elements
#'   will be updated.
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
        #' # CSS Selector
        #'
        #' The `cssSelector` parameter to many methods is a typical CSS
        #' selector. Currently `tagQuery()` understands how to handle any
        #' combination of the following CSS selector information:
        #'
        #' * `element`:Match against a tag name. Example: `div`
        #' * `id`: Match against a tag `id` attribute. Example: `#myID`
        #' * `class`: Match against a tag's class attribute. Example:
        #' `.my-class`. This may include multiple classes in any order.
        #'
        #' The `$find(cssSelector)` method allows for a CSS selector with
        #' multiple selections. Example: `div span` or `.outer > span.inner`.
        #' All other functions only allow for single-element CSS selections,
        #' such as `div#myID.my-class`.
        #'
        #' # Methods
        #'
        #' All methods return the altered tag query object unless otherwise
        #' stated.
        #'
        #' ## Select tags
        #'
        #'
        #' * `$find(cssSelector)`: Find all tag elements matching the
        #' multi-element `cssSelector` starting from each selected element's
        #' children. If nothing has been selected, it will start from the root
        #' elements. The tag query object's selected elements will be updated
        #' with the matching set of tag environments. A new `tagQuery()` object
        #' will be created with the selected items set to the found elements.
        find = function(cssSelector) {
          newTagQuery(
            tagQueryFindAll(selected_, cssSelector)
          )
        },
        #' * `$children(cssSelector = NULL)`: Update the selected elements to
        #' contain all direct child elements of the selected elements. If a CSS
        #' selector is provided, only the direct children matching the
        #' single-element CSS selector will be selected. A new `tagQuery()`
        #' object will be created with the selected items set to the children
        #' elements.
        children = function(cssSelector = NULL) {
          newTagQuery(
            tagQueryFindChildren(selected_, cssSelector)
          )
        },
        #' * `$parent(cssSelector = NULL)`: Update the selected elements to
        #' contain the unique set of direct parent of the selected elements. If
        #' a CSS selector is provided, only the direct parents matching the
        #' single-element CSS selector will be selected. A new `tagQuery()`
        #' object will be created with the selected items set to the parent
        #' elements.
        parent = function(cssSelector = NULL) {
          newTagQuery(
            tagQueryFindParent(selected_, cssSelector)
          )
        },
        #' * `$parents(cssSelector = NULL)`: Update the selected elements to
        #' contain the unique set of all ancestors of the selected elements. If
        #' a CSS selector is provided, only the ancestors matching the
        #' single-element CSS selector will be selected. A new `tagQuery()`
        #' object will be created with the selected items set to the ancestor
        #' elements.
        parents = function(cssSelector = NULL) {
          newTagQuery(
            tagQueryFindParents(selected_, cssSelector)
          )
        },
        #' * `$closest(cssSelector = NULL)`: For each selected element, get the
        #' closest ancestor element (including itself) that matches the
        #' single-element CSS selector. If `cssSelector = NULL`, it is
        #' equivalent to calling `$selectedTags()`. A new `tagQuery()` object will be
        #' created with the selected items set to the closest matching elements.
        closest = function(cssSelector = NULL) {
          newTagQuery(
            tagQueryFindClosest(selected_, cssSelector)
          )
        },
        #' * `siblings(cssSelector = NULL)`: Get the siblings of each element in
        #' the set of matched elements. If a CSS selector is provided, only the
        #' siblings matching the single-element CSS selector will be selected. A
        #' new `tagQuery()` object will be created with the selected items set
        #' to the sibling elements.
        siblings = function(cssSelector = NULL) {
          newTagQuery(
            tagQueryFindSiblings(selected_, cssSelector)
          )
        },
        #' * `$filter(fn)`: Update the selected elements to a subset of the
        #' selected elements given `fn(el, i)` returns `TRUE`. If `fn` is a CSS
        #' selector, then the selected elements will be filtered if they match
        #' the single-element CSS selector. A new `tagQuery()` object will be
        #' created with the selected items set to the filtered selected
        #' elements. Remember, any alterations to the provided tag environments will persist
        #' in calling tag query object. If you need to make local changes, consider
        #' using `tagQuery(el)$selectedTags()` to use standard tag objects.
        filter = function(fn) {
          newSelected <- tagQueryFindFilter(selected_, fn)
          rebuild_()
          newTagQuery(newSelected)
        },
        #' * `$resetSelected()`: A new `tagQuery()` object will be created with the
        #' selected items set to the top level tag objects.
        resetSelected = function() {
          newTagQuery(
            tagQueryFindResetSelected(pseudoRoot)
          )
        },
        ## end Find


        #' ## Update selected tag info
        #'
        #' * `$addClass(class)`: Apps class(es) to each of the the selected
        #' elements.
        addClass = function(class) {
          tagQueryClassAdd(selected_, class)
          self
        },
        #' * `$removeClass(class)`: Removes a set of class values from all of
        #' the selected elements.
        removeClass = function(class) {
          tagQueryClassRemove(selected_, class)
          self
        },
        #' * `$hasClass(class)`: Determine whether the selected elements have a
        #' given class. Returns a vector of logical values.
        hasClass = function(class) {
          tagQueryClassHas(selected_, class)
        },
        #' * `$toggleClass(class)`: If the a class value is missing, add it. If
        #' a  class value already exists, remove it.
        toggleClass = function(class) {
          tagQueryClassToggle(selected_, class)
          self
        },

        #' * `$addAttrs(...)`: Add named attributes to all selected children.
        #' Similar to [`tagAppendAttributes()`].
        addAttrs = function(...) {
          tagQueryAttrsAdd(selected_, ...)
          self
        },
        #' * `$removeAttrs(attrs)`: Removes the provided attributes in each of
        #' the selected elements.
        removeAttrs = function(attrs) {
          tagQueryAttrsRemove(selected_, attrs)
          self
        },
        #' * `$emptyAttrs()`: Removes all attributes in each of the selected
        #' elements.
        emptyAttrs = function() {
          tagQueryAttrsEmpty(selected_)
          self
        },
        #' * `$hasAttr(attr)`: Returns a vector whose values are whether the
        #' selected element contains the non-`NULL` attribute.
        hasAttr = function(attr) {
          tagQueryAttrHas(selected_, attr)
        },

        #' ## Adjust child elements
        #'
        #' * `$append(...)`: Add all `...` objects as children **after** any
        #' existing children to the selected elements. Similar to
        #' [`tagAppendChildren()`]
        append = function(...) {
          tagQueryChildrenAppend(selected_, ...)
          self
        },
        #' * `$prepend(...)`: Add all `...` objects as children **before** any
        #' existing children to the selected elements. A variation of
        #' [`tagAppendChildren()`]
        prepend = function(...) {
          tagQueryChildrenPrepend(selected_, ...)
          self
        },
        #' * `$empty()`: Remove all children in the selected elements. Use this
        #' method before calling `$append(...)` to replace all selected
        #' elements' children.
        empty = function() {
          tagQueryChildrenEmpty(selected_)
          self
        },
        ## end Adjust Children

        #' ## Adjust Siblings
        #'
        #' * `$after(...)`: Add all `...` objects as siblings after each of the
        #' selected elements.
        after = function(...) {
          tagQuerySiblingAfter(selected_, ...)
          self
        },
        #' * `$before(...)`: Add all `...` objects as siblings before each of
        #' the selected elements.
        before = function(...) {
          tagQuerySiblingBefore(selected_, ...)
          self
        },
        #' * `$replaceWith(...)`: Replace all selected elements with `...`. This
        #' also sets the selected elements to an empty set. A new `tagQuery()`
        #' object will be created with an empty set of selected elements.
        replaceWith = function(...) {
          tagQuerySiblingReplaceWith(selected_, ...)
          newTagQuery(list())
        },
        #' * `$remove(...)`: Remove all selected elements from the `tagQuery()`
        #' object. The selected elements is set to an empty set. A new
        #' `tagQuery()` object will be created with an empty set of selected
        #' elements.
        remove = function() {
          tagQuerySiblingRemove(selected_)
          # Remove items from selected info
          newTagQuery(list())
        },
        ## end Adjust Siblings

        ## Generic Methods
        #' ## Generic methods
        #'
        #' * `$each(fn)`: Perform function `fn` on each of the selected
        #' elements. `fn` should accept two arguments: a selected element and
        #' the selected element's position within the selected elements. This
        #' argument order is different than jQuery's `$().each()` as there is no
        #' concept of a `this` object inside the function execution. To stay
        #' consistent with other methods, the each of the selected tag
        #' environments will be given first, followed by the index position.
        #' Remember, any alterations to the provided tag environments will persist
        #' in calling tag query object. If you need to make local changes, consider
        #' using `tagQuery(el)$selectedTags()` to use standard tag objects.
        each = function(fn) {
          tagQueryEach(selected_, fn)
          # MUST rebuild full tree as anything could have been done to the tag envs
          rebuild_()
          self
        },
        ## end Generic Methods

        #' ## Tag Query methods
        #'
        #' * `$allTags()`: Converts the top level (root) tag
        #' elements (and their descendants) from tag environments to
        #' standard [`tag`] objects. All root tags will be returned in a
        #' [`tagList()`].
        allTags = function() {
          tagQueryTopLevelTags(pseudoRoot)
        },
        #' * `$selectedTags()`: Converts the selected tag environments
        #' to standard [`tag`] objects. The selected tags will be returned in a
        #' [`tagList()`].
        selectedTags = function() {
          tagQuerySelectedAsTags(selected_)
        }
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
      if (!isTagEnv(el)) return()
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


# Add attribute values
tagQueryAttrsAdd <- function(els, ...) {
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    el <- tagAppendAttributes(el, ...)
    el$attribs <- flattenTagAttribs(el$attribs)
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
    for (attrVal in attrs) {
      el$attribs[[attrVal]] <- NULL
    }
  })
}
# Remove attribute values
tagQueryAttrsEmpty <- function(els) {
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    el$attribs <- list()
  })
}
# Check if els have attributes
tagQueryAttrHas <- function(els, attr) {
  attr <- as_character2(attr)[[1]]
  if (length(attr) != 1 || !is.character(attr)) {
    stop("`attr` must be a single character value")
  }
  unlist(
    tagQueryLapply(els, function(el) {
      if (!isTagEnv(el)) return(FALSE)
      tagHasAttribute(el, attr)
    }),
    use.names = FALSE
  )
}


getCssClass <- function(class) {
  class <- as_character2(class)
  if (length(class) == 0 || !is.character(class)) {
    stop("`class` must resolve to a character value with a length of at least 1")
  }
  splitCssClass(class)
}
splitCssClass <- function(class) {
  if (length(class) > 1) {
    class <- paste0(class, collapse = " ")
  }
  strsplit(class, " ")[[1]]
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
      classVal <- el$attribs$class
      if (is.null(classVal)) {
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
# add classes that don't already exist
tagQueryClassAdd <- function(els, class) {
  # Quit early if class == NULL | character(0)
  if (length(class) == 0) return()

  classes <- getCssClass(class)
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    classVal <- el$attribs$class %||% ""
    elClasses <- splitCssClass(classVal)
    newClasses <- c(elClasses, removeFromSet(classes, elClasses))
    el$attribs$class <- joinCssClass(newClasses)
  })
}
# remove classes that exist
tagQueryClassRemove <- function(els, class) {
  # Quit early if class == NULL | character(0)
  if (length(class) == 0) return()

  classes <- getCssClass(class)
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    classVal <- el$attribs$class
    if (is.null(classVal)) return()
    elClasses <- splitCssClass(classVal)
    newClasses <- removeFromSet(elClasses, classes)
    el$attribs$class <- joinCssClass(newClasses)
  })
}
# toggle class existence depending on if they already exist or not
tagQueryClassToggle <- function(els, class) {
  # Quit early if class == NULL | character(0)
  if (length(class) == 0) return()

  classes <- getCssClass(class)
  tagQueryWalk(els, function(el) {
    if (!isTagEnv(el)) return()
    classVal <- el$attribs$class %||% ""
    elClasses <- splitCssClass(classVal)
    hasClass <- (classes %in% elClasses)
    if (any(hasClass)) {
      elClasses <- removeFromSet(elClasses, classes)
    }
    if (any(!hasClass)) {
      elClasses <- c(elClasses, classes[!hasClass])
    }
    el$attribs$class <- joinCssClass(elClasses)
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
tagQueryFindFilter <- function(els, fn) {
  if (is.character(fn)) {
    selector <- cssSelectorToSelector(fn)
    fn <- function(el, i) {
      elMatchesSelector(el, selector)
    }
  }
  validateFnCanIterate(fn)

  filterStack <- envirStackUnique()
  selectedWalkI(els, function(el, i) {
    if (fn(el, i)) {
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
    if ( (el$attribs$id %||% "") != selector$id) {
      return(FALSE)
    }
  }

  # match on class values
  if (!is.null(selector$classes)) {
    if (
      # no tag class values at all
      is.null(el$attribs$class) ||
      # missing a class value in tag
      ! all(
        selector$classes %in% strsplit(el$attribs$class %||% "", " ")[[1]]
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
