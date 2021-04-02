# TODO-barret
# * Add in `$env_key` to each tag env. This will avoid CONSTANT calling of format.default(tag_env)
# * First pass of tests
# * Implement `>` in css selector
# * Testing - find div, then find div. This should not return first div, but an inner div
# * Describe why using `props` and not `attr`
#    * Skipping. The htmltools package has no concept of props. This would only create confusion.
# * Remove obviously dead code

# TODO-barret followup PR
# * onRender(x, fn) - tagFunction(x, fn)

# TODO-barret remove!
debug_message <- function(...) {
  if (FALSE) {
    message(...)
  }
}

## Methods not implemented
# `$set_selected(selected)` & `$set(selected_item, pos)` - These methods are not available in jQuery and is very brittle in implementation. Do not pursue!
# With `$set(selected, pos)` not implemented, `[[<-.tag_graph` should not be implemented
# With `$set(selected, pos_vector)` not implemented, `[<-.tag_graph` should not be implemented
# If not doing, `[[<-.tag_graph` or `[<-.tag_graph`, then `[[.tag_graph` and `[.tag_graph` should not be implemented. Same with `length.tag_graph`
# `$set_children(...)` - jQuery does not have this. Instead, you can call `$empty()$append(...)`
# jQuery.val() - Get the current value of the first element in the set of matched elements or set the value of every matched element.
# jQuery.text() - Get the combined text contents of each element in the set of matched elements, including their descendants, or set the text contents of the matched elements.
# jQuery.css() - Get the value of a computed style property for the first element in the set of matched elements or set one or more CSS properties for every matched element.
# jQuery.prop() - Get the value of a property for the first element in the set of matched elements or set one or more properties for every matched element.




## Skip these implementations for now as the tag graph methods are small and composable.
## Instead write them where they are needed since they are small. (Just like we don't wrap dplyr code)
# tagReplaceAttributesAt <- function(tag, css_selector, ...) {
#   tag_graph(tag)$find(css_selector)$replace_attrs(...)$root_as_tags()
# }
# tagAppendAttributesAt <- function(tag, css_selector, ...) {
#   tag_graph(tag)$find(css_selector)$add_attrs(...)$root_as_tags()
# }
# tagAddClassAt <- function(tag, css_selector, class) {
#   tag_graph(tag)$find(css_selector)$add_class(class)$root_as_tags()
# }
# tagMutateAt <- function(x, css_selector, fn) {
#   tag_graph(tag)$find(css_selector)$each(fn)$root_as_tags()
# }
# tagFindAt <- function(x, css) {
#   tag_graph(tag)$find(css_selector)$selected_as_tags()
# }


# # Design notes for tag graph..

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
# * Fast to convert to a "linked list graph" of elements
# * Access to parents
# * Calculations on siblings can now be done, even after alterations have been completed
# * Once a `find(".x")` has been completed, a set of element environment pointers can be stored.
#   * This makes followup alterations have the minimal O(k) complexity (where k is _found_ elements), not O(n) + O(k) graph search + reconstruction and k _found_ element alterations
#
# Disadvantages
# * MUST be careful not alter the environment object before converting back to a list. (Ex: Do not remove the element environment's children)
# * The item returned is a set of environments that will alter in place. We will need to be careful about documenting and/or safeguarding this


# ## Final design choice:
# Use environment elements
# * Being able to search and have a list of eles to immediately look at and alter in place is AMAZING!
# * Being able to ask for a grandparent (or obj$parent$parent) and be able to alter it in place is AMAZING! This has a strongly influenced by jquery.

# ----------

# # Current design decisions
# * Tag graphs or tag environments can NOT be used in UI. These objects MUST be converted back to standard tag objects.
# * tagFunctions will not be altered in place
#   * To alter tagFunction()s, use the `onRender(x)` method to register a method to be called after `as.tags(x)` is called.
#   * `onRender(x, expr)` will wrap create a tag function that will resolve the tags before running the expr.


envir_key_fn <- function(x) { format.default(x) }

envir_map <- function() {
  map <- fastmap::fastmap()
  list(
    keys = function() {
      map$keys()
    },
    values = function() {
      map$values()
    },
    has = function(envir) {
      map$has(envir_key_fn(envir))
    },
    set = function(envir, value = envir) {
      map$set(envir_key_fn(envir), value)
    }
  )
}




# Retrieve all attributes that can be manually set
# ?attr
# Note that some attributes (namely ‘class’, ‘comment’, ‘dim’,
# ‘dimnames’, ‘names’, ‘row.names’ and ‘tsp’) are treated specially
# and have restrictions on the values which can be set.
safe_attr_values <- function(x) {
  bad_el_attrs <- c("class", "comment", "dim", "dimnames", "names", "row.names", "tsp")
  attr_vals <- attributes(x)
  attr_vals[bad_el_attrs] <- NULL
  attr_vals
}

# Convert a list to an environment and keep class and attribute information
safe_list_2_env <- function(x, new_class = NULL) {
  x_list <- x

  ret <- list2env(x_list, new.env(parent = emptyenv()))

  attr_vals <- safe_attr_values(x_list)
  Map(names(attr_vals), attr_vals, f = function(attr_name, attr_value) {
    attr(ret, attr_name) <<- attr_value
  })

  class(ret) <- c(new_class, class(x_list), "environment")
  ret
}

# Convert an environment to a list and keep class and attribute information
safe_env_2_list <- function(x, old_class = NULL) {
  x_env <- x
  ret <- as.list.environment(x_env)

  attr_vals <- safe_attr_values(x_env)
  Map(names(attr_vals), attr_vals, f = function(attr_name, attr_value) {
    attr(ret, attr_name) <<- attr_value
  })

  class(ret) <- setdiff(class(x), c(old_class, "environment"))
  ret
}


# Do not export to encourage direct use of `tag_graph()`
as_tag_env <- function(x) {
  if (isTagGraph(x)) {
    return(as_tag_env(x$root))
  }

  if (!isTagEnv(x)) {
    if (!isTag(x)) {
      # force all methods to send in tags, lists / tagLists are not allowed
      stop("`as_tag_env()` can only accept tag envs or tag objects. It does not accept `lists()` or `tagLists()`")
    }
  }
  as_tag_env_(x, parent = x$parent)
}
as_tag_env_ <- function(x, parent = NULL, seen_map = envir_map()) {
  debug_message("as_tag_env_()")
  is_tag <- isTag(x)
  is_tag_env <- isTagEnv(x)

  if (is_tag || is_tag_env) {
    if (!is_tag_env) {
      x_list <- x
      x <- safe_list_2_env(x_list, "htmltools.tag.env")
    }
    if (seen_map$has(x)) {
      stop(
        "Circular tag reference found with tag.env", envir_key_fn(x), "\n",
        # Not necessarily the order of the circular dependency
        # TODO-later show actual circular dependency and not all visited nodes? This should be rare
        "Tags processed:\n", paste0("* ", seen_map$keys(), collapse = "\n")
      )
    }
    seen_map$set(x, TRUE)
    # add parent env
    x$parent <- parent
    # recurse through children
    if (length(x$children) != 0) {
      # Possible optimization... name the children tags to the formatted values.
      # * Allows for faster child look up later.
      # * Comes with the cost of always formatting the env values even if children names are not needed.
      # Attributes may be dropped
      # * Could replace with `x$children[] <- ....`
      # * Leaving as is to see if people mis-use the children field
      x$children <- lapply(
        # Simplify the structures by flatting the tags
        # Does NOT recurse to grand-children etc.
        flattenTags(x$children, validate = FALSE),
        # recurse through each child
        as_tag_env_,
        parent = x,
        seen_map = seen_map
      )
    }
  }
  x
}

# This method MUST undo everything done in `as_tag_env(x)`
# Do not export to encourage direct use of `tag_graph()$root_as_tags()` or `tag_graph()$selected_as_tags()`
tag_env_to_tags <- function(x) {
  debug_message("tag_env_to_tags()")
  if (isTagEnv(x)) {
    x_el <- x
    # convert to list first to avoid altering the original env obj
    x <- safe_env_2_list(x_el, c("htmltools.tag.env"))
    # undo parent env
    x$parent <- NULL
    # recurse through children
    if (!is.null(x$children)) {
      x$children <- lapply(x$children, tag_env_to_tags)
    }
  }
  x
}


isTagEnv <- function(x) {
  inherits(x, "htmltools.tag.env")
}
isTagGraph <- function(x) {
  inherits(x, "htmltools.tag.graph")
}
assert_not_tag_env_like <- function(x, fn_name) {
  if (isTagEnv(x)) {
    stop("Tag environment objects which inherit `htmltools.tag.env` are not allowed to be processed in `", fn_name, "()`")
  }
  if (isTagGraph(x)) {
    stop("Tag graphs which inherit `htmltools.tag.graph` are not allowed to be processed in `", fn_name, "()`")
  }
  invisible()
}


shiny_tag_el_str <- "<!-- htmltools.tag.env -->"

#' @export
as.tags.htmltools.tag.env <- function(x, ...) {
  debug_message("as.tags.htmltools.tag.env()")
  stop("Method not allowed", call. = TRUE)
  # as.tags(tag_env_to_tags(x), ...)
}
#' @export
print.htmltools.tag.env <- function(x, ...) {
  debug_message("print.htmltools.tag.env()")
  cat(shiny_tag_el_str, "\n")
  print(tag_env_to_tags(x), ...)
}
#' @export
format.htmltools.tag.env <- function(x, ...) {
  debug_message("format.htmltools.tag.env()")
  format(tag_env_to_tags(x), ...)
}
#' @export
as.character.htmltools.tag.env <- function(x, ...) {
  debug_message("as.character.htmltools.tag.env()")
  as.character(tag_env_to_tags(x), ...)
}
#' @export
str.htmltools.tag.env <- function(object, ...) {
  debug_message("str.htmltools.tag.env()")
  cat(shiny_tag_el_str, "\n")
  str(tag_env_to_tags(object), ...)
}

#' @export
as.tags.htmltools.tag.graph <- function(x, ...) {
  stop("Method not allowed", call. = TRUE)
  # as.tags(x$selected_as_tags())
}
#' @export
print.htmltools.tag.graph <- function(x, ...) {
  cat("Root:\n")
  print(x$root_as_tags())

  cat("\nSelected Elements:")
  selected <- x$get_selected()
  if (length(selected) == 0) {
    cat(" (Empty)\n")
  } else {
    if (length(selected) == 1 && identical(x$root(), selected[[1]])) {
      cat(" (Root)\n")
    } else {
      cat("\n")
      print(x$selected_as_tags())
    }
  }
}
#' @export
format.htmltools.tag.graph <- function(x, ...) {
  stop(
    "`format.htmltools.tag.graph(x)` not allowed.\n",
    "Please call `format()` the result of `$root_as_tags()` or `$selected_as_tags()`"
  )
}
#' @export
as.character.htmltools.tag.graph <- function(x, ...) {
  stop(
    "`as.character.htmltools.tag.graph(x)` not allowed.\n",
    "Please call `as.character()` the result of `$root_as_tags()` or `$selected_as_tags()`"
  )
}



#' Tag graph
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function is VERY experimental. The result api will most likely change. **Use at your own risk.**
#'
#' `tag_graph()` is modeled after [`jQuery`](jquery.com)'s [DOM maninipulation methods](https://api.jquery.com/category/manipulation/) and [Tree Traversal](https://api.jquery.com/category/traversing/tree-traversal/) categories with some alterations. One of the main differences is that there is no centralized `window.document` object to use or reference. Instead, only the original `root` tag provided to `tag_graph(root=)` is fully search-able.
#'
#' `tag_graph()` is built to perform complex alterations within a set of tags. For example, it is difficult to find a set of tags and alter the parent tag when working with standard [`tag`] objects. With `tag_graph()`, it is possible to find all `<span>` tags that match the css selector `div .inner span`, then ask for the grandparent tag objects, then add a class to these grandparent tag elements.  This could be accomplished using code similar to
#'
#' ```r
#' tag_graph(root)$find("div .inner span")$parent()$parent()$add_class("custom-class")$root_as_tags()
#' ```
#'
#' This style of alteration is not easily achieved when using typical "pass by value" R objects or standard tag objects.
#'
#' # Tag graph components
#'
#' ## Tag environments
#'
#' "Tag environments" are the building blocks to `tag_graph()`. When creating a `tag_graph()`, each tag object is converted to a tag environment. This conversion allows for element alterations to happen in place (pass by reference). Meaning that if a css class is added to each selected tag environment using `$add_class()` and the result of the method call is ignored, the selected tag environments in the tag graph will still contain the class addition.  The added class will exist when the tag graph is converted back to standard tags objects with `$root_as_tags()` or `$selected_as_tags()`.
#'
#' Tag environments also contain an extra field, `.$parent`. The `.$parent` value contains their parent tag environment. The `root` element will have a `NULL` value for `.$parent`.
#'
#' The set of tag environments in a pointing to each other within a tag graph can be thought of as a linked list while allowing for a "1 to many" parent to child relationship and up to 1 parent per child.
#'
#' ## Tag graphs
#'
#' A `tag_graph()` behaves simliar to an R6 object (but a tag graph is not implemented with `R6`). The `tag_graph()`'s methods will return itself as much as possible, unless the method is directly asking for information, e.g. `$get_selected()` or `$root_as_tags()`.
#'
#' Internally, two important pieces of information are maintained: the root element and the selected elements. The root tag environment will always point (after upgrading to a tag environment) to the original tag object provided to `tag_graph(tag=)`. However, the selected elements are a list of tag environments that update for every `$find(css_selector)` call.  The selected elements are initialized to a list containing the `root` tag environment. All `tag_graph()` methods will act on the selected elements unless declared otherwise.
#'
#'
#' @section Limitations:
#'
#' `tag_graph()`s can **not** be used directly within typical `tag` locations. An error should be thrown. Instead, please call `$selected_as_tags()` or `$root_as_tags()` to retrieve the tag structures of the selected tag elements or root element respectively.
#'
#' @param root A single tag object with possibly many children tag objects. Currently, [`tagList()`] or [`list()`]s of tags are not allowed.
#' @return A `tag_graph()` object. The `tag` supplied will be considered the `root` object. At the time of initialization, the `root` is also considered the single selected item. If any selections are made, the selected elements will be updated.
#' @md
#' @export
tag_graph <- function(root) {
  root <- as_tag_env(root)
  selected_env <- new.env(parent = emptyenv())
  selected_env$data <- list(root)
  set_selected <- function(selected) { stopifnot(is.list(selected)); selected_env$data <- selected }
  get_selected <- function() { selected_env$data }

  # make sure all elements are tag envs
  rebuild <- function() {
    # safe to do as `root` will never be turned into a standard list
    as_tag_env(root)
  }

  self <-
    structure(
      class = "htmltools.tag.graph",
      list(
        #' @details # Methods
        #'
        #' All methods return the altered tag graph object unless otherwise stated.
        #'
        #' ## Select tags
        #' * `$find(css_selector)`: Find all tag elements matching the `css_selector` starting from each selected element. The selected elements will be updated with the found set of tag environment.
        find = function(css_selector) {
          rebuild()
          set_selected(
            tag_graph_find(get_selected(), css_selector)
          )
          self
        },
        #' * `$fitler(fn)`: Update the selected elements to contain all direct child elements of the selected elements.
        filter = function(fn) {
          rebuild()
          set_selected(
            tag_graph_find_filter(get_selected(), fn)
          )
          rebuild() # the fn could have altered the content
          self
        },
        #' * `$children()`: Update the selected elements to contain all direct child elements of the selected elements.
        children = function() {
          rebuild()
          set_selected(
            tag_graph_find_children(get_selected())
          )
          self
        },
        #' * `$parent()`: Update the selected elements to contain the unique set of direct parent of the selected elements.
        parent = function() {
          rebuild()
          set_selected(
            tag_graph_find_parent(get_selected())
          )
          self
        },
        #' * `$parents()`: Update the selected elements to contain the unique set of all ancestors of the selected elements.
        parents = function() {
          rebuild()
          set_selected(
            tag_graph_find_parents(get_selected())
          )
          self
        },
        #' * `$reset()`: Resets the selected elements to the root.
        reset = function() {
          rebuild()
          set_selected(
            tag_graph_find_reset(root)
          )
          self
        },
        # TODO-later
        #  .closest()
        #    For each element in the set, get the first element that matches the selector by testing the element itself and traversing up through its ancestors in the DOM tree.
        #    Find the nearest el (starting with self) that matches "css selector"
        #  .siblings()
        #    Get the siblings of each element in the set of matched elements, optionally filtered by a selector.
        ## end Find


        #' ## Update selected tag info
        #' * `$add_class(class)`: Apps class(es) to each of the the selected elements.
        add_class = function(class) {
          rebuild()
          tag_graph_add_class(get_selected(), class)
          self
        },

        # .hasClass() - Determine whether any of the matched elements are assigned the given class.
        # TODO-followup PR has_class = function(class) { },
        # .removeClass() - Remove a single class, multiple classes, or all classes from each element in the set of matched elements.
        # TODO-followup PR remove_class = function(class) { },
        # .toggleClass() - Add or remove one or more classes from each element in the set of matched elements, depending on either the class’s presence or the value of the state argument.
        # TODO-followup PR toggle_class = function(class) { },

        # TODO-later attr(). But this is difficult. Maybe separate out into `$set_attr(key, value)` or `$get_attr(key)`
        # TODO-followup PR set_attrs = function(...) { },
        # TODO-followup PR get_attrs = function(attributes) { },
        # TODO-followup PR remove_attrs = function(attributes) { },
          # .removeAttr()
          # Remove an attribute from each element in the set of matched elements.
          # TODO-Followup PR - Once all attrs are flattened, this should be easy to implement
        # TODO-followup PR? has_attr = function(attribute) {},

        #' * `$add_attrs(...)`: Add element attributes to all selected children. Similar to [`tagAppendAttributes()`].
        add_attrs = function(...) {
          rebuild()
          tag_graph_add_attrs(get_selected(), ...)
          self
        },

        #' ## Adjust child elements
        #' * `$append(...)`: Add all `...` objects as children **after** any existing children to the selected elements. Similar to [`tagAppendChildren()`]
        append = function(...) {
          rebuild()
          tag_graph_append_children(get_selected(), ...)
          rebuild()
          self
        },
        #' * `$prepend(...)`: Add all `...` objects as children **before** any existing children to the selected elements. A variation of [`tagAppendChildren()`]
        prepend = function(...) {
          rebuild()
          tag_graph_prepend_children(get_selected(), ...)
          rebuild()
          self
        },
        #' * `$empty(...)`: Remove all children in the selected elements. Use this method before calling `$append(...)` to replace all selected elements' children.
        empty = function() {
          rebuild()
          tag_graph_empty_children(get_selected())
          # no need to rebuild
          self
        },
        ## end Adjust Children

        ## Adjust Siblings
        # TODO-followup pr - All methods below
        # Remove the set of matched elements from the DOM.
        # remove = function() {
        #   rebuild()
        #   tag_graph_remove(get_selected())
        #   rebuild()
        #   self
        # },
        # .after()
        # Insert content, specified by the parameter, after each element in the set of matched elements.
        # .before()
        # Insert content, specified by the parameter, before each element in the set of matched elements.
        # .replaceWith()
        # Replace each element in the set of matched elements with the provided new content and return the set of elements that was removed.
        ## end Adjust Siblings

        ## Generic Methods
        #' ## Generic methods
        #' * `$each(fn)`: Perform function `fn` on each of the selected elements. `fn` should accept two arguments: a selected element and the selected element's position within the selected elements. This argument order is different than jQuery's `$().each()` as there is no concept of a `this` object inside the function execution. To stay consistent with other methods, the each of the selected tag environments will be given first, followed by the index position. Any alterations to the provided tag environments will persist in calling tag graph.
        each = function(fn) {
          rebuild()
          tag_graph_each(get_selected(), fn)
          rebuild()
          self
        },
        ## end Generic Methods

        #' ## Tag Graph functions
        #' * `$rebuild()`: Makes sure that all tags have been upgraded to tag environments. Objects wrapped in `HTML()` will not be inspected or altered. This method is internally called before each method executes and after any alterations where standard tag objects could be introduced into the tag structure.
        rebuild = function() {
          rebuild()
          self
        },
        #' * `$root()`: Return the root tag environment.
        root = function() {
          rebuild()
          root
        },
        #' * `$get_selected()`: Returns a list of selected tag environments.
        get_selected = function() {
          rebuild()
          get_selected()
        },
        #' * `$get(position)`: Returns the selected tag element at the position `position`.
        get = function(position) {
          rebuild()
          selected <- get_selected()
          validate_position(position, selected)

          selected[[position]]
        },
        ## end Tag Graph fns

        ## To tags
        #' ## Convert to tags
        #' * `$root_as_tags()`: Converts the root tag environment (and all of its children elements) back to standard tag objects and returns the root tag value.
        root_as_tags = function() {
          rebuild()
          tag_env_to_tags(root)
        },
        # = .html()
        # Get the HTML contents of the first element in the set of matched elements or set the HTML contents of every matched element.
        #' * `$selected_as_tags()`: Converts each selected tag environments (and all of their child elements) back to standard tag objects. A `tagList()` is returned, wrapping around the set of selected tags.
        selected_as_tags = function() {
          rebuild()
          # return as tagList
          do.call(tagList, lapply(get_selected(), tag_env_to_tags))
        }
        ## end To tags
      )
    )
  self
}


validate_position <- function(position, selected) {
  stopifnot(is.numeric(position))
  stopifnot(position > 0)
  stopifnot(position <= length(selected))
}

validate_fn_can_iterate <- function(fn) {
  stopifnot(is.function(fn))
  fn_formals <- formals(fn)
  if (! ("..." %in% names(fn_formals))) {
    if (length(fn_formals) < 2) {
      stop("`$each(fn=)` must be a function that accepts at least two arguments: `selected[[i]]` and `i` ")
    }
  }
}



# Call `.f(x[[i]], ...)` for all values of i
walk <- function(.x, .f, ...) {
  for (i in seq_along(.x)) {
    .f(.x[[i]], ...)
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
walk_i <- function(.x, .f, ...) {
  for (i in seq_along(.x)) {
    .f(.x[[i]], i, ...)
  }
  NULL
}
# Calls `.f(x[[i]], i, ...)` in reverse order
walk_i_rev <- function(.x, .f, ...) {
  for (i in rev(seq_along(.x))) {
    .f(.x[[i]], i, ...)
  }
  NULL
}


# Make sure each item in list is a tag env
tag_graph_verify_selected <- function(els) {
  stopifnot(is.list(els))
  walk_i(els, function(el, i) {
    if (!isTagEnv(el)) {
      stop("Object in position `", i, "` is not a tag environment")
    }
  })
}

# Return function that will verify elements before performing `func(els, fn)`
selected_walk_gen <- function(func) {
  function(els, fn) {
    tag_graph_verify_selected(els)
    stopifnot(is.function(fn))

    func(els, fn)
  }
}
tag_graph_walk <- selected_walk_gen(walk)
# selected_walk_rev <- selected_walk_gen(walk_rev)
selected_walk_i <- selected_walk_gen(walk_i)
selected_walk_i_rev <- selected_walk_gen(walk_i_rev)


# Perform `fn` on each el in els
tag_graph_each <- function(els, fn) {
  validate_fn_can_iterate(fn)
  selected_walk_i(els, fn)
}


# For each el in els, go to el parent and find el's position
# Then call `fn(parent, el, el_pos)`
# Perform this matching in reverse order
tag_graph_match_child_rev <- function(els, func) {
  tag_graph_walk(els, function(el) {
    el_key <- envir_key_fn(el)
    el_parent <- el$parent
    # Walk in reverse to be able to remove all matches in a single pass
    selected_walk_i_rev(el_parent$children, function(child, child_pos) {
      child_key <- envir_key_fn(child)
      if (el_key == child_key) {
        func(el_parent, el, child_pos)
      }
    })
  })
}
# Remove each el in els from their parent.
# Also remove parent pointer from within el
tag_graph_remove <- function(els) {
  tag_graph_match_child_rev(els, function(el_parent, el, child_pos) {
    # remove parent / child relationship
    el$parent <- NULL
    el_parent$children[[child_pos]] <- NULL
  })
}


tag_graph_set_children <- function(els, ...) {
  tag_graph_walk(els, function(el) {
    tagSetChildren(el, ...)
  })
}
tag_graph_empty_children <- function(els) {
  tag_graph_set_children(els, list())
}
tag_graph_append_children <- function(els, ...) {
  tag_graph_walk(els, function(el) {
    tagAppendChildren(el, ...)
  })
}
tag_graph_prepend_children <- function(els, ...) {
  tag_graph_walk(els, function(el) {
    cur_children <- els$children
    tagSetChildren(el, ..., cur_children)
  })
}


tag_graph_add_class <- function(els, class) {
  tag_graph_add_attrs(els, class = class)
}
tag_graph_add_attrs <- function(els, ...) {
  tag_graph_walk(els, function(el) {
    tagAppendAttributes(el, ...)
  })
}


# Return a list of `root`.
# This may change if root ends up becoming a list of elements
tag_graph_find_reset <- function(root) {
  list(root)
}
# Return a list of the unique set of parent elements
tag_graph_find_parent <- function(els) {
  parent_map <- envir_map()
  tag_graph_walk(els, function(el) {
    parent_map$set(el$parent)
  })
  parent_map$values()
}
# Return a list of the unique set of ancestor elements
# By only looking for elements that have not been seen before, searching is as lazy as possible
tag_graph_find_parents <- function(els) {
  ancestors_map <- envir_map()

  # First pass should contain the current elements
  cur_els <- els
  while(length(cur_els) > 0) {
    # Make a map of elements to explore in the next loop iteration
    next_els_map <- envir_map()

    # For each element in `cur_els`
    tag_graph_walk(cur_els, function(cur_el) {
      # If the element has not been seen before...
      if (!ancestors_map$has(cur_el)) {
        # Add to next iteration set
        next_els_map$set(cur_el)
        # Add to all ancestors
        ancestors_map$set(cur_el)
      }
    })

    # At this point, we have found a new set of unexplored ancestors: next_els_map
    # Update `cur_els` to contain all tag envs to continue exploration
    cur_els <- dropNulls(next_els_map$values())
  }

  ancestors_map$values()
}
# Get all unique children tag envs
tag_graph_find_children <- function(els) {
  children_map <- envir_map()
  tag_graph_walk(els, function(el) {
    tag_graph_walk(el$children, function(child) {
      children_map$set(child)
    })
  })
  children_map$values()
}

# Filter the selected elements using a function
# The answer of `fn(el, i)` should work in an `if` block
tag_graph_find_filter <- function(els, fn) {

  validate_fn_can_iterate(fn)

  filter_map <- envir_map()
  selected_walk_i(els, function(el, i) {
    if (fn(el, i)) {
      filter_map$set(el)
    }
  })

  filter_map$values()
}

# Find all elements within `els` that match the `selector`
tag_graph_find <- function(els, selector) {
  found_map <- envir_map()
  selector <- as_selector_list(selector)
  # For every element...
  tag_graph_walk(els, function(el) {
    # Ignore the element and
    # Walk through each child...
    tag_graph_walk(el$children, function(child) {
      # Find decendent matching the `selector`
      tag_graph_find_(child, selector, function(found_el) {
        found_map$set(found_el)
      })
    })
  })
  found_map$values()
}

# Recursive function to find all elements that match a selector
# If a match is found, call `fn(matched_el)`
tag_graph_find_ <- function(el, selector, fn) {

  if (isTagEnv(el)) {
    # Grab the first element
    cur_selector <- selector[[1]]

    is_match <- TRUE
    if (!cur_selector$match_everything) {
      # match on element
      if (is_match && !is.null(cur_selector$element)) {
        is_match <- el$name == cur_selector$element
      }
      # match on id
      if (is_match && !is.null(cur_selector$id)) {
        is_match <- (el$attribs$id %||% "") == cur_selector$id
      }
      # match on class values
      if (is_match && !is.null(cur_selector$classes)) {
        is_match <- all(strsplit(el$attribs$class %||% "", " ")[[1]] %in% cur_selector$classes)
      }

      # if it is a match, drop a selector
      if (is_match) {
        # remove first element while maintaining class info
        selector[[1]] <- NULL
      }
    }

    # if there are children and remaining selectors, recurse through
    if (length(selector) > 0 && length(el$children) > 0) {
      walk(el$children, tag_graph_find_, fn = fn, selector = selector)
    }

    # if it was a match
    if (is_match) {
      if (
        # it is a "leaf" match
        length(selector) == 0 ||
        # or should match everything
        cur_selector$match_everything
      ) {
        # run method it
        fn(el)
      }
    }

  } else if (is.list(el)) {
    # For each item in the list like object, recurse through
    walk(el, tag_graph_find_, fn = fn, selector = selector)
  } else if (is.atomic(el) || is.function(el)) {
    # Can not match on atomics or functions
    return()
  } else {
    message("tag_graph_find_() - Unknown Type! This has not happened before:")
    str(el)
    stop("Unknown type in tag_graph_find_()")
  }

  invisible()
}




# TODO- Remove once lobstr PR gets accepted. Add in S3 methods to make for nice printing
tag_env_explain <- function(x, ..., before = "", max = Inf, seen_map = envir_map()) {
  if (max == 0) {
    return(invisible(x))
  }

  cat0 <- function(...) {
    cat(before, ..., "\n", sep = "")
  }

  if (isTagGraph(x)) {
    tag_env_explain(x$root(), before = before, max = max, seen_map = seen_map)
    cat0("search_list:")
    tag_env_explain(x$get_selected(), before = before, max = 1, seen_map = seen_map)
    return(invisible(x))
  }

  if (is.environment(x)) {
    if (seen_map$has(x)) {
      cat0(envir_key_fn(x))
      return(invisible(x))
    }
    seen_map$set(x, TRUE)
  }

  if (is.null(x)) {
    cat0(" - NULL")
    return(invisible(x))
  }

  if (is.function(x)) {
    cat0("function(){}")
  } else if (inherits(x, "htmltools.tag.env")) {
    x_list <- as.list.environment(x)
    cat0(envir_key_fn(x))
    Map(x_list, rlang::names2(x_list), f = function(value, key) {
      cat0(key, if (max > 1) ":")
      tag_env_explain(value, before = paste0(before, ". "), seen_map = seen_map, max = max - 1)
    })
  } else {
    if (is.list(x)) {
      if (length(x) == 0) {
        cat0("list()")
      }
      Map(x, rlang::names2(x), f = function(value, key) {

        pre <-
          if (key == "") {
            "-"
          } else {
            if (is.atomic(value)) {
              c(key, ":")
            } else {
              c(key, if (max > 1) ":")
            }
          }
        if (is.atomic(value)) {
          # str(value)
          cat0(pre, " ", value)
        } else {
          cat0(pre)
          tag_env_explain(value, before = paste0(before, ". "), seen_map = seen_map, max = max - 1)
        }
      })
    } else {
      cat0(x)
    }
  }

  x_attrs <- safe_attr_values(x)
  if (length(x_attrs) > 0) {
    names(x_attrs) <- paste0("attr(*, \"", names(x_attrs), "\")")
    tag_env_explain(x_attrs, before = paste0(before), seen_map = seen_map, max = max - 1)
  }

  invisible(x)

}


if (FALSE) {
  s <-
    div(
      class = "outer",
      div(
        class = "inner",
        shiny::sliderInput("x", "X", 1, 4, 2)
      )
    )
  tag_graph(s) $ find(".inner") $ parent() $ add_class("bar") $ root_as_tags()

  # Using square bracket
  tag_graph(s)[[1]]
  tag_graph(s)[[1]] <- div("hello")  # Needs to convert to env

  # Using getter/setter
  tag_graph(s)$get(1)
  tag_graph(s)$set(1, div("hello"))
  tag_graph(s)$append(div("hello"))
}

if (FALSE) {
  x <- list(a =1, b =2)
  e <- list2env(x)

  system.time({
    for (i in 1:1000000) {
      as.list.environment(e)
    }
  })

  system.time({
    for (i in 1:1000) {
      x <- as_tag_env(s)
      x1 <- to_list(x)
    }
  })

  library(magrittr)

  s <- sliderInput("x", "X", 1, 4, 2)

  s <- as_tag_env(s)
  # goal
  s %>% el_children() %>% add_child(div(class = "foo", "Hello")) %>% find(".foo") %>%
    to_list()

  s <- as_tag_env(sliderInput("x", "X", 1, 4, 2))
  s <- as_tag_env(s)

  # library(shiny)
  ui <- fluidPage(
    as_tag_env(sliderInput("obs", "Number of observations:",
      min = 0, max = 1000, value = 500
    )),
    # # for html dependencies only
    # sliderInput("s", "For deps only:",
    #   min = 0, max = 1000, value = 500
    # ),
    plotOutput("distPlot")
  )

  # Server logic
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
  }

  # Complete app with UI and server components
  shinyApp(ui, server)
}
