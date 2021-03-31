# TODO-barret
# * find div, then find div. This should not return first div, but an inner div

# TODO-barret remove!
debug_message <- function(...) {
  if (FALSE) {
    message(...)
  }
}


# TODO-barret followup PR - onRender(x, expr)

# TODO-barret followup PR - Implement a more wrapper functions like
# tagAddClass <- function(tag, css_selector, class) {
#   tag_graph(tag)$find(css_selector)$add_class(class)$root_as_tags()
# }
# tagMutate <- function(x, css_selector, fn) {
#   tag_graph(tag)$find(css_selector)$walk_selected(fn)$root_as_tags()
# }
# tagFind <- function(x, css) {
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
# * Tag graphs or tag environments to be used in UI. These objects MUST be converted back to standard tag objects.
# * tagFunctions will not be altered in place
#   * To alter tagFunction()s, use the `onRender(x)` method to register a method to be called after `as.tags(x)` is called.
#   * `onRender(x, expr)` will wrap create a tag function that will resolve the tags before running the expr.


envir_key_fn <- format.default

# TODO-barret replace with fastmap::fastmap?
envir_map <- function() {
  ret <- list()

  list(
    key_fn = envir_key_fn,
    keys = function() {
      names(ret)
    },
    values = function() {
      ret
    },
    has = function(envir) {
      !is.null(ret[[envir_key_fn(envir)]])
    },
    set = function(envir, value = envir) {
      ret[[envir_key_fn(envir)]] <<- value
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
    x <- as_tag_env(x$root)
  } else {
    if (!isTagEnv(x)) {
      if (!isTag(x)) {
        # force all methods to send in tags, lists / tagLists are not allowed
        stop("`as_tag_env()` can only accept tag envs or tag objects. It does not accept lists or tagLists")
      }
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
      stop("Circular tag reference found with tag.env", seen_map$key_fn(x), "\nTags found:\n", paste0("* ", seen_map$keys(), collapse = "\n"))
    }
    seen_map$set(x, TRUE)
    # add parent env
    x$parent <- parent
    # recurse through children
    if (!is.null(x$children)) {
      x$children <- lapply(
        # simplify the structures
        flattenTags_(x$children),
        # recurse through
        function(tag) { as_tag_env_(tag, parent = x, seen_map = seen_map) }
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
​
#' @export
as.tags.htmltools.tag.env <- function(x, ...) {
  debug_message("as.tags.htmltools.tag.env()")
  as.tags(tag_env_to_tags(x), ...)
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
str.htmltools.tag.env <- function(x, ...) {
  debug_message("str.htmltools.tag.env()")
  cat(shiny_tag_el_str, "\n")
  str(tag_env_to_tags(x), ...)
}

#' @export
as.tags.htmltools.tag.graph <- function(x) {
  stop("Method not allowed", call. = TRUE)
  # as.tags(x$selected_as_tags())
}
#' @export
print.htmltools.tag.graph <- function(x) {
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
format.htmltools.tag.graph <- function(x) {
  stop(
    "`format.htmltools.tag.graph(x)` not allowed.\n",
    "Please call `format()` the result of `$root_as_tags()` or `$selected_as_tags()`"
  )
}
#' @export
as.character.htmltools.tag.graph <- function(x) {
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
#' `tag_graph()` is built to perform highly complex alterations on a set of tags. For example, adding a class value, `"custom_class"`, to a grandparent of any `<span>` tag that match the css selector `div .inner span`. This alteration is not easily accomplished when using standard "pass by value" R objects.
#'
#' # Graph components
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
#' Internally, two important pieces of information are maintained: the root element and the selected elements. The root tag environment will always point (after upgrading to a tag environment) to the original tag object provided to `tag_graph(tag=)`. However, the selected elements are a list of tag environments that update for every `$find(css_selector)` or `$set_selected(selected)` call.  The selected elements are initialized to a list containing the `root` tag environment. All `tag_graph()` methods will act on the selected elements unless declared otherwise.
#'
#' # Methods
#'
#' All methods return the altered tag graph object unless otherwise stated.
#'
#' ## Select tags
#' * `$find(css_selector)`: Find all tag elements matching the `css_selector` starting from each selected element. The selected elements will be updated with the found set of tag environment.
#' * `$find_parents()`: Update the selected elements to contain the unique set of parents of the selected elements.
#' * `$find_children()`: Update the selected elements to contain all direct child elements of the selected elements.
#' * `$find_reset()`: Resets the selected elements to the root.
#'
#' ## Update selected tag info
#'
#' * `$add_class(class)`: Append a class to the selected elements.
#' * `$add_attrs(...)`: Add element attributes to all selected children. Similar to [`tagAppendAttributes()`].
#'
#' ## Adjust child elements
#' * `$append(...)`: Add all `...` objects as children after any existing children to the selected elements.
#' * `$prepend(...)`: Add all `...` objects as children before any existing children to the selected elements.
#' * `$empty(...)`: Remove all children in the selected elements.
#'
#' ## Adjust sibling elements
# ' * `$append(...)`: Add all `...` objects as children after any existing children to the selected elements.
# ' * `$prepend(...)`: Add all `...` objects as children before any existing children to the selected elements.
# ' * `$empty(...)`: Remove all children in the selected elements.
#'
#' ## Generic methods
#' * `$walk_selected(fn)`: Perform function `fn` on each of the selected elements.
#'
#' ## Tag Graph functions
#' * `$rebuild()`: Makes sure that all tags have been upgraded to tag environments. Objects wrapped in `HTML()` will not be inspected or altered. This method is internally called before each method executes and after any alterations where standard tag objects could be introduced into the tag structure.
#' * `$root()`: Return the root tag environment.
#' * `$get_selected()`: Returns a list of selected tag environments.
#' * `$get(position)`: Returns the selected tag element at the position `position`.
#' * `$set_selected(selected)`: TODO should this be included?
#' * `$set(tag_env, i)`: TODO should this be included?
#'
#' # Convert to tags
#' * `$root_as_tags()`: Converts the root tag environment (and all of its children elements) back to standard tag objects and returns the root tag value.
#' * `$selected_as_tags()`: Converts each selected tag environments (and all of their child elements) back to standard tag objects. A `tagList()` is returned, wrapping around the set of selected tags.
#'
#'
#' # Limitations
#'
#' `tag_graph()`s can **not** be used directly within typical `tag` locations. An error should be thrown. Instead, please call `$selected_as_tags()` or `$root_as_tags()` to retrieve the tag structures of the selected tag elements or root element respectively.
#'
#' @param tag A single tag object with possibly many children tag objects. Currently, [`tagList()`] or [`list()`]s of tags are not allowed.
#' @return A `tag_graph()` object. The `tag` supplied will be considered the `root` object. At the time of initialization, the `root` is also considered the single selected item. If any selections are made, the selected elements will be updated.
#' @md
#' @export
tag_graph <- function(tag) {
  root <- as_tag_env(tag)
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
        ## Find
        find = function(css_selector) {
          rebuild()
          set_selected(
            tag_graph_find(get_selected(), css_selector)
          )
          self
        },
        find_parents = function() {
          rebuild()
          set_selected(
            tag_graph_find_parent(get_selected())
          )
          self
        },
        find_children = function() {
          rebuild()
          set_selected(
            tag_graph_find_children(get_selected())
          )
          self
        },
        find_reset = function() {
          rebuild()
          set_selected(
            list(root)
          )
          self
        },
        ## end Find

        ## Tag Info
        # "Adds the specified class(es) to each element in the set of matched elements."
        add_class = function(class) {
          rebuild()
          tag_graph_add_class(get_selected(), class)
          self
        },
        add_attrs = function(...) {
          rebuild()
          tag_graph_add_attrs(get_selected(), ...)
          self
        },
        # .prop()
        # Get the value of a property for the first element in the set of matched elements or set one or more properties for every matched element.
        # .removeAttr()
        # Remove an attribute from each element in the set of matched elements.
        # .removeClass()
        # Remove a single class, multiple classes, or all classes from each element in the set of matched elements.
        # .toggleClass()
        # Add or remove one or more classes from each element in the set of matched elements, depending on either the class’s presence or the value of the state argument.
        ## end Tag Info

        ## Adjust Children
        # "Insert content, specified by the parameter, to the end of each element in the set of matched elements."
        append = function(...) {
          rebuild()
          tag_graph_append_children(get_selected(), ...)
          rebuild()
          self
        },
        prepend = function(...) {
          rebuild()
          tag_graph_prepend_children(get_selected(), ...)
          rebuild()
          self
        },
        empty = function() {
          rebuild()
          tag_graph_empty_children(get_selected())
          # no need to rebuild
          self
        },
        ## end Adjust Children

        ## Adjust Siblings
        # .detach() / .remove()
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
        ### Questionable methods that require parent alterting their children
        # .unwrap()
        # Remove the parents of the set of matched elements from the DOM, leaving the matched elements in their place.
        # .wrap()
        # Wrap an HTML structure around each element in the set of matched elements.
        # .wrapAll()
        # Wrap an HTML structure around all elements in the set of matched elements.
        # .wrapInner()
        # Wrap an HTML structure around the content of each element in the set of matched elements.
        ## end Adjust Siblings

        ## Generic Methods
        walk_selected = function(fn) {
          rebuild()
          tag_graph_walk(get_selected(), fn)
          rebuild()
          self
        },
        ## end Generic Methods

        ## Tag Graph fns
        # TODO is this method necessary to export if we always handle it?
        rebuild = function() {
          rebuild()
          self
        },
        set_children = function(...) {
          rebuild()
          tag_graph_set_children(get_selected, ...)
          rebuild()
          self
        },
        root = function() {
          rebuild()
          root
        },
        get_selected = function() {
          rebuild()
          get_selected()
        },
        get = function(position) {
          rebuild()
          selected <- get_selected()
          validate_position(position, selected)

          selected[[position]]
        },
        set_selected = function(selected) {
          rebuild()
          set_selected(
            tag_graph_validate_selected(root, selected)
          )
          rebuild()
          self
        },
        set = function(tag_env, position) {
          rebuild()
          selected <- get_selected()
          validate_position(position, selected)
          if (is.null(tag_env)) {
            # no need to rebuild() or validate tag_env is a part of the graph when removing selected elements
            selected[[position]] <- NULL
            set_selected(selected)
          } else {
            validated_tag_env <- tag_graph_validate_selected(root, list(tag_env))[[1]]
            selected[[position]] <- validated_tag_env
            set_selected(dropNulls(selected))
            rebuild()
          }
          self
        },
        ## end Tag Graph fns


        ### IDK how to implement
        # .val()
        # Get the current value of the first element in the set of matched elements or set the value of every matched element.

        ### Methods that do not return self
        # .text()
        # Get the combined text contents of each element in the set of matched elements, including their descendants, or set the text contents of the matched elements.
        # .css()
        # Get the value of a computed style property for the first element in the set of matched elements or set one or more CSS properties for every matched element.
        # .hasClass()
        # TODO-barret - also make a top level function
        # Determine whether any of the matched elements are assigned the given class.
        # .prop()
        # Get the value of a property for the first element in the set of matched elements or set one or more properties for every matched element.

        ## To tags
        root_as_tags = function() {
          rebuild()
          tag_env_to_tags(root)
        },
        # = .html()
        # Get the HTML contents of the first element in the set of matched elements or set the HTML contents of every matched element.
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
​
​
#' @export
`[[.tag_graph` <- function(x, position) {
  if (is.numeric(position)) {
    x$get(position)
  } else {
    NextMethod()
  }
}
​
# ' @export​
`[[<-.tag_graph` <- function(x, position, value) {
  if (is.numeric(position)) {
    x$set(value, position)
    x
  } else {
    NextMethod()
  }
}
​
​
#' @export
`[.tag_graph` <- function(x, positions) {
  if (is.numeric(positions)) {
    # return result of lapply
    lapply(positions, function(position) {
      x$get(position)
    })
  } else {
    NextMethod()
  }
}
​
#' @export
`[<-.tag_graph` <- function(x, positions, values) {
  if (is.numeric(positions)) {
    # Map handles vector recycling
    Map(
      values,
      positions,
      f = function(value, position) {
        x$set(value, position)
      }
    )
    x
  } else {
    NextMethod()
  }
}
​
​
​
validate_position <- function(position, selected) {
  stopifnot(is.numeric(position))
  stopifnot(position > 0)
  stopifnot(position <= length(selected))
}


# Call `.f(x[[i]], ...)` for all values of i
walk <- function(.x, .f, ...) {
  for (i in seq_along(.x)) {
    .f(.x[[i]], ...)
  }
  NULL
}
# walk_rev <- function(.x, .f, ...) {
#   for (i in rev(seq_along(.x))) {
#     .f(.x[[i]], ...)
#   }
#   NULL
# }
# # call `.f(x[[i]], i, ...)`
walk_i <- function(.x, .f, ...) {
  for (i in seq_along(.x)) {
    .f(.x[[i]], i, ...)
  }
  NULL
}
# Include `i` and call in reverse order
walk_i_rev <- function(.x, .f, ...) {
  for (i in rev(seq_along(.x))) {
    .f(.x[[i]], i, ...)
  }
  NULL
}

tag_graph_verify_selected <- function(els) {
  stopifnot(is.list(els))
  walk_i(els, function(el, i) {
    if (!isTagEnv(el)) {
      stop("Object in position `", i, "` is not a tag environment")
    }
  })
}

# return function that will verify before performing `func(els, fn)`
tag_graph_walk_gen <- function(func) {
  function(els, fn) {
    tag_graph_verify_selected(els)
    stopifnot(is.function(fn))

    func(els, fn)
  }
}
# tag_graph_lapply <- tag_graph_walk_gen(lapply)
tag_graph_walk <- tag_graph_walk_gen(walk)
# tag_graph_walk_rev <- tag_graph_walk_gen(walk_rev)
tag_graph_walk_i <- tag_graph_walk_gen(walk_i)
tag_graph_walk_i_rev <- tag_graph_walk_gen(walk_i_rev)



tag_graph_match_child_i <- function(els, func) {
  tag_graph_walk(els, function(el) {
    el_key <- envir_key_fn(el)
    el_parent <- el$parent
    tag_graph_walk_i_rev(parent$children, function(child, child_pos) {
      child_key <- envir_key_fn(child)
      if (el_key == child_key) {
        func(el_parent, el, child_pos)
      }
    })
  })
}
tag_graph_remove <- function(els) {
  tag_graph_match_child_i(els, function(el_parent, el, child_pos) {
    # remove parent / child relationship
    el$parent <- NULL
    el_parent$children[[child_pos]] <- NULL
  })
}


tag_graph_validate_selected <- function(root, selected) {

  if (is.null(selected)) {
    selected <- list()
  }
  root_envir_key <- envir_key_fn(root)
  # For each element...
  tag_graph_walk_i(selected, function(el, el_pos) {
    # Find the top ancestor
    cur_el <- el
    while (!is.null(parent <- cur_el$parent)) {
      cur_el <- cur_el$parent
    }

    # Validate that the top ancestor is `root`
    if (root_envir_key != envir_key_fn(cur_el)) {
      stop(
        "`selected[[", el_pos, "]]` does not have a final ancestor of this tag graph's root element.\n",
        "Root: ", root_envir_key, "\n",
        "`selected[[", i, "]]`: ", envir_key_fn(cur_el), "\n"
      )
    }
  })

  selected
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


tag_graph_find_parent <- function(els) {
  parent_map <- envir_map()
  tag_graph_walk(els, function(el) {
    parent_map$set(el$parent)
  })
  parent_map$values()
}
tag_graph_find_children <- function(els) {
  children_map <- envir_map()
  tag_graph_walk(els, function(el) {
    tag_graph_walk(el$children, function(child) {
      children_map$set(child)
    })
  })
  children_map$values()
}

tag_graph_find <- function(els, selector) {
  found_map <- envir_map()
  selector <- as_selector_list(selector)
  # for every element...
  tag_graph_walk(els, function(el) {
    # exclude the element and
    # walk through each child...
    tag_graph_walk(el$children, function(child) {
      # including the child, matching with a selector
      tag_graph_find_(child, selector, function(found_el) {
        found_map$set(found_el)
      })
    })
  })
  found_map$values()
}

tag_graph_find_ <- function(el, selector, fn) {

  if (isTagEnv(el)) {
    # grab the first element
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
    walk(el, tag_graph_find_, fn = fn, selector = selector)
  } else if (is.atomic(el) || is.function(el)) {
    return()
  } else {
    message("tag_graph_find_()")
    str(el)
    message("unknown type!!!")
  }

  invisible()
}





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
      cat0(seen_map$key_fn(x))
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
  ​
  # Using square bracket
  tag_graph(s)[[1]]
  tag_graph(s)[[1]] <- div("hello")  # Needs to convert to env
  ​
  # Using getter/setter
  tag_graph(s)$get(1)
  tag_graph(s)$set(1, div("hello"))
  tag_graph(s)$append(div("hello"))
}

if (FALSE) {
​  x <- list(a =1, b =2)
  e <- list2env(x)
  ​
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
​
  s <- sliderInput("x", "X", 1, 4, 2)
  ​
  s <- as_tag_env(s)
  # goal
  s %>% el_children() %>% add_child(div(class = "foo", "Hello")) %>% find(".foo") %>%
    to_list()
  ​

  s <- as_tag_env(sliderInput("x", "X", 1, 4, 2))
  s <- as_tag_env(s)
  ​
  ​
  ​
  # ​library(shiny)
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
  ​
  # Server logic
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
  }
  ​
  # Complete app with UI and server components
  shinyApp(ui, server)
}
