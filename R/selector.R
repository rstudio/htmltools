
# as future expansion becomes a thing, look into `selectr::parse(selector)`
# https://github.com/sjp/selectr/blob/master/R/parser.R
# selectr:::parse("#a.warning > b.mine:not(.theres) d")[[1]]$show()
#> CombinedSelector[CombinedSelector[Class[Hash[Element[*]#a].warning] > Negation[Class[Element[b].mine]:not(Class[Element[*].theres])]] <followed> Element[d]]
## ^^ R6 output

SELECTOR_EVERYTHING <- "everything"
SELECTOR_REGULAR <- "regular"

SELECTOR_SPACE <- "space"
SELECTOR_CHILD <- "child"

is_selector <- function(x) {
  inherits(x, "shiny_selector")
}
is_selector_list <- function(x) {
  inherits(x, "shiny_selector_list")
}

# only handles id and classes
as_selector <- function(selector) {
  if (is_selector(selector) || is_selector_list(selector)) {
    return(selector)
  }

  # make sure it's a trimmed string
  selector <- str_trim(paste0(selector, collapse = " "))

  # yell if there is a comma
  if (str_detect(selector, ",", fixed = TRUE)) {
    stop("Do not know how to handle comma separated selector values")
  }
  # yell if there is a `[`
  if (str_detect(selector, "[", fixed = TRUE)) {
    stop("Do not know how to handle `[` in selector values")
  }

  # yell if there is a `:`
  if (str_detect(selector, ":", fixed = TRUE)) {
    stop("Do not know how to handle special pseudo classes like `:first-child` or `:not()` in selector values")
  }

  # if it contains multiple elements, recurse
  if (str_detect(selector, "* ", fixed = TRUE)) {
    # we already match on all elements. No need to know about this selector
    warning("Removing `* ` from selector. ")
    selector <- str_remove_all(selector, "* ", fixed = TRUE)
  }

  # Check here to avoid inf recursion
  if (str_detect(selector, ">", fixed = TRUE)) {
    # If there is a `>`, pad it with spaces
    if (str_detect(selector, "(^>)|(>$)")) {
      stop(
        "Direct children selector, `>`, must not be the first element or last element",
        " in a css selector. Please add more selector information, such as `*`."
      )
    }
    # While there are any consecutive `> >` items...
    while(str_detect(selector, ">\\s*>")) {
      # If there are any `>>`, replace them with `> * >`
      selector <- str_replace_all(selector, ">\\s*>", "> * >")
    }

    # Split by `>` and convert to selectors
    # Alter parts (execpt first) to say they are a direct child
    # Return selector list
    selector_items <- lapply(strsplit(selector, ">")[[1]], as_selector)
    selector_list_items <- Map(
      selector_items,
      seq_along(selector_items),
      f = function(selector_item, i) {
        if (is_selector(selector_item)) {
          if (i > 1) selector_item$traversal <- SELECTOR_CHILD
          as_selector_list(selector_item)
        } else {
          if (i > 1) selector_item[[1]]$traversal <- SELECTOR_CHILD
          selector_item
        }
      }
    )
    selector_list <- as_selector_list(
      unlist(selector_list_items, recursive = FALSE)
    )
    return(selector_list)
  }

  # Split into a selector parts and recurse one more time
  if (str_detect(selector, "\\s")) {
    selector_items <- lapply(strsplit(selector, "\\s+")[[1]], as_selector)
    selector_list <- as_selector_list(selector_items)
    return(selector_list)
  }

  # https://www.w3.org/TR/selectors-3/#selectors

  type <- NULL
  traversal <- SELECTOR_SPACE
  element <- NULL
  id <- NULL
  classes <- NULL

  if (isTRUE(selector == "*")) {
    type <- SELECTOR_EVERYTHING
  } else {
    type <- SELECTOR_REGULAR

    ## Not needed as the regex values below work around this.
    # # if there is more than a `*`, such as `*.warning`, treat as `.warning`
    # if (str_detect(selector, "^\\*"))
    #   selector <- sub("^\\*", "", selector)
    #   if (grepl("^\\*", selector)) {
    #     stop("malformed css selector. Found at least two `**` that were not separated by a space")
    #   }
    # }

    element_regex <- "^[a-zA-Z0-9]+"
    element <- str_match_first(selector, element_regex)
    if (!is.null(element)) {
      selector <- str_remove(selector, element_regex)
    }

    ## https://www.w3.org/TR/CSS21/syndata.html#value-def-identifier
    ##  In CSS, identifiers (including element names, classes, and IDs in selectors) can contain only the characters [a-zA-Z0-9] and ISO 10646 characters U+00A0 and higher, plus the hyphen (-) and the underscore (_); they cannot start with a digit, two hyphens, or a hyphen followed by a digit. Identifiers can also contain escaped characters and any ISO 10646 character as a numeric code (see next item). For instance, the identifier "B&W?" may be written as "B\&W\?" or "B\26 W\3F".
    # # define simpler (maybe not accurate) regex
    # id_regex <- "^#[^#.:[\\s]+" # `#` then everything that isn't a `#`, `.`, `:`, or white space
    # class_regex <- "^\\.[^#.:[\\s]+" # `.` then everything that isn't a `.`, `:`, or white space

    tmp_id <- str_match_first(selector, "#[^.:[]+")
    if (!is.null(tmp_id)) {
      id <- str_remove(tmp_id, "^#")
      selector <- str_remove(selector, tmp_id, fixed = TRUE)
    }

    classes <- str_remove(str_match_all(selector, "\\.[^.:[]+"), "^\\.")
    if (length(classes) == 0) {
      classes <- NULL
    }
    # if (!is.null(classes)) {
    #   selector <- str_remove(selector, "\\.[^.:[]+")
    # }
  }

  structure(class = "shiny_selector", list(
    element = element,
    id = id,
    classes = classes,
    type = type,
    traversal = traversal
  ))
}


as_selector_list <- function(selector) {
  if (is_selector_list(selector)) {
    return(selector)
  }
  if (is.character(selector)) {
    selector <- as_selector(selector)
  }
  if (is_selector(selector)) {
    selector <- list(selector)
  }
  if (!is.list(selector)) {
    stop("Do not know how to convert non list object into a `shiny_selector_list`")
  }

  is_selector_vals <- vapply(selector, is_selector, logical(1))
  if (!all(is_selector_vals)) {
    stop("Can only convert a list of selectors to a `shiny_selector_list`")
  }
  structure(class = "shiny_selector_list", selector)
}

#' @export
format.shiny_selector <- function(x, ...) {
  paste0(
    c(
      if (x$traversal == SELECTOR_CHILD) "> ",
      if (x$type == SELECTOR_EVERYTHING) {
        "*"
      } else {
        c(
          x$element,
          if (!is.null(x$id)) paste0("#", x$id),
          if (!is.null(x$classes)) paste0(".", x$classes)
        )
      }
    ),
    collapse = ""
  )
}
#' @export
format.shiny_selector_list <- function(x, ...) {
  paste0(unlist(lapply(x, format, ...)), collapse = " ")
}

#' @export
print.shiny_selector <- function(x, ...) {
  cat("// htmltools css selector\n")
  cat(format(x, ...), "\n")
}
#' @export
print.shiny_selector_list <- function(x, ...) {
  cat("// htmltools css selector list\n")
  cat(format(x, ...), "\n")
}



str_replace <- function(text, pattern, replacement, fixed = FALSE) {
  sub(pattern = pattern, replacement = replacement, x = text, perl = !fixed, fixed = fixed)
}

str_replace_all <- function(text, pattern, replacement, fixed = FALSE) {
  gsub(pattern = pattern, replacement = replacement, x = text, perl = !fixed, fixed = fixed)
}

str_remove <- function(x, pattern, ...) {
  str_replace(x, pattern, "", ...)
}
str_remove_all <- function(x, pattern, ...) {
  str_replace_all(x, pattern, "", ...)
}

trim_leading <- function(text) {
  str_remove_all(text, pattern = "^\\s+")
}

trim_trailing <- function(text) {
  str_remove_all(text, pattern = "\\s+$")
}

str_trim <- function(text, side = "both") {
  if (side == "both" || side == "left") {
    text <- trim_leading(text)
  }
  if (side == "both" || side == "right") {
    text <- trim_trailing(text)
  }
  text
}

str_detect <- function(text, pattern, fixed = FALSE) {
  grepl(pattern = pattern, x = text, perl = !fixed, fixed = fixed)
}

# finds first, NOT all
str_match_first <- function(x, pattern, ...) {
  reg_info <- regexpr(pattern, x, ...)
  if (length(reg_info) == 1 && reg_info == -1) {
    return(NULL)
  }

  regmatches(x, reg_info)
}

# return a vector of matches or NULL
str_match_all <- function(x, pattern, ...) {
  if (length(x) != 1) {
    stop("`x` must have a length of 1")
  }
  reg_info <- gregexpr(pattern, x, ...)
  first <- reg_info[[1]]
  if (length(first) == 1 && first == -1) {
    return(NULL)
  }

  regmatches(x, reg_info)[[1]]
}



is_html <- function(x) {
  identical(attr(x, "html"), TRUE)
}
