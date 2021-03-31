
# as future expansion becomes a thing, look into `selectr::parse(selector)`
# https://github.com/sjp/selectr/blob/master/R/parser.R
# selectr:::parse("#a.warning > b.mine:not(.theres) d")[[1]]$show()
#> CombinedSelector[CombinedSelector[Class[Hash[Element[*]#a].warning] > Negation[Class[Element[b].mine]:not(Class[Element[*].theres])]] <followed> Element[d]]
## ^^ R6 output

# only handles id and classes
as_selector <- function(selector) {
  if (inherits(selector, "shiny_selector") || inherits(selector, "shiny_selector_list")) {
    return(selector)
  }

  # make sure it's a trimmed string
  selector <- str_trim(selector)


  # yell if there is a comma
  if (str_detect(selector, ",", fixed = TRUE)) {
    stop("Do not know how to handle comma separated selector values")
  }
  # yell if there is a `[`
  if (str_detect(selector, "[", fixed = TRUE)) {
    stop("Do not know how to handle `[` in selector values")
  }

  # if it contains multiple elements, recurse
  if (str_detect(selector, " ")) {
    if (str_detect(selector, "* ", fixed = TRUE)) {
      stop("Do not know how to handle `*` in the middle of a selector. Please remove it.")
    }

    selector_items <- lapply(strsplit(selector, "\\s+")[[1]], as_selector)
    selector_list <- structure(class = "shiny_selector_list", selector_items)
    return(selector_list)
  }

  if (str_detect(selector, ":", fixed = TRUE)) {
    if (str_detect(selector, ":(?!text$)", perl = TRUE)) {
      stop("Do not know how to handle special pseudo classes like `:first-child` in selector values other than `:text`")
    }
  }

  # https://www.w3.org/TR/selectors-3/#selectors

  match_everything <- isTRUE(all.equal(selector, "*"))

  element <- NULL
  id <- NULL
  classes <- NULL
  text_children <- FALSE

  if (!match_everything) {
    # # if there is more than a `*`, such as `*.warning`, treat as `.warning`
    # if (str_detect(selector, "^\\*"))
    #   selector <- sub("^\\*", "", selector)
    #   if (grepl("^\\*", selector)) {
    #     stop("malformed css selector. Found at least two `**` that were not separated by a space")
    #   }
    # }

    element_regex <- "^[a-zA-Z0-9]+"
    element <- str_match(selector, element_regex)
    if (!is.null(element)) {
      selector <- str_remove(selector, element_regex)
    }

    # # https://www.w3.org/TR/CSS21/syndata.html#value-def-identifier
    # #  In CSS, identifiers (including element names, classes, and IDs in selectors) can contain only the characters [a-zA-Z0-9] and ISO 10646 characters U+00A0 and higher, plus the hyphen (-) and the underscore (_); they cannot start with a digit, two hyphens, or a hyphen followed by a digit. Identifiers can also contain escaped characters and any ISO 10646 character as a numeric code (see next item). For instance, the identifier "B&W?" may be written as "B\&W\?" or "B\26 W\3F".
    # # define simpler (maybe not accurate) regex
    # id_regex <- "^#[^#.:[\\s]+" # `#` then everything that isn't a `#`, `.`, `:`, or white space
    # class_regex <- "^\\.[^#.:[\\s]+" # `.` then everything that isn't a `.`, `:`, or white space

    # # continuously go over selector for id, class, etc
    # found <- TRUE
    # while (found) {
    #   if (nchar(selector) == 0) {
    #     break
    #   }
    #   # make sure the selector string is shrinking
    #   found <- FALSE
    #   prior_size <- cur_size

    #   if (str_detect(selector, id_regex)) {
    #     if (!is.null(id)) {
    #       stop("prior id: `", id, "` already found")
    #     }
    #     found <- TRUE
    #     id <- str_match(selector, id_regex)
    #     selector <- str_remove(selector, id_regex)
    #   }

    #   if (str_detect(selector, class_regex)) {
    #     found <- TRUE
    #     class_val <- str_match(selector, class_regex)
    #     classes <- c(classes, class_val)
    #     selector <- str_remove(selector, class_regex)
    #   }
    # }
    # if (found) {
    #   stop("could not reduce remaining selector: ", selector)
    # }

    id <- str_remove(str_match(selector, "#[^.:[]+"), "#")
    if (!is.null(id)) {
      selector <- str_remove(selector, "#[^.:[]+")
    }

    classes <- str_remove(str_match_all(selector, "\\.[^.:[]+"), "^\\.")
    if (!is.null(classes)) {
      selector <- str_remove(selector, "\\.[^.:[]+")
    }

    text_children <- str_detect(selector, ":text$")
  }

  structure(class = "shiny_selector", list(
    element = element,
    id = id,
    classes = classes,
    match_everything = match_everything,
    text_children = text_children
  ))
}


as_selector_list <- function(selector) {
  selector <- as_selector(selector)
  if (inherits(selector, "shiny_selector")) {
    selector <- structure(class = "shiny_selector_list", list(selector))
  }

  # validate
  is_text <- which(vapply(selector, `[[`, logical(1), "text_children"))
  if (length(is_text) > 1) {
    stop("Only one `:text` pseudo class can be used")
  }
  if (length(is_text) == 1 && is_text != length(selector)) {
    stop("The pseudo class `:text` can only be applied on the last element selection")
  }

  selector
}

#' @export
format.shiny_selector <- function(x, ...) {
  if (x$match_everything) {
    paste0("*")
  } else {
    paste0(x$element, if (!is.null(x$id)) paste0("#", x$id), if (!is.null(x$classes)) paste0(".", x$classes, collapse = ""), if(x$text_children) ":text")
  }
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



str_trim <- function(x) {
  sub(
    "\\s+$", "",
    sub("^\\s+", "", x)
  )
}

str_detect <- function(x, pattern, ...) {
  grepl(pattern, x, ...)
}

# finds first, NOT all
str_match <- function(x, pattern) {
  reg_info <- regexpr(pattern, x)
  if (length(reg_info) == 1 && reg_info == -1) {
    return(NULL)
  }

  regmatches(x, reg_info)
}

# return a vector of matches or NULL
str_match_all <- function(x, pattern) {
  stopifnot(length(x) == 1)
  reg_info <- gregexpr(pattern, x)
  first <- reg_info[[1]]
  if (length(first) == 1 && first == -1) {
    return(NULL)
  }

  regmatches(x, reg_info)[[1]]
}

str_replace <- function(x, pattern, value) {
  reg_info <- regexpr(pattern, x)
  if (length(reg_info) == 1 && reg_info == -1) {
    return(x)
  }

  regmatches(x, reg_info) <- value
  x
}

str_remove <- function(x, pattern) {
  str_replace(x, pattern, "")
}


is_html <- function(x) {
  identical(attr(x, "html"), TRUE)
}
