#' Allow tags to intelligently fill their container
#'
#' Create fill containers and items. If a fill item is a direct child of a fill
#' container, and that container has an opinionated height, then the item is
#' allowed to grow and shrink to its container's size.
#'
#' @param x a [tag()] object. Can also be a valid [tagQuery()] input if
#'   `.cssSelector` is specified.
#' @param ... currently unused.
#' @param item whether or not to treat `x` as a fill item.
#' @param container whether or not to treat `x` as a fill container. Note, this
#'   will set the CSS `display` property on the tag to `flex` which can change how
#'   its direct children are rendered. Thus, one should be careful not to
#'   mark a tag as a fill container when it needs to rely on other `display`
#'   behavior.
#' @param overwrite whether or not to override previous calls to
#'   `bindFillRole()` (e.g., to remove the item/container role from a tag).
#' @param .cssSelector A character string containing a CSS selector for
#'   targeting particular (inner) tag(s) of interest. For more details on what
#'   selector(s) are supported, see [tagAppendAttributes()].
#'
#' @returns The original tag object (`x`) with additional attributes (and a
#'   [htmlDependency()]).
#'
#' @export
#' @examples
#'
#' tagz <- div(
#'   id = "outer",
#'   style = css(
#'     height = "600px",
#'     border = "3px red solid"
#'   ),
#'   div(
#'     id = "inner",
#'     style = css(
#'       height = "400px",
#'       border = "3px blue solid"
#'     )
#'   )
#' )
#'
#' # Inner doesn't fill outer
#' if (interactive()) browsable(tagz)
#'
#' tagz <- bindFillRole(tagz, container = TRUE)
#' tagz <- bindFillRole(tagz, item = TRUE, .cssSelector = "#inner")
#'
#' # Inner does fill outer
#' if (interactive()) browsable(tagz)
#'
bindFillRole <- function(x, ..., item = FALSE, container = FALSE, overwrite = FALSE, .cssSelector = NULL) {

  check_dots_empty()

  hasSelection <- FALSE
  query <- NULL
  if (!is.null(.cssSelector)) {

    try(silent = TRUE, {
      query <- tagQuery(x)$find(.cssSelector)
      hasSelection <- length(query$selectedTags()) > 0
    })

    if (!hasSelection) {
      rlang::warn(
        paste0(
          "`bindFillRole()` didn't find any tags matching the .cssSelector: '", .cssSelector, "'. ",
          "Thus, it won't apply any fill roles."
        ),
        class = "htmltools_fill_role_selector"
      )
      return(x)
    }
  }

  if (!(inherits(x, "shiny.tag") || hasSelection)) {
    rlang::warn(
      paste0(
        "`bindFillRole()` only works on htmltools::tag() objects (e.g., div(), p(), etc.), ",
        "not objects of type '", class(x)[1], "'. "
      ),
      class = "htmltools_fill_role_object"
    )
    return(x)
  }

  x <- tagAppendAttributes(
    x, .cssSelector = .cssSelector,
    class = if (item) "html-fill-item",
    class = if (container) "html-fill-container"
  )

  if (container) {
    x <- attachDependencies(x, fillDependencies(), append = TRUE)
  }

  if (!overwrite) {
    return(x)
  }

  query <- query %||% tagQuery(x)

  # removeClass() removes all occurrences of a given class
  if (!item) {
    query <- query$removeClass("html-fill-item")
  }
  if (!container) {
    query <- query$removeClass("html-fill-container")
  }

  query$allTags()
}


fillDependencies <- function() {
  htmlDependency(
    name = "htmltools-fill",
    version = get_package_version("htmltools"),
    package = "htmltools",
    src = "fill",
    stylesheet = "fill.css"
  )
}
