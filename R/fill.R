#' Allow tags to intelligently fill their container
#'
#' Create fill containers and items. If a fill item is a direct child of a fill
#' container with a fixed height, then the item is allowed to grow and shrink to
#' its container's size.
#'
#' @details `asFillContainer()` changes the CSS `display` property on the tag to
#'   `flex`, which changes the way it does layout of it's direct children. Thus,
#'   one should be careful not to mark a tag as a fill container when it needs
#'   to rely on other `display` behavior.
#'
#' @param x a [tag()] object.
#' @param ... arguments passed along to [tagAppendAttributes()].
#' @param height,width Any valid [CSS unit][htmltools::validateCssUnit] (e.g.,
#'   height="200px").
#' @param asItem whether or not to also treat the container as an item. This is
#'   useful if the tag wants to both be a direct child of a fill container and a
#'   direct parent of a fill item.
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
#' tagz <- asFillContainer(tagz)
#' tagz <- asFillItem(tagz, .cssSelector = "#inner")
#'
#' # Inner does fill outer
#' if (interactive()) browsable(tagz)
#'
asFillContainer <- function(x, ..., height = NULL, width = NULL, asItem = FALSE) {
  if (!inherits(x, "shiny.tag")) {
    return(throwFillWarning(x))
  }

  x <- tagAppendAttributes(
    x, class = "html-fill-container",
    class = if (asItem) "html-fill-item",
    style = css(
      height = validateCssUnit(height),
      width = validateCssUnit(width)
    ),
    ...
  )

  attachDependencies(x, fillDependencies(), append = TRUE)
}


#' @export
#' @rdname asFillContainer
asFillItem <- function(x, ..., height = NULL, width = NULL) {
  if (!inherits(x, "shiny.tag")) {
    return(throwFillWarning(x, "item"))
  }

  tagAppendAttributes(
    x, class = "html-fill-item",
    style = css(
      height = validateCssUnit(height),
      width = validateCssUnit(width)
    ),
    ...
  )
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

throwFillWarning <- function(x, type = "container") {
  rlang::warn(
    paste0(
      "Don't know how to treat an object of type '",
      class(x)[1], "' as a fill ", type, ". ",
      "Only a htmltools::tag() object may be treated as a fill ", type
    ),
    class = "htmltools-fill-input"
  )
  x
}

