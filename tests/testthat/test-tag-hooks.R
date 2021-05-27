# expect_tag_hooks <- function(tagFunc, ..., .render = NULL, .postRender = NULL) {
#   x <- tagFunc(..., .renderHook = .render, .postRenderHook = .postRender)
#
#   y <- tagFunc(...)
#   y <- tagAddHooks(y, .render, tagAddRenderHook)
#   y <- tagAddHooks(y, .postRender, tagAddPostRenderHook)
#
#   expect_same_html(x, y)
# }
#
# expect_same_html <- function(x, y, equal = TRUE) {
#   local_edition(3)
#   if (equal) expect_equal(x, y)
#   expect_snapshot(
#     renderTags(x)[c("dependencies", "html")],
#     cran = TRUE
#   )
# }
#
# test_that("tag(.renderHook, .postRenderHook) basics", {
#   expect_tag_hooks(div, .render = span)
#   expect_tag_hooks(div, .postRender = span)
#   expect_tag_hooks(div, .render = span, .postRender = span)
#   expect_tag_hooks(
#     div, .render = function(x) stop("boom"),
#     .postRender = function(x) stop("boom2")
#   )
#
#   # Adding accumulates by default
#   expect_tag_hooks(div, .render = list(span, span))
#   expect_tag_hooks(div, .postRender = list(span, span))
#   expect_tag_hooks(div, .render = list(span, span), .postRender = list(span, span))
#
#   # But can be also be replaced
#   expect_same_html(
#     tagAddRenderHook(
#       tagAddRenderHook(div(), span),
#       h1, replace = TRUE
#     ),
#     div(.renderHook = h1)
#   )
#   expect_same_html(
#     tagAddPostRenderHook(
#       tagAddPostRenderHook(div(), span),
#       h1, replace = TRUE
#     ),
#     div(.postRenderHook = h1)
#   )
# })
#
#
# test_that("tagList(.renderHook, .postRenderHook) basics", {
#   expect_tag_hooks(tagList, "a", .render = div)
#   expect_tag_hooks(tagList, "a", .postRender = div)
#   expect_tag_hooks(tagList, "a", .render = span, .postRender = span)
#   expect_tag_hooks(tagList, "a", .render = list(span, span))
#   expect_tag_hooks(tagList, "a", .postRender = list(span, span))
#   expect_tag_hooks(tagList, "a", .postRender = list(span, span))
# })
#
#
# test_that("Can return various types of output in render hooks", {
#
#   # Strings
#   hook <- function(x) "foo"
#
#   expect_tag_hooks(div, .render = hook)
#   expect_tag_hooks(div, .postRender = hook)
#   expect_tag_hooks(tagList, .render = hook)
#   expect_tag_hooks(tagList, .postRender = hook)
#
#   # HTML dependencies
#   hook <- function(x) {
#     attachDependencies(x, htmlDependency("foo", "1.0", ""))
#   }
#
#   expect_tag_hooks(div, .render = hook)
#   expect_tag_hooks(div, .postRender = hook)
#   expect_tag_hooks(tagList, .render = hook)
#   expect_tag_hooks(tagList, .postRender = hook)
#
#   # Unresolved tags
#   hook <- function(x) span(x, .renderHook = h1)
#
#   expect_tag_hooks(div, .render = hook)
#   expect_tag_hooks(div, .postRender = hook)
#   expect_tag_hooks(tagList, .render = hook)
#   expect_tag_hooks(tagList, .postRender = hook)
#
#   # Unresolved tagList()s
#   hook <- function(x) tagList(span(), x, .renderHook = h1)
#
#   expect_tag_hooks(div, .render = hook)
#   expect_tag_hooks(div, .postRender = hook)
#   expect_tag_hooks(tagList, .render = hook)
#   expect_tag_hooks(tagList, .postRender = hook)
#
#   # List of unresolved tags
#   hook <- function(x) list(span(x, .renderHook = h1), span(x, .renderHook = h1))
#
#   expect_tag_hooks(div, .render = hook)
#   expect_tag_hooks(div, .postRender = hook)
#   expect_tag_hooks(tagList, .render = hook)
#   expect_tag_hooks(tagList, .postRender = hook)
#
#   # Nothing
#   hook <- function(x) NULL
#
#   expect_tag_hooks(div, .render = hook)
#   expect_tag_hooks(div, .postRender = hook)
#   expect_tag_hooks(tagList, .render = hook)
#   expect_tag_hooks(tagList, .postRender = hook)
# })

test_that("Pre hooks render in order", {
  # Note that, unlike tagFunction(), .renderHook's order of execution
  # doesn't follow DOM tree order (preorder, depth-first traversal),
  # but that seems like a feature, not a bug, since if you need to control
  # state of html, you can do tagList(myTag, html) for "guaranteed control"
  # over the state
  state <- 0
  renderTags(tagList(
    div(
      .renderHook = function(x) {
        expect_equal(state, 0)
        state <<- state + 1
      },
      .postRenderHook = function() {
        expect_equal(state, 2)
        state <<- state + 1
      }
    ),
    div(
      .renderHook = function(x) {
        expect_equal(state, 1)
        state <<- state + 1
      },
      .postRenderHook = function() {
        expect_equal(state, 3)
        state <<- state + 1
      }
    )
  ))
  expect_equal(state, 4)

  state <- 0
  renderTags(tagList(
    div(
      .renderHook = function(x) {
        state <<- state + 1
        message("A", state)
      },
      div(
        .renderHook = function(x) {
          state <<- state + 1
          message("B", state)
        },
        .postRenderHook = function() {
          state <<- state + 1
          message("C", state)
        }
      ),
      .postRenderHook = function() {
        state <<- state + 1
        message("D", state)
      }
    )
  ))
  expect_equal(state, 4)


  # post render hook still executes on failure
  state <- 0
  expect_warning(renderTags(tagList(
    div(
      .renderHook = function(x) {
        state <<- state + 1
        stop("boom")
      },
      .postRenderHook = function() {
        expect_equal(state, 2)
      }
    ),
    div(
      .renderHook = function(x) {
        state <<- state + 1
        stop("boom")
      },
      .postRenderHook = function() {
        expect_equal(state, 2)
      }
    )
  )))
  expect_equal(state, 2)
})

test_that("render hooks can be used to ", {
  local_edition(3)

  bar_widget <- div(
    .renderHook = function(x) {
      if (isTRUE(getOption("bar"))) tagQuery(x)$addClass("bar")$allTags() else x
    }
  )

  expect_snapshot(bar_widget, cran = TRUE)

  bar_framework <- tagList(
    htmlDependency("bar", "1.0", ""),
    .renderHook = function(x) {
      options("bar" = TRUE)
      x
    },
    .postRenderHook = function() {
      options("bar" = NULL)
    }
  )

  html <- tagList(bar_framework, bar_widget)

  expect_null(getOption("bar"))
  expect_snapshot(renderTags(html), cran = TRUE)
  expect_null(getOption("bar"))
})
