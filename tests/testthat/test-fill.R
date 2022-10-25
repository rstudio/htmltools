# Some basic test coverage of asFillContainer() and asFillItem().
# Note that these expectations aren't as important as the e2e test coverage
# we'll have via bslib::card(), shiny::plotOutput(), shiny::uiOutput()
# (those will also be testing the client-side CSS)
test_that("asFillContainer() and asFillItem()", {

  x <- bindFillRole(div(), container = TRUE)
  expect_true(
    doRenderTags(x) == "<div class=\"html-fill-container\"></div>"
  )

  x <- bindFillRole(div(), item = TRUE)
  expect_true(
    doRenderTags(x) == "<div class=\"html-fill-item\"></div>"
  )

  x <- bindFillRole(x, container = TRUE, overwrite = TRUE)
  expect_true(
    doRenderTags(x) == "<div class=\"html-fill-container\"></div>"
  )

  x <- bindFillRole(
    div(span()), .cssSelector = "span", container = TRUE, item = TRUE
  )
  expect_true(
    doRenderTags(x) == "<div>\n  <span class=\"html-fill-item html-fill-container\"></span>\n</div>"
  )

  x <- bindFillRole(x, .cssSelector = "span", container = FALSE, item = FALSE, overwrite = TRUE)

  expect_true(
    doRenderTags(x) == "<div>\n  <span></span>\n</div>"
  )

  x <- bindFillRole(
    tagList(div(span())), .cssSelector = "span", container = TRUE
  )
  expect_true(
    doRenderTags(x) == "<div>\n  <span class=\"html-fill-container\"></span>\n</div>"
  )

  expect_warning(
    bindFillRole(tagList()),
    "htmltools::tag"
  )
  expect_warning(
    bindFillRole(tagList()),
    "htmltools::tag"
  )

  expect_warning(
    bindFillRole(div(span()), .cssSelector = "foo"),
    "cssSelector"
  )
})
