# Some basic test coverage of asFillContainer() and asFillItem().
# Note that these expectations aren't as important as the e2e test coverage
# we'll have via bslib::card(), shiny::plotOutput(), shiny::uiOutput()
# (those will also be testing the client-side CSS)
test_that("asFillContainer() and asFillItem()", {

  container <- asFillContainer(div())
  item <- asFillItem(div())
  expect_equal(tagGetAttribute(container, "class"), "html-fill-container")
  expect_equal(tagGetAttribute(item, "class"), "html-fill-item")

  container <- asFillContainer(
    div(span()), asItem = TRUE, .cssSelector = "span", height = 300
  )
  expect_equal(
    tagGetAttribute(container$children[[1]], "class"),
    "html-fill-container html-fill-item"
  )
  expect_equal(
    tagGetAttribute(container$children[[1]], "style"),
    "height:300px;"
  )

  expect_warning(
    asFillContainer(tagList()),
    "Don't know how to treat an object of type"
  )
  expect_warning(
    asFillItem(tagList()),
    "Don't know how to treat an object of type"
  )
})
