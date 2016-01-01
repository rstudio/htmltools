context("templates")

# Searches for an html dependency of format name[version], as in "d3[3.5.10]",
# within the html-dependencies script tag
findDep <- function(x, name, version) {
  deps <- sub(
    '.*<script type="application/html-dependencies">([^<]*)</script>.*',
    "\\1",
    x
  )
  grepl(paste0(name, "[", version, "]"), deps, fixed = TRUE)
}

test_that("Code blocks are evaluated and rendered correctly", {
  template <- htmlTemplate("template-document.html",
    x = div(class = "foo", "bar")
  )
  html <- renderDocument(template)

  expect_true(grepl('<div class="foo">bar</div>', html))
})

test_that("UTF-8 characters in templates", {
  template <- htmlTemplate("template-document.html", x = "")
  html <- renderDocument(template)

  # Create the string 'Δ★😎', making sure it's UTF-8 encoded on all platforms.
  # These characters are 2, 3, and 4 bytes long, respectively.
  pat <- rawToChar(as.raw(c(0xce, 0x94, 0xe2, 0x98, 0x85, 0xf0, 0x9f, 0x98, 0x8e)))
  Encoding(pat) <- "UTF-8"
  expect_true(grepl(pat, html))
})


test_that("Dependencies are added properly", {
  dep <- htmlDependency("d3", "3.5.10", c(href="shared"), script = "d3.js")

  # Add dependency by inserting a tag with a dependency
  template <- htmlTemplate("template-document.html",
    x = attachDependencies(div(), dep)
  )
  html <- renderDocument(template)
  expect_true(findDep(html, "d3", "3.5.10"))
  expect_true(grepl('<script src="shared/d3.js"></script>', html, fixed = TRUE))

  # Add dependency via a renderDocument
  template <- htmlTemplate("template-document.html", x = "")
  html <- renderDocument(template, dep)
  expect_true(findDep(html, "d3", "3.5.10"))
  expect_true(grepl('<script src="shared/d3.js"></script>', html, fixed = TRUE))
})


test_that("Dependencies can be suppressed", {
  # The template includes suppressDependencies("jquery"), so we shouldn't see
  # this dependency in the final output.
  dep <- htmlDependency("jquery", "1.11.3", c(href="shared"), script = "jquery.js")

  # Add dependency by inserting a tag with a dependency
  template <- htmlTemplate("template-document.html",
    x = attachDependencies(div(), dep)
  )
  html <- renderDocument(template)
  expect_true(findDep(html, "jquery", "9999"))
  expect_false(grepl('<script[^>]+jquery[^>]+>', html))

  # Add dependency via a renderDocument
  template <- htmlTemplate("template-document.html", x = "")
  html <- renderDocument(template, dep)
  expect_true(findDep(html, "jquery", "9999"))
  expect_false(grepl('<script[^>]+jquery[^>]+>', html))
})
