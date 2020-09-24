context("dependencies")

format.html_dependency <- function(x, ...) {
  sprintf("%s v%s @ %s", x$name, x$version, format(x$src))
}
print.html_dependency <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

expect_resolved_deps <- function(input, output) {
  expect_identical(resolveDependencies(input), output)
}

test_that("Dependency resolution works", {

  a1.1 <- htmlDependency("a", "1.1", c(href="/"))
  a1.2 <- htmlDependency("a", "1.2", c(href="/"))
  a1.2.1 <- htmlDependency("a", "1.2.1", c(href="/"))
  b1.0.0 <- htmlDependency("b", "1.0.0", c(href="/"))
  b1.0.1 <- htmlDependency("b", "1.0.1", c(href="/"))
  c1.0 <- htmlDependency("c", "1.0", c(href="/"))

  expect_resolved_deps(
    list(a1.1, b1.0.0, b1.0.1, a1.2, a1.2.1, b1.0.0, b1.0.1, c1.0),
    list(a1.2.1, b1.0.1, c1.0)
  )
  expect_resolved_deps(
    list(tagFunction(function() { NULL })),
    list()
  )
  expect_resolved_deps(
    list(tagFunction(function() { a1.2 })),
    list(a1.2)
  )
  expect_resolved_deps(
    list(a1.1, tagFunction(function() { NULL })),
    list(a1.1)
  )
  expect_resolved_deps(
    list(a1.1, tagFunction(function() { a1.2 })),
    list(a1.2)
  )
  expect_resolved_deps(
    list(a1.1, tagFunction(function() { list(a1.2, a1.2.1) })),
    list(a1.2.1)
  )
  expect_resolved_deps(
    list(a1.1, tagFunction(function() { div("foo", a1.2, a1.2.1) })),
    list(a1.2.1)
  )
  expect_resolved_deps(
    list(a1.1, tagFunction(function() { tagList(a1.2, a1.2.1) })),
    list(a1.2.1)
  )
  expect_resolved_deps(
    list(a1.1, tagFunction(function() {
      tagList(div("foo", a1.2), div("foo", a1.2.1))
    })),
    list(a1.2.1)
  )
  expect_resolved_deps(
    list(a1.1, tagFunction(function() {
      tagList(div("foo", a1.2), div("foo", tagFunction(function() { a1.2.1 }))) }
    )),
    list(a1.2.1)
  )
  res <- subtractDependencies(list(a1.2.1, b1.0.1), list(a1.1), warnOnConflict = FALSE)
  expect_identical(res, list(b1.0.1))
  expect_warning(subtractDependencies(list(a1.2.1, b1.0.1), list(a1.1)))
})

test_that("Inline dependencies", {
  # Test out renderTags and findDependencies when tags are inline
  a1.1 <- htmlDependency("a", "1.1", c(href="/"))
  a1.2 <- htmlDependency("a", "1.2", c(href="/"))

  # tagLists ----------------------------------------------------------
  x <- tagList(a1.1, div("foo"), "bar")
  expect_identical(findDependencies(x), dots_list(a1.1))
  expect_identical(as.character(renderTags(x)$html), "<div>foo</div>\nbar")

  x <- tagList(a1.1, div("foo"), a1.2, "bar")
  expect_identical(findDependencies(x), dots_list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html), "<div>foo</div>\nbar")

  x <- tagList(a1.1, div("foo"), "bar", tagFunction(function() { a1.2 }))
  expect_identical(findDependencies(x), dots_list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html), "<div>foo</div>\nbar")

  x <- tagList(a1.1, div("foo"), "bar", tagFunction(function() { div("baz", a1.2) }))
  expect_identical(findDependencies(x), dots_list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html), "<div>foo</div>\nbar\n<div>baz</div>")

  # Mixing inline and attribute dependencies
  x <- attachDependencies(
    tagList(a1.1, div("foo"), "bar"),
    a1.2, append = TRUE
  )
  expect_identical(findDependencies(x), dots_list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html), "<div>foo</div>\nbar")

  x <- attachDependencies(
    tagList(a1.1, div("foo"), "bar"),
    tagFunction(function() { a1.2 }),
    append = TRUE
  )
  expect_identical(findDependencies(x), dots_list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html), "<div>foo</div>\nbar")

  x <- attachDependencies(
    tagList(div("foo"), "bar", tagFunction(function() { a1.1 })),
    tagFunction(function() { a1.2 }),
    append = TRUE
  )
  expect_identical(findDependencies(x), dots_list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html), "<div>foo</div>\nbar")

  # tags with children ------------------------------------------------
  x <- div(a1.1, div("foo"), "bar")
  expect_identical(findDependencies(x), list(a1.1))
  expect_identical(as.character(renderTags(x)$html),
                   "<div>\n  <div>foo</div>\n  bar\n</div>")

  x <- div(
    tagFunction(function() { a1.1 }),
    tagFunction(function() { div("foo") }),
    tagFunction(function() { "bar" })
  )
  expect_identical(findDependencies(x), list(a1.1))
  expect_identical(as.character(renderTags(x)$html),
                   "<div>\n  <div>foo</div>\n  bar\n</div>")

  x <- tagFunction(function() {
    div(div("foo"), a1.2, tagFunction(function() { "bar"}), a1.1)
  })
  expect_identical(findDependencies(x), list(a1.2, a1.1))
  expect_identical(as.character(renderTags(x)$html),
                   "<div>\n  <div>foo</div>\n  bar\n</div>")

  x <- attachDependencies(div(a1.1, div("foo"), "bar"), a1.2, append = TRUE)
  expect_identical(findDependencies(x), list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html),
                   "<div>\n  <div>foo</div>\n  bar\n</div>")

  x <- attachDependencies(div(a1.1, div("foo"), "bar"), tagFunction(function() { a1.2 }), append = TRUE)
  expect_identical(findDependencies(x), list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html),
                   "<div>\n  <div>foo</div>\n  bar\n</div>")

  # Passing normal lists to tagLists and tag functions  ---------------
  x <- tagList(list(a1.1, div("foo")), "bar")
  expect_identical(findDependencies(x), dots_list(a1.1))

  x <- tagList(list(tagFunction(function() { a1.1 }), div("foo")), "bar")
  expect_identical(findDependencies(x), dots_list(a1.1))

  x <- div(list(a1.1, div("foo")), "bar")
  expect_identical(findDependencies(x), list(a1.1))

  x <- div(list(tagFunction(function() { a1.1 }), div("foo")), "bar")
  expect_identical(findDependencies(x), list(a1.1))

})

test_that("Modifying children using dependencies", {
  a1.1 <- htmlDependency("a", "1.1", c(href="/"))
  a1.2 <- htmlDependency("a", "1.2", c(href="/"))

  x <- tagAppendChild(div(a1.1), a1.2)
  expect_identical(findDependencies(x), list(a1.1, a1.2))

  x <- tagAppendChild(div(a1.1), tagFunction(function() { a1.2 }))
  expect_identical(findDependencies(x), list(a1.1, a1.2))

  # TODO: appending doesn't currently work with a top-level tagFunction(), but it probably could...
  x <- tagAppendChild(div(tagFunction(function() { div(a1.1) })), tagFunction(function() { a1.2 }))
  expect_identical(findDependencies(x), list(a1.1, a1.2))

  x <- tagAppendChild(div(a1.1), list(a1.2))
  expect_identical(findDependencies(x), list(a1.1, a1.2))

  x <- tagAppendChildren(div(), a1.1, list(a1.2))
  expect_identical(findDependencies(x), list(a1.1, a1.2))

  x <- tagAppendChildren(div(), a1.1, tagFunction(function() { list(a1.2) }))
  expect_identical(findDependencies(x), list(a1.1, a1.2))

  x <- tagSetChildren(div("foo", a1.1), a1.2)
  expect_identical(findDependencies(x), list(a1.2))

  x <- tagSetChildren(div("foo", a1.1), tagFunction(function() { a1.2 }))
  expect_identical(findDependencies(x), list(a1.2))
})
