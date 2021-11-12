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
  expect_warning(expect_resolved_deps(
    list(a1.1, tagFunction(function() { div("foo", a1.2, a1.2.1) })),
    list(a1.2.1)
  ))
  expect_warning(expect_resolved_deps(
    list(a1.1, tagFunction(function() { tagList(a1.2, a1.2.1) })),
    list(a1.2.1)
  ))
  expect_warning(expect_resolved_deps(
    list(a1.1, tagFunction(function() {
      tagList(div("foo", a1.2), div("foo", a1.2.1))
    })),
    list(a1.2.1)
  ))
  expect_warning(expect_resolved_deps(
    list(a1.1, tagFunction(function() {
      tagList(div("foo", a1.2), div("foo", tagFunction(function() { a1.2.1 }))) }
    )),
    list(a1.2.1)
  ))
  res <- subtractDependencies(list(a1.2.1, b1.0.1), list(a1.1), warnOnConflict = FALSE)
  expect_identical(res, list(b1.0.1))
  expect_warning(subtractDependencies(list(a1.2.1, b1.0.1), list(a1.1)))
})

expect_html_deps <- function(x, html, deps) {
  expect_identical(as.character(renderTags(x)$html), html)
  expect_output(print(as.tags(x)), html)
  expect_identical(findDependencies(x), deps)
}

test_that("Inline dependencies", {
  # Test out renderTags and findDependencies when tags are inline
  a1.1 <- htmlDependency("a", "1.1", c(href="/"))
  a1.2 <- htmlDependency("a", "1.2", c(href="/"))

  # tagLists ----------------------------------------------------------
  x <- tagList(a1.1, div("foo"), "bar")
  expect_html_deps(x, "<div>foo</div>\nbar", list(a1.1))

  x <- tagList(a1.1, div("foo"), a1.2, "bar")
  expect_html_deps(x, "<div>foo</div>\nbar", list(a1.1, a1.2))

  x <- tagList(a1.1, div("foo"), "bar", tagFunction(function() { a1.2 }))
  expect_html_deps(x, "<div>foo</div>\nbar", list(a1.1, a1.2))

  x <- tagList(a1.1, div("foo"), "bar", tagFunction(function() { div("baz", a1.2) }))
  expect_html_deps(x, "<div>foo</div>\nbar\n<div>baz</div>", list(a1.1, a1.2))

  # Mixing inline and attribute dependencies
  x <- attachDependencies(
    tagList(a1.1, div("foo"), "bar"),
    a1.2, append = TRUE
  )
  expect_html_deps(x, "<div>foo</div>\nbar", list(a1.1, a1.2))

  x <- attachDependencies(
    tagList(a1.1, div("foo"), "bar"),
    tagFunction(function() { a1.2 }),
    append = TRUE
  )
  expect_html_deps(x, "<div>foo</div>\nbar", list(a1.1, a1.2))

  x <- attachDependencies(
    tagList(div("foo"), "bar", tagFunction(function() { a1.1 })),
    tagFunction(function() { a1.2 }),
    append = TRUE
  )
  expect_html_deps(x, "<div>foo</div>\nbar", list(a1.1, a1.2))

  # tags with children ------------------------------------------------
  x <- div(a1.1, div("foo"), "bar")
  expect_html_deps(x, "<div>\n  <div>foo</div>\n  bar\n</div>", list(a1.1))

  x <- div(
    tagFunction(function() { a1.1 }),
    tagFunction(function() { div("foo") }),
    tagFunction(function() { "bar" })
  )
  expect_html_deps(x, "<div>\n  <div>foo</div>\n  bar\n</div>", list(a1.1))

  x <- tagFunction(function() {
    div(div("foo"), a1.2, tagFunction(function() { "bar"}), a1.1)
  })
  expect_html_deps(x, "<div>\n  <div>foo</div>\n  bar\n</div>", list(a1.2, a1.1))

  x <- attachDependencies(div(a1.1, div("foo"), "bar"), a1.2, append = TRUE)
  expect_html_deps(x, "<div>\n  <div>foo</div>\n  bar\n</div>", list(a1.1, a1.2))

  x <- attachDependencies(div(a1.1, div("foo"), "bar"), tagFunction(function() { a1.2 }), append = TRUE)
  expect_html_deps(x, "<div>\n  <div>foo</div>\n  bar\n</div>", list(a1.1, a1.2))

  # Passing normal lists to tagLists and tag functions  ---------------
  x <- tagList(list(a1.1, div("foo")), "bar")
  expect_html_deps(x, "<div>foo</div>\nbar", list(a1.1))

  x <- tagList(list(tagFunction(function() { a1.1 }), div("foo")), "bar")
  expect_html_deps(x, "<div>foo</div>\nbar", list(a1.1))

  x <- div(list(a1.1, div("foo")), "bar")
  expect_html_deps(x, "<div>\n  <div>foo</div>\n  bar\n</div>", list(a1.1))

  x <- div(list(tagFunction(function() { a1.1 }), div("foo")), "bar")
  expect_html_deps(x, "<div>\n  <div>foo</div>\n  bar\n</div>", list(a1.1))

  # Top-level lists -----------------------------------
  x <- list(div("ab"), "cd", a1.1)
  expect_html_deps(x, "<div>ab</div>\ncd", list(a1.1))

  x <- structure(list(div("ab"), "cd", a1.1), class = "foo")
  expect_html_deps(x, "<div>ab</div>\ncd", list(a1.1))

  x <- tagList(tagFunction(function() { list(div("ab"), "cd", a1.1) }), "bar")
  expect_html_deps(x, "<div>ab</div>\ncd\nbar", list(a1.1))
})

test_that("Modifying children using dependencies", {
  a1.1 <- htmlDependency("a", "1.1", c(href="/"))
  a1.2 <- htmlDependency("a", "1.2", c(href="/"))

  x <- tagAppendChild(div(a1.1), a1.2)
  expect_identical(findDependencies(x), list(a1.1, a1.2))

  x <- tagAppendChild(div(a1.1), tagFunction(function() { a1.2 }))
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


test_that("able to resolve HTML scripts supplied with & without integrity", {
  src1 <- "https://cdn.com/libs/p1/0.1/"
  src2 <- "https://cdn/libs/p2/0.2/"
  deps <- list(
    htmlDependency(
      name = "p1",
      version = "0.1",
      src = list(href = src1),
      script = list(
        src = "p1.min.js",
        integrity = "longhash",
        crossorigin = "anonymous",
        defer = NA
      )
    ),
    htmlDependency(
      "p2", version = "0.2",
      src = list(href = src2),
      script = "p2.min.js"
    )
  )

  expect1 <- paste(
    '<script src="', src1, 'p1.min.js','" ',
    'integrity="longhash" ',
    'crossorigin="anonymous" defer></script>',
    sep = ''
  )
  expect2 <- paste(
    '<script src="', src2, 'p2.min.js','"></script>',
    sep = ''
  )

  expect <- paste(expect1, expect2, sep = '\n')

  class(expect) <- c("html", "character")

  actual <- renderDependencies(deps)



  expect_equal(!!strsplit(actual, "\n"), !!strsplit(expect, "\n"))
})

test_that(
  "can render scripts given as lists of nested lists + scalar strings", {
    src = "https://cdn.com/libs/p1/0.1"
    nm <- "p1.js"

    d1 <- htmlDependency(
      "p1", "0.1", src = list(href = src),
      script = list(src = nm)
    )

    deps1 <- list(
      d1,
      htmlDependency(
        "p1", "0.2", src = list(href = src),
        script = nm
      ),
      htmlDependency(
        "p1", "0.3", src = list(href = src),
        script = list(list(src = nm))
      )
    )

    out <- renderDependencies(deps1)

    deps2 <- list(
      d1,
      d1,
      d1
    )

    expect_length(unique(unlist(strsplit(out, "\n"))), 1)

    expect_equal(renderDependencies(deps1), renderDependencies(deps2))

    nm2 <- "p1-0.1.js"

    deps3 <- list(
      htmlDependency(
        "p1", "0.1", src = list(href = src),
        script = c(nm, nm2)
      )
    )

    out <- renderDependencies(deps3)

    src_urls <- c(
      file.path(src, nm),
      file.path(src, nm2)
      )

    expect <- paste(
      '<script src="', src_urls[[1]],'"></script>\n',
      '<script src="', src_urls[[2]],'"></script>',
      sep = "")

    expect_equal(!!as.character(out), !!expect)

    deps4 <- list(
      htmlDependency(
        "p1", "0.1", src = list(href = src),
        script = list(list(src = nm, integrity = "hash"), nm2)
      )
    )

    out <- renderDependencies(deps4)

    expect <- paste(
      '<script src="', src_urls[[1]], '" integrity="hash"></script>\n',
      '<script src="', src_urls[[2]], '"></script>',
      sep = "")

    expect_equal(!!as.character(out), !!expect)
  })

test_that("html escaping is carried out correctly in script rendering", {
  src = "https://cdn.com/libs/p1/0.1"
  nm <- "p1.js"
  funky_hash <- "<hash>"

  deps <- list(
    htmlDependency(
      "p1", "0.1", src = list(href = src),
      script = list(src = nm, integrity = funky_hash)
    )
  )

  src_url <- file.path(src, nm)

  expect <- paste(
    '<script',
    ' src="', src_url, '"',
    ' integrity="', htmlEscape(funky_hash), '"',
    '></script>',
    sep = ""
  )

  out <- renderDependencies(deps)
  expect_equal(!!as.character(out), !!expect)

})

test_that("copyDependencyToDir() doesn't create an empty directory", {
  tmp_dep <- tempfile("dep")
  dir.create(tmp_dep)
  on.exit(unlink(tmp_dep))

  tmp_rmd <- tempfile("rmd_files")
  dir.create(tmp_rmd)
  on.exit(unlink(tmp_rmd), add = TRUE)

  empty <- htmltools::htmlDependency(
    name = "empty",
    version = "9.9.9",
    src = tmp_dep,
    head = "<script>alert('boo')</script>",
    all_files = FALSE
  )

  copied_dep <- copyDependencyToDir(empty, tmp_rmd)
  # no directory is created for the empty dep
  expect_equal(dir(tmp_rmd), character())
  # copied dependency src points to folder where files should be so that
  # to keep relativeTo() from throwing an error later in Rmd render process
  expect_match(copied_dep$src$file, normalizePath(tmp_rmd, "/", TRUE), fixed = TRUE)
})
