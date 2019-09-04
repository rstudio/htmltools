test_that("print.html preserves dependencies for HTML()", {
  # Regression test for issue #125
  dep <- htmlDependency("dummytestdep", "1.0", c(href="http://example.com/"),
    script = "test.js"
  )

  url <- NULL
  op <- options(viewer = function(url) {
    url <<- url
  })
  on.exit(options(op))

  print(attachDependencies(HTML("test"),
    list(dep)
  ), browse = TRUE)

  result_contents <- readLines(url)
  expect_true(any(grepl("http://example.com/test.js", result_contents)))
})

test_that("CRLF is properly handled", {
  txt <- paste(c("x", "y", ""), collapse = "\r\n")

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))

  writeBin(charToRaw(txt), tmp)

  obj <- tagList(
    includeHTML(tmp),
    includeCSS(tmp),
    includeMarkdown(tmp),
    includeScript(tmp),
    includeText(tmp),
    txt,
    HTML(txt)
  )

  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  save_html(obj, out)

  chr <- readChar(out, file.size(out))
  expect_false(grepl("\r\r\n", chr))

  expect_false(grepl("\r\r\n", as.character(obj)))
})

test_that("Special characters are not re-encoded", {
  # https://github.com/rstudio/htmltools/pull/117
  f <- tempfile(fileext = ".html")
  withr::with_options(
    list(encoding = "UTF-8"),
    save_html(div("brûlée"), f)
  )
  any(grepl("brûlée", readLines(f)))
})
