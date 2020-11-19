test_that("print.html preserves dependencies for HTML()", {
  # Regression test for issue #125
  dep <- htmlDependency("dummytestdep", "1.0", c(href="http://example.com/"),
    script = "test.js"
  )

  url <- NULL
  op <- options(viewer = function(url) {
    url <<- url
  })
  on.exit(options(op), add = TRUE)

  print(attachDependencies(HTML("test"),
    list(dep)
  ), browse = TRUE)

  result_contents <- readLines(url)
  expect_true(any(grepl("http://example.com/test.js", result_contents)))
})

test_that("CRLF is properly handled", {
  txt <- paste(c("x", "y", ""), collapse = "\r\n")

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)

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
  on.exit(unlink(out), add = TRUE)

  wd <- getwd()
  save_html(obj, out)
  # Verify that save_html doesn't alter working dir
  expect_identical(getwd(), wd)

  chr <- readChar(out, file.size(out))
  expect_false(grepl("\r\r\n", chr))

  expect_false(grepl("\r\r\n", as.character(obj)))
})

test_that("Special characters are not re-encoded", {
  skip_on_cran()
  # https://github.com/rstudio/htmltools/pull/117
  f <- tempfile(fileext = ".html")
  withr::with_options(
    list(encoding = "UTF-8"),
    {
      save_html(div("brûlée"), f)
      expect_true(any(grepl("brûlée", readLines(f))))
    }
  )
})


test_that("save_html() language parameter is set", {
  output <- tempfile(fileext = ".html")
  # test for default
  save_html("<h2>Howdy</h2>", output)
  output_read <- readLines(output)
  expect_true(
    grepl("<html lang=\"en\">", paste(output_read, collapse = " "))
  )
  # test for fr
  save_html("<h2>Howdy</h2>", output, lang = "fr")
  output_read <- readLines(output)
  expect_true(
    grepl("<html lang=\"fr\">", paste(output_read, collapse = " "))
  )
})

test_that("save_html() can write to subdirectories", {
  tmpDir <- tempfile()
  dir.create(tmpDir)
  withr::local_dir(tmpDir)
  dir.create("foo")
  save_html(tags$h2("Howdy"), "foo/bar.html")
  expect_true(
    grepl("<h2>Howdy</h2>", paste(readLines("foo/bar.html"), collapse = " "))
  )
})

test_that("save_html() can write to a file connection", {
  f <- file()
  on.exit(close(f), add = TRUE)
  save_html(tags$h2("Howdy"), f)
  expect_true(
    grepl("<h2>Howdy</h2>", paste(readLines(f), collapse = " "))
  )
})
