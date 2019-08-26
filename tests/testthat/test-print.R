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
