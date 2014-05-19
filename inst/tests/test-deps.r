context("dependencies")

format.html_dependency <- function(x, ...) {
  sprintf("%s v%s @ %s", x$name, x$version, format(x$src))
}
print.html_dependency <- function(x, ...) {
  cat(format(x), "\n")
}

test_that("Dependency resolution works", {

  a1.1 <- htmlDependency("a", "1.1", c(href="/"))
  a1.2 <- htmlDependency("a", "1.2", c(href="/"))
  a1.2.1 <- htmlDependency("a", "1.2.1", c(href="/"))
  b1.0.0 <- htmlDependency("b", "1.0.0", c(href="/"))
  b1.0.1 <- htmlDependency("b", "1.0.1", c(href="/"))
  c1.0 <- htmlDependency("c", "1.0", c(href="/"))

  result1 <- resolveDependencies(
    list(a1.1, b1.0.0, b1.0.1, a1.2, a1.2.1, b1.0.0, b1.0.1, c1.0)
  )
  expect_identical(result1, list(a1.2.1, b1.0.1, c1.0))

  result2 <- subtractDependencies(result1, list(a1.1), warnOnConflict = FALSE)
  expect_identical(result2, list(b1.0.1, c1.0))

  expect_warning(subtractDependencies(result1, list(a1.1)))
})
