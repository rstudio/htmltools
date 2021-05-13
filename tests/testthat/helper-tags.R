
# Needed to compare tags that go from lists to envs and back to lists.
expect_equal_tags <- function(x, y) {
  expect_equal_tags_ <- function(x, y) {
    if (isTag(x)) {
      expect_true(isTag(y))
      expect_equal(x$parent, NULL)
      expect_equal(y$parent, NULL)
      expect_equal(x$envKey, NULL)
      expect_equal(y$envKey, NULL)
      # Recurse through children
      expect_equal_tags_(x$children, y$children)
    } else if (is.list(x)) {
      expect_true(is.list(y))
      expect_equal(length(x), length(y))
      Map(x, y, f = expect_equal_tags_)
    } else {
      # no tags to recurse
    }
  }

  # Should be fully equal.
  expect_equal(x, y)
  # Do custom checks to make sure tagQuery undid any internal changes
  expect_equal_tags_(x, y)
}
