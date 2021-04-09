
sortInternalNames <- function(x) {
  if (is.list(x) && is_named(x)) {
    x[order(names(x))]
  } else {
    x
  }
}

# Needed to compare tags that go from lists to envs and back to lists.
# The names are alpha sorted in the final tag object
expect_equal_tags <- function(x, y) {
  if (isTag(x)) {
    expect_true(isTag(y))
    expect_equal(x$parent, NULL)
    expect_equal(y$parent, NULL)
    expect_equal(x$envKey, NULL)
    expect_equal(y$envKey, NULL)
    x <- sortInternalNames(x)
    y <- sortInternalNames(y)
    # compare everything but the children
    expect_equal(
      x[setdiff(names(x), "children")],
      y[setdiff(names(y), "children")]
    )
    expect_equal_tags(x$children, y$children)
  } else if (is.list(x)) {
    if (isTagList(x)) {
      expect_true(isTagList(y))
    } else {
      expect_true(is.list(y))
    }
    expect_equal(length(x), length(y))
    expect_equal(names2(x), names2(y))
    Map(x, y, f = expect_equal_tags)
  } else {
    expect_equal(x, y)
  }
}

test_that("safeListToEnv and safeEnvToList undo each other", {

  x <- structure(
    list(
      A = 1,
      B = 2
    ),
    class = "test_class",
    extra_dep = list(42),
    other_dep = "exists"
  )

  xExpected <- x

  xEnv <- safeListToEnv(x, "extra_class")

  expect_type(xEnv, "environment")
  expect_s3_class(xEnv, "test_class")
  expect_s3_class(xEnv, "extra_class")

  expect_equal(names(xEnv), c("A", "B"))
  expect_equal(safeAttrValues(xEnv), list(extra_dep = list(42), other_dep = "exists"))

  expect_equal(safeEnvToList(xEnv, "extra_class"), xExpected)
})


test_that("asTagEnv upgrades objects", {

  expect_error(asTagEnv(list()), "does not accept")
  expect_error(asTagEnv(tagList()), "does not accept")

  x <- div(class = "test_class", span(class = "inner"))
  xTagEnv <- asTagEnv(x)

  expect_s3_class(xTagEnv, "htmltools.tag.env")
  expect_s3_class(xTagEnv, "shiny.tag")

  expect_null(xTagEnv$parent)
  expect_equal(xTagEnv$envKey, sexp_address(xTagEnv))
  expect_equal(xTagEnv$name, x$name)
  expect_equal(xTagEnv$attribs, x$attribs)

  expect_equal(length(xTagEnv$children), length(x$children))
  lapply(xTagEnv$children, function(child) {
    expect_s3_class(child, "htmltools.tag.env")
    expect_equal(child$parent$envKey, xTagEnv$envKey)
  })

})

test_that("asTagEnv finds cycles", {
  x <- div(class = "test_class", span(class = "inner"))
  xTagEnv <- asTagEnv(x)
  expect_error(asTagEnv(xTagEnv), NA)

  testSpanEnv <- xTagEnv$children[[1]]
  xTagEnv$children[[2]] <- testSpanEnv
  xTagEnv$children[[3]] <- testSpanEnv

  expect_error(asTagEnv(xTagEnv), NA)
  expect_equal_tags(
    tagEnvToTags(xTagEnv),
    div(
      class = "test_class",
      span(class = "inner"),
      span(class = "inner"),
      span(class = "inner")
    )
  )

  # make a cycle
  testSpanEnv$children[[1]] <- xTagEnv
  expect_error(asTagEnv(xTagEnv), "Circular")
})



test_that("tagQuery() root values", {
  expect_error(tagQuery(div()), NA)
  expect_error(tagQuery(list()), NA)
  expect_error(tagQuery(tagList()), NA)
  expect_error(tagQuery(5), NA)
  expect_error(tagQuery("a"), NA)
})

test_that("tagQuery() structure", {
  x <- tagQuery(div(span()))

  expect_s3_class(x, "htmltools.tag.query")
  lapply(x, function(xI) { expect_true(is.function(xI)) })
})

test_that("tagQuery()$find()", {
  x <- tagQuery(div(span("a"), span("b")))

  x$find("span")
  expect_length(x$selected(), 2)
  expect_equal_tags(
    x$asTags(),
    tagList(span("a"), span("b"))
  )

  ul <- tags$ul
  li <- tags$li
  x <- tagQuery(div(div(div(ul(li("a"), li("b"), li("c"))))))
  x$find("div")
  expect_length(x$selected(), 3)
  x$find("div")
  expect_length(x$selected(), 2)

  x <- tagQuery(
    tagList(
      div(a(span(p("text1")))),
      div(a(p("text2")))
    )
  )
  x$find("a")
  expect_length(x$selected(), 2)
  x$reset()

  x$find("a > p")
  expect_length(x$selected(), 1)
  expect_equal_tags(x$asTags(), p("text2"))
  x$reset()

  x$find("a > > p")
  expect_length(x$selected(), 1)
  expect_equal_tags(x$asTags(), p("text1"))
  x$reset()

  x$find("div > *")
  expect_length(x$selected(), 2)
  expect_equal_tags(x$asTags(), tagList(a(span(p("text1"))), a(p("text2"))))
  x$reset()

  x$find("div>>p")
  expect_length(x$selected(), 1)
  expect_equal_tags(x$asTags(), p("text2"))
})

test_that("tagQuery()$filter()", {
  x <- tagQuery(div(span(1), span(2), span(3), span(4), span(5)))

  x$find("span")
  expect_length(x$selected(), 5)

  # keep the even found elements
  x$filter(function(item, i) {
    # is even
    (i %% 2) == 0
  })
  expect_length(x$selected(), 2)

  # keep the filtered even elements. Should only have the 4th one remaining
  x$filter(function(item, i) {
    # is even
    (i %% 2) == 0
  })
  expect_length(x$selected(), 1)

  # Compare to `"4"` as `flattenTags` calls `as.tags.default()` which calls `as.character()`
  expect_equal_tags(x$asTags(), span("4"))

})

test_that("tagQuery()$children() & tagQuery()$parent()", {
  x <- tagQuery(
    tagList(
      div(class="a",
        span(class="A", "1"),
        span(class="B", "2")),
      div(class = "b",
        span(class = "C", "3"),
        span(class = "D", "4")
      )
    )
  )

  x$find("div")
  expect_length(x$selected(), 2)

  x$children()
  expect_length(x$selected(), 4)
  expect_equal_tags(
    x$asTags(),
    tagList(
      span(class = "A", "1"),
      span(class = "B", "2"),
      span(class = "C", "3"),
      span(class = "D", "4")
    )
  )

  x$parents()
  expect_length(x$selected(), 2)

  x$children(".C")
  expect_length(x$selected(), 1)

  x$parent()
  expect_length(x$selected(), 1)
  secondDiv <- div(class = "b", span(class = "C", "3"), span(class = "D", "4"))
  expect_equal_tags(x$asTags(), secondDiv)

  x$reset()
  x$find("span")
  x$parents(".b")
  expect_length(x$selected(), 1)
  expect_equal_tags(x$asTags(), secondDiv)
})




test_that("tagQuery()$parents()", {
  xTags <-
    div(class = "outer",
      div(class = "inner",
        span("a"), span("b"), span("c"), span("d"), span("e")
      )
    )
  x <- tagQuery(xTags)

  expect_length(x$selected(), 0)
  x$find("span")
  x$parents()
  expect_length(x$selected(), 2)

  expect_equal_tags(
    x$asTags(),
    tagList(
      xTags$children[[1]],
      xTags
    )
  )

  x$reset()
  x$find("span")
  x$parents(".outer")
  expect_length(x$selected(), 1)

  expect_equal_tags(
    x$asTags(),
    xTags
  )
})


test_that("tagQuery()$siblings()", {
  xTags <- tagList(
    span("a"),
    span("b"),
    span("c"),
    span("d"),
    span("e")
  )
  x <- tagQuery(xTags)
  expect_length(x$selected(), 0)
  x$find("span")
  expect_length(x$selected(), 5)
  x$siblings()
  expect_length(x$selected(), 5)

  xTags <- tagList(
    span("a"),
    span("b"),
    span("c", class = "middle"),
    span("d"),
    span("e")
  )
  x <- tagQuery(xTags)
  expect_length(x$selected(), 0)
  x$find(".middle")
  expect_length(x$selected(), 1)
  x$siblings()
  expect_length(x$selected(), 4)
})

test_that("tagQuery()$addClass()", {
  xTags <-
    div(class = "outer",
      div(class = "inner",
        span("a"), span("b"), span("c"), span("d"), span("e")
      )
    )
  x <- tagQuery(xTags)

  expect_length(x$selected(), 0)
  x$find("div.inner")$addClass("test-class")
  expect_length(x$selected(), 1)

  expect_equal(x$asTags()$attribs$class, "inner test-class")

})

test_that("tagQuery()$hasClass(), $toggleClass(), $removeClass()", {
  xTags <-
    div(class = "outer",
      div(class = "A B",
        span(class = "odd", "a"),
        span(class = "even", "b"),
        span(class = "odd", "c"),
        span(class = "even", "d"),
        span(class = "odd", "e")
      )
    )
  x <- tagQuery(xTags)

  x$find("div.A")
  expect_length(x$selected(), 1)
  expect_equal(x$hasClass("B A"), TRUE)
  expect_equal(x$hasClass("A B"), TRUE)
  expect_equal(x$hasClass("B"), TRUE)
  expect_equal(x$hasClass("A"), TRUE)
  expect_equal(x$hasClass("C"), FALSE)
  expect_equal(x$hasClass("A C"), FALSE)

  x$reset()
  x$find("span")
  expect_equal(x$hasClass("even"), c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(x$hasClass("odd"), c(TRUE, FALSE, TRUE, FALSE, TRUE))
  x$toggleClass("even odd")
  expect_equal(x$hasClass("even"), c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(x$hasClass("odd"), c(FALSE, TRUE, FALSE, TRUE, FALSE))

  x$removeClass("even")
  expect_equal(x$hasClass("even"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(x$hasClass("odd"), c(FALSE, TRUE, FALSE, TRUE, FALSE))
  x$removeClass("other odd")
  expect_equal(x$hasClass("odd"), c(FALSE, FALSE, FALSE, FALSE, FALSE))

})


test_that("tagQuery()$addAttrs(), $removeAttrs(), $emptyAttrs(), $hasAttr", {
  xTags <- tagList(
      span(key = "value - a", "a"),
      span(key = "value - b", "b"),
      span(                   "c"),
      span(                   "d"),
      span(key = "value - e", "e")
    )
  x <- tagQuery(xTags)

  x$find("span")
  expect_length(x$selected(), 5)
  expect_equal(x$hasAttr("key"), c(TRUE, TRUE, FALSE, FALSE, TRUE))

  x$addAttrs(key2 = "val2", key3 = "val3")
  expect_equal(x$hasAttr("key"), c(TRUE, TRUE, FALSE, FALSE, TRUE))
  expect_equal(x$hasAttr("key2"), c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(x$hasAttr("key3"), c(TRUE, TRUE, TRUE, TRUE, TRUE))

  x$removeAttrs(c("key", "key3"))
  expect_equal(x$hasAttr("key"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(x$hasAttr("key2"), c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(x$hasAttr("key3"), c(FALSE, FALSE, FALSE, FALSE, FALSE))

  x$emptyAttrs()
  expect_equal(x$hasAttr("key"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(x$hasAttr("key2"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(x$hasAttr("key3"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("tagQuery()$append()", {
  xTags <- div()
  x <- tagQuery(xTags)

  newa <- span("a")
  x$append(newa)
  expect_equal_tags(
    x$asTags(selected = FALSE),
    tagList(
      xTags,
      newa
    )
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$append(new1, new2)

  expect_equal_tags(
    x$asTags(selected = FALSE),
    tagList(
      xTags,
      newa,
      new1,
      new2
    )
  )
})

test_that("tagQuery()$prepend()", {
  xTags <- div()
  x <- tagQuery(xTags)

  newa <- span("a")
  x$prepend(newa)
  expect_equal_tags(
    x$asTags(selected = FALSE),
    tagList(
      newa,
      xTags
    )
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$prepend(new1, new2)

  expect_equal_tags(
    x$asTags(selected = FALSE),
    tagList(
      new1, new2,
      newa,
      xTags
    )
  )
})

test_that("tagQuery()$each()", {
  xTags <- div(span("a"), h1("title"), span("b"))
  x <- tagQuery(xTags)

  x$find("span")

  expect_error(x$each("4"), "function")
  expect_error(x$each(function(item) {}), "two")
  expect_error(x$each(function(...) {}), NA)

  x$each(function(el, i) {
    el$children <- lapply(el$children, toupper)
    "ignored"
  })

  expect_equal_tags(
    x$asTags(selected = FALSE),
    div(span("A"), h1("title"), span("B"))
  )
})



test_that("tagQuery()$root() & tagQuery()$rebuild()", {

  xTags <- div(span("a"), h1("title"), span("b"))
  x <- tagQuery(xTags)

  # pull out root el
  root <- x$root()
  # add a child to the root
  root$children[[length(root$children) + 1]] <- div("test")
  # rebuild the root within the graph (which is the root var in the line above)
  x$rebuild()

  # retrieve the root (and direct children) from graph
  rootChildren <- x$root()$children
  lastChild <- rootChildren[[length(rootChildren)]]

  # make sure the last child is a tag env (not a standard tag)
  expect_true(isTagEnv(lastChild))
  # make sure it equals what was manually added
  expect_equal_tags(tagEnvToTags(lastChild), div("test"))
})



test_that("tagQuery()$root() & tagQuery()$rebuild()", {

  xTags <- div(span("a"), span("b"), span("c"), span("d"), span("e"))
  x <- tagQuery(xTags)

  x$find("span")

  expect_error(x$get("a"))
  expect_error(x$get(numeric(0)))
  expect_error(x$get(-1))
  expect_error(x$get(8))
  expect_error(x$get(2), NA)

  expect_equal_tags(
    tagEnvToTags(x$get(4)),
    span("d")
  )
})




























#
