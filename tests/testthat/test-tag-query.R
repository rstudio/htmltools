
fakeJqueryDep <- htmlDependency("jquery", "1.11.3", c(href="shared"), script = "jquery.js")
fakeTagFunction <- tagFunction(function(){ span("inner span") })

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

  expect_error(asTagEnv(xTagEnv), "Duplicate tag environment found")
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
  expect_error(asTagEnv(xTagEnv), "Duplicate tag environment")
})



test_that("tagQuery() root values", {
  expect_error(tagQuery(div()), NA)
  expect_error(tagQuery(list()), "initial set")
  expect_error(tagQuery(tagList()), "initial set")
  expect_error(tagQuery(tagList(div())), NA)
  expect_error(tagQuery(5), "initial set")
  expect_error(tagQuery("a"), "initial set")
  expect_error(tagQuery(fakeJqueryDep), "initial set")
  expect_error(tagQuery(fakeTagFunction), "initial set")
})

test_that("tagQuery() structure", {
  x <- tagQuery(div())

  expect_s3_class(x, "htmltools.tag.query")
  lapply(x, function(xI) { expect_true(is.function(xI)) })
})

test_that("tagQuery()$find()", {
  x <- tagQuery(div(span("a"), span("b")))

  # Make sure the found elements do not persist
  newX <- x$find("span")
  expect_failure(
    expect_equal(
      x$selected(),
      newX$selected()
    )
  )

  x <- x$find("span")
  expect_length(x$selected(), 2)
  expect_equal_tags(
    x$asTags(),
    tagList(span("a"), span("b"))
  )

  ul <- tags$ul
  li <- tags$li
  x <- tagQuery(div(div(div(ul(li("a"), li("b"), li("c"))))))
  expect_length(x$selected(), 1)
  x <- x$find("div")
  expect_length(x$selected(), 2)
  x <- x$find("div")
  expect_length(x$selected(), 1)

  x <- tagQuery(
    div(
      class = "outer",
      div(a(span(p("text1")))),
      div(a(p("text2")))
    )
  )
  x <- x$find("a")
  expect_length(x$selected(), 2)
  x <- x$reset()

  x <- x$find("a > p")
  expect_length(x$selected(), 1)
  expect_equal_tags(x$asTags(), p("text2"))
  x <- x$reset()

  x <- x$find("a > > p")
  expect_length(x$selected(), 1)
  expect_equal_tags(x$asTags(), p("text1"))
  x <- x$reset()

  x <- x$find("div > *")
  expect_length(x$selected(), 2)
  expect_equal_tags(x$asTags(), tagList(a(span(p("text1"))), a(p("text2"))))
  x <- x$reset()

  x <- x$find("div>>p")
  expect_length(x$selected(), 1)
  expect_equal_tags(x$asTags(), p("text2"))
})

test_that("tagQuery()$filter()", {
  x <- tagQuery(div(span(1), span(2), span(3), span(4), span(5)))

  x <- x$find("span")
  expect_length(x$selected(), 5)

  # keep the even found elements
  x <- x$filter(function(item, i) {
    # is even
    (i %% 2) == 0
  })
  expect_length(x$selected(), 2)

  # keep the filtered even elements. Should only have the 4th one remaining
  x <- x$filter(function(item, i) {
    # is even
    (i %% 2) == 0
  })
  expect_length(x$selected(), 1)

  expect_equal_tags(x$asTags(), span(4))
})

test_that("tagQuery()$children() & tagQuery()$parent()", {
  x <- tagQuery(
    div(class="outer",
      div(class="a",
        span(class="A", "1"),
        span(class="B", "2")),
      div(class = "b",
        span(class = "C", "3"),
        span(class = "D", "4")
      )
    )
  )

  x <- x$find("div")
  expect_length(x$selected(), 2)

  x <- x$children()
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

  x <- x$parent()
  expect_length(x$selected(), 2)

  x <- x$children(".C")
  expect_length(x$selected(), 1)

  x <- x$parent()
  expect_length(x$selected(), 1)
  secondDiv <- div(class = "b", span(class = "C", "3"), span(class = "D", "4"))
  expect_equal_tags(x$asTags(), secondDiv)
  x <- x$reset()$find("span")$parents(".b")
  expect_length(x$selected(), 1)
  expect_equal_tags(x$asTags(), secondDiv)
})




test_that("tagQuery()$parents() && tagQuery()$closest()", {
  xTags <-
    div(class = "outer",
      div(class = "inner",
        p(class="p",
          span("a"), span("b"), span("c"), span("d"), span("e")
        )
      )
    )
  x <- tagQuery(xTags)

  expect_length(x$selected(), 1)

  xc <- x$find("span")$closest("div")
  expect_length(xc$selected(), 1)
  expect_true(xc$hasClass("inner"))

  xc <- x$find("span")$closest()
  expect_length(xc$selected(), 5)
  xc$each(function(el, i) {
    expect_equal(el$name, "span")
  })

  xp <- x$find("span")$parents("div")
  expect_length(xp$selected(), 2)
  expect_equal(xp$hasClass("outer"), c(FALSE, TRUE))
  expect_equal(xp$hasClass("inner"), c(TRUE, FALSE))

  x <- x$find("span")$parents()
  expect_length(x$selected(), 3)

  expect_equal_tags(
    x$asTags(),
    tagList(
      xTags$children[[1]]$children[[1]],
      xTags$children[[1]],
      xTags
    )
  )

  x <- x$reset()$find("span")$parents(".outer")
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
  expect_length(x$selected(), 5)
  x <- x$siblings()
  expect_length(x$selected(), 5)

  xTags <- tagList(
    span("a"),
    span("b"),
    span("c", class = "middle"),
    span("d"),
    span("e")
  )
  x <- tagQuery(xTags)
  expect_length(x$selected(), 5)
  x <- x$filter(".middle")
  expect_length(x$selected(), 1)
  x <- x$siblings()
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

  expect_length(x$selected(), 1)
  x <- x$find("div.inner")$addClass("test-class")
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

  x <- x$find("div.A")
  expect_length(x$selected(), 1)
  expect_equal(x$hasClass("B A"), TRUE)
  expect_equal(x$hasClass("A B"), TRUE)
  expect_equal(x$hasClass("B"), TRUE)
  expect_equal(x$hasClass("A"), TRUE)
  expect_equal(x$hasClass("C"), FALSE)
  expect_equal(x$hasClass("A C"), FALSE)

  x <- x$reset()$find("span")
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
  xTags <- div(span("child"))
  x <- tagQuery(xTags)

  newa <- span("a")
  x$append(newa)
  expect_equal_tags(
    x$asTags(selected = FALSE),
    div(span("child"), newa)
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$append(new1, new2)

  expect_equal_tags(
    x$asTags(selected = FALSE),
    div(span("child"), newa, new1, new2)
  )
})

test_that("tagQuery()$prepend()", {
  xTags <- div(span("child"))
  x <- tagQuery(xTags)

  newa <- span("a")
  x$prepend(newa)
  expect_equal_tags(
    x$asTags(selected = FALSE),
    div(newa, span("child"))
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$prepend(new1, new2)

  expect_equal_tags(
    x$asTags(selected = FALSE),
    div(new1, new2, newa, span("child"))
  )
})

test_that("tagQuery()$each()", {
  xTags <- div(span("a"), h1("title"), span("b"))
  x <- tagQuery(xTags)

  x <- x$find("span")

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

  x <- x$find("span")

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





test_that("tagQuery()$remove()", {

  xTags <-
    div(
      span("a"),
      span("b", class = "A"),
      span("c"),
      span("d", class = "A"),
      span("e")
    )
  x <- tagQuery(xTags)$find("span")
  expect_length(x$selected(), 5)
  x <- x$filter(".A")$remove()
  expect_length(x$selected(), 0)

  expect_equal_tags(
    x$asTags(selected = FALSE),
    div(span("a"), span("c"), span("e"))
  )

  x <- x$reset()$find("span")
  expect_length(x$selected(), 3)
  x <- x$remove()
  expect_equal_tags(
    x$asTags(selected = FALSE),
    div()
  )
})


test_that("tagQuery()$after()", {
  xTags <- div()
  x <- tagQuery(xTags)

  newa <- span("a")
  x$after(newa)
  expect_equal_tags(
    x$asTags(selected = FALSE),
    tagList(xTags, newa)
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$after(new1, new2)

  expect_equal_tags(
    x$asTags(selected = FALSE),
    tagList(xTags, new1, new2, newa)
  )
})

test_that("tagQuery()$before()", {
  xTags <- div()
  x <- tagQuery(xTags)

  newa <- span("a")
  x$before(newa)
  expect_equal_tags(
    x$asTags(selected = FALSE),
    tagList(newa, xTags)
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$before(new1, new2)

  expect_equal_tags(
    x$asTags(selected = FALSE),
    tagList(newa, new1, new2, xTags)
  )
})


test_that("tagQuery(x)$asTags(selected = FALSE)", {

  xTags <- tagList(
    fakeJqueryDep,
    div(
      fakeTagFunction
    )
  )

  x <- tagQuery(xTags)

  expect_equal_tags(
    x$asTags(selected = FALSE),
    xTags
  )
})



test_that("tagQuery() objects inherit from each other objects", {
  xTags <- div(span("text"))
  x <- tagQuery(xTags)$find("span")
  y <- tagQuery(x)
  z <- tagQuery(x$get(1))
  w <- tagQuery(x$selected())
  y$addClass("extra")

  expected <- div(span(class="extra", "text"))

  expect_equal_tags(x$asTags(), expected$children[[1]])
  expect_equal_tags(y$asTags(), expected$children[[1]])
  expect_equal_tags(z$asTags(), expected$children[[1]])
  expect_equal_tags(w$asTags(), expected$children[[1]])

  expect_equal_tags(x$asTags(selected = FALSE), expected)
  expect_equal_tags(y$asTags(selected = FALSE), expected)
  expect_equal_tags(z$asTags(selected = FALSE), expected)
  expect_equal_tags(w$asTags(selected = FALSE), expected)
})



test_that("tagQuery() objects can not inherit from mixed objects", {
  xTags <- div(span("text"), span("extra"))
  x <- tagQuery(xTags)$find("span")
  y <- tagQuery(xTags)$find("span")

  expect_error(
    tagQuery(tagList(
      div(),
      x$get(1)
    )),
    "not be a mix"
  )
  expect_error(
    tagQuery(tagList(
      x$get(1),
      y$get(1)
    )),
    "share the same root"
  )
})






















#
