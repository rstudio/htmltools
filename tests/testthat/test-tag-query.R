
fakeJqueryDep <- htmlDependency("jquery", "1.11.3", c(href="shared"), script = "jquery.js")
fakeTagFunction <- tagFunction(function(){ span("inner span") })


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
})


test_that("asTagEnv upgrades objects", {

  expect_error(asTagEnv(list()), "does not accept")
  expect_error(asTagEnv(tagList()), "does not accept")

  x <- div(class = "test_class", span(class = "inner"))
  xTagEnv <- asTagEnv(x)

  expect_s3_class(xTagEnv, "shiny.tag.env")
  expect_s3_class(xTagEnv, "shiny.tag")

  expect_null(xTagEnv$parent)
  expect_equal(xTagEnv$envKey, obj_address(xTagEnv))
  expect_equal(xTagEnv$name, x$name)
  expect_equal(xTagEnv$attribs, x$attribs)

  expect_equal(length(xTagEnv$children), length(x$children))
  lapply(xTagEnv$children, function(child) {
    expect_s3_class(child, "shiny.tag.env")
    expect_equal(child$parent$envKey, xTagEnv$envKey)
  })

})

## Cycles are not tested for anymore. Keeping in case they are brought back
# test_that("asTagEnv finds cycles", {
#   x <- div(class = "test_class", span(class = "inner"))
#   xTagEnv <- asTagEnv(x)
#   expect_error(asTagEnv(xTagEnv), NA)

#   testSpanEnv <- xTagEnv$children[[1]]
#   xTagEnv$children[[2]] <- testSpanEnv
#   xTagEnv$children[[3]] <- testSpanEnv

#   expect_error(asTagEnv(xTagEnv), "Duplicate tag environment found")
#   expect_equal_tags(
#     tagEnvToTags(xTagEnv),
#     div(
#       class = "test_class",
#       span(class = "inner"),
#       span(class = "inner"),
#       span(class = "inner")
#     )
#   )

#   # make a cycle
#   testSpanEnv$children[[1]] <- xTagEnv
#   expect_error(asTagEnv(xTagEnv), "Duplicate tag environment")
# })



test_that("tagQuery() root values", {
  expect_error(tagQuery(div()), NA)
  expect_error(tagQuery(list()), "initial set")
  expect_error(tagQuery(tagList()), "initial set")
  expect_error(tagQuery(tagList(div())), NA)
  expect_error(tagQuery(5), "initial set")
  expect_error(tagQuery("a"), "initial set")
  expect_error(tagQuery(fakeJqueryDep), "initial set")
  expect_error(tagQuery(fakeTagFunction), "initial set")

  x <- tagQuery(div(span(), a()))$find("span")
  # expect_equal_tags(x$selectedTags(), tagListPrintAsList(span()))
  # expect_equal_tags(x$selectedTags(), tagListPrintAsList(div(span(), a())))

  # supply a tag query object
  expect_equal_tags(tagQuery(x)$selectedTags(), x$selectedTags())
  expect_equal_tags(tagQuery(x)$allTags(), x$allTags())

  # supply a list of tag envs
  tagEnvs <- list()
  x$each(function(el, i) { tagEnvs[[length(tagEnvs) + 1]] <<- el})
  expect_equal_tags(tagQuery(tagEnvs)$selectedTags(), x$selectedTags())
  expect_equal_tags(tagQuery(tagEnvs)$allTags(), x$allTags())

  # supply a single tag env
  expect_equal_tags(tagQuery(tagEnvs[[1]])$selectedTags(), x$selectedTags())
  expect_equal_tags(tagQuery(tagEnvs[[1]])$allTags(), x$allTags())
})

test_that("tagQuery() structure", {
  x <- tagQuery(div())

  expect_s3_class(x, "shiny.tag.query")
  lapply(x, function(xI) { expect_true(is.function(xI)) })
})

test_that("tagQuery()$find()", {
  x <- tagQuery(div(span("a"), span("b")))

  # Make sure the found elements do not persist
  newX <- x$find("span")
  expect_failure(
    expect_equal_tags(
      x$selectedTags(),
      newX$selectedTags()
    )
  )

  x <- x$find("span")
  expect_equal(x$length(), 2)
  expect_length(x$selectedTags(), 2)
  expect_equal_tags(
    x$selectedTags(),
    tagListPrintAsList(span("a"), span("b"))
  )

  ul <- tags$ul
  li <- tags$li
  x <- tagQuery(div(div(div(ul(li("a"), li("b"), li("c"))))))
  expect_equal(x$length(), 1)
  expect_length(x$selectedTags(), 1)
  x <- x$find("div")
  expect_equal(x$length(), 2)
  expect_length(x$selectedTags(), 2)
  x <- x$find("div")
  expect_equal(x$length(), 1)
  expect_length(x$selectedTags(), 1)

  x <- tagQuery(
    div(
      class = "outer",
      div(a(span(p("text1")))),
      div(a(p("text2")))
    )
  )
  x <- x$find("a")
  expect_equal(x$length(), 2)
  expect_length(x$selectedTags(), 2)
  x <- x$resetSelected()

  x <- x$find("a > p")
  expect_equal(x$length(), 1)
  expect_length(x$selectedTags(), 1)
  expect_equal_tags(x$selectedTags(), tagListPrintAsList(p("text2")))
  x <- x$resetSelected()

  x <- x$find("a > > p")
  expect_equal(x$length(), 1)
  expect_length(x$selectedTags(), 1)
  expect_equal_tags(x$selectedTags(), tagListPrintAsList(p("text1")))
  x <- x$resetSelected()

  x <- x$find("div > *")
  expect_equal(x$length(), 2)
  expect_length(x$selectedTags(), 2)
  expect_equal_tags(x$selectedTags(), tagListPrintAsList(a(span(p("text1"))), a(p("text2"))))
  x <- x$resetSelected()

  x <- x$find("div>>p")
  expect_equal(x$length(), 1)
  expect_length(x$selectedTags(), 1)
  expect_equal_tags(x$selectedTags(), tagListPrintAsList(p("text2")))
})

test_that("tagQuery()$filter()", {
  x <- tagQuery(div(span(1), span(2), span(3), span(4), span(5)))

  x <- x$find("span")
  expect_length(x$selectedTags(), 5)

  # keep the even found elements
  x <- x$filter(function(item, i) {
    # is even
    (i %% 2) == 0
  })
  expect_length(x$selectedTags(), 2)

  # keep the filtered even elements. Should only have the 4th one remaining
  x <- x$filter(function(item, i) {
    # is even
    (i %% 2) == 0
  })
  expect_length(x$selectedTags(), 1)

  expect_equal_tags(x$selectedTags(), tagListPrintAsList(span(4)))
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
  expect_length(x$selectedTags(), 2)

  x <- x$children()
  expect_length(x$selectedTags(), 4)
  expect_equal_tags(
    x$selectedTags(),
    tagListPrintAsList(
      span(class = "A", "1"),
      span(class = "B", "2"),
      span(class = "C", "3"),
      span(class = "D", "4")
    )
  )

  x <- x$parent()
  expect_length(x$selectedTags(), 2)

  x <- x$children(".C")
  expect_length(x$selectedTags(), 1)

  x <- x$parent()
  expect_length(x$selectedTags(), 1)
  secondDiv <- div(class = "b", span(class = "C", "3"), span(class = "D", "4"))
  expect_equal_tags(x$selectedTags(), tagListPrintAsList(secondDiv))
  x <- x$resetSelected()$find("span")$parents(".b")
  expect_length(x$selectedTags(), 1)
  expect_equal_tags(x$selectedTags(), tagListPrintAsList(secondDiv))
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

  expect_length(x$selectedTags(), 1)

  xc <- x$find("span")$closest("div")
  expect_length(xc$selectedTags(), 1)
  expect_true(xc$hasClass("inner"))

  xc <- x$find("span")$closest()
  expect_length(xc$selectedTags(), 5)
  xc$each(function(el, i) {
    expect_equal(el$name, "span")
  })

  xp <- x$find("span")$parents("div")
  expect_length(xp$selectedTags(), 2)
  expect_equal(xp$hasClass("outer"), c(FALSE, TRUE))
  expect_equal(xp$hasClass("inner"), c(TRUE, FALSE))

  x <- x$find("span")$parents()
  expect_length(x$selectedTags(), 3)

  expect_equal_tags(
    x$selectedTags(),
    tagListPrintAsList(
      xTags$children[[1]]$children[[1]],
      xTags$children[[1]],
      xTags
    )
  )

  x <- x$resetSelected()$find("span")$parents(".outer")
  expect_length(x$selectedTags(), 1)

  expect_equal_tags(
    x$selectedTags(),
    tagListPrintAsList(xTags)
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
  expect_length(x$selectedTags(), 5)
  x <- x$siblings()
  expect_length(x$selectedTags(), 5)

  xTags <- tagList(
    span("a"),
    span("b"),
    span("c", class = "middle"),
    span("d"),
    span("e")
  )
  x <- tagQuery(xTags)
  expect_length(x$selectedTags(), 5)
  x <- x$filter(".middle")
  expect_length(x$selectedTags(), 1)
  x <- x$siblings()
  expect_length(x$selectedTags(), 4)
})

test_that("tagQuery()$addClass()", {
  xTags <-
    div(class = "outer",
      div(class = "inner",
        span("a"), span("b"), span("c"), span("d"), span("e")
      )
    )
  x <- tagQuery(xTags)

  expect_length(x$selectedTags(), 1)
  x <- x$find("div.inner")$addClass("test-class")
  expect_length(x$selectedTags(), 1)

  expect_equal(x$selectedTags()[[1]]$attribs$class, "inner test-class")

  expect_silent({
    x$addClass(NULL)
    x$removeClass(NULL)
    x$toggleClass(NULL)
    expect_equal(x$hasClass(NULL), c(FALSE))
  })

  expect_silent({
    x$addClass(character(0))
    x$removeClass(character(0))
    x$toggleClass(character(0))
    expect_equal(x$hasClass(character(0)), c(FALSE))
  })

  expect_equal_tags(
    tagQuery(
      div(class="A", class="B", "text")
    )$
      addClass("C")$
      removeClass("B")$
      allTags(),
    div(class = "A C", "text")
  )

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
  expect_length(x$selectedTags(), 1)
  expect_equal(x$hasClass("B A"), TRUE)
  expect_equal(x$hasClass("A B"), TRUE)
  expect_equal(x$hasClass("B"), TRUE)
  expect_equal(x$hasClass("A"), TRUE)
  expect_equal(x$hasClass("C"), FALSE)

  x <- x$resetSelected()$find("span")
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


test_that("tagQuery()$addAttrs(), $removeAttrs(), $s", {
  xTags <- tagList(
      span(key = "value - a", "a"),
      span(key = "value - b", "b"),
      span(                   "c"),
      span(                   "d"),
      span(key = "value - e", "e")
    )
  x <- tagQuery(xTags)

  expect_length(x$selectedTags(), 5)
  expect_equal(x$hasAttrs("key"), c(TRUE, TRUE, FALSE, FALSE, TRUE))

  x$addAttrs(key2 = "val2", key3 = "val3")
  expect_equal(x$hasAttrs("key"), c(TRUE, TRUE, FALSE, FALSE, TRUE))
  expect_equal(x$hasAttrs("key2"), c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(x$hasAttrs("key3"), c(TRUE, TRUE, TRUE, TRUE, TRUE))

  x$removeAttrs(c("key", "key3"))
  expect_equal(x$hasAttrs("key"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(x$hasAttrs("key2"), c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(x$hasAttrs("key3"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("tagQuery()$append()", {
  xTags <- div(span("child"))
  x <- tagQuery(xTags)

  newa <- span("a")
  x$append(newa)
  expect_equal_tags(
    x$allTags(),
    div(span("child"), newa)
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$append(new1, new2)

  expect_equal_tags(
    x$allTags(),
    div(span("child"), newa, new1, new2)
  )
})

test_that("tagQuery()$prepend()", {
  xTags <- div(span("child"))
  x <- tagQuery(xTags)

  newa <- span("a")
  x$prepend(newa)
  expect_equal_tags(
    x$allTags(),
    div(newa, span("child"))
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$prepend(new1, new2)

  expect_equal_tags(
    x$allTags(),
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
    x$allTags(),
    div(span("A"), h1("title"), span("B"))
  )
})



test_that("tagQuery()$allTags() & tagQuery()$rebuild()", {

  xTags <- div(span("a"), h1("title"), span("b"))
  x <- tagQuery(xTags)

  x$each(function(root, i) {
    # add a child to the root
    root$children[[length(root$children) + 1]] <- div("test")
  })

  # retrieve the root (and direct children) from graph
  rootChildren <- x$allTags()$children
  lastChild <- rootChildren[[length(rootChildren)]]

  # make sure the last child is a tag env (not a standard tag)
  expect_false(isTagEnv(lastChild))
  # make sure it equals what was manually added
  expect_equal_tags(lastChild, div("test"))
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
  expect_equal(x$length(), 5)
  expect_length(x$selectedTags(), 5)
  x <- x$filter(".A")$remove()
  expect_equal(x$length(), 0)
  expect_length(x$selectedTags(), 0)

  expect_equal_tags(
    x$allTags(),
    div(span("a"), span("c"), span("e"))
  )

  x <- x$resetSelected()$find("span")
  expect_equal(x$length(), 3)
  expect_length(x$selectedTags(), 3)
  x <- x$remove()
  expect_equal_tags(
    x$allTags(),
    div()
  )
})


test_that("tagQuery()$after()", {
  xTags <- div()
  x <- tagQuery(xTags)

  newa <- span("a")
  x$after(newa)
  expect_equal_tags(
    x$allTags(),
    tagList(xTags, newa)
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$after(new1, new2)

  expect_equal_tags(
    x$allTags(),
    tagList(xTags, new1, new2, newa)
  )
})

test_that("tagQuery()$before()", {
  xTags <- div()
  x <- tagQuery(xTags)

  newa <- span("a")
  x$before(newa)
  expect_equal_tags(
    x$allTags(),
    tagList(newa, xTags)
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$before(new1, new2)

  expect_equal_tags(
    x$allTags(),
    tagList(newa, new1, new2, xTags)
  )
})


test_that("tagQuery(x)$allTags()", {

  xTags <- tagList(
    fakeJqueryDep,
    div(
      fakeTagFunction
    )
  )

  x <- tagQuery(xTags)

  expect_equal_tags(
    x$allTags(),
    tagList(!!!xTags)
  )
})



test_that("tagQuery() objects inherit from each other objects", {
  xTags <- div(span("text"))
  x <- tagQuery(xTags)$find("span")
  y <- tagQuery(x)
  zEnv <- NULL
  wEnvs <- NULL
  x$each(function(el, i) {
    zEnv <<- el
    wEnvs <<- append(wEnvs, list(el))
  })
  z <- tagQuery(zEnv)
  w <- tagQuery(wEnvs)

  y$addClass("extra")

  expected <- div(span(class="extra", "text"))
  expect_equal_tags(x$selectedTags(), tagListPrintAsList(!!!expected$children))
  expect_equal_tags(y$selectedTags(), tagListPrintAsList(!!!expected$children))
  expect_equal_tags(z$selectedTags(), tagListPrintAsList(!!!expected$children))
  expect_equal_tags(w$selectedTags(), tagListPrintAsList(!!!expected$children))

  expect_equal_tags(x$allTags(), expected)
  expect_equal_tags(y$allTags(), expected)
  expect_equal_tags(z$allTags(), expected)
  expect_equal_tags(w$allTags(), expected)
})



test_that("tagQuery() objects can not inherit from mixed objects", {
  xTags <- div(span("text"), span("extra"))
  x <- tagQuery(xTags)$find("span")
  y <- tagQuery(xTags)$find("span")

  xEnv <- NULL
  x$each(function(el, i) {
    xEnv <<- el
  })

  yEnv <- NULL
  y$each(function(el, i) {
    yEnv <<- el
  })

  expect_error(
    tagQuery(tagList(
      div(),
      xEnv
    )),
    "not be a mix"
  )
  expect_error(
    tagQuery(tagList(
      xEnv,
      yEnv
    )),
    "share the same root"
  )
})



test_that("rebuilding tag envs after inserting children is done", {
  xTags <- div(div(), div())

  expect_equal_tags(
    tagQuery(xTags)$find("div")$before(span())$allTags(),
    div(span(), div(), span(), div())
  )

  expect_equal_tags(
    tagQuery(xTags)$find("div")$replaceWith(span())$allTags(),
    div(span(), span())
  )

  expect_equal_tags(
    tagQuery(xTags)$find("div")$after(span())$allTags(),
    div(div(), span(), div(), span())
  )
})


test_that("tagQuery() print method displays custom output for selected tags", {
  local_edition(3)

  expect_snapshot_output(print(
    tagQuery(div(span()))
  ))

  expect_snapshot_output(print(
    tagQuery(div(span()))$find("span")
  ))

  expect_snapshot_output(print(
    tagQuery(div(span()))$find("empty")
  ))

})


test_that("tagQuery() allows for tags with extra top level items and will preserve them", {
  html <- div(span())
  html$test <- "extra"
  html <- c(list(first = TRUE), html)
  class(html) <- "shiny.tag"

  # Test different removal types: setting the value to NULL and removing the value from the envir completely.
  for (removeType in c("set", "rm")) {
    expect_error(
      tagQuery(html)$each(function(el, i) {
        switch(removeType,
          set = {
            el$name <- NULL
          },
          rm = {
            rm(list = "name", envir = el)
          }
        )
      })$allTags(),
      "lost its `$name`", fixed = TRUE
    )

    for (missing_key in c("__not_a_match__", "attribs", "children")) {
      htmlQ <- tagQuery(html)
      if (missing_key %in% names(html)) {
        htmlQ$each(function(el, i) {
          switch(removeType,
            set = {
              el[[missing_key]] <- NULL
            },
            rm = {
              rm(list = missing_key, envir = el)
            }
          )
          el[[missing_key]] <- NULL
        })
      }
      htmlPostQ <- htmlQ$allTags()
      html_out <- html
      if (missing_key == "attribs") html_out$attribs <- dots_list()
      if (missing_key == "children") html_out$children <- list()
      # expect first three names to be standard tag names
      expect_equal(names(htmlPostQ)[1:3], names(div()))

      # expect all other names to be included somewhere
      expect_setequal(names(htmlPostQ), names(html_out))

      # If done in the same order, it should be equal
      back_to_orig <- htmlPostQ[names(html_out)]
      class(back_to_orig) <- "shiny.tag"
      expect_equal(back_to_orig, html_out)
    }
  }

})


test_that("flattenTagsRaw() and flattenTags() do not drop html deps", {
  testSpan <- span()
  htmlDependencies(testSpan) <- list(fakeJqueryDep)
  otherObj <- HTML("test")
  html <- tagList(div(), testSpan, otherObj)
  htmlDependencies(html) <- list(fakeJqueryDep)

  expect_equal(flattenTags(html), html)
  expect_equal(flattenTagsRaw(html), html)
})


test_that("tag methods do not unexpectedly alter tag envs", {

  expect_equal_tags(
    tagEnvToTags(tagAppendAttributes(asTagEnv(div()), key = "a")),
    tagAppendAttributes(div(), key = "a")
  )

  expect_equal_tags(
    tagHasAttribute(asTagEnv(div(key = "a")), "key"),
    tagHasAttribute(div(key = "a"), "key")
  )

  expect_equal_tags(
    tagGetAttribute(asTagEnv(div(key = "a")), "key"),
    tagGetAttribute(div(key = "a"), "key")
  )

  expect_equal_tags(
    tagEnvToTags(tagAppendChild(asTagEnv(div()), span())),
    tagAppendChild(div(), span())
  )

  expect_equal_tags(
    tagEnvToTags(tagAppendChildren(asTagEnv(div()), span(), h1())),
    tagAppendChildren(div(), span(), h1())
  )

  expect_equal_tags(
    tagEnvToTags(tagSetChildren(asTagEnv(div()), span(), h1())),
    tagSetChildren(div(), span(), h1())
  )

  expect_equal_tags(
    tagEnvToTags(tagInsertChildren(asTagEnv(div()), span(), h1(), after = 12)),
    tagInsertChildren(div(), span(), h1(), after = 12)
  )
})









#
