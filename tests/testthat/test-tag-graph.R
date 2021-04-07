
sort_internal_names <- function(x) {
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
    expect_equal(x$env_key, NULL)
    expect_equal(y$env_key, NULL)
    x <- sort_internal_names(x)
    y <- sort_internal_names(y)
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

test_that("safe_list_2_env and safe_env_2_list undo each other", {

  x <- structure(
    list(
      A = 1,
      B = 2
    ),
    class = "test_class",
    extra_dep = list(42),
    other_dep = "exists"
  )

  x_expected <- x

  x_env <- safe_list_2_env(x, "extra_class")

  expect_type(x_env, "environment")
  expect_s3_class(x_env, "test_class")
  expect_s3_class(x_env, "extra_class")

  expect_equal(names(x_env), c("A", "B"))
  expect_equal(safe_attr_values(x_env), list(extra_dep = list(42), other_dep = "exists"))

  expect_equal(safe_env_2_list(x_env, "extra_class"), x_expected)
})


test_that("as_tag_env upgrades objects", {

  expect_error(as_tag_env(list()), "does not accept")
  expect_error(as_tag_env(tagList()), "does not accept")

  x <- div(class = "test_class", span(class = "inner"))
  x_tag_env <- as_tag_env(x)

  expect_s3_class(x_tag_env, "htmltools.tag.env")
  expect_s3_class(x_tag_env, "shiny.tag")

  expect_null(x_tag_env$parent)
  expect_equal(x_tag_env$env_key, format.default(x_tag_env))
  expect_equal(x_tag_env$name, x$name)
  expect_equal(x_tag_env$attribs, x$attribs)

  expect_equal(length(x_tag_env$children), length(x$children))
  lapply(x_tag_env$children, function(child) {
    expect_s3_class(child, "htmltools.tag.env")
    expect_equal(child$parent$env_key, x_tag_env$env_key)
  })

})

test_that("as_tag_env finds cycles", {
  x <- div(class = "test_class", span(class = "inner"))
  x_tag_env <- as_tag_env(x)
  expect_error(as_tag_env(x_tag_env), NA)

  test_span_env <- x_tag_env$children[[1]]
  x_tag_env$children[[2]] <- test_span_env
  x_tag_env$children[[3]] <- test_span_env

  expect_error(as_tag_env(x_tag_env), NA)
  expect_equal_tags(
    tag_env_to_tags(x_tag_env),
    div(
      class = "test_class",
      span(class = "inner"),
      span(class = "inner"),
      span(class = "inner")
    )
  )

  # make a cycle
  test_span_env$children[[1]] <- x_tag_env
  expect_error(as_tag_env(x_tag_env), "Circular")
})



test_that("tag_graph() root values", {
  expect_error(tag_graph(div()), NA)
  expect_error(tag_graph(list()), NA)
  expect_error(tag_graph(tagList()), NA)
  expect_error(tag_graph(5), NA)
  expect_error(tag_graph("a"), NA)
})

test_that("tag_graph() structure", {
  x <- tag_graph(div(span()))

  expect_s3_class(x, "htmltools.tag.graph")
  lapply(x, function(x_i) { expect_true(is.function(x_i)) })
})

test_that("tag_graph()$find()", {
  x <- tag_graph(div(span("a"), span("b")))

  x$find("span")
  expect_length(x$selected(), 2)
  expect_equal_tags(
    x$as_tags(),
    tagList(span("a"), span("b"))
  )

  ul <- tags$ul
  li <- tags$li
  x <- tag_graph(div(div(div(ul(li("a"), li("b"), li("c"))))))
  x$find("div")
  expect_length(x$selected(), 3)
  x$find("div")
  expect_length(x$selected(), 2)

  x <- tag_graph(
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
  expect_equal_tags(x$as_tags(), tagList(p("text2")))
  x$reset()

  x$find("a > > p")
  expect_length(x$selected(), 1)
  expect_equal_tags(x$as_tags(), tagList(p("text1")))
  x$reset()

  x$find("div > *")
  expect_length(x$selected(), 2)
  expect_equal_tags(x$as_tags(), tagList(a(span(p("text1"))), a(p("text2"))))
  x$reset()

  x$find("div>>p")
  expect_length(x$selected(), 1)
  expect_equal_tags(x$as_tags(), tagList(p("text2")))
})

test_that("tag_graph()$filter()", {
  x <- tag_graph(div(span(1), span(2), span(3), span(4), span(5)))

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
  expect_equal_tags(x$as_tags(), tagList(span("4")))

})

test_that("tag_graph()$children() & tag_graph()$parent()", {
  x <- tag_graph(div(span(1), span(2), span(3), span(4), span(5)))

  x$find("div")
  expect_length(x$selected(), 1)

  x$children()

  expect_length(x$selected(), 5)

  Map(
    x$as_tags(),
    seq_len(5),
    f = function(s, i) {
      expect_equal(s$name, "span")
      expect_equal(s$children, list(as.character(i)))
    }
  )

  x$parent()
  expect_length(x$selected(), 1)
  expect_equal_tags(x$as_tags(), tagList(x$as_tags(selected = FALSE)))
})




test_that("tag_graph()$parents()", {
  x_tags <-
    div(class = "outer",
      div(class = "inner",
        span("a"), span("b"), span("c"), span("d"), span("e")
      )
    )
  x <- tag_graph(x_tags)

  expect_length(x$selected(), 0)
  x$find("span")
  x$parents()
  expect_length(x$selected(), 2)

  expect_equal_tags(
    x$as_tags(),
    tagList(
      x_tags$children[[1]],
      x_tags
    )
  )
})


test_that("tag_graph()$siblings()", {
  x_tags <- tagList(
    span("a"),
    span("b"),
    span("c"),
    span("d"),
    span("e")
  )
  x <- tag_graph(x_tags)
  expect_length(x$selected(), 0)
  x$find("span")
  expect_length(x$selected(), 5)
  x$siblings()
  expect_length(x$selected(), 5)

  x_tags <- tagList(
    span("a"),
    span("b"),
    span("c", class = "middle"),
    span("d"),
    span("e")
  )
  x <- tag_graph(x_tags)
  expect_length(x$selected(), 0)
  x$find(".middle")
  expect_length(x$selected(), 1)
  x$siblings()
  expect_length(x$selected(), 4)
})

test_that("tag_graph()$add_class()", {
  x_tags <-
    div(class = "outer",
      div(class = "inner",
        span("a"), span("b"), span("c"), span("d"), span("e")
      )
    )
  x <- tag_graph(x_tags)

  expect_length(x$selected(), 0)
  x$find("div.inner")$add_class("test-class")
  expect_length(x$selected(), 1)

  expect_equal(x$as_tags()[[1]]$attribs$class, "inner test-class")

})

test_that("tag_graph()$has_class(), $toggle_class(), $remove_class()", {
  x_tags <-
    div(class = "outer",
      div(class = "A B",
        span(class = "odd", "a"),
        span(class = "even", "b"),
        span(class = "odd", "c"),
        span(class = "even", "d"),
        span(class = "odd", "e")
      )
    )
  x <- tag_graph(x_tags)

  x$find("div.A")
  expect_length(x$selected(), 1)
  expect_equal(x$has_class("B A"), TRUE)
  expect_equal(x$has_class("A B"), TRUE)
  expect_equal(x$has_class("B"), TRUE)
  expect_equal(x$has_class("A"), TRUE)
  expect_equal(x$has_class("C"), FALSE)
  expect_equal(x$has_class("A C"), FALSE)

  x$reset()
  x$find("span")
  expect_equal(x$has_class("even"), c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(x$has_class("odd"), c(TRUE, FALSE, TRUE, FALSE, TRUE))
  x$toggle_class("even odd")
  expect_equal(x$has_class("even"), c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(x$has_class("odd"), c(FALSE, TRUE, FALSE, TRUE, FALSE))

  x$remove_class("even")
  expect_equal(x$has_class("even"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(x$has_class("odd"), c(FALSE, TRUE, FALSE, TRUE, FALSE))
  x$remove_class("other odd")
  expect_equal(x$has_class("odd"), c(FALSE, FALSE, FALSE, FALSE, FALSE))

})


test_that("tag_graph()$add_attrs(), $remove_attrs(), $empty_attrs(), $has_attr", {
  x_tags <- tagList(
      span(key = "value - a", "a"),
      span(key = "value - b", "b"),
      span(                   "c"),
      span(                   "d"),
      span(key = "value - e", "e")
    )
  x <- tag_graph(x_tags)

  x$find("span")
  expect_length(x$selected(), 5)
  expect_equal(x$has_attr("key"), c(TRUE, TRUE, FALSE, FALSE, TRUE))

  x$add_attrs(key2 = "val2", key3 = "val3")
  expect_equal(x$has_attr("key"), c(TRUE, TRUE, FALSE, FALSE, TRUE))
  expect_equal(x$has_attr("key2"), c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(x$has_attr("key3"), c(TRUE, TRUE, TRUE, TRUE, TRUE))

  x$remove_attrs(c("key", "key3"))
  expect_equal(x$has_attr("key"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(x$has_attr("key2"), c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(x$has_attr("key3"), c(FALSE, FALSE, FALSE, FALSE, FALSE))

  x$empty_attrs()
  expect_equal(x$has_attr("key"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(x$has_attr("key2"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(x$has_attr("key3"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("tag_graph()$append()", {
  x_tags <- div()
  x <- tag_graph(x_tags)

  newa <- span("a")
  x$append(newa)
  expect_equal_tags(
    x$as_tags(selected = FALSE),
    tagList(
      x_tags,
      newa
    )
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$append(new1, new2)

  expect_equal_tags(
    x$as_tags(selected = FALSE),
    tagList(
      x_tags,
      newa,
      new1,
      new2
    )
  )
})

test_that("tag_graph()$prepend()", {
  x_tags <- div()
  x <- tag_graph(x_tags)

  newa <- span("a")
  x$prepend(newa)
  expect_equal_tags(
    x$as_tags(selected = FALSE),
    tagList(
      newa,
      x_tags
    )
  )

  new1 <- div("new1")
  new2 <- div("new2")
  x$prepend(new1, new2)

  expect_equal_tags(
    x$as_tags(selected = FALSE),
    tagList(
      new1, new2,
      newa,
      x_tags
    )
  )
})

test_that("tag_graph()$each()", {
  x_tags <- div(span("a"), h1("title"), span("b"))
  x <- tag_graph(x_tags)

  x$find("span")

  expect_error(x$each("4"), "function")
  expect_error(x$each(function(item) {}), "two")
  expect_error(x$each(function(...) {}), NA)

  x$each(function(el, i) {
    el$children <- lapply(el$children, toupper)
    "ignored"
  })

  expect_equal_tags(
    x$as_tags(selected = FALSE),
    div(span("A"), h1("title"), span("B"))
  )
})



test_that("tag_graph()$graph() & tag_graph()$rebuild()", {

  x_tags <- div(span("a"), h1("title"), span("b"))
  x <- tag_graph(x_tags)

  # pull out root el
  root <- x$graph()
  # add a child to the root
  root$children[[length(root$children) + 1]] <- div("test")
  # rebuild the root within the graph (which is the root var in the line above)
  x$rebuild()

  # retrieve the root (and direct children) from graph
  root_children <- x$graph()$children
  last_child <- root_children[[length(root_children)]]

  # make sure the last child is a tag env (not a standard tag)
  expect_true(isTagEnv(last_child))
  # make sure it equals what was manually added
  expect_equal_tags(tag_env_to_tags(last_child), div("test"))
})



test_that("tag_graph()$graph() & tag_graph()$rebuild()", {

  x_tags <- div(span("a"), span("b"), span("c"), span("d"), span("e"))
  x <- tag_graph(x_tags)

  x$find("span")

  expect_error(x$get("a"))
  expect_error(x$get(numeric(0)))
  expect_error(x$get(-1))
  expect_error(x$get(8))
  expect_error(x$get(2), NA)

  expect_equal_tags(
    tag_env_to_tags(x$get(4)),
    span("d")
  )
})




























#
