context("tags")

test_that("Basic tag writing works", {
  expect_equal(as.character(tagList("hi")), "hi")
  expect_equal(
    as.character(tagList("one", "two", tagList("three"))),
    "one\ntwo\nthree")
  expect_equal(
    as.character(tags$b("one")),
    "<b>one</b>")
  expect_equal(
    as.character(tags$b("one", "two")),
    "<b>\n  one\n  two\n</b>")
  expect_equal(
    as.character(tagList(list("one"))),
    "one")
  expect_equal(
    as.character(tagList(list(tagList("one")))),
    "one")
  expect_equal(
    as.character(tagList(tags$br(), "one")),
    "<br/>\none")
})

test_that("Hanging commas don't break things", {
  expect_equal(as.character(tagList("hi",)), "hi")
  expect_equal(as.character(div("one",)), "<div>one</div>")
  # Multiple commas still throw
  expect_error(as.character(div("one",,)), "is empty")
  # Non-trailing commas still throw
  expect_error(as.character(div(,"one",)), "is empty")
})


test_that("withTags works", {
  output_tags <- tags$div(class = "myclass",
    tags$h3("header"),
    tags$p("text here")
  )
  output_withhtml <- withTags(
    div(class = "myclass",
      h3("header"),
      p("text here")
    )
  )
  expect_identical(output_tags, output_withhtml)


  # Check that current environment is searched
  x <- 100
  expect_identical(tags$p(x), withTags(p(x)))

  # Just to make sure, run it in a function, which has its own environment
  foo <- function() {
    y <- 100
    withTags(p(y))
  }
  expect_identical(tags$p(100), foo())
})


test_that("HTML escaping in tags", {
  # Regular text is escaped
  expect_equivalent(format(div("<a&b>")), "<div>&lt;a&amp;b&gt;</div>")

  # Text in HTML() isn't escaped
  expect_equivalent(format(div(HTML("<a&b>"))), "<div><a&b></div>")

  # Text in a property is escaped
  expect_equivalent(format(div(class = "<a&b>", "text")),
    '<div class="&lt;a&amp;b&gt;">text</div>')

  # HTML() has no effect in a property like 'class'
  expect_equivalent(format(div(class = HTML("<a&b>"), "text")),
    '<div class="&lt;a&amp;b&gt;">text</div>')
})


test_that("Adding child tags", {
  tag_list <- list(tags$p("tag1"), tags$b("tag2"), tags$i("tag3"))

  # Creating nested tags by calling the tag$div function and passing a list
  t1 <- tags$div(class="foo", tag_list)
  expect_equal(length(t1$children), 1)
  expect_equal(length(t1$children[[1]]), 3)
  expect_equal(t1$children[[1]][[1]]$name, "p")
  expect_equal(t1$children[[1]][[1]]$children[[1]], "tag1")
  expect_equal(t1$children[[1]][[2]]$name, "b")
  expect_equal(t1$children[[1]][[2]]$children[[1]], "tag2")
  expect_equal(t1$children[[1]][[3]]$name, "i")
  expect_equal(t1$children[[1]][[3]]$children[[1]], "tag3")


  # div tag used as starting point for tests below
  div_tag <- tags$div(class="foo")

  # Appending each child
  t2 <- tagAppendChild(div_tag, tag_list[[1]])
  t2 <- tagAppendChild(t2, tag_list[[2]])
  t2 <- tagAppendChild(t2, tag_list[[3]])
  t2a <- do.call(tags$div, c(tag_list, class="foo"))
  expect_identical(t2a, t2)


  # tagSetChildren, using list argument
  t2 <- tagSetChildren(div_tag, list = tag_list)
  expect_identical(t2a, t2)

  # tagSetChildren, using ... arguments
  t2 <- tagSetChildren(div_tag, tag_list[[1]], tag_list[[2]], tag_list[[3]])
  expect_identical(t2a, t2)

  # tagSetChildren, using ... and list arguments
  t2 <- tagSetChildren(div_tag, tag_list[[1]], list = tag_list[2:3])
  expect_identical(t2a, t2)

  # tagSetChildren overwrites existing children
  t2 <- tagAppendChild(div_tag, p("should replace this tag"))
  t2 <- tagSetChildren(div_tag, list = tag_list)
  expect_identical(t2a, t2)


  # tagAppendChildren, using list argument
  t2 <- tagAppendChild(div_tag, tag_list[[1]])
  t2 <- tagAppendChildren(t2, list = tag_list[2:3])
  expect_identical(t2a, t2)

  # tagAppendChildren, using ... arguments
  t2 <- tagAppendChild(div_tag, tag_list[[1]])
  t2 <- tagAppendChildren(t2, tag_list[[2]], tag_list[[3]])
  expect_identical(t2a, t2)

  # tagAppendChildren, using ... and list arguments
  t2 <- tagAppendChild(div_tag, tag_list[[1]])
  t2 <- tagAppendChildren(t2, tag_list[[2]], list = list(tag_list[[3]]))
  expect_identical(t2a, t2)

  # tagAppendChildren can start with no children
  t2 <- tagAppendChildren(div_tag, list = tag_list)
  expect_identical(t2a, t2)


  # tagSetChildren preserves attributes
  x <- tagSetChildren(div(), HTML("text"))
  expect_identical(attr(x$children[[1]], "html", TRUE), TRUE)

  # tagAppendChildren preserves attributes
  x <- tagAppendChildren(div(), HTML("text"))
  expect_identical(attr(x$children[[1]], "html", TRUE), TRUE)
})


test_that("Creating simple tags", {
  # Empty tag
  expect_identical(
    div(),
    structure(
      list(name = "div", attribs = dots_list(), children = list()),
      .Names = c("name", "attribs", "children"),
      class = "shiny.tag"
    )
  )

  # Tag with text
  expect_identical(
    div("text"),
    structure(
      list(name = "div", attribs = dots_list(), children = list("text")),
      .Names = c("name", "attribs", "children"),
      class = "shiny.tag"
    )
  )

  # NULL attributes are dropped
  expect_identical(
    div(a = NULL, b = "value"),
    div(b = "value")
  )

  # length-0 attributes are dropped
  expect_identical(
    div(a = character(), b = "value"),
    div(b = "value")
  )

  # NULL children are dropped
  expect_identical(
    renderTags(div("foo", NULL, list(NULL, list(NULL, "bar"))))$html,
    renderTags(div("foo", "bar"))$html
  )

  # length-0 children are dropped
  expect_identical(
    renderTags(div("foo", character(), list(character(), list(list(), "bar"))))$html,
    renderTags(div("foo", "bar"))$html
  )

  # Numbers are coerced to strings
  expect_identical(
    renderTags(div(1234))$html,
    renderTags(div("1234"))$html
  )
})


test_that("Creating nested tags", {
  # Simple version
  # Note that the $children list should not have a names attribute
  expect_identical(
    div(class="foo", list("a", "b")),
    structure(
      list(name = "div",
        attribs = structure(list(class = "foo"), .Names = "class"),
        children = list(list("a", "b"))),
      .Names = c("name", "attribs", "children"),
      class = "shiny.tag"
    )
  )

  # More complex version
  t1 <- withTags(
    div(class = "foo",
      p("child tag"),
      list(
        p("in-list child tag 1"),
        "in-list character string",
        p(),
        p("in-list child tag 2")
      ),
      "character string",
      1234
    )
  )

  # t1 should be identical to this data structure.
  # The nested list should be flattened, and non-tag, non-strings should be
  # converted to strings
  t1_full <- structure(
    list(
      name = "div",
      attribs = list(class = "foo"),
      children = list(
        structure(list(name = "p",
          attribs = list(),
          children = list("child tag")),
          class = "shiny.tag"
        ),
        structure(list(name = "p",
          attribs = list(),
          children = list("in-list child tag 1")),
          class = "shiny.tag"
        ),
        "in-list character string",
        structure(list(name = "p",
          attribs = list(),
          children = list()),
          class = "shiny.tag"
        ),
        structure(list(name = "p",
          attribs = list(),
          children = list("in-list child tag 2")),
          class = "shiny.tag"
        ),
        "character string",
        "1234"
      )
    ),
    class = "shiny.tag"
  )

  expect_identical(renderTags(t1)$html, renderTags(t1_full)$html)
})

# The .noWS option was added in 0.3.6.9003; we may still encounter tags created
# in an older version (perhaps saved to an RDS file and restored). They would
# lack this element in their structure.
test_that("Old tags without the .noWS option can still be rendered", {
  oldTag <- structure(
    list(name = "div", attribs = dots_list(), children = list("text")),
    .Names = c("name", "attribs", "children"),
    class = "shiny.tag"
  )
  w <- WSTextWriter()
  tagWrite(oldTag, w)

  expect_identical(
    w$readAll(),
    "<div>text</div>\n"
  )
})

# We moved to rlang::dots_list in 0.3.6; we may still encounter tags created
# in an older version (perhaps saved to an RDS file and restored). They would
# use old-school lists.
test_that("Old tags predating rlang::list2 can still be rendered", {
  oldTag <- structure(
    list(name = "div", attribs = list(), children = list("text")),
    .Names = c("name", "attribs", "children"),
    class = "shiny.tag"
  )
  w <- WSTextWriter()
  tagWrite(oldTag, w)

  expect_identical(
    w$readAll(),
    "<div>text</div>\n"
  )
})

test_that("tag with noWS works",{
  oneline <- tag("span", list(tag("strong", "Super strong", .noWS="outside")))
  expect_identical(as.character(oneline), "<span><strong>Super strong</strong></span>")
})

test_that("tag/s with invalid noWS fails fast", {
  expect_error(tag("span", .noWS="wrong"))
  expect_error(tags$a(.noWS="wrong"))
})

test_that("Attributes are preserved", {
  # HTML() adds an attribute to the data structure (note that this is
  # different from the 'attribs' field in the list)
  x <- HTML("<tag>&&</tag>")
  expect_identical(attr(x, "html", TRUE), TRUE)
  expect_equivalent(format(x), "<tag>&&</tag>")

  # Make sure attributes are preserved when wrapped in other tags
  x <- div(HTML("<tag>&&</tag>"))
  expect_equivalent(x$children[[1]], HTML("<tag>&&</tag>"))
  expect_identical(attr(x$children[[1]], "html", TRUE), TRUE)
  expect_equivalent(format(x), "<div><tag>&&</tag></div>")

  # Deeper nesting
  x <- div(p(HTML("<tag>&&</tag>")))
  expect_equivalent(x$children[[1]]$children[[1]], HTML("<tag>&&</tag>"))
  expect_identical(attr(x$children[[1]]$children[[1]], "html", TRUE), TRUE)
  expect_equivalent(format(x), "<div>\n  <p><tag>&&</tag></p>\n</div>")
})

test_that("Adding attributes to tags", {
  t1 <- tags$div("foo")

  # Adding attributes to empty tag
  expect_identical(t1$attribs, dots_list())
  expect_identical(
    tagAppendAttributes(t1, class = "c1")$attribs,
    list(class = "c1")
  )

  # Adding attribute with multiple values
  expect_identical(
    tagAppendAttributes(t1, class = "c1 c2")$attribs,
    list(class = "c1 c2")
  )

  # Adding two different attributes
  expect_identical(
    tagAppendAttributes(t1, class = "c1", id = "foo")$attribs,
    list(class = "c1", id = "foo")
  )

  # Adding attributes in two successive calls
  expect_identical(
    tagAppendAttributes(
      tagAppendAttributes(t1, class = "c1 c2"), class = "c3")$attribs,
    list(class = "c1 c2", class = "c3")
  )

  t2 <- tags$div("foo", class = "c1")

  # Adding attributes on a tag with other attributes
  expect_identical(
    tagAppendAttributes(t2, id = "foo")$attribs,
    list(class = "c1", id = "foo")
  )

  # Adding attributes on a tag with the same attribute
  expect_identical(
    tagAppendAttributes(t2, class = "c2")$attribs,
    list(class = "c1", class = "c2")
  )
})

test_that("Testing for attributes on tags", {
  t1 <- tags$div("foo", class = "c1", class = "c2", id = "foo")

  # Testing for attribute that does not exist
  expect_identical(
    tagHasAttribute(t1, "nope"),
    FALSE
  )

  # Testing for an attribute that exists once
  expect_identical(
    tagHasAttribute(t1, "id"),
    TRUE
  )

  # Testing for an attribute that exists multiple times
  expect_identical(
    tagHasAttribute(t1, "class"),
    TRUE
  )

  # Testing for substring of an attribute that exists
  expect_identical(
    tagHasAttribute(t1, "clas"),
    FALSE
  )

  # Testing for superstring of an attribute that exists
  expect_identical(
    tagHasAttribute(t1, "classes"),
    FALSE
  )

  # Testing for attribute with empty value
  t2 <- tags$div("foo", foo = "")
  expect_identical(
    tagHasAttribute(t2, "foo"),
    TRUE
  )

  # Testing for attribute with NULL value
  t3 <- tags$div("foo", foo = NULL)
  expect_identical(
    tagHasAttribute(t3, "foo"),
    FALSE
  )
})

test_that("Getting attributes from tags", {
  # Getting an attribute from a tag with no attributes
  t1 <- tags$div("foo")
  expect_identical(
    tagGetAttribute(t1, "class"),
    NULL
  )

  t2 <- tags$div("foo", class = "c1")

  # Getting an attribute from a tag without the correct attribute
  expect_identical(
    tagGetAttribute(t2, "id"),
    NULL
  )

  # Getting an attribute from a tag with the a single value for the attribute
  expect_identical(
    tagGetAttribute(t2, "class"),
    "c1"
  )

  # Getting an attribute from a tag with multiple matching attributes
  t3 <- tags$div("foo", class = "c1", id = "foo", class = "c2")
  expect_identical(
    tagGetAttribute(t3, "class"),
    "c1 c2"
  )

  # Getting an attribute from a tag where the attributes were factors
  t4 <- tags$div("foo", class = as.factor("c1"), class = as.factor("c2"))
  expect_identical(
    tagGetAttribute(t4, "class"),
    "c1 c2"
  )

  # Getting a numeric attribute from a tag
  t5 <- tags$div("foo", class = 78)
  expect_identical(
    tagGetAttribute(t5, "class"),
    "78"
  )
})

test_that("NA attributes are rendered correctly", {
  expect_identical(
    as.character(tags$div("text", foo = NA)),
    '<div foo>text</div>'
  )
  expect_identical(
    as.character(tags$div("text", class = "a", foo = NA)),
    '<div class="a" foo>text</div>'
  )
  expect_identical(
    as.character(tags$div("text", class = "a", foo = NA, class = "b")),
    '<div class="a b" foo>text</div>'
  )

  # Multiple NA's are coalesced
  expect_identical(
    as.character(tags$div("text", class = "a", foo = NA, class = "b", foo = NA)),
    '<div class="a b" foo>text</div>'
  )

  # A non-NA value supersedes NA
  expect_identical(
    as.character(tags$div("text", class = "a", foo = NA, foo = "b")),
    '<div class="a" foo="b">text</div>'
  )
  expect_identical(
    as.character(tags$div("text", class = "a", foo = "b", foo = NA, foo = "c")),
    '<div class="a" foo="b c">text</div>'
  )
  expect_identical(
    as.character(tags$div("text", class = "a", foo = "b", foo = NA, foo = NA, foo = "c")),
    '<div class="a" foo="b c">text</div>'
  )
})


test_that("Flattening a list of tags", {
  # Flatten a nested list
  nested <- list(
    "a1",
    list(
      "b1",
      list("c1", "c2"),
      list(),
      "b2",
      list("d1", "d2")
    ),
    "a2"
  )

  flat <- list("a1", "b1", "c1", "c2", "b2", "d1", "d2", "a2")
  expect_identical(flattenTags(nested), flat)

  # no-op for flat lists
  expect_identical(flattenTags(list(a="1", "b")), list(a="1", "b"))

  # numbers are coerced to character
  expect_identical(flattenTags(list(a=1, "b")), list(a="1", "b"))

  # empty list results in empty list
  expect_identical(flattenTags(list()), list())

  # preserve attributes
  nested <- list("txt1", list(structure("txt2", prop="prop2")))
  flat <- list("txt1",
    structure("txt2", prop="prop2"))
  expect_identical(flattenTags(nested), flat)
})

test_that("Head and singleton behavior", {
  result <- renderTags(tagList(
    tags$head(singleton("hello"))
  ))

  expect_identical(result$html, HTML(""))
  expect_identical(result$head, HTML("  hello"))
  expect_identical(result$singletons, "089cce0335cf2bae2bcb08cc753ba56f8e1ea8ed")

  # Ensure that "hello" actually behaves like a singleton
  result2 <- renderTags(tagList(
    tags$head(singleton("hello"))
  ), singletons = result$singletons)

  expect_identical(result$singletons, result2$singletons)
  expect_identical(result2$head, HTML(""))
  expect_identical(result2$html, HTML(""))

  result3 <- renderTags(tagList(
    tags$head(singleton("hello"), singleton("hello"))
  ))
  expect_identical(result$singletons, result3$singletons)
  expect_identical(result3$head, HTML("  hello"))

  # Ensure that singleton can be applied to lists, not just tags
  result4 <- renderTags(list(singleton(list("hello")), singleton(list("hello"))))
  expect_identical(result4$singletons, "110d1f0ef6762db2c6863523a7c379a697b43ea3")
  expect_identical(result4$html, renderTags(HTML("hello"))$html)

  result5 <- renderTags(tagList(singleton(list(list("hello")))))
  expect_identical(result5$html, renderTags("hello")$html)
})

test_that("Factors are treated as characters, not numbers", {
  myfactors <- factor(LETTERS[1:3])
  expect_identical(
    as.character(tags$option(value=myfactors[[1]], myfactors[[1]])),
    '<option value="A">A</option>'
  )

  expect_identical(
    as.character(tags$option(value=myfactors[[1]], value='B', value=3, myfactors[[1]])),
    '<option value="A B 3">A</option>'
  )
})

test_that("Unusual list contents are rendered correctly", {
  expect_identical(renderTags(list(NULL)), renderTags(HTML("")))
  expect_identical(renderTags(list(100)), renderTags(HTML("100")))
  expect_identical(renderTags(list(list(100))), renderTags(HTML("100")))
  expect_identical(renderTags(list(list())), renderTags(HTML("")))
  expect_identical(renderTags(NULL), renderTags(HTML("")))
})

test_that("Low-level singleton manipulation methods", {
  # Default arguments drop singleton duplicates and strips the
  # singletons it keeps of the singleton bit
  result1 <- takeSingletons(tags$div(
    singleton(tags$head(tags$script("foo"))),
    singleton(tags$head(tags$script("foo")))
  ))

  expect_identical(result1$ui$children[[2]], NULL)
  expect_false(is.singleton(result1$ui$children[[1]]))

  # desingleton=FALSE means drop duplicates but don't strip the
  # singleton bit
  result2 <- takeSingletons(tags$div(
    singleton(tags$head(tags$script("foo"))),
    singleton(tags$head(tags$script("foo")))
  ), desingleton=FALSE)

  expect_identical(result2$ui$children[[2]], NULL)
  expect_true(is.singleton(result2$ui$children[[1]]))

  result3 <- surroundSingletons(tags$div(
    singleton(tags$script("foo")),
    singleton(tags$script("foo"))
  ))

  expect_identical(
    renderTags(result3)$html,
    HTML("<div>
  <!--SHINY.SINGLETON[98eee9ba1f9e4ab3db75f33036bf91d4e214342b]-->
  <script>foo</script>
  <!--/SHINY.SINGLETON[98eee9ba1f9e4ab3db75f33036bf91d4e214342b]-->
  <!--SHINY.SINGLETON[98eee9ba1f9e4ab3db75f33036bf91d4e214342b]-->
  <script>foo</script>
  <!--/SHINY.SINGLETON[98eee9ba1f9e4ab3db75f33036bf91d4e214342b]-->
</div>")
    )
})

test_that("Indenting can be controlled/suppressed", {
  expect_identical(
    renderTags(tags$div("a", "b"))$html,
    HTML("<div>\n  a\n  b\n</div>")
  )
  expect_identical(
    format(tags$div("a", "b")),
    "<div>\n  a\n  b\n</div>"
  )

  expect_identical(
    renderTags(tags$div("a", "b"), indent = 2)$html,
    HTML("    <div>\n      a\n      b\n    </div>")
  )
  expect_identical(
    format(tags$div("a", "b"), indent = 2),
    "    <div>\n      a\n      b\n    </div>"
  )

  expect_identical(
    renderTags(tags$div("a", "b"), indent = FALSE)$html,
    HTML("<div>\na\nb\n</div>")
  )
  expect_identical(
    format(tags$div("a", "b"), indent = FALSE),
    "<div>\na\nb\n</div>"
  )

  expect_identical(
    renderTags(tagList(tags$div("a", "b")), indent = FALSE)$html,
    HTML("<div>\na\nb\n</div>")
  )
  expect_identical(
    format(tagList(tags$div("a", "b")), indent = FALSE),
    "<div>\na\nb\n</div>"
  )
})

test_that("cssList tests", {
  expect_identical("", css())
  expect_identical("", css())
  expect_identical(
    css(
      font.family = 'Helvetica, "Segoe UI"',
      font_size = "12px",
      `font-style` = "italic",
      font.variant = NULL,
      "font-weight!" = factor("bold"),
      padding = c("10px", "9px", "8px")
    ),
    "font-family:Helvetica, \"Segoe UI\";font-size:12px;font-style:italic;font-weight:bold !important;padding:10px 9px 8px;"
  )

  # Unnamed args not allowed
  expect_error(css("10"))
  expect_error(css(1, b=2))

  # NULL and empty string are dropped
  expect_identical(css(a="", b = NULL, "c!" = NULL, d = character()), "")

  # We are dumb about duplicated properties. Probably don't do that.
  expect_identical(css(a=1, a=2), "a:1;a:2;")
})

test_that("Non-tag objects can be coerced", {

  .GlobalEnv$as.tags.testcoerce1 <- function(x) {
    list(singleton(list("hello")))
  }
  on.exit(rm("as.tags.testcoerce1", pos = .GlobalEnv), add = TRUE)

  # Make sure tag-coerceable objects are tagified
  result1 <- renderTags(structure(TRUE, class = "testcoerce1"))
  expect_identical(result1$html, HTML("hello"))
  expect_identical(result1$singletons, "110d1f0ef6762db2c6863523a7c379a697b43ea3")

  # Make sure tag-coerceable objects are tagified before singleton handling
  # occurs, but that over-flattening doesn't happen
  result2 <- renderTags(tagList(
    singleton(list("hello")),
    structure(TRUE, class = "testcoerce1")
  ))
  expect_identical(result2$html, HTML("hello"))
  expect_identical(result2$singletons, "110d1f0ef6762db2c6863523a7c379a697b43ea3")

})

test_that("Latin1 and system encoding are converted to UTF-8", {
  #Sys.setlocale(, "Chinese")
  latin1_str <- rawToChar(as.raw(0xFF))
  Encoding(latin1_str) <- "latin1"

  divLatin1 <- as.character(tags$div(latin1_str))
  expect_identical(
    charToRaw(divLatin1),
    as.raw(c(0x3c, 0x64, 0x69, 0x76, 0x3e, 0xc3, 0xbf, 0x3c, 0x2f,
      0x64, 0x69, 0x76, 0x3e))
  )
  expect_identical(Encoding(divLatin1), "UTF-8")

  expect_identical(Encoding("\u4E11"), "UTF-8")
  divUTF8 <- as.character(tags$div("\u4E11"))
  expect_identical(
    charToRaw(divUTF8),
    as.raw(c(0x3c, 0x64, 0x69, 0x76, 0x3e, 0xe4, 0xb8, 0x91, 0x3c,
      0x2f, 0x64, 0x69, 0x76, 0x3e))
  )
  expect_identical(Encoding(divUTF8), "UTF-8")

  divMixed <- format(tags$div(
    "\u4E11", latin1_str,
    tags$span(a="\u4E11", latin1_str),
    tags$span(b=latin1_str, HTML("\u4E11"))
  ))
  expect_identical(
    charToRaw(divMixed),
    as.raw(c(0x3c, 0x64, 0x69, 0x76, 0x3e, 0x0a, 0x20, 0x20, 0xe4,
      0xb8, 0x91, 0x0a, 0x20, 0x20, 0xc3, 0xbf, 0x0a, 0x20, 0x20, 0x3c,
      0x73, 0x70, 0x61, 0x6e, 0x20, 0x61, 0x3d, 0x22, 0xe4, 0xb8, 0x91,
      0x22, 0x3e, 0xc3, 0xbf, 0x3c, 0x2f, 0x73, 0x70, 0x61, 0x6e, 0x3e,
      0x0a, 0x20, 0x20, 0x3c, 0x73, 0x70, 0x61, 0x6e, 0x20, 0x62, 0x3d,
      0x22, 0xc3, 0xbf, 0x22, 0x3e, 0xe4, 0xb8, 0x91, 0x3c, 0x2f, 0x73,
      0x70, 0x61, 0x6e, 0x3e, 0x0a, 0x3c, 0x2f, 0x64, 0x69, 0x76, 0x3e
    ))
  )
  expect_identical(Encoding(divMixed), "UTF-8")

  # Encoding(HTML(latin1_str)) is "UTF-8" on Linux; even just
  # paste(latin1_str) returns a UTF-8 encoded string
  #expect_identical(Encoding(HTML(latin1_str)), "latin1")

  expect_identical(Encoding(format(HTML(latin1_str))), "UTF-8")
  expect_identical(Encoding(format(tagList(latin1_str))), "UTF-8")

  # ensure the latin1 attribute returns correctly after escaping
  latin1_str2 <- rawToChar(as.raw(c(0xff, 0x0d, 0x0a)))
  Encoding(latin1_str2) <- "latin1"
  spanLatin <- as.character(tags$span(latin1_str2, title = latin1_str2))
  expect_identical(Encoding(spanLatin), "UTF-8")
  expect_identical(
    charToRaw(spanLatin),
    as.raw(c(0x3c, 0x73, 0x70, 0x61, 0x6e, 0x20, 0x74, 0x69, 0x74,
             0x6c, 0x65, 0x3d, 0x22, 0xc3, 0xbf, 0x26, 0x23, 0x31, 0x33, 0x3b,
             0x26, 0x23, 0x31, 0x30, 0x3b, 0x22, 0x3e, 0xc3, 0xbf, 0x0d, 0x0a,
             0x3c, 0x2f, 0x73, 0x70, 0x61, 0x6e, 0x3e
    ))
  )
})

test_that("paste8 in Chinese locale works", {
  loc <- "Chinese"
  testthat::skip_if_not(is_locale_available(loc), "Chinese locale not available")

  withr::with_locale(c(LC_COLLATE=loc, LC_CTYPE=loc, LC_MONETARY=loc, LC_TIME=loc), {
    x <- "\377"
    Encoding(x) <- "latin1"
    expect_identical(x, "\Uff")
    expect_identical(Encoding(x), "latin1")

    y <- "\U4E2d"  # Using \Uxxxx always is encoded as UTF-8
    expect_identical(y, "\U4E2d")
    expect_identical(Encoding(y), "UTF-8")

    xy <- paste8(x, y)
    xy
    expect_identical(xy, "\Uff \U4E2d")
    expect_identical(Encoding(xy), "UTF-8")

    xy <- paste8(c(x, y), collapse = "")
    expect_identical(xy, "\Uff\U4E2d")
    expect_identical(Encoding(xy), "UTF-8")
  })
})

test_that("Printing tags works", {
  expect_identical(
    capture.output(print(tags$a(href = "#", "link"))),
    '<a href="#">link</a>'
  )
})

test_that("htmlEscape will try to coerce inputs to characters", {
  x <- list(a1 = "b", a2 = list("b1", "b2"))
  expect_identical(
    htmlEscape(x),
    as.character(x)
  )
})
