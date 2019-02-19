context("textwriter")

describe("TextWriter", {
  it("basically works", {
    tw <- TextWriter$new()
    on.exit(tw$close())

    expect_identical(tw$readAll(), "")

    tw$write("")
    expect_identical(tw$readAll(), "")

    tw$write("line one")
    expect_identical(tw$readAll(), "line one")

    tw$write("\nanother line")
    expect_identical(tw$readAll(), "line one\nanother line")

    tw$write("more content")
    expect_identical(tw$readAll(), "line one\nanother linemore content")
  })

  it("bookmarks are respected", {
    tw <- TextWriter$new()
    on.exit(tw$close())

    tw$restorePosition()
    expect_identical(tw$readAll(), "")

    tw$write("foo")
    tw$restorePosition()
    expect_identical(tw$readAll(), "")

    tw$write("bar")
    tw$savePosition()
    tw$restorePosition()
    expect_identical(tw$readAll(), "bar")

    tw$write(" baz")
    tw$write(" qux")
    expect_identical(tw$readAll(), "bar baz qux")
    tw$restorePosition()
    expect_identical(tw$readAll(), "bar")
  })

  it("suppresses writes", {
    tw <- TextWriter$new()
    on.exit(tw$close())

    tw$setSuppress(TRUE)
    expect_identical(tw$readAll(), "")

    tw$write("this should be ignored")
    expect_identical(tw$readAll(), "")

    tw$setSuppress(FALSE)
    expect_identical(tw$readAll(), "")
    tw$write("this should not be ignored")
    expect_identical(tw$readAll(), "this should not be ignored")

    tw$write("!")
    expect_identical(tw$readAll(), "this should not be ignored!")

    # Make sure savePosition works well when we're suppressed
    tw$setSuppress(TRUE)
    tw$savePosition()
    tw$write("another ignored write")
    tw$setSuppress(FALSE)
    tw$write("\na second non-ignored line")
    expect_identical(tw$readAll(), "this should not be ignored!\na second non-ignored line")
    tw$restorePosition()
    expect_identical(tw$readAll(), "this should not be ignored!")
  })
})

describe("WSTextWriter", {
  it("eats past and future whitespace", {
    wtw <- WSTextWriter$new()
    on.exit(wtw$close())

    expect_identical(wtw$readAll(), "")
    wtw$writeWS("   ")
    expect_identical(wtw$readAll(), "   ")
    wtw$writeWS("   ")
    wtw$writeWS("   ")
    wtw$eatWS()
    expect_identical(wtw$readAll(), "")
    wtw$writeWS("   ")
    wtw$writeWS("   ")
    wtw$writeWS("   ")
    expect_identical(wtw$readAll(), "")

    wtw$write("Hello")
    expect_identical(wtw$readAll(), "Hello")
    wtw$writeWS("  ")
    expect_identical(wtw$readAll(), "Hello  ")
    wtw$eatWS()
    expect_identical(wtw$readAll(), "Hello")
    wtw$writeWS("  ")
    expect_identical(wtw$readAll(), "Hello")
  })
})
