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
