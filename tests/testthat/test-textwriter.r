context("textwriter")

describe("WSTextWriter", {
  it("basically works", {
    wsw <- WSTextWriter()

    expect_identical(wsw$readAll(), "")

    wsw$write("")
    expect_identical(wsw$readAll(), "")

    wsw$write("line one")
    expect_identical(wsw$readAll(), "line one")

    wsw$write("\nanother line")
    expect_identical(wsw$readAll(), "line one\nanother line")

    wsw$write("more content")
    expect_identical(wsw$readAll(), "line one\nanother linemore content")

    expect_error(wsw$write(1))
    expect_error(wsw$write(letters[1:2]))
  })
  it("eats past and future whitespace", {
    wtw <- WSTextWriter()

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


  it("handles full buffers of collapsable writes", {
    wtw <- WSTextWriter(bufferSize = 2)

    wtw$write("a")
    wtw$write("b")
    wtw$write("c")
    wtw$write("d")
    wtw$write("e")
    wtw$write("f")

    expect_identical(wtw$readAll(), "abcdef")
    wtw$eatWS()
    expect_identical(wtw$readAll(), "abcdef")

    wtw$write("g")
    wtw$writeWS(" ")
    expect_identical(wtw$readAll(), "abcdefg ")
    wtw$eatWS()
    expect_identical(wtw$readAll(), "abcdefg")
  })

  it("handles full buffers of non-collapsable writeWS's", {
    wtw <- WSTextWriter(bufferSize = 2)

    # fill the buffer with whitespace that it might need to backtrack over
    wtw$write("a")
    wtw$writeWS(" ")
    wtw$writeWS(" ")
    wtw$writeWS(" ")
    wtw$writeWS(" ")

    expect_identical(wtw$readAll(), "a    ")
    wtw$eatWS()
    expect_identical(wtw$readAll(), "a")

    wtw$write("b")
    expect_identical(wtw$readAll(), "ab")
  })
})

describe("validateNoWS",{
  it("basically works", {
    validateNoWS(NULL)
    validateNoWS(noWSOptions[1])
    validateNoWS(noWSOptions[1:2])
    validateNoWS(noWSOptions)
    expect_error(validateNoWS("badOption"))
    expect_error(validateNoWS(c(noWSOptions, "badOption")))

    # capitalization matters
    expect_error(validateNoWS(toupper(noWSOptions[1])))
  })
})
