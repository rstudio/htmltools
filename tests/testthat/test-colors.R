context("test-colors")

colors_bad <- readLines(test_path("colors-bad.txt"))
colors_good <- readLines(test_path("colors-good.txt"))

test_that("parseCssColors", {
  for (color in colors_bad) {
    expect_error(parseCssColors(color))
  }
  for (color in colors_good) {
    expect_error(parseCssColors(color), NA)
  }
})

test_that("decode_hex", {
  expect_identical(decode_hex(sprintf("%02X", 0:255)), 0:255)
  expect_identical(decode_hex(sprintf("%02x", 0:255)), 0:255)
  expect_identical(decode_hex(sprintf("%1X", 0:15)), as.integer(0:15*16 + 0:15))
  expect_identical(decode_hex(sprintf("%1x", 0:15)), as.integer(0:15*16 + 0:15))

  expect_identical(decode_hex(character(0)), integer(0))

  # Too many digits
  expect_error(decode_hex("000"))
  # Too few digits
  expect_error(decode_hex(""))
  # Not a valid number
  expect_error(decode_hex("z"))
})

test_that("decode_float_255", {
  expect_identical(decode_float_255(character(0)), integer(0))

  expect_identical(decode_float_255("-1"), 0L)
  expect_identical(decode_float_255("-0"), 0L)
  expect_identical(decode_float_255("-1.2"), 0L)
  expect_identical(decode_float_255("-.3"), 0L)
  expect_identical(decode_float_255("-.300"), 0L)
  expect_identical(decode_float_255("-300.300"), 0L)

  expect_identical(decode_float_255("125"), 125L)
  expect_identical(decode_float_255("125.5"), 126L)
  # Rounding goes to nearest even number. :shrug:
  expect_identical(decode_float_255("0.5"), 0L)
  expect_identical(decode_float_255(".5"), 0L)
  expect_identical(decode_float_255("1.5"), 2L)

  expect_identical(decode_float_255(300), 255L)

  expect_identical(decode_float_255(as.character(0:255)), 0:255)
  expect_identical(decode_float_255(as.character(0:255 + 0.2)), 0:255)

  expect_error(decode_float_255("-"))
  expect_error(decode_float_255(""))
  expect_error(decode_float_255("1."))
  expect_error(decode_float_255("."))
  expect_error(decode_float_255(" 1 "))
  expect_error(decode_float_255(" "))
  expect_error(decode_float_255("aa"))
})

test_that("decode_float_1", {
  expect_identical(
    decode_float_1(as.character(0:1000/1000)),
    as.integer(round((0:1000/1000)*255))
  )
  # Without leading 0
  expect_identical(
    decode_float_1(
      gsub("^0+\\.", ".", as.character(0:1000/1000))
    ),
    as.integer(round((0:1000/1000)*255))
  )

  # Clamp
  expect_identical(decode_float_1(-1), 0L)
  expect_identical(decode_float_1(2), 255L)

  expect_identical(decode_float_1(character(0)), integer(0))

  expect_error(decode_float_1("a"))
  expect_error(decode_float_1(""))
  expect_error(decode_float_1(" "))
  expect_error(decode_float_1(" 0 "))
})

test_that("decode_float_identity", {
  rand <- runif(1000, min = -1000, max = 1000)
  rand <- c(rand, 0)
  expect_equal(decode_float_identity(as.character(rand)), rand)

  expect_identical(decode_float_identity(character(0)), numeric(0))

  expect_error(decode_float_identity("0."))
  expect_error(decode_float_identity("."))
  expect_error(decode_float_identity(""))
  expect_error(decode_float_identity(NA))
})

test_that("decode_color_keyword", {
  expect_identical(
    encode_hex(decode_color_keyword(c("cornflowerblue", "orange"))),
    c("#6495ED", "#FFA500")
  )

  expect_error(decode_color_keyword(""))
  expect_error(decode_color_keyword("notvalid"))
  expect_error(decode_color_keyword(" orange "))
  expect_error(decode_color_keyword(NA))
})
