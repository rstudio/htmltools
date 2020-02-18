context("test-colors")

colors_bad <- readLines(test_path("colors-bad.txt"))
colors_good <- readLines(test_path("colors-bad.txt"))

test_that("decode_hex", {
  expect_identical(decode_hex(sprintf("%02X", 0:255)), 0:255)
  expect_identical(decode_hex(sprintf("%02x", 0:255)), 0:255)
  expect_identical(decode_hex(sprintf("%1X", 0:15)), as.integer(0:15*16 + 0:15))
  expect_identical(decode_hex(sprintf("%1x", 0:15)), as.integer(0:15*16 + 0:15))

  # Too many digits
  expect_error(decode_hex("000"))
  # Too few digits
  expect_error(decode_hex(""))
  # Not a valid number
  expect_error(decode_hex("z"))
})

test_that("decode_float_255", {
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

  expect_error(decode_float_1("a"))
  expect_error(decode_float_1(""))
  expect_error(decode_float_1(" "))
  expect_error(decode_float_1(" 0 "))
})
