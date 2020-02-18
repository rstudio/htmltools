context("test-colors")

colors_bad <- readLines(test_path("colors-bad.txt"))
colors_good <- readLines(test_path("colors-bad.txt"))

test_that("conv_hexstr_to_num255", {
  expect_identical(conv_hexstr_to_num255(sprintf("%02X", 0:255)), 0:255)
  expect_identical(conv_hexstr_to_num255(sprintf("%02x", 0:255)), 0:255)
  expect_identical(conv_hexstr_to_num255(sprintf("%1X", 0:15)), as.integer(0:15*16 + 0:15))
  expect_identical(conv_hexstr_to_num255(sprintf("%1x", 0:15)), as.integer(0:15*16 + 0:15))

  # Too many digits
  expect_error(conv_hexstr_to_num255("000"))
  # Too few digits
  expect_error(conv_hexstr_to_num255(""))
  # Not a valid number
  expect_error(conv_hexstr_to_num255("z"))
})

test_that("conv_decstr_to_num255", {
  expect_identical(conv_decstr_to_num255("-1"), 0L)
  expect_identical(conv_decstr_to_num255("-0"), 0L)
  expect_identical(conv_decstr_to_num255("-1.2"), 0L)
  expect_identical(conv_decstr_to_num255("-.3"), 0L)
  expect_identical(conv_decstr_to_num255("-.300"), 0L)
  expect_identical(conv_decstr_to_num255("-300.300"), 0L)

  expect_identical(conv_decstr_to_num255("125"), 125L)
  expect_identical(conv_decstr_to_num255("125.5"), 126L)
  # Rounding goes to nearest even number. :shrug:
  expect_identical(conv_decstr_to_num255("0.5"), 0L)
  expect_identical(conv_decstr_to_num255(".5"), 0L)
  expect_identical(conv_decstr_to_num255("1.5"), 2L)

  expect_identical(conv_decstr_to_num255(300), 255L)

  expect_identical(conv_decstr_to_num255(as.character(0:255)), 0:255)
  expect_identical(conv_decstr_to_num255(as.character(0:255 + 0.2)), 0:255)

  expect_error(conv_decstr_to_num255("-"))
  expect_error(conv_decstr_to_num255(""))
  expect_error(conv_decstr_to_num255("1."))
  expect_error(conv_decstr_to_num255("."))
  expect_error(conv_decstr_to_num255(" 1 "))
  expect_error(conv_decstr_to_num255(" "))
  expect_error(conv_decstr_to_num255("aa"))
})

test_that("conv_unitstr_to_num255", {
  expect_identical(
    conv_unitstr_to_num255(as.character(0:1000/1000)),
    as.integer(round((0:1000/1000)*255))
  )
  # Without leading 0
  expect_identical(
    conv_unitstr_to_num255(
      gsub("^0+\\.", ".", as.character(0:1000/1000))
    ),
    as.integer(round((0:1000/1000)*255))
  )

  # Clamp
  expect_identical(conv_unitstr_to_num255(-1), 0L)
  expect_identical(conv_unitstr_to_num255(2), 255L)

  expect_error(conv_unitstr_to_num255("a"))
  expect_error(conv_unitstr_to_num255(""))
  expect_error(conv_unitstr_to_num255(" "))
  expect_error(conv_unitstr_to_num255(" 0 "))
})
