context("test-colors")

colors_bad <- readLines(test_path("colors-bad.txt"))
colors_good <- readLines(test_path("colors-bad.txt"))

test_that("conv_hexstr_to_num255", {
  for (i in 0:255) {
    expect_identical(conv_hexstr_to_num255(sprintf("%02X", i)), i)
    expect_identical(conv_hexstr_to_num255(sprintf("%02x", i)), i)
  }
  for (i in 0:15) {
    expect_identical(conv_hexstr_to_num255(sprintf("%1X", i)), as.integer(i*16 + i))
    expect_identical(conv_hexstr_to_num255(sprintf("%1x", i)), as.integer(i*16 + i))
  }

  # Too many digits
  expect_error(conv_hexstr_to_num255("000"))
  # Too few digits
  expect_error(conv_hexstr_to_num255(""))
  # Not a valid number
  expect_error(conv_hexstr_to_num255("z"))
})

test_that("conv_decstr_to_num255", {
  expect_identical(conv_decstr_to_num255("-1"), 0)
  expect_identical(conv_decstr_to_num255("-0"), 0)
  expect_identical(conv_decstr_to_num255("-1.2"), 0)
  expect_identical(conv_decstr_to_num255("-.3"), 0)
  expect_identical(conv_decstr_to_num255("-.300"), 0)
  expect_identical(conv_decstr_to_num255("-300.300"), 0)

  expect_identical(conv_decstr_to_num255("125"), 125)
  expect_identical(conv_decstr_to_num255("125.5"), 126)
  # Rounding goes to nearest even number. :shrug:
  expect_identical(conv_decstr_to_num255("0.5"), 0)
  expect_identical(conv_decstr_to_num255(".5"), 0)
  expect_identical(conv_decstr_to_num255("1.5"), 2)

  expect_identical(conv_decstr_to_num255(300), 255)

  for (i in as.numeric(0:255)) {
    expect_identical(conv_decstr_to_num255(as.character(i)), i)
    expect_identical(conv_decstr_to_num255(as.character(i + 0.2)), i)
  }

  expect_error(conv_decstr_to_num255("-"))
  expect_error(conv_decstr_to_num255(""))
  expect_error(conv_decstr_to_num255("1."))
  expect_error(conv_decstr_to_num255("."))
  expect_error(conv_decstr_to_num255(" 1 "))
  expect_error(conv_decstr_to_num255(" "))
  expect_error(conv_decstr_to_num255("aa"))
})
