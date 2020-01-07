context("images")

test_that("capturePlot works with device functions with various signatures", {

  # If these run without throwing, that's success

  capturePlot(plot(cars), device = grDevices::png)

  capturePlot(plot(cars), device = function(filename, width, height) {
    grDevices::png(filename = filename, width = width, height = height)
  })

  capturePlot(plot(cars), device = function(filename, ...) {
    grDevices::png(filename = filename, ...)
  })

  # So testthat knows we didn't skip testing
  expect_true(TRUE)
})
