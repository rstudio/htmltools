context("images")

test_that("capturePlot works with device functions with various signatures", {

  capturePlot(plot(cars), device = grDevices::png)

  capturePlot(plot(cars), device = function(filename, width, height) {
    grDevices::png(filename = filename, width = width, height = height)
  })

  capturePlot(plot(cars), device = function(filename, ...) {
    grDevices::png(filename = filename, ...)
  })

  # Ensure blank plot works
  plotTag({}, alt = "", device = png)

  # So testthat knows we didn't skip testing
  expect_true(TRUE)
})
