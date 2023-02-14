context("images")

test_that("capturePlot works with device functions with various signatures", {
  expect_error(library(ragg), NA)

  # If these run without throwing, that's success
  capturePlot(plot(cars))

  capturePlot(
     plot(pressure),
     tempfile(fileext = ".svg"),
     grDevices::svg,
     width = 8, height = 3.75
  )

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
