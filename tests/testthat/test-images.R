context("images")

test_that("capturePlot works with device functions with various signatures", {

  # If these run without throwing, that's success

  capturePlot(plot(cars), device = function(file, width, height, res) {
    grDevices::png(filename = file, width = width, height = height, res = res)
  })

  capturePlot(plot(cars), device = function(file, width, height) {
    grDevices::png(filename = file, width = width, height = height)
  })

  capturePlot(plot(cars), device = function(filename, ...) {
    grDevices::png(filename = filename, ...)
  })
})
