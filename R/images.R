#' @export
capturePlot <- function(expr, filename = tempfile(fileext = ".png"),
  device = defaultPngDevice(), width = 400, height = 400, res = 72) {

  expr <- rlang::enquo(expr)

  tempFile <- missing(filename)

  device(filename = filename, width = width, height = height, res = res)
  dev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dev), add = TRUE, after = FALSE)

  op <- graphics::par(mar = rep(0, 4))
  tryCatch(graphics::plot.new(), finally = graphics::par(op))

  tryCatch({
    result <- withVisible(rlang::eval_tidy(expr))
    if (result$visible) {
      capture.output(print(result))
    }
    filename
  }, error = function(e) {
    try({
      if (tempFile && file.exists(filename))
        unlink(filename)
    })
    stop(e)
  })
}

#' @rdname capturePlot
#' @export
plotTag <- function(expr, alt, device = defaultPngDevice(), width = 400, height = 400,
  pixelratio = 2, contentType = "image/png", attribs = list(),
  suppressSize = c("none", "x", "y", "xy")) {

  suppressSize <- match.arg(suppressSize)
  if (suppressSize == "xy") {
    suppressSize <- c("x", "y")
  }

  file <- capturePlot(expr,
    device = device,
    width = width * pixelratio,
    height = height * pixelratio,
    res = 72 * pixelratio)

  on.exit(unlink(file), add = TRUE, after = FALSE)

  tags$img(
    src = base64enc::dataURI(file = file, mime = contentType),
    style = css(
      width = if (!"x" %in% suppressSize) validateCssUnit(width),
      height = if (!"y" %in% suppressSize) validateCssUnit(height)
    ),
    alt = alt,
    !!!attribs
  )
}

#' @rdname capturePlot
#' @export
defaultPngDevice <- function() {
  if (capabilities("aqua")) {
    grDevices::png
  } else if (nchar(system.file(package = "ragg"))) {
    ragg::agg_png
  } else if (nchar(system.file(package = "Cairo"))) {
    Cairo::CairoPNG
  } else {
    grDevices::png
  }
}
