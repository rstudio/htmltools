#' Capture a plot as a saved file
#'
#' Easily generates a .png file (or other graphics file) from a plotting
#' expression.
#'
#' @param expr A plotting expression that generates a plot (or yields an object
#'   that generates a plot when printed, like a ggplot2). We evaluate this
#'   expression after activating the graphics device (\code{device}).
#' @param filename The output filename. By default, a temp file with \code{.png}
#'   extension will be used; you should provide a filename with a different
#'   extension if you provide a non-PNG graphics device function.
#' @param device A graphics device function; by default, this will be either
#'   \code{\link[grDevices:png]{grDevices::png()}},
#'   \code{\link[ragg:agg_png]{ragg::agg_png()}}, or
#'   \code{\link[Cairo:CairoPNG]{Cairo::CairoPNG()}}, depending on
#'   your system and configuration. See \code{\link{defaultPngDevice}}.
#' @param width,height,res,... Additional arguments to the \code{device} function.
#'
#' @seealso \code{\link{plotTag}} saves plots as a self-contained \code{<img>}
#'   tag.
#'
#' @examples
#'
#' # Default settings
#' res <- capturePlot(plot(cars))
#'
#' if (interactive()) browseURL(res)
#'
#' # Use custom width/height
#' pngpath <- tempfile(fileext = ".png")
#' capturePlot(plot(pressure), pngpath, width = 800, height = 375)
#'
#' if (interactive()) browseURL(pngpath)
#'
#' # Use SVG
#' svgpath <- capturePlot(
#'   plot(pressure),
#'   tempfile(fileext = ".svg"),
#'   grDevices::svg,
#'   width = 8, height = 3.75)
#'
#' if (interactive()) browseURL(svgpath)
#'
#' # Clean up
#' unlink(res)
#' unlink(pngpath)
#' unlink(svgpath)
#'
#' @export
capturePlot <- function(expr, filename = tempfile(fileext = ".png"),
  device = defaultPngDevice(), width = 400, height = 400, res = 72,
  ...) {
  if (!is.function(device)) {
    stop(call. = FALSE, "The `device` argument should be a function, e.g. `grDevices::png`")
  }

  expr <- rlang::enquo(expr)

  tempFile <- missing(filename)

  args <- rlang::list2(width = width, height = height, res = res)
  argnms <- names(formals(device))
  if (!"..." %in% argnms) {
    # Only include `width`, `height`, and `res` if the corresponding formal
    # parameters are present.
    args <- args[names(args) %in% argnms]
  }
  args <- c(list(filename = filename), args, rlang::list2(...))

  do.call(device, args)
  dev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dev), add = TRUE)

  # Call plot.new() so that even if no plotting operations are performed at
  # least we have a blank background. N.B. we need to set the margin to 0
  # temporarily before plot.new() because when the plot size is small (e.g.
  # 200x50), we will get an error "figure margin too large", which is triggered
  # by plot.new() with the default (large) margin. However, this does not
  # guarantee user's code in `expr` will not trigger the error -- they may have
  # to set par(mar = smaller_value) before they draw base graphics.
  op <- graphics::par(mar = rep(0, 4))
  # Prevent examples() from prompting
  grDevices::devAskNewPage(FALSE)
  tryCatch(graphics::plot.new(), finally = graphics::par(op))

  tryCatch({
    result <- withVisible(rlang::eval_tidy(expr))
    if (result$visible) {
      capture.output(print(result$value))
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

#' Capture a plot as a self-contained \code{<img>} tag
#'
#' @param expr A plotting expression that generates a plot (or yields an object
#'   that generates a plot when printed, like a ggplot2).
#' @param alt A single-element character vector that contains a text description
#'   of the image. This is used by accessibility tools, such as screen readers
#'   for vision impaired users.
#' @param device A graphics device function; by default, this will be either
#'   \code{\link[grDevices:png]{grDevices::png()}},
#'   \code{\link[ragg:agg_png]{ragg::agg_png()}}, or
#'   \code{\link[Cairo:CairoPNG]{Cairo::CairoPNG()}}, depending on your system
#'   and configuration. See \code{\link{defaultPngDevice}}.
#' @param width,height The width/height that the generated tag should be
#'   displayed at, in logical (browser) pixels.
#' @param pixelratio Indicates the ratio between physical and logical units of
#'   length. For PNGs that may be displayed on high-DPI screens, use \code{2};
#'   for graphics devices that express width/height in inches (like
#'   \code{\link[grDevices:svg]{grDevices::svg()}}), try \code{1/72} or
#'   \code{1/96}.
#' @param mimeType The MIME type associated with the \code{device}. Examples are
#'   \code{image/png}, \code{image/tiff}, \code{image/svg+xml}.
#' @param deviceArgs A list of additional arguments that should be included when
#'   the \code{device} function is invoked.
#' @param attribs A list of additional attributes that should be included on the
#'   generated \code{<img>} (e.g. \code{id}, \code{class}).
#' @param suppressSize By default, \code{plotTag} will include a \code{style}
#'   attribute with \code{width} and \code{height} properties specified in
#'   pixels. If you'd rather specify the image size using other methods (like
#'   responsive CSS rules) you can use this argument to suppress width
#'   (\code{"x"}), height (\code{"y"}), or both (\code{"xy"}) properties.
#'
#' @return A \code{\link{browsable}} HTML \code{<img>} tag object. Print it at
#'   the console to preview, or call \code{\link{as.character}} on it to view the HTML
#'   source.
#'
#' @seealso \code{\link{capturePlot}} saves plots as an image file.
#'
#' @examples
#'
#' img <- plotTag({
#'   plot(cars)
#' }, "A plot of the 'cars' dataset", width = 375, height = 275)
#'
#' if (interactive()) img
#'
#'
#' svg <- plotTag(plot(pressure), "A plot of the 'pressure' dataset",
#'   device = grDevices::svg, width = 375, height = 275, pixelratio = 1/72,
#'   mimeType = "image/svg+xml")
#'
#' if (interactive()) svg
#'
#' @export
plotTag <- function(expr, alt, device = defaultPngDevice(), width = 400, height = 400,
  pixelratio = 2, mimeType = "image/png", deviceArgs = list(), attribs = list(),
  suppressSize = c("none", "x", "y", "xy")) {

  suppressSize <- match.arg(suppressSize)
  if (suppressSize == "xy") {
    suppressSize <- c("x", "y")
  }

  file <- rlang::eval_tidy(rlang::expr(capturePlot({{expr}},
    device = device,
    width = width * pixelratio,
    height = height * pixelratio,
    res = 72 * pixelratio,
    !!!deviceArgs)))

  on.exit(unlink(file), add = TRUE)

  browsable(tags$img(
    src = base64enc::dataURI(file = file, mime = mimeType),
    style = css(
      width = if (!"x" %in% suppressSize) validateCssUnit(width),
      height = if (!"y" %in% suppressSize) validateCssUnit(height)
    ),
    alt = alt,
    !!!attribs
  ))
}


#'   \code{\link[grDevices:png]{grDevices::png()}},
#'   \code{\link[ragg:agg_png]{ragg::agg_png()}}, or
#'   \code{\link[Cairo:CairoPNG]{Cairo::CairoPNG()}}, depending on your system

#' Determine the best PNG device for your system
#'
#' Returns the best PNG-based graphics device for your system, in the opinion of
#' the \code{htmltools} maintainers. On Mac,
#' \code{\link[grDevices:png]{grDevices::png()}} is used; on all other
#' platforms, either \code{\link[ragg:agg_png]{ragg::agg_png()}} or
#' \code{\link[Cairo:CairoPNG]{Cairo::CairoPNG()}} are used if their packages
#' are installed. Otherwise, \code{\link[grDevices:png]{grDevices::png()}} is
#' used.
#'
#' @return A graphics device function.
#'
#' @export
defaultPngDevice <- function() {
  if (capabilities("aqua")) {
    grDevices::png
  } else if (system.file(package = "ragg") != "") {
    ragg::agg_png
  } else if (system.file(package = "Cairo") != "") {
    Cairo::CairoPNG
  } else {
    grDevices::png
  }
}
