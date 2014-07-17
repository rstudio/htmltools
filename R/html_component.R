

#' @export
htmlComponent <- function(generator, sizing = NULL) {
  structure(class = "html_component", list(
    generator = generator,
    sizing = sizing
  ))
}

#' @export
print.html_component <- function(x, ...) {

  # Generate HTML and wrap it in a DIV that we can apply sizing policy to.
  # This could involve any number of things:
  #
  #   (1) Rendering in the Viewer and auto-sizing to it's frame size
  #   (2) Rendering in the Viewer with a predetermied size
  #   (3) Rendering in the Viewer and attempting to force the viewer
  #       to be vertically larger
  #   (4) Opting out of the Viewer entirely and rendering in a full
  #       web browser with either a fixed or dynamic size
  #
  # (For now we just use a hard-coded width and height)
  #
  div <- tags$div(x$generator(), width = 400, height = 300)

  # print it
  html_print(div)
}

#' @export
knit_print.html_component <- function(x, options, inline, ...) {

  # Generate HTLM and wrap it in a DIV that we can size. This could
  # involve:
  #
  #   (1) Always respecting the knitr chunk option sizes
  #   (2) Forcing a specific size and/or sizing based on user supplied
  #       function arguments
  #
  # (For now we just respect the kntir chunk option sizes)
  #
  div <- tags$div(x$generator(),
                  width = options$out.width.px,
                  height = options$out.height.px)

  # knit print it
  knit_print(div, options, inline, ...)
}

#' @export
as.tags.html_component <- function(x, ...) {

  # Convert to HTML tags by calling the generator
  #
  # what are the correct/appropriate sizing semantics for a shiny
  # application? i.e. same as plot for the default case?, somehow
  # responsive to the uiOutput container created?
  #
  # (for now just used hard coded width and height)
  #

  tags$div(x$generator(), width = "100%", height = 400)
}

#
#
# TODO: Print method that works within the "notebook" or "web console"
# Sizing semantics of this are TBD, but it seems likely that if we
# can define a sizing policy abstraction that encompasses the above
# use cases then we'll be able to do something reasonable by default
# in the notebook without additional notebook-specific options
#
#









