# @staticimports pkg:staticimports
#  system_file is_installed

# Implements a "whitespace eating" writer.
#
# WSTextWriter relies on the caller distinguishing between writes of important
# content, and writes of whitespace that may or may not be elided (`.$write()`
# vs `.$writeWS()`).
#
# At any point, `eatWS` may be called, which will cause any recent `writeWS`
# operations (i.e. those since either the beginning of time, or the most recent
# `write` operation) to be undone, AND for any future `writeWS` calls to be
# ignored. A call to `write` will be respected, and will restore normal
# behavior.
#
# Text is automatically converted to UTF-8 before being written.
#' @param bufferSize The initial size of the buffer in which writes are stored.
#' @noRd
WSTextWriter <- function(bufferSize=1024) {
  if (bufferSize < 3) {
    stop("Buffer size must be at least 3")
  }

  # The buffer into which we enter all the writes.
  buffer <- character(bufferSize)

  # The index storing the position in the buffer of the most recent write.
  marked <- 0

  # The index storing the position in the buffer of the most recent write or writeWS.
  position <- 0

  # TRUE if we're eating whitespace right now, in which case calls to writeWS are no-ops.
  suppressing <- FALSE

  # Logic to do the actual write
  writeImpl <- function(text) {
    # force `text` to evaluate and check that it's the right shape
    if (!is.character(text)) {
      stop("Text to be written must be a length-one character vector")
    }

    # The text that is written to this writer will be converted to
    # UTF-8 using enc2utf8. The rendered output will always be UTF-8
    # encoded.
    enc <- enc2utf8(text)

    # Move the position pointer and store the (encoded) write
    n <- length(text)
    # TODO is it faster if we special case for n = 1?
    new_position <- position + n
    buffer[(position + 1):new_position] <<- enc
    position <<- new_position
  }

  # The actual object returned
  list(
    # Write content. Updates the marker and stops suppressing whitespace writes.
    #
    # @param text Single element character vector
    write = function(text) {
      writeImpl(text)

      suppressing <<- FALSE
      marked <<- position
    },
    # Write whitespace. If eatWS() was called and its effect has not been
    # canceled, then this method no-ops.
    # @param text Single element character vector containing only
    #   whitespace characters
    writeWS = function(text) {
      if (suppressing) {
        return()
      }
      writeImpl(text)
    },
    # Return the contents of the TextWriter, as a single element character
    # vector, from the beginning to the current writing position (normally this
    # is the end of the last write or writeWS, unless eatWS() was called).
    readAll = function() {
      # Collapse everything in the buffer up to `position`
      paste(buffer[seq_len(position)], collapse="")
    },
    # Removes both recent and upcoming whitespace writes
    eatWS = function() {
      # Reset back to the most recent marker
      position <<- marked
      suppressing <<- TRUE
    }
  )
}

# Given a vector/list, return TRUE if any elements are named, FALSE otherwise.
anyNamed <- function(x) {
  # Zero-length vector
  if (length(x) == 0) return(FALSE)

  nms <- names(x)

  # List with no name attribute
  if (is.null(nms)) return(FALSE)

  # List with name attribute; check for any ""
  any(nzchar(nms))
}

# Given a vector/list, return TRUE if any elements are unnamed, FALSE otherwise.
anyUnnamed <- function(x) {
  # Zero-length vector
  if (length(x) == 0) return(FALSE)

  nms <- names(x)

  # List with no name attribute
  if (is.null(nms)) return(TRUE)

  # List with name attribute; check for any ""
  any(!nzchar(nms))
}

# Get source filename(s) out of a script, stylesheet, or attachment entry of an
# htmlDependency object. The spec is here:
# https://github.com/rstudio/shiny/blob/474f1400/srcts/src/shiny/render.ts#L79-L115
# This returns a character vector of filenames.
#  `attr` should be "src" for script, and "href" for stylesheet and attachment
find_dep_filenames <- function(x, attr = "src") {
  # In the case below, the structure is "abc" or c("abc", "xyz")
  if (is.character(x)) return(x)

  if (is.list(x)) {
    # In the case below, the structure is list(src="abc")
    if (!is.null(x[[attr]])) return(x[[attr]])

    # If we get here, the structure is list(list(src="abc"), list(src="xyz")).
    return(unlist(lapply(x, find_dep_filenames)))
  }

  # If we get here, we didn't find anything.
  character(0)
}
