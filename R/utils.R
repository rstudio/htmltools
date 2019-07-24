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
#'   The buffer will be periodically cleared, if possible, to cache the writes
#'   as a string. If the buffer cannot be cleared (because of the need to be
#'   able to backtrack to fulfill an `eatWS()` call), then the buffer size will
#'   be doubled.
#' @noRd
WSTextWriter <- function(bufferSize=1024){
  if (bufferSize < 3){
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

  # Collapses the text in the buffer to create space for more writes. The first
  # element in the buffer will be the concatenation of any writes up to the
  # current marker. The second element in the buffer will be the concatenation
  # of all writes after the marker.
  collapseBuffer <- function(){
    # Collapse the writes in the buffer up to the marked position into the first buffer entry
    nonWS <- ""
    if (marked > 0){
      nonWS <- paste(buffer[seq_len(marked)], collapse="")
    }

    # Collapse any remaining whitespace
    ws <- ""
    remaining <- position - marked
    if (remaining > 0){
      # We have some whitespace to collapse. Collapse it into the second buffer entry.
      ws <- paste(buffer[seq(from=marked+1,to=marked+remaining)], collapse="")
    }

    buffer[1] <<- nonWS
    buffer[2] <<- ws
    position <<- 2
    marked <<- 1
  }

  # Logic to do the actual write
  writeImpl <- function(text) {
    # force `text` to evaluate and check that it's the right shape
    # TODO: We could support vectors with multiple elements here and perhaps
    #   find some way to combine with `paste8()`. See
    #   https://github.com/rstudio/htmltools/pull/132#discussion_r302280588
    if (length(text) != 1 || !is.character(text)){
      stop("Text to be written must be a length-one character vector")
    }

    # Are we at the end of our buffer?
    if (position == length(buffer)) {
      collapseBuffer()
    }

    # The text that is written to this writer will be converted to
    # UTF-8 using enc2utf8. The rendered output will always be UTF-8
    # encoded.
    enc <- enc2utf8(text)

    # Move the position pointer and store the (encoded) write
    position <<- position + 1
    buffer[position] <<- enc
  }

  # The actual object returned
  list(
    # Write content. Updates the marker and stops suppressing whitespace writes.
    #
    # @param text Single element character vector
    write = function(text){
      writeImpl(text)

      suppressing <<- FALSE
      marked <<- position
    },
    # Write whitespace. If eatWS() was called and its effect has not been
    # canceled, then this method no-ops.
    # @param text Single element character vector containing only
    #   whitespace characters
    writeWS = function(text) {
      if (suppressing){
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
