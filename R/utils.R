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
  # A string into which we can collapse entries in the buffer if the buffer gets full.
  accumulated <- ""

  # The buffer into which we enter all the writes.
  buffer <- character(bufferSize)

  # The index storing the position in the buffer of the most recent write.
  marked <- 0

  # The index storing the position in the buffer of the most recent write or writeWS.
  position <- 0

  # TRUE if we're eating whitespace right now, in which case calls to eatWS are no-ops.
  suppressing <- FALSE

  # Logic to do the actual write
  writeImpl <- function(els) {
    elLength <- length(els)

    # The text that is written to this writer will be converted to
    # UTF-8 using enc2utf8. The rendered output will always be UTF-8
    # encoded.
    # Also ensures that we were given a character vector
    # Force evaluation early on before we mutate our state.
    enc <- enc2utf8(els)

    # Are we at the end of our buffer?
    if (position+elLength >= length(buffer)) {

      # If our `marked` position has advanced well into the buffer,
      # we can go ahead and collapse the string up to the marked position
      # to free up room in the buffer.
      if (marked >= length(buffer)/2) {
        # Collapse the writes in the buffer up to the marked position
        str <- paste(buffer[seq_len(marked)], collapse="")
        accumulated <<- paste0(accumulated, str)

        # Move the remainder of the buffer up to the front and update the markers
        remaining <- position - marked
        if (remaining > 0){
          buffer[seq_len(remaining)] <<-
            buffer[seq(from=marked+1,to=marked+remaining)]
        }
        position <<- position - marked
        marked <<- 0
      }

      # We may still need to increase the buffer size
      while (position+elLength >= length(buffer)){
        # The writes in the buffer are still eligible to get eaten, so we can't
        # collapse them. Instead, we'll have to grow the buffer
        buffer[length(buffer)*2] <<- NA_character_
      }
    }

    # Move the position pointer and store the (encoded) write
    buffer[seq(from=position+1, length.out=elLength)] <<- enc
    position <<- position + elLength
  }

  # The actual writer object returned
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
      # Collapse everything in the buffer
      s <- paste(buffer[seq_len(position)], collapse="")

      # Combine with anything we'd already accumulated
      paste0(accumulated, s)
    },
    # Removes both recent and upcoming whitespace writes
    eatWS = function() {
      # Reset back to the most recent marker
      position <<- marked
      suppressing <<- TRUE
    }
  )
}
