# Implements "whitespace eating" functionality on top of a normal TextWriter.
# (WSTextWriter doesn't inherit from TextWriter, as it presents a different
# interface.)
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
WSTextWriter <- R6Class("WSTextWriter",
  private = list(
    writer = "TextWriter",
    suppressing = logical(1)
  ),
  public = list(
    initialize = function() {
      private$writer <- TextWriter$new()
      private$suppressing <- FALSE
    },
    close = function() {
      private$writer$close()
    },
    write = function(text) {
      private$writer$write(text)

      # Reset suppressing behavior
      private$suppressing <- FALSE

      # Set a bookmark here, so that future calls to eatWS() don't affect this
      # write
      private$writer$savePosition()
    },
    # Write whitespace. If suppressWhitespace() was called and
    # its effect has not been canceled, then this method no-ops.
    # @param text Single element character vector containing only
    #   whitespace characters
    writeWS = function(text) {
      if (private$suppressing){
        return()
      }
      # If an error is going to happen while evaluating `text`, let's make it
      # happen before we mutate any of our internal state
      force(text)
      private$writer$write(text)
    },
    readAll = function() {
      private$writer$readAll()
    },
    # Removes both recent and upcoming whitespace writes
    eatWS = function() {
      # Undo recent whitespace writes
      private$writer$restorePosition()
      # Ignore upcoming whitespace writes
      private$suppressing <- TRUE
    }
  )
)

#' TextWriter class
#'
#' A class that manages the gradual concatenation of text. It
#' provides two additional features (over a normal connection)
#' that are important to us for tag writing:
#'
#' 1. Text is automatically converted to UTF-8.
#'
#' 2. Ability to bookmark (save) a write position, and jump back
#'    to it (restore). Only one bookmark at a time is allowed.
#'    Restoring a bookmark essentially truncates content that
#'    was written after the bookmark was set.
#'
#' @noRd
#' @importFrom R6 R6Class
TextWriter <- R6Class("TextWriter",
  private = list(
    con = "ANY",
    marked = numeric(1),
    position = numeric(1)
  ),
  public = list(
    initialize = function() {
      # We use a file() here instead of textConnection() or paste/c to
      # avoid the overhead of copying, which is huge for moderately
      # large numbers of calls to .$write(). Generally when you want
      # to incrementally build up a long string out of immutable ones,
      # you want to use a mutable/growable string buffer of some kind;
      # since R doesn't have something like that (that I know of),
      # file() is the next best thing.
      private$con <- file("", "w+b", encoding = "UTF-8")
      private$marked <- 0
      # position could be obtained by calling `seek(where=NA)`, but it's too expensive
      private$position <- 0
    },
    close = function() {
      close(private$con)
    },
    # Write content
    #
    # @param text Single element character vector
    write = function(text) {
      # The text that is written to this TextWriter will be converted to
      # UTF-8 using enc2utf8. The rendered output will always be UTF-8
      # encoded.
      raw <- charToRaw(enc2utf8(text))

      # Update our current position
      private$position <- private$position + length(raw)

      # This is actually writing UTF-8 bytes, not chars
      writeBin(raw, private$con)
    },
    # Return the contents of the TextWriter, as a single element
    # character vector, from the beginning to the current writing
    # position (normally this is the end of the connection, unless
    # restorePosition() was called).
    readAll = function() {
      seek(private$con, where = 0, origin = "start", rw = "read")
      s <- readChar(private$con, private$position, useBytes = TRUE)
      Encoding(s) <- "UTF-8"
      s
    },
    # Mark the current writing position
    savePosition = function() {
      # Save the current write pos
      private$marked <- private$position
    },
    # Jump to the most recently marked position
    restorePosition = function() {
      private$position <- private$marked
      seek(private$con, private$marked, origin = "start", rw = "write")
    }
  )
)
