#' TextWriter class
#'
#' A class that manages the gradual concatenation of text. It
#' provides three additional features (over a normal connection)
#' that are important to us for tag writing:
#'
#' 1. Text is automatically converted to UTF-8.
#'
#' 2. Ability to bookmark a write position, and jump back to it.
#'    (Only one bookmark at a time is allowed.) We use this to
#'    speculatively write whitespace that the next tag may elect
#'    to strip; we set a bookmark between the end of significant
#'    content and trailing whitespace for that content.
#'
#' 3. Distinct operations for whitespace-only writes and regular
#'    writes (the former of which may be dropped based on ws
#'    directives, and the latter of which will never be dropped),
#'    plus a suppressWhitespace() method that eats writeWS calls
#'    until the next time non-whitespace is emitted. This is used
#'    to allow tags to strip all whitespace between themselves
#'    and the next piece of content.
#'
#' @noRd
#' @importFrom R6 R6Class
TextWriter <- R6Class("TextWriter",
  private = list(
    con = "ANY",
    marked = numeric(1),
    suppressingWS = logical(1),
    # The text that is written to this TextWriter will be converted to
    # UTF-8 using enc2utf8. The rendered output will always be UTF-8
    # encoded.
    #
    # We use a file() here instead of textConnection() or paste/c to
    # avoid the overhead of copying, which is huge for moderately
    # large numbers of calls to .$write(). Generally when you want
    # to incrementally build up a long string out of immutable ones,
    # you want to use a mutable/growable string buffer of some kind;
    # since R doesn't have something like that (that I know of),
    # file() is the next best thing.
    doWrite = function(text) {
      raw <- charToRaw(enc2utf8(text))
      # This is actually writing UTF-8 bytes, not chars
      writeBin(raw, private$con)
    }
  ),
  public = list(
    initialize = function() {
      private$con <- file("", "w+b", encoding = "UTF-8")
      private$marked <- NA_real_
      private$suppressingWS <- FALSE
    },
    close = function() {
      close(private$con)
    },
    # Write content; may or may not contain incidental whitespace,
    # but whitespace that needs to be suppressible (like newlines
    # and indents that the user did not explicitly request) should
    # use writeWS() instead.
    #
    # Calling write() causes suppressWhitespace() to be cancelled.
    #
    # @param text Single element character vector
    write = function(text) {
      private$suppressingWS <- FALSE
      private$doWrite(text)
    },
    # Write whitespace. If suppressWhitespace() was called and
    # its effect has not been canceled, then this method no-ops.
    # @param text Single element character vector containing only
    #   whitespace characters
    writeWS = function(text) {
      if (!private$suppressingWS) {
        private$doWrite(text)
      }
    },
    # Return the contents of the TextWriter, as a single element
    # character vector, from the beginning to the current writing
    # position (normally this is the end of the connection, unless
    # restorePosition() was called).
    readAll = function() {
      wpos <- seek(private$con, where = NA, rw = "write")
      seek(private$con, where = 0, origin = "start", rw = "read")
      s <- readChar(private$con, wpos, useBytes = TRUE)
      Encoding(s) <- "UTF-8"
      s
    },
    # Mark the current writing position
    savePosition = function() {
      # Save the current write pos
      private$marked <- seek(private$con, where = NA, rw = "write")
    },
    # Jump to the most recently marked position
    restorePosition = function() {
      if (!is.na(private$marked)) {
        seek(private$con, private$marked, origin = "start", rw = "write")
      }
    },
    # Call to prevent subsequent writeWS() from having any effect.
    # The suppression is cancelled the next time write() is called.
    suppressWhitespace = function() {
      private$suppressingWS <- TRUE
    }
  )
)
