color_strip_ws <- function(str) {
  str <- gsub("\\s+", " ", str)
  str <- gsub("^ | $", "", str)
  str <- gsub(" ?, ?", ",", str)
  str <- gsub("\\( ", "(", str)
  str <- gsub(" \\)", ")", str)
  str
}

# Takes a vector of strings whose elements represent a SINGLE hex color channel
# (one or two hexadecimal digits) and return an integer in the range [0-255].
conv_hexstr_to_num255 <- function(str) {
  stopifnot(all(nchar(str) %in% c(1,2)))

  # Single hex digits get doubled up
  str <- ifelse(nchar(str) == 1, paste0(str, str), str)

  res <- strtoi(str, 16)
  stopifnot(!any(is.na(res)))
  res
}

# Convert strings of floating point numbers [0-255] to integer values in the
# same range. Valid values outside the range will be clamped. Invalid values
# will raise errors.
conv_decstr_to_num255 <- function(str) {
  stopifnot(all(grepl("^-?[0-9]+(\\.[0-9]+)?$", str) | grepl("^-?\\.[0-9]+$", str)))
  as.integer(pmax(0, pmin(255, round(as.numeric(str)))))
}

# Convert strings of floating point numbers [0-1] to integer values [0-255].
# Valid values outside the range will be clamped. Invalid values will raise
# errors.
conv_unitstr_to_num255 <- function(str) {
  stopifnot(all(grepl("^-?[0-9]+(\\.[0-9]+)?$", str) | grepl("^-?\\.[0-9]+$", str)))
  as.integer(pmax(0, pmin(255, round(as.numeric(str) * 255))))
}
