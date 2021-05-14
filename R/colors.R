#' Parse CSS color strings
#'
#' Parses/normalizes CSS color strings, and returns them as strings in
#' `"#RRGGBB"` and/or `"#RRGGBBAA"` format. Understands hex colors in 3, 4, 6,
#' and 8 digit forms, `rgb()`/`rgba()`, `hsl()`/`hsla()`, and color keywords.
#'
#' Note that `parseCssColors` may return colors in `#RRGGBBAA` format. Such
#' values are not understood by Internet Explorer, and must be converted to
#' `rgba(red, green, blue, alpha)` format to be safe for the web.
#'
#' @param str CSS color strings
#' @param mustWork If true, invalid color strings will cause an error; if false,
#'   then the result will contain `NA` for invalid colors.
#' @return A vector of strings in `#RRGGBB` or `#RRGGBBAA` format (the latter is
#'   only used for colors whose alpha values are less than `FF`), or `NA` for
#'   invalid colors when `mustWork` is false. Such strings are suitable for
#'   use in plots, or parsing with [col2rgb()] (be sure to pass `alpha = TRUE`
#'   to prevent the alpha channel from being discarded).
#'
#' @examples
#' parseCssColors(c(
#'   "#0d6efd",
#'   "#DC35457F",
#'   "rgb(32,201,151)",
#'   "  rgba( 23 , 162 , 184 , 0.5 )  ",
#'   "hsl(261, 51%, 51%)",
#'   "cornflowerblue"
#' ))
#' @export
parseCssColors <- function(str, mustWork = TRUE) {
  # Logic below assumes a character string with non-missing values
  # Note that an empty string is not a valid color, so parsing fails
  # on NA input values, and thus, will be converted back to NA
  # when `mustWork = FALSE`
  isNA <- is.na(str)
  if (!(is.character(str) || all(isNA))) {
    stop("`str` must be a character vector (or NA).")
  }
  str[isNA] <- ""

  # Strip insignificant whitespace
  str <- color_strip_ws(str)

  strategies <- list(
    # #RRGGBBAA and #RRGGBB
    list(
      pattern = "^#([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})?$",
      decoders = list(
        decode_hex, # red
        decode_hex, # green
        decode_hex, # blue
        decode_optional(decode_hex, 0xFF) # alpha, optional
      ),
      encoder = encode_hex
    ),
    # #RGBA and #RGB
    list(
      pattern = "^#([[:xdigit:]])([[:xdigit:]])([[:xdigit:]])([[:xdigit:]])?$",
      decoders = list(
        decode_hex, # red
        decode_hex, # green
        decode_hex, # blue
        decode_optional(decode_hex, 0xFF) # alpha, optional
      ),
      encoder = encode_hex
    ),
    # rgb() and rgba()
    list(
      pattern = paste0("^rgba?\\(",
        "(", regex_float, "),",
        "(", regex_float, "),",
        "(", regex_float, ")",
        regex_non_capturing_group(",(", regex_float, ")"), "?",
        "\\)$"),
      decoders = list(
        decode_float_255, # red   [0-255]
        decode_float_255, # green [0-255]
        decode_float_255, # blue  [0-255]
        decode_optional(decode_float_1, 0xFF) # alpha [0-1], optional
      ),
      encoder = encode_hex
    ),
    # hsl() and hsla()
    list(
      pattern = paste0("^hsla?\\(",
        "(", regex_float, "),",
        "(", regex_float, ")%,",
        "(", regex_float, ")%",
        regex_non_capturing_group(",(", regex_float, ")"), "?",
        "\\)$"),
      decoders = list(
        decode_float_identity, # hue        [0-360]
        decode_float_identity, # saturation [0-255]
        decode_float_identity, # lightness  [0-255]
        decode_optional(decode_float_1, 0xFF) # alpha [0-1], optional
      ),
      encoder = encode_hsl
    ),
    # color keywords
    list(
      pattern = paste0("^", regex_color_keyword, "$"),
      decoders = list(decode_color_keyword),
      encoder = encode_hex
    )
  )

  success <- rep_len(FALSE, length(str))
  result <- rep_len(NA_character_, length(str))

  for (strat in strategies) {
    if (all(success)) {
      break
    }

    res <- match_and_decode(
      str[!success],
      strat$pattern,
      !!!strat$decoders
    )
    if (any(res$matching_rows)) {
      result[!success][res$matching_rows] <- strat$encoder(res$values)
      success[!success][res$matching_rows] <- TRUE
    }
  }

  if (mustWork && any(!success)) {
    stop(sprintf("CSS color value(s) could not be parsed: '%s'", paste0(str[!success], collapse = "', '")))
  }

  result
}

# Strips whitespace that isn't significant in the parsing of CSS colors.
color_strip_ws <- function(str) {
  str <- gsub("\\s+", " ", str)
  str <- gsub("^ | $", "", str)
  str <- gsub(" ?, ?", ",", str)
  str <- gsub("\\( ", "(", str)
  str <- gsub(" \\)", ")", str)
  str
}

#' Match and decode a string
#'
#' Given a vector of strings, applies a regex that contains one or more
#' capturing groups. Each group's matching substrings are then passed to a
#' decoder function, which is responsible for returning decoded values. (One
#' decoder function is provided per capturing roup.)
#'
#' After decoding, all of the decoded values are cbind-ed together. The caller
#' needs to know which elements in `str` actually matched; therefore, the actual
#' return value is a list with names `matching_rows` and `values` (the latter is
#' only present if one or more rows actually matched).
#'
#' @param pattern Regex that contains the same number of capturing groups as
#'   unnamed arguments in `...`. The capturing groups MUST be non-overlapping.
#' @param ... Functions for decoding each capturing group
#' @noRd
match_and_decode <- function(str, pattern, ...) {
  # Example:
  # str <- c("#123456", "#ABCDEF")
  # pattern <- "#([0-9A-F]{2})([0-9A-F]{2})([0-9A-F]{2})"
  # args <- list(decode_hex, decode_hex, decode_hex)

  args <- rlang::list2(...)
  stopifnot(all(vapply(args, is.function, logical(1))))

  m <- regexec(pattern, str, ignore.case = TRUE)
  matching_rows <- vapply(m, function(x) { x[1] >= 0 }, logical(1)) # Ex: c(T,T)
  matches <- regmatches(str[matching_rows], m[matching_rows])
  if (length(matches) == 0) {
    return(list(
      matching_rows = matching_rows
    ))
  }

  col_count <- length(matches[[1]])
  str_matrix <- matrix(unlist(matches), ncol = col_count, byrow = TRUE)
  # Drop the first column, which is the entire matched string; we only want the
  # capturing groups
  str_matrix <- str_matrix[,-1,drop=FALSE]
  # Number of function arguments should match number of regex's capturing groups
  stopifnot(length(args) == ncol(str_matrix))
  # Ex: str_matrix
  #      [,1] [,2] [,3]
  # [1,] "12" "34" "56"
  # [2,] "AB" "CD" "EF"
  vals <- lapply(seq_len(ncol(str_matrix)), function(i) {
    # Ex: decode_hex(c("12", "AB")) => c(18, 171)
    args[[i]](str_matrix[,i])
  })
  results <- do.call(cbind, vals)
  # Ex: results
  #      [,1] [,2] [,3]
  # [1,]   18   52   86
  # [2,]  171  205  239

  return(list(
    matching_rows = matching_rows,
    values = results
  ))
}

decode_optional <- function(func, default_value) {
  force(func)
  force(default_value)

  function(str) {
    result <- rep_len(default_value, length(str))
    has_value <- nzchar(str, keepNA = FALSE) & !is.na(str)
    result[has_value] <- func(str[has_value])
    result
  }
}

# Takes a vector of strings whose elements represent a SINGLE hex color channel
# (one or two hexadecimal digits) and return an integer in the range [0-255].
decode_hex <- function(str) {
  stopifnot(all(nchar(str) %in% c(1,2)))

  # Single hex digits get doubled up
  str <- ifelse(nchar(str) == 1, paste0(str, str), str)

  res <- strtoi(str, 16)
  stopifnot(!anyNA(res))
  res
}

# Convert strings of floating point numbers [0-255] to integer values in the
# same range. Valid values outside the range will be clamped. Invalid values
# will raise errors.
decode_float_255 <- function(str) {
  as.integer(pmax(0, pmin(255, round(decode_float_identity(str)))))
}

# Convert strings of floating point numbers [0-1] to integer values [0-255].
# Valid values outside the range will be clamped. Invalid values will raise
# errors.
decode_float_1 <- function(str) {
  as.integer(pmax(0, pmin(255, round(decode_float_identity(str) * 255))))
}

decode_float_identity <- function(str) {
  stopifnot(all(grepl(paste0("^", regex_float, "$"), str)))
  as.numeric(str)
}

encode_hex <- function(values) {
  if (length(values) == 0) {
    return(character(0))
  }

  if (!is.matrix(values)) {
    stop("encode_hex requires a matrix argument")
  }
  if (ncol(values) < 3) {
    stop("encode_hex called with too few columns")
  }
  if (ncol(values) > 4) {
    stop("encode_hex called with too many columns")
  }
  if (!is.numeric(values)) {
    stop("encode_hex requires numeric values")
  }

  if (!is.integer(values)) {
    values <- round(values)
  }

  if (any(values > 255) || any(values < 0)) {
    stop("encode_hex values out of bounds")
  }

  red <- values[,1]
  green <- values[,2]
  blue <- values[,3]
  alpha <- if (ncol(values) > 3) {
    values[,4]
  } else {
    0xFF
  }
  colors <- ifelse(alpha == 0xFF,
    sprintf("#%02X%02X%02X", red, green, blue),
    sprintf("#%02X%02X%02X%02X", red, green, blue, alpha)
  )
  colors
}

# Convert HTML color keywords (plus "transparent") to integer matrix with 3
# columns (r, g, b) and length(str) rows. Errors on invalid strings.
decode_color_keyword <- function(str) {
  color <- css_color_keywords[tolower(str)]
  if (anyNA(color)) {
    stop("Invalid color keyword(s)")
  }
  unname(t(grDevices::col2rgb(color, alpha = TRUE)))
}

encode_hsl <- function(values) {
  if (length(values) == 0) {
    return(character(0))
  }

  if (!is.matrix(values)) {
    stop("encode_hsl requires a matrix argument")
  }
  if (ncol(values) < 3) {
    stop("encode_hsl called with too few columns")
  }
  if (ncol(values) > 4) {
    stop("encode_hsl called with too many columns")
  }
  if (!is.numeric(values)) {
    stop("encode_hsl requires numeric values")
  }

  # https://www.w3.org/TR/css-color-3/#hsl-color

  H <- values[,1]
  S <- values[,2] / 100
  L <- values[,3] / 100
  alpha <- if (ncol(values) > 3) {
    values[,4]
  } else {
    0xFF
  }

  # Clamp
  H <- (((H %% 360) + 360) %% 360) / 360
  S <- pmax(0, pmin(1, S))
  L <- pmax(0, pmin(1, L))

  hue_to_rgb <- function(m1, m2, h) {
    h <- ifelse(h < 0, h + 1,
      ifelse(h > 1, h - 1,
        h))
    ifelse(h * 6 < 1, m1+(m2-m1)*h*6,
      ifelse(h * 2 < 1, m2,
        ifelse(h * 3 < 2, m1+(m2-m1)*(2/3-h)*6,
          m1)))
  }

  M2 <- ifelse(L <= 0.5,
    L * (S + 1),
    L + S - L * S
  )
  M1 <- L * 2 - M2

  rgb <- cbind(
    hue_to_rgb(M1, M2, H+1/3),
    hue_to_rgb(M1, M2, H    ),
    hue_to_rgb(M1, M2, H-1/3)
  ) * 255
  rgb <- cbind(rgb, alpha)

  encode_hex(rgb)
}

css_color_keywords <- c(
  "transparent" = "#00000000",
  "aliceblue" = "#F0F8FF", "antiquewhite" = "#FAEBD7", "aqua" = "#00FFFF", "aquamarine" = "#7FFFD4", "azure" = "#F0FFFF", "beige" = "#F5F5DC", "bisque" = "#FFE4C4", "black" = "#000000", "blanchedalmond" = "#FFEBCD", "blue" = "#0000FF", "blueviolet" = "#8A2BE2", "brown" = "#A52A2A", "burlywood" = "#DEB887", "cadetblue" = "#5F9EA0", "chartreuse" = "#7FFF00", "chocolate" = "#D2691E", "coral" = "#FF7F50", "cornflowerblue" = "#6495ED", "cornsilk" = "#FFF8DC", "crimson" = "#DC143C", "cyan" = "#00FFFF", "darkblue" = "#00008B", "darkcyan" = "#008B8B", "darkgoldenrod" = "#B8860B", "darkgray" = "#A9A9A9", "darkgreen" = "#006400", "darkgrey" = "#A9A9A9", "darkkhaki" = "#BDB76B", "darkmagenta" = "#8B008B", "darkolivegreen" = "#556B2F", "darkorange" = "#FF8C00", "darkorchid" = "#9932CC", "darkred" = "#8B0000", "darksalmon" = "#E9967A", "darkseagreen" = "#8FBC8F", "darkslateblue" = "#483D8B", "darkslategray" = "#2F4F4F", "darkslategrey" = "#2F4F4F", "darkturquoise" = "#00CED1", "darkviolet" = "#9400D3", "deeppink" = "#FF1493", "deepskyblue" = "#00BFFF", "dimgray" = "#696969", "dimgrey" = "#696969", "dodgerblue" = "#1E90FF", "firebrick" = "#B22222", "floralwhite" = "#FFFAF0", "forestgreen" = "#228B22", "fuchsia" = "#FF00FF", "gainsboro" = "#DCDCDC", "ghostwhite" = "#F8F8FF", "gold" = "#FFD700", "goldenrod" = "#DAA520", "gray" = "#808080", "green" = "#008000", "greenyellow" = "#ADFF2F", "grey" = "#808080", "honeydew" = "#F0FFF0", "hotpink" = "#FF69B4", "indianred" = "#CD5C5C", "indigo" = "#4B0082", "ivory" = "#FFFFF0", "khaki" = "#F0E68C", "lavender" = "#E6E6FA", "lavenderblush" = "#FFF0F5", "lawngreen" = "#7CFC00", "lemonchiffon" = "#FFFACD", "lightblue" = "#ADD8E6", "lightcoral" = "#F08080", "lightcyan" = "#E0FFFF", "lightgoldenrodyellow" = "#FAFAD2", "lightgray" = "#D3D3D3", "lightgreen" = "#90EE90", "lightgrey" = "#D3D3D3", "lightpink" = "#FFB6C1", "lightsalmon" = "#FFA07A", "lightseagreen" = "#20B2AA", "lightskyblue" = "#87CEFA", "lightslategray" = "#778899", "lightslategrey" = "#778899", "lightsteelblue" = "#B0C4DE", "lightyellow" = "#FFFFE0", "lime" = "#00FF00", "limegreen" = "#32CD32", "linen" = "#FAF0E6", "magenta" = "#FF00FF", "maroon" = "#800000", "mediumaquamarine" = "#66CDAA", "mediumblue" = "#0000CD", "mediumorchid" = "#BA55D3", "mediumpurple" = "#9370DB", "mediumseagreen" = "#3CB371", "mediumslateblue" = "#7B68EE", "mediumspringgreen" = "#00FA9A", "mediumturquoise" = "#48D1CC", "mediumvioletred" = "#C71585", "midnightblue" = "#191970", "mintcream" = "#F5FFFA", "mistyrose" = "#FFE4E1", "moccasin" = "#FFE4B5", "navajowhite" = "#FFDEAD", "navy" = "#000080", "oldlace" = "#FDF5E6", "olive" = "#808000", "olivedrab" = "#6B8E23", "orange" = "#FFA500", "orangered" = "#FF4500", "orchid" = "#DA70D6", "palegoldenrod" = "#EEE8AA", "palegreen" = "#98FB98", "paleturquoise" = "#AFEEEE", "palevioletred" = "#DB7093", "papayawhip" = "#FFEFD5", "peachpuff" = "#FFDAB9", "peru" = "#CD853F", "pink" = "#FFC0CB", "plum" = "#DDA0DD", "powderblue" = "#B0E0E6", "purple" = "#800080", "rebeccapurple" = "#663399", "red" = "#FF0000", "rosybrown" = "#BC8F8F", "royalblue" = "#4169E1", "saddlebrown" = "#8B4513", "salmon" = "#FA8072", "sandybrown" = "#F4A460", "seagreen" = "#2E8B57", "seashell" = "#FFF5EE", "sienna" = "#A0522D", "silver" = "#C0C0C0", "skyblue" = "#87CEEB", "slateblue" = "#6A5ACD", "slategray" = "#708090", "slategrey" = "#708090", "snow" = "#FFFAFA", "springgreen" = "#00FF7F", "steelblue" = "#4682B4", "tan" = "#D2B48C", "teal" = "#008080", "thistle" = "#D8BFD8", "tomato" = "#FF6347", "turquoise" = "#40E0D0", "violet" = "#EE82EE", "wheat" = "#F5DEB3", "white" = "#FFFFFF", "whitesmoke" = "#F5F5F5", "yellow" = "#FFFF00", "yellowgreen" = "#9ACD32"
)

regex_non_capturing_group <- function(...) { paste0("(?:", ..., ")")}
regex_float <- "[-+]?[0-9]*\\.?[0-9]+"
regex_color_keyword <- paste0("(", paste0(names(css_color_keywords), collapse = "|"), ")")
