
#' Escape HTML entities
#'
#' Escape HTML entities contained in a character vector so that it can be safely
#' included as text or an attribute value within an HTML document
#'
#' @param text Text to escape
#' @param attribute Escape for use as an attribute value
#'
#' @return Character vector with escaped text.
#'
#' @export
htmlEscape <- local({

  .htmlSpecials <- list(
    `&` = '&amp;',
    `<` = '&lt;',
    `>` = '&gt;'
  )
  .htmlSpecialsPattern <- paste(names(.htmlSpecials), collapse='|')
  .htmlSpecialsAttrib <- c(
    .htmlSpecials,
    `'` = '&#39;',
    `"` = '&quot;',
    `\r` = '&#13;',
    `\n` = '&#10;'
  )
  .htmlSpecialsPatternAttrib <- paste(names(.htmlSpecialsAttrib), collapse='|')

  function(text, attribute=FALSE) {
    pattern <- if(attribute)
      .htmlSpecialsPatternAttrib
    else
      .htmlSpecialsPattern

    # Short circuit in the common case that there's nothing to escape
    if (!any(grepl(pattern, text, useBytes = TRUE)))
      return(text)

    specials <- if(attribute)
      .htmlSpecialsAttrib
    else
      .htmlSpecials

    for (chr in names(specials)) {
      text <- gsub(chr, specials[[chr]], text, fixed = TRUE, useBytes = TRUE)
    }

    return(text)
  }
})
