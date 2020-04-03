## Source with `source("./scripts/svg_tags.R", echo = TRUE, prompt.echo = ">")`
library(magrittr)


get_tags <- function(url, css) {
  items <-
    url %>%
    httr::GET() %>%
    httr::content() %>%
    rvest::html_nodes(css) %>%
    rvest::html_text()
  has_brackets <- grepl("^<", items) & grepl(">$", items)
  items <- items[has_brackets]
  has_bang <- grepl("!", items, fixed = TRUE)
  items <- items[!has_bang]
  items <- items %>%
    sub("^<", "", .) %>%
    sub(">$", "", .)
  item_has_h1 <- grepl("h1", items)
  if (any(item_has_h1)) {
    items <- c(items[-1 * which(grepl("h1", items))], paste0("h", 1:6))
  }
  items %>%
    sort() %>%
    unique() %>%
    print()
}

## W3 Schools
## Mozilla seemed to have a more up to date set of what is possible
# w3html_tags <- get_tags("https://www.w3schools.com/tags/default.asp", "#htmltags tr td:first-child a:not(.notsupported)")
## Had extra tags not seen in other places `altGlyph`
# w3svg_tags <- get_tags("https://www.w3schools.com/graphics/svg_reference.asp", "#main td:first-child")

## W3 Standard
# # The original spec websites made it very hard to determine what was obsolete / shouldn't be used and what was to be used
# html_tags <- get_tags("https://www.w3.org/TR/2018/WD-html53-20181018/single-page.html", "dfn[data-dfn-type='element']")
# svg_tags <- get_tags("https://svgwg.org/svg2-draft/single-page.html", "dfn[data-dfn-type='element']")


## Mozilla
# do not include the last section of obsolete tags
html_tags <- get_tags("https://developer.mozilla.org/en-US/docs/Web/HTML/Element", "article table:not(:last-child) td:first-child code")
# html_tags_obsolete <- get_tags("https://developer.mozilla.org/en-US/docs/Web/HTML/Element", "#content table:last-child td:first-child a")

# do not include tags that do not contain documentation articles
svg_tags <- get_tags("https://developer.mozilla.org/en-US/docs/Web/SVG/Element", "article a:not([rel='nofollow']) code")


# Both SVG2 and HTML5
svg_tags[svg_tags %in% html_tags]

new_tags <- c(svg_tags, html_tags) %>% unique() %>% sort()
old_tags <- names(htmltools::tags)

# tags which should not HTML5 / SVG2 supported
setdiff(old_tags, new_tags)
#> "command"     "eventsource" "keygen"


# New HTML5 tags
setdiff(html_tags, old_tags)
#> "rb"   "rtc"  "slot"
# New SVG2 tags
setdiff(svg_tags, old_tags)
### ...basically all svg tags

save_tags <- c(new_tags, old_tags) %>% unique() %>% sort()

save_line <- paste0(
  format(paste0("  \"", save_tags, "\"", ifelse(seq_along(save_tags) == length(save_tags), "", ",")), justify = "left"), "#",
  ifelse(save_tags %in% html_tags, " html", "     "),
  ifelse(save_tags %in% svg_tags, " svg", "")
) %>%
  sub("\\s+$", "", .)
cat(
  "known_tags <- c(",
    paste0(save_line, collapse = "\n"),
  ")",
  sep = "\n"
)

## (Save to middle of `./R/tags.R`)
