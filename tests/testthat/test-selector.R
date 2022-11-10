
makeTags <- function(text1, text2) {
  inputTags <- div(
    class = "outer",
    tagList(
      div(
        class = "inner",
        a(href="example.com", "`a` ", list(strong(text1), em(p(text2))), " text."),
      ),
      span(
        class = "sibling",
        "sibling text"
      )
    )
  )

  inputTags <- span(list(inputTags, inputTags))
  inputTags
}

test_that("error checks", {
  expect_error(asSelector("div, span"), "contain `,`")
  expect_error(asSelector("div[foo]"), "contain `[`", fixed = TRUE)
  expect_error(asSelector("div:text"), "Pseudo CSS selectors")
})


test_that("selector parses string", {
  selector <- asSelector("h1#myId-value.class-name .child-class#child-id_value")
  expect_equal(
    format(selector),
    "h1#myId-value.class-name #child-id_value.child-class"
  )
})

test_that("selector parses valid names", {
  verbose <- FALSE
  wrapped_expect_equal <- function (object, expected, label) {
    if (verbose) message(label)
    expect_equal(object, expected, label = label)
  }
  valid_names <- list(
    single_character               = "a",
    single_CHARACTER               = "A",
    multiple_characters            = "abc",
    multiple_cHaRaCtErS            = "aBcD",
    just_an_underscore             = "_",
    starts_with_an_underscore      = "_abc",
    contains_underscore            = "a_abc",
    contains_hyphen_and_underscore = "a_abc-def"
  )

  # check elements parse
  for (key in names(valid_names)) {
    this_time <- valid_names[[key]]
    wrapped_expect_equal(
      format(asSelector(this_time)),
      this_time,
      label = paste0("checking element ", key, " with content ", this_time)
    )
  }

  # check ids parse
  for (key in names(valid_names)) {
    this_time <- paste0("#", valid_names[[key]])
    wrapped_expect_equal(
      format(asSelector(this_time)),
      this_time,
      label = paste0("checking id ", key, " with content ", this_time)
    )
  }

  # check classes parse
  for (key in names(valid_names)) {
    this_time <- paste0(".", valid_names[[key]])
    wrapped_expect_equal(
      format(asSelector(this_time)),
      this_time,
      label = paste0("checking class name ", key, " with content ", this_time)
    )
  }

  # check compounds parse
  for (element_key in names(valid_names)) {
    for (id_key in names(valid_names)) {
      for (class_key_1 in names(valid_names)) {
        for (class_key_2 in names(valid_names)) {
          this_time <- paste0(
            valid_names[[element_key]],
            "#", valid_names[[id_key]],
            ".", valid_names[[class_key_1]],
            ".", valid_names[[class_key_2]]
          )
          wrapped_expect_equal(
            format(asSelector(this_time)),
            this_time,
            label = paste0("checking compound element ", paste(element_key, id_key, class_key_1, class_key_2, sep="|"), " with content ", this_time)
          )
          reordered_this_time <- paste0(
            valid_names[[element_key]],
            ".", valid_names[[class_key_1]],
            "#", valid_names[[id_key]],
            ".", valid_names[[class_key_2]]
          )
          wrapped_expect_equal(
            format(asSelector(reordered_this_time)),
            this_time,
            label = paste0("checking reordered compound element ", paste(element_key, id_key, class_key_1, class_key_2, sep="|"), " with content ", reordered_this_time)
          )
        }
      }
    }
  }
})

test_that("* checks", {
  expect_equal(format(asSelector(" * ")), "*")
  expect_equal(format(asSelector(" *.class-name")), ".class-name")
  expect_s3_class(asSelector(" * "), selectorClass)
})


test_that("> checks", {
  expect_error(asSelector("> div"), "first element")
  expect_error(asSelector("div >"), "last element")
  expect_error(asSelectorList("> div"), "first element")
  expect_error(asSelectorList("div >"), "last element")

  expect_equal(format(asSelector("div>span")), "div > span")
  expect_equal(format(asSelector("div>>span")), "div > * > span")
})





# x <- div(class = "foo", "text 1",
#   div(class = "bar", "text 2"),
#   div(class = "bar", "text 3",
#     span("more text")
#   )
# )
# y <- x
# x <- x[1:length(x)]
# mutate_in_place(x)
# x %>% find(".bar", function(item) { toupper(x$children); item })
# y <- x %>% el_find(".bar") %>% el_find(".x") %>% el_mutate(function(x) toupper(x$children))
# x %>% find(".bar") %>% addClass("abc")
# x %>% find(".bar") %>% removeClass("abc")
# x %>% find(".bar") %>% attr(`data-x` = 123)
# x %>% find(".bar") %>% css(color = "red")
# actionButton("x", "X") %>% removeClass("btn-default") %>% addClass("btn-primary")
# x <- actionButton("x", "X")
# x$attribs$class <- sub("btn-default", "btn-primary", x$attribs$class)
# removeClass.shiny.tag <- function(x, class) {
#   ...
# }
# x <- div(
#   actionButton("x", "X")
# )
# # Selection types:
# #  - tags
# #  - classes
# #  - ID
# #  - descendent   ".btn .x"
# # Future:
# #  - direct child ".btn > .x"
# #
# # Tag functions:
# #  - delay execution, wrap them somehow
# #  - need to know we have a way forward
# x %>% el_select(".btn") %>% el_remove_class("btn-default") %>% el_add_class("btn-primary")
# x %>% find(".btn") %>% { . %>% removeClass("btn") %>% addClass("btn-primary") }
# x$select(".btn")$removeClass("btn-default")$addClass("btn-primary")
# `$.shiny.tag` <- function(x, name, ...) {
#   el_fns[[name]](x, ...)
# }
