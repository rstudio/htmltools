context("whitespace")

with(tags, {
  test_that("Whitespace directives basic tests", {
    # Default
    expect_identical(
      as.character(
        div(
          span(
            strong()
          )
        )
      ),
      paste(collapse = "\n", c(
        "<div>",
        "  <span>",
        "    <strong></strong>",
        "  </span>",
        "</div>"
      ))
    )

    expect_identical(
      as.character(
        div(
          span(.noWS = "before",
            strong()
          )
        )
      ),
      paste(collapse = "\n", c(
        "<div><span>",
        "    <strong></strong>",
        "  </span>",
        "</div>"
      ))
    )

    expect_identical(
      as.character(
        div(
          span(.noWS = "after",
            strong()
          )
        )
      ),
      paste(collapse = "\n", c(
        "<div>",
        "  <span>",
        "    <strong></strong>",
        "  </span></div>"
      ))
    )

    expect_identical(
      as.character(
        div(
          span(.noWS =c("before", "after"),
            strong()
          )
        )
      ),
      paste(collapse = "\n", c(
        "<div><span>",
        "    <strong></strong>",
        "  </span></div>"
      ))
    )

    expect_identical(
      as.character(
        div(
          span(.noWS = c("after-begin", "before-end"),
            strong()
          )
        )
      ),
      paste(collapse = "\n", c(
        "<div>",
        "  <span><strong></strong></span>",
        "</div>"
      ))
    )

    expect_identical(
      as.character(
        div(.noWS = c("after-begin", "before-end"),
          span(.noWS = "before",
            strong()
          )
        )
      ),
      paste(collapse = "\n", c(
        "<div><span>",
        "    <strong></strong>",
        "  </span></div>"
      ))
    )

    expect_identical(
      as.character(
        div(
          HTML("one", .noWS = "before"),
          HTML("two")
        )
      ),
      paste(collapse = "\n", c(
        "<div>one",
        "  two",
        "</div>"
      ))
    )

    expect_identical(
      as.character(
        div(
          HTML("one", .noWS = c("before", "after")),
          HTML("two")
        )
      ),
      paste(collapse = "\n", c(
        "<div>onetwo",
        "</div>"
      ))
    )

    expect_identical(
      as.character(
        div(.noWS = c("after-begin", "before-end"),
          HTML("one"),
          HTML("two")
        )
      ),
      paste(collapse = "\n", c(
        "<div>one",
        "  two</div>"
      ))
    )

    expect_identical(
      as.character(
        div(
          HTML("one", .noWS = "outside"),
          HTML("two", .noWS = "outside"),
        )
      ),
      paste(collapse = "\n", c(
        "<div>onetwo</div>"
      ))
    )
  })
})
