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
          span(.noWS = "before after",
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
          span(.noWS = "after-begin before-end",
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
        div(.noWS = "after-begin before-end",
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
  })
})
