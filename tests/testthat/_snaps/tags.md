# Adding unnamed attributes creates a warning

    Code
      expect_warning(print(tagAppendAttributes(tags$div(), "value")), "include an attribute name")
    Output
      <div></div>

