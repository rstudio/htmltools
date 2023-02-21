# Hanging commas don't break things

    Code
      (expect_error(as.character(div("one", , ))))
    Output
      <error/rlang_error>
      Error in `dots_list()`:
      ! Argument 2 can't be empty.
    Code
      (expect_error(as.character(div(, "one", ))))
    Output
      <error/rlang_error>
      Error in `dots_list()`:
      ! Argument 1 can't be empty.

# html render method

    Code
      as.character(obj)
    Output
      [1] "<span>example</span>"

---

    Code
      as.character(spanExtra)
    Output
      [1] "<span class=\"extra\">example</span>"

---

    Code
      as.character(divExtra)
    Output
      [1] "<div class=\"extra\">example</div>"

---

    Code
      as.character(spanExtended)
    Output
      [1] "<span>\n  example\n  <strong>bold text</strong>\n</span>"

---

    Code
      as.character(tagFuncExt)
    Output
      [1] "<span>example</span>\n<p>test</p>"

---

    Code
      renderTags(newDep)
    Output
      $head
      
      
      $singletons
      character(0)
      
      $dependencies
      $dependencies[[1]]
      List of 10
       $ name      : chr "jqueryui"
       $ version   : chr "1.11.4"
       $ src       :List of 1
        ..$ href: chr "shared/jqueryui"
       $ meta      : NULL
       $ script    : chr "jquery-ui.min.js"
       $ stylesheet: NULL
       $ head      : NULL
       $ attachment: NULL
       $ package   : NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      $dependencies[[2]]
      List of 10
       $ name      : chr "font-awesome"
       $ version   : chr "4.5.0"
       $ src       :List of 1
        ..$ href: chr "shared/font-awesome"
       $ meta      : NULL
       $ script    : NULL
       $ stylesheet: chr "css/font-awesome.min.css"
       $ head      : NULL
       $ attachment: NULL
       $ package   : NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      
      $html
      <span>example</span>
      

---

    Code
      as.character(newObj)
    Output
      [1] "<p>Something else</p>"

