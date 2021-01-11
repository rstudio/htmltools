# shinyMobile

<details>

* Version: 0.7.0
* GitHub: https://github.com/RinteRface/shinyMobile
* Source code: https://github.com/cran/shinyMobile
* Date/Publication: 2020-06-17 11:00:02 UTC
* Number of recursive dependencies: 78

Run `revdep_details(, "shinyMobile")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           █
        1. └─shinyMobile:::create_app_ui(...) test-preview_mobile.R:17:2
        2.   ├─shiny::fluidPage(...)
        3.   │ ├─shiny::bootstrapPage(...)
        4.   │ │ ├─htmltools::attachDependencies(...)
        5.   │ │ └─htmltools::tagList(...)
        6.   │ │   └─rlang::dots_list(...)
        7.   │ └─htmltools::div(class = "container-fluid", ...)
        8.   │   └─rlang::dots_list(...)
        9.   └─htmltools::attachDependencies(devices_css_deps, shiny::br())
       10.     └─htmltools:::asDependencies(value)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 71 ]
      Error: Test failures
      Execution halted
    ```

