# render hooks can be used to 

    Code
      bar_widget
    Output
      <div></div>

---

    Code
      renderTags(html)
    Output
      $head
      
      
      $singletons
      character(0)
      
      $dependencies
      $dependencies[[1]]
      List of 10
       $ name      : chr "bar"
       $ version   : chr "1.0"
       $ src       :List of 1
        ..$ file: chr ""
       $ meta      : NULL
       $ script    : NULL
       $ stylesheet: NULL
       $ head      : NULL
       $ attachment: NULL
       $ package   : NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      
      $html
      <div class="bar"></div>
      

