is_locale_available <- function(loc){
  set_locale_failed <- FALSE
  tryCatch(
    Sys.setlocale("LC_ALL", loc),
    warning = function(e){ set_locale_failed <<- TRUE }
  )
  !set_locale_failed
}
