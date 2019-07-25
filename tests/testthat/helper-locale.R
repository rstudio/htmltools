is_locale_available <- function(loc){
  set_locale_failed <- FALSE
  tryCatch(
    withr::with_locale(c(LC_COLLATE=loc), {}),
    warning = function(e){ set_locale_failed <<- TRUE }
  )
  !set_locale_failed
}
