#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import utils digest
#' @importFrom fastmap fastmap faststack
#' @importFrom rlang obj_address
#' @useDynLib htmltools, .registration = TRUE
## usethis namespace: end
NULL


# For usethis::use_release_issue()
release_bullets <- function() {
  c(
    "Update static imports: `staticimports::import()`"
  )
}

