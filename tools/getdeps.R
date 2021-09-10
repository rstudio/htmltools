lib <- rprojroot::find_package_root_file("inst/lib")
unlink(lib, recursive = TRUE)

get_package_version <- function(pkg) {
  url <- paste0("https://unpkg.com/", pkg)
  url_long <- httr::GET(url)$url
  url_suffix <- substr(url_long, nchar(url) + 1, nchar(url_long))
  sub("^@", "", strsplit(url_suffix, "/")[[1]][1])
}

pkgs <- c("preact", "htm", "react", "react-dom", "@babel/standalone")
versions <- lapply(pkgs, get_package_version)
versions <- setNames(versions, pkgs)

download_to_lib <- function(pkg, files) {
  pkg_dir <- file.path(lib, pkg)
  dir.create(pkg_dir, recursive = TRUE)
  owd <- setwd(pkg_dir)
  on.exit(setwd(owd))
  urls <- file.path("https://unpkg.com", paste0(pkg, "@", versions[[pkg]]), files)
  invisible(lapply(urls, function(url) download.file(url, basename(url))))
}

# TODO: include esm and source maps (for umd)
download_to_lib("preact", file.path("dist", c("preact.min.js", "preact.min.js.map")))
download_to_lib("htm", "dist/htm.umd.js")
download_to_lib("react", "umd/react.production.min.js")
download_to_lib("react-dom", "umd/react-dom.production.min.js")
download_to_lib("@babel/standalone", "babel.min.js")


cat(
  paste("versions <-", paste0(capture.output(dput(versions)), collapse = "\n")),
  file = rprojroot::find_package_root_file("R/versions.R")
)
