\name{html_dependencies_as_character}
\alias{html_dependencies_as_character}
\title{Generate HTML for dependencies}
\usage{
html_dependencies_as_character(dependencies, lib_dir = NULL)
}
\arguments{
  \item{dependencies}{List of dependencies created using
  \code{\link{html_dependency}}.}

  \item{lib_dir}{Optional directory to copy the libraries
  into (generated HTML will include relative links to the
  dependencies which presume that the \code{lib_dir} exists
  in the same directory as the HTML document).}
}
\value{
Character vector with generated HTML for the dependencies
}
\description{
Given a list of HTML dependencies produce a character
representation suitable for inclusion within the head of an
HTML document.
}

