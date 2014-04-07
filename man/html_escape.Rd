\name{html_escape}
\alias{html_escape}
\title{Escape HTML entities}
\usage{
html_escape(text, attribute = FALSE)
}
\arguments{
  \item{text}{Text to escape}

  \item{attribute}{Escape for use as an attribute value}
}
\value{
Character vector with escaped text.
}
\description{
Escape HTML entities contained in a character vector so
that it can be safely included as text or an attribute
value within an HTML document
}

