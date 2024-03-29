% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tags.R
\name{tagAppendAttributes}
\alias{tagAppendAttributes}
\alias{tagHasAttribute}
\alias{tagGetAttribute}
\title{Append tag attributes}
\usage{
tagAppendAttributes(tag, ..., .cssSelector = NULL)

tagHasAttribute(tag, attr)

tagGetAttribute(tag, attr)
}
\arguments{
\item{tag}{a \link{tag} object.}

\item{...}{Attributes to append as named argument-value pairs. A named
argument with an \code{NA} value is rendered as a boolean attribute (see
example).}

\item{.cssSelector}{A character string containing a \href{https://developer.mozilla.org/en-US/docs/Learn/CSS/Building_blocks/Selectors}{CSS selector}
for targeting particular (inner) tags of interest. At the moment, only a
combination of
\href{https://www.w3.org/TR/CSS22/selector.html#type-selectors}{type} (e.g,
\code{div}), \href{https://www.w3.org/TR/CSS22/selector.html#class-html}{class}
(e.g., \code{.my-class}),
\href{https://www.w3.org/TR/CSS22/selector.html#id-selectors}{id} (e.g.,
\verb{#myID}), and
\href{https://www.w3.org/TR/CSS22/selector.html#universal-selector}{universal}
(\code{*}) selectors within a given \href{https://www.w3.org/TR/CSS22/selector.html#selector-syntax}{simple selector} is
supported. Note, if \code{.cssSelector} is used, the returned tags will have
their \verb{$children} fields flattened to a single \code{list()} via \code{\link[=tagQuery]{tagQuery()}}.}

\item{attr}{The name of an attribute.}
}
\description{
Append (\code{tagAppendAttributes()}), check existence (\code{tagHasAttribute()}),
and obtain the value (\code{tagGetAttribute()}) of HTML attribute(s).
}
\examples{
html <- div(a())
tagAppendAttributes(html, class = "foo")
tagAppendAttributes(html, .cssSelector = "a", class = "bar")
tagAppendAttributes(html, contenteditable = NA)

tagHasAttribute(div(foo = "bar"), "foo")
tagGetAttribute(div(foo = "bar"), "foo")

}
\seealso{
\code{\link[=tagAppendChildren]{tagAppendChildren()}}, \code{\link[=tagQuery]{tagQuery()}}
}
