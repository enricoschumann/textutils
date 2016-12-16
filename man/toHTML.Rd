\name{toHTML}
\alias{toHTML}
\title{
  Convert R Objects to HTML
}
\description{
  Convert an R object to an HTML snippet.
}
\usage{
toHTML(x, ...)
}
\arguments{
  \item{x}{
    an object
}
  \item{\dots}{
    arguments passed to methods
}
}
\details{

  There exists \code{toHTML} methods in several packages, e.g. in
  \pkg{tools} or \pkg{XML}. Package \pkg{R2HTML} has a \code{HTML}
  generic.

  The \sQuote{semantics} of this function are that they take an
  arbitrary R object and return an \acronym{HTML} snippet that can be
  placed in reports (i.e. in the same spirit as
  \code{\link{toLatex}}). By contrast, the purpose of
  \code{\link[tools]{toHTML}} in \pkg{tools} is to provide a whole
  \acronym{HTML} document.
  
}
\value{
  A character vector.
}
\author{
Enrico Schumann
}
\seealso{
  \code{\link{toLatex}}
}
%% \examples{
%% ## see returns in package
%% }
