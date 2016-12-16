\name{toText}
\alias{toText}
\alias{toText.default}
\title{
  Convert Objects to (Plain) Text
}
\description{
  Converts an R object into a text representation.
}
\usage{
toText(x, ...)

\method{toText}{default}(x, \dots)

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
  
  A generic function. Method are expected to coerce a given object to
  lines of human-readable text that can be used, for instance, for
  reports. The purpose of \code{toText} is \bold{not} to store data in a
  form that can be read and understood by R; for that, see
  \code{\link{dput}} or \code{\link{dump}}.
  
  The \code{print} method is essentially equivalent to
    \code{cat(x, sep = "\n")} .
  
  There is no restriction on encoding, so plain text does not
  necessarily mean \acronym{ASCII}.  But current methods do not map into
  markup-representations.

}
\value{

  A character vector (lines of text), possibly with a class attribute
  \code{text}.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{toLatex}}, \code{\link{toHTML}}
}
\examples{
toText(c("a", "b", "c"))
cat(toHTML(toText(c("a", "b", "c"))))

}
