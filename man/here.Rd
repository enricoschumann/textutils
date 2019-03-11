\name{here}
\alias{here}
\title{
  Here Documents
}
\description{
  Read lines and convert into appropriate vector.
}
\usage{
here(s, drop = TRUE, guess.type = TRUE, sep = NULL, header = TRUE,
     stringsAsFactors = FALSE, trim = TRUE, \dots)
}
\arguments{
  \item{s}{
    a string
}
  \item{drop}{
    logical: drop empty first and last element
}
  \item{guess.type}{
    logical
}
  \item{sep}{
    NULL or character
}
  \item{header}{
    logical
}
  \item{stringsAsFactors}{
    logical
}
  \item{trim}{
    logical
}
  \item{\dots}{
    named arguments to be passed to \code{\link[utils]{read.table}}
}
}
\details{
  Experimental. (Notably, the function's name may change.)
}
\value{
  A vector.
}
\references{
  \url{http://rosettacode.org/wiki/Here_document}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{type.convert}}
}
\examples{
## numbers
here("
1
2
3
4
")

## character
here("
Al
Bob
Carl
David
")
}
