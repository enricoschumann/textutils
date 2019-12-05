\name{here}
\alias{here}
\title{
  Here Documents
}
\description{
  Read lines and convert into appropriate vector or data frame.
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
    logical: trim whitespace?
}
  \item{\dots}{
    named arguments to be passed to \code{\link[utils]{read.table}}
}
}
\details{

  Experimental. (Notably, the function's name may change.)

  The function reads a (typically multi-line) string and treats each
  line as one element of a vector or, if sep is specified, a
  \code{data.frame}.

  If \code{sep} is not specified, \code{here} calls
  \code{\link[utils]{type.convert}} on the input \code{s}. 

  If \code{sep} is specified, the input \code{s} is fed to
  \code{\link[utils]{read.table}}. Additional arguments may be passed
  through \dots.
  
}
\value{
  a vector or, if \code{sep} is specified, a \code{\link{data.frame}}
}
\references{

  \url{http://rosettacode.org/wiki/Here_document}

  (note that \R supports multi-line strings, so in a way it has
  built-in support for here documents as defined on
  that website)
  
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

## data frame
here("
letter, number
     x,      1
     y,      2
     z,      3",
sep = ",")
}
