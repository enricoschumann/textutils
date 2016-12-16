\name{strexp}
\alias{strexp}
\title{
  Expand String to Fixed Width
}
\description{  

  Expand strings to a fixed \sQuote{length} (in the sense of
  \code{\link{nchar}}).

}
\usage{
strexp(s, after, width, fill = " ", at)
}
\arguments{
  \item{s}{a character vector}
  \item{after}{
    character: a pattern, to be passed to \code{\link{regexpr}}
  }
  \item{width}{integer}
  \item{fill}{character}
  \item{at}{integer}
}
\details{

  \code{strexp} inserts blanks into the elements of a character vector
  such that all elements have the same \code{width} (i.e.
  \code{\link{nchar}}).  Note that it will (currently) not contract a
  string, only expand it.
}
\value{
  a character vector
}
\seealso{
   \code{\link{strwrap}}, \code{\link{format}}
}
\author{
  Enrico Schumann
}
\examples{
## expand to width 10, but keep two initial blanks 
s <- c("  A 1", "  B     2")
strexp(s, after = " +[^ ]+ +", width = 10)
}
