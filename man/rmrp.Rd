\name{rmrp}
\alias{rmrp}
\title{
  Remove Repeated Pattern
}
\description{
  Remove a repeated pattern in a character vector.
}
\usage{
rmrp(s, pattern, \dots)
}
\arguments{
  \item{s}{a character vector}
  \item{pattern}{a regular expression}
  \item{\dots}{arguments passed to \code{\link{grep}}}
}
\details{

  \code{rmrp} removes a repeated pattern in a character vector (e.g.
  repeated blank lines).

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
## remove repeated blanks from vector
s <- c("* Header", "", "  ","", "** Subheader")
rmrp(s, "^ *$")
}
