\name{valign}
\alias{valign}
\title{
  Vertically Align Strings
}
\description{
  Vertically align character vectors.
}
\usage{
valign(s, align = "|", insert.at = "<>", replace = TRUE, fixed = TRUE)
}
\arguments{
  \item{s}{a character vector}
  \item{align}{a regular expression}
  \item{insert.at}{a regular expression}
  \item{replace}{logical}
  \item{fixed}{logical}
}
\details{

  The function expands the elements of a character vector in such a way
  that the elements are vertically aligned, which can be handy when
  generating reports. See Examples.

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
s <- c("Player 1 <>| 100",
       "another player <>| 999999")

cat(paste(s, collapse = "\n"))
## Player 1 <>| 100
## another player <>| 999999

cat(paste(valign(s), collapse = "\n"))
## Player 1        100
## another player  999999

}
