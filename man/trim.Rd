\name{trim}
\alias{trim}
\alias{rmspace}
\title{
  Remove Leading and Trailing White Space
}
\description{  
  Remove leading and/or trailing white space from character vectors.
}
\usage{
trim(s, leading = TRUE, trailing = TRUE, perl = TRUE, \dots)
rmspace(s, leading = TRUE, trailing = TRUE)
}
\arguments{
  \item{s}{a character vector}
  \item{leading}{logical}
  \item{trailing}{logical}
  \item{perl}{logical}
  \item{\dots}{arguments passed to \code{\link{gsub}}}
}
\details{

  \code{trim} removes leading and trailing space; space is recognised
  through the (Perl) regular expression \code{\\s}.

  \code{rmspace} should not be used any more and will be removed in the
  future.
  
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
s <- c("\t 2 2\n \t", "   ab  ")
trim(s)
}
