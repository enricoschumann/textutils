\name{title_case}
\alias{title_case}
\title{
  Remove Leading and Trailing White Space
}
\description{  
  Remove leading and/or trailing white space from character vectors.
}
\usage{
title_case(s, strict = FALSE, ignore = NULL)
}
\arguments{
  \item{s}{a character vector}
  \item{strict}{logical}
  \item{ignore}{character}
}
\details{

  Set string in title case.
  
}
\value{
  a character vector
}
\seealso{
  \code{\link{tolower}}, \code{\link{toupper}}
}
\author{
  Enrico Schumann
}
\examples{
title_case("Numeric methods")
}
