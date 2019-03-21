\name{HTMLencode}
\alias{HTMLdecode}
\alias{HTMLencode}
\title{
  Decode and Encode HTML Entities
}
\description{
  Decode and encode HTML entities.
}
\usage{
HTMLdecode(x)
HTMLencode(x)
}
\arguments{
  \item{x}{
   a string (character vector of length one)
}
}
\details{

  \code{HTMLdecode} replaces \acronym{HTML}5 named
  entities (seeReferences) with \acronym{UTF}-8.

  \code{HTMLencode} replaces \acronym{UTF}-8-encoded
  substrings with \acronym{HTML}5 named entities (if
  available). Note that a semicolon \sQuote{\code{;}}
  will not be replaced by the entity
  \sQuote{\code{&semi;}}.

}
\value{
character
}
\references{
  \url{https://www.w3.org/TR/html5/syntax.html#named-character-references}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{TeXencode}}
}
\examples{
HTMLdecode(c("Max &amp; Moritz", "4 &lt; 9"))
## [1] "Max & Moritz" "4 < 9"

HTMLencode(c("Max & Moritz", "4 < 9"))
## [1] "Max &amp; Moritz" "4 &LT; 9"
}
