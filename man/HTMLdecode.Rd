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
HTMLencode(x, use.iconv = FALSE)
}
\arguments{
  \item{x}{
   a string (character vector of length one)
 }
  \item{use.iconv}{

    logical. Should conversion via \code{\link{iconv}}
    be tried from native encoding to \acronym{UTF}-8?

 }
}
\details{

  \code{HTMLdecode} replaces named entities (as defined
  by \acronym{HTML}5; see References) with
  \acronym{UTF}-8.

  \code{HTMLencode} replaces \acronym{UTF}-8-encoded
  substrings with \acronym{HTML}5 named entities
  (a.k.a.  \dQuote{named character references}).  A
  semicolon \sQuote{\code{;}} will not be replaced by
  the entity \sQuote{\code{&semi;}}.  Other than that,
  however, \code{HTMLencode} is quite thorough in its
  job: it will replace all characters for which named
  entities exists, even \sQuote{\code{&comma;}} and or
  \sQuote{\code{&quest;}}.

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
