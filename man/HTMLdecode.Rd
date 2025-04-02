\name{HTMLencode}
\alias{HTMLdecode}
\alias{HTMLencode}
\alias{HTMLrm}
\title{
  Decode and Encode HTML Entities
}
\description{
  Decode and encode HTML entities.
}
\usage{
HTMLdecode(x, named = TRUE, hex = TRUE, decimal = TRUE)
HTMLencode(x, use.iconv = FALSE, encode.only = NULL)
HTMLrm(x, \dots, tag.replace = "")
}
\arguments{
  \item{x}{
    \code{HTMLdecode}, \code{HTMLencode}: a character vector of length one;
    for \code{HTMLrm}: a character vector
  }
  \item{use.iconv}{

    logical. Should conversion via \code{\link{iconv}}
    be tried from native encoding to \acronym{UTF}-8?

  }
  \item{named}{
    logical: replace named character references?
  }
  \item{hex}{
    logical: replace hexadecimal character references?
  }
  \item{decimal}{
    logical: replace decimal character references?
  }
  \item{encode.only}{
    character
  }
  \item{tag.replace}{
    string: replacementt for tags
  }
  \item{\dots}{
    other arguments
  }

}
\details{

  \code{HTMLdecode} replaces named, hexadecimal and decimal
  character references as defined by \acronym{HTML}5 (see
  References) with characters. The resulting character vector
  is marked as \acronym{UTF}-8 (see \code{\link{Encoding}}).

  \code{HTMLencode} replaces \acronym{UTF}-8-encoded
  substrings with \acronym{HTML}5 named entities (a.k.a.
  \dQuote{named character references}).  A semicolon
  \sQuote{\code{;}} will not be replaced by the entity
  \sQuote{\code{&semi;}}.  Other than that, however,
  \code{HTMLencode} is quite thorough in its job: it will
  replace all characters for which named entities exists, even
  \sQuote{\code{&comma;}} and or \sQuote{\code{&quest;}}.  You
  can restrict the characters to be replaced by specifying
  \code{encode.only}.

  \code{HTMLrm} removes \acronym{HTML} tags.  All content
  between \code{style} and \code{head} tags is removed, as are
  comments.  Note that each element of \code{x} is considered
  a single \acronym{HTML} document; so for multiline
  documents, paste/collapse the document.

}
\value{
character
}
\references{
  \url{https://www.w3.org/TR/html5/syntax.html#named-character-references}

  \url{https://html.spec.whatwg.org/multipage/syntax.html#character-references}
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

HTMLencode("Max, Moritz & more")
## [1] "Max&comma; Moritz &amp; more"
HTMLencode("Max, Moritz & more", encode.only = c("&", "<", ">"))
## [1] "Max, Moritz &amp; more"


HTMLrm("before <a href='https://enricoschumann.net'>LINK</a>  after")
## [1] "before https://enricoschumann.net  after"
}
