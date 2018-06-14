\name{HTMLdecode}
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
    character
}
}
\details{
  Decodes to \acronym{UTF}-8.
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
%% \seealso{
%%   URL
%% }
\examples{
HTMLdecode(c("Max &#38; Moritz",
             "4 &lt; 9"))
}
