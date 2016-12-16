\name{HTMLdecode}
\alias{HTMLdecode}
\title{
  Decode HTML Entities
}
\description{
  Decode HTML entities.
}
\usage{
HTMLdecode(x)
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
HTMLdecode("Max &#38; Moritz")
HTMLdecode("4 &lt; 9")
}
