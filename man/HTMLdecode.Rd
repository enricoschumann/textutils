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
  Decode to UTF-8.
}
\value{
character
}
%% \references{
%% }
\author{
  Enrico Schumann
}
%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
HTMLdecode("Max &#38; Moritz")
}
