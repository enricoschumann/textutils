\name{fill_in}
\alias{fill_in}
\title{
  Fill In Templates
}
\description{

  Light-weight template filling: replace 
  placeholders in a string by values.

}
\usage{
fill_in(s, ..., delim = c("{", "}"))
}
\arguments{
  \item{s}{
    character
}
  \item{\dots}{
    typically name/value pairs. See Examples.
}
  \item{delim}{
    characters
}
}
\details{

  A light-weight replacement function.


}
\value{
  character
}
\author{
  Enrico Schumann
}
%% \seealso{
%% }
\examples{
template <- "{1} meets {2}"
fill_in(template, "Peter", "Paul")  ## "Peter meets Paul"

template <- "{one} meets {other}"
fill_in(template, one = "Peter", other = "Paul")  ## "Peter meets Paul"
}
