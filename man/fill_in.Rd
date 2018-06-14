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
fill_in(s, ..., delim = c("{", "}"), replace.NA = TRUE)
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
  \item{replace.NA}{
    logical: if TRUE, \code{\link{NA}} values are replaced by
    the string \code{"NA"}.
    May also be a string. See Examples.
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


## handling missing values
fill_in("{name}: {score}", name = "Peter", score = NA)
## [1] "Peter: NA"

fill_in("{name}: {score}", name = "Peter", score = NA, replace.NA = ".")
## [1] "Peter: ."
}
