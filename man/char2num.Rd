\name{char2num}
\alias{char2num}
\title{
  Convert Strings to Numbers
}
\description{  
  Convert strings to numbers.
}
\usage{
char2num(s, dec = ",", big.mark = ".")
}
\arguments{
  \item{s}{a character vector}
  \item{dec}{
    character: the decimal point; will be replaced with \dQuote{\code{.}} 
  }
  \item{big.mark}{
    character: will be removed, see Details.
  }
}
\details{

  \code{char2num} uses \code{\link{as.numeric}} on a character vector
  after removing/replacing certain characters: all occurences of
  \code{big.mark} are removed, and \code{dec} is changed to a dot.

}
\value{
  a character vector
}
\seealso{
   \code{\link{as.numeric}}, \code{\link{type.convert}}
}
\author{
  Enrico Schumann
}
\examples{
char2num("12.000,23")
char2num("12,000.23", big.mark = ",")
char2num("12000.23",  big.mark = " ") ## but 'as.numeric' is simpler
char2num("12'000.23", big.mark = "'")
char2num("12 000|23", dec = "|", big.mark = " ")
}
