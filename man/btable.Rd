\name{btable}
\alias{btable}
\title{
  Barplot Table
}
\description{
  Create a LaTeX-table.
}
\usage{
btable(x, unit = "cm", before = "", after = "", raise = "0.2ex",
       height = "1ex", ...)
}
\arguments{
  \item{x}{
    numeric: the numbers for which the barplot is to be created
}
  \item{unit}{
    character: a valid TeX unit
}
  \item{before}{
    character
}
  \item{after}{
    character
}
  \item{raise}{
    character
}
  \item{height}{
    character
}
  \item{\dots}{
    more arguments
}
}
\details{
  Creates a barplot table.
}
\value{
  character
}
%% \references{
%% }
\author{
  Enrico Schumann
}
\seealso{

  \code{\link{toLatex}}, \code{\link[NMOF]{qTable}}, \code{\link{TeXunits}}

}
\examples{
## see vignette
}
