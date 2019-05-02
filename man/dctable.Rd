\name{dctable}
\alias{dctable}
\title{
  Dotchart Table
}
\description{
  Create a LaTeX-table.
}
\usage{
dctable(x, unitlength = "1 cm", width = 5,
        y.offset = 0.07, circle.size = 0.1, xlim,
        na.rm = FALSE)
}
\arguments{
  \item{x}{
    numeric: the numbers for which the barplot is to be created
}
  \item{unitlength}{
    character
}
  \item{width}{
    numeric
}
  \item{y.offset}{
    numeric
}
  \item{circle.size}{
    numeric
}
  \item{xlim}{
    character
}
  \item{na.rm}{
    logical
}
}
\details{

  Creates a dotchart table.

  This function is currently very experimental.
  
}
\value{
  character
}
\references{

  Cleveland, W. S. (1985) \emph{The Elements of Graphing Data.} Wadsworth.

}
\author{
  Enrico Schumann
}
\seealso{

  \code{\link{toLatex}}, \code{\link[NMOF]{qTable}}, \code{\link{TeXunits}}

}
\examples{
## see vignette
}
