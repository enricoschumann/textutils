\name{insert}
\alias{insert}
\title{
  Vector Insertion
}
\description{
  Insert elements into a vector.
}
\usage{
insert(x, values, before.index)
}
\arguments{
  \item{x}{
    a vector
  }
  \item{values}{
    elements to insert
}
  \item{before.index}{
    numeric: before which positions of
    the original vector to insert the new elements
}
}
\details{

  Inserts elements into a vector.

}
\value{

  A vector with \code{values} inserted.  If either
  \code{values} or \code{before.index} are of length
  zero, the original vector is returned.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{append}}
}
\examples{
x <- letters[1:5]
## [1] "a" "b" "c" "d" "e"
insert(x, values = "Z", c(2, 5))
## [1] "a" "Z" "b" "c" "d" "Z" "e"
}
