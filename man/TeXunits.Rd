\name{TeXunits}
\alias{TeXunits}
\title{
  Translate TeX Units of Measurement
}
\description{

  Translates units of measurement known to TeX and LaTeX.

}
\usage{
TeXunits(from, to, from.unit = NULL)
}
\arguments{
  \item{from}{

    Typically character, such as \code{"1in"}. When numeric,
    \code{from.unit} needs to be specified.

}
  \item{to}{
    character
}
  \item{from.unit}{
    character
}
}
\details{

  Available units are centimetre (\code{cm}), inch (\code{in}), point
  (\code{pt}), pica (\code{pc}), big point(\code{bp}), millimetre
  (\code{mm}), Didot points (\code{dd}) and Cicero (\code{cc}).

  See Chapter 10 of the TeXbook for details.

}
\value{
  numeric
}
\references{

  Donald E. Knuth. \emph{The TeXbook.}  Addison Wesley, 1986 (with
  corrections made in 1996).
}
\author{
  Enrico Schumann
}
%% \seealso{

%% }
\examples{
TeXunits("1in",
         c("in", "mm", "pt", "in"))
TeXunits(c("1in", "2in"),
         "cm")
}
