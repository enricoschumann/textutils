\name{TeXencode}
\alias{TeXencode}
\title{
  Encode Specical Characters for TeX/LaTeX
}
\description{

  Encode specical characters for TeX/LaTeX.

}
\usage{
TeXencode(s)
}
\arguments{
  \item{s}{
    character
  }
}
\details{

  Probably incomplet
  
}
\value{
  numeric
}
\references{

  Donald E. Knuth. \emph{The TeXbook.}
  Addison Wesley, 1986 (with corrections made in 1996).  

  Leslie Lamport. \emph{LaTeX: A Document Preparation System.}
  Addison Wesley, 1994.

}
\author{
  Enrico Schumann
}
%% \seealso{
  
%% }
\examples{
TeXencode("Peter & Paul")
## [1] "Peter \\& Paul"
}
