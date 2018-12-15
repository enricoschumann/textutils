\name{toLatex.data.frame}
\alias{toLatex.data.frame}
\title{
  Convert Data Frame to LaTeX
}
\description{

  Convert data frames to character vector in LaTeX markup.

}
\usage{
\method{toLatex}{data.frame}(object, row.names = FALSE,
        col.handlers = list(), class.handlers = list(),
        eol = "\\\\\\\\", ...)
}
\arguments{
  \item{object}{
    a \code{\link{data.frame}}
}
  \item{row.names}{
    include the row names as the first column
}
  \item{col.handlers}{
    a list of named functions
}
  \item{class.handlers}{
    a list of named functions
}
  \item{eol}{
    character: the line ending
}
  \item{\dots}{
    other arguments
}
}
\details{

  A method for \code{\link{toLatex}} that converts data
  frames into LaTeX markup. Any formatting to be
  applied must be specifed as a function and passed
  with \code{col.handlers} and \code{class.handlers}.

  \code{col.handlers} take precedent over
  \code{class.handlers}.  
}
\value{
  character
}
%% \references{
%%   %% ~put references to the literature/web site here ~
%% }
\author{
  Enrico Schumann 
}
%% \note{
%% %%  ~~further notes~~
%% }

%% %% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  \code{\link{toLatex}}
}
\examples{
df <- data.frame(letter = letters[1:5],
                 number = runif(5),
                 stringsAsFactors = FALSE)
toLatex(df,
        col.handlers = list(letter = toupper),
        class.handlers = list(numeric = function(x) format(x, digits = 4)),
        eol = "\\\\[1ex]")
cat(toLatex(df,
            col.handlers = list(letter = toupper),
            class.handlers = list(numeric = function(x) format(x, digits = 4)),
            eol = "\\\\[1ex]"), sep = "\n")
}
