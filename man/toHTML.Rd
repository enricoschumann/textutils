\name{toHTML}
\alias{toHTML}
\alias{toHTML.data.frame}
\title{
  Convert R Objects to HTML
}
\description{
  Convert an R object to an HTML snippet.
}
\usage{
toHTML(x, ...)

\method{toHTML}{data.frame}(x, \dots,
                            row.names = FALSE,
                            col.names = TRUE,
                            class.handlers = list(),
                            col.handlers = list(),
                            replace.NA = NULL,
                            td.id = FALSE)
}
\arguments{
  \item{x}{
    an object
  }
  \item{\dots}{
    arguments passed to methods
  }
  \item{row.names}{%

    logical (whether to include row names or not) or string
    (the column name used for the row-names column)

  }
  \item{col.names}{%

    logical (whether to include column names or not) or
    character (specify column names)

  }
  \item{class.handlers}{
    a list of named functions. See Examples.
  }
  \item{col.handlers}{
    a list of named functions. See Examples.
  }
  \item{replace.NA}{%
    \code{NULL} (do nothing), or a string that replaces
    all NA values.  NA values are noted \emph{before}
    any handlers are called.
  }
  \item{td.id}{
    logical
  }
}
\details{

  There exists \code{toHTML} methods in several packages,
  e.g. in \pkg{tools} or \pkg{XML}. Package \pkg{R2HTML} has
  a \code{HTML} generic.

  The \sQuote{semantics} of this function may differ from
  other implementations: the function is expected to take an
  arbitrary \R object and return an \acronym{HTML} snippet
  that can be placed in reports, i.e. the function works in
  the same spirit as \code{\link{toLatex}}. By contrast, the
  purpose of \code{\link[tools]{toHTML}} in \pkg{tools} is to
  provide a whole \acronym{HTML} document.

  The \code{\link{data.frame}} method has two \code{handlers}
  arguments: these may store helper functions for formatting
  columns, either of a specific name (\code{col.handlers}) or
  of a specific class(\code{class.handlers}). The functions in
  \code{col.handlers} are applied first; and the affected
  columns are not touched by \code{class.handlers}.  See
  Examples.

  If \code{td.id} is \code{TRUE}, all data cells in the table
  (i.e. td elements) gain an \code{id}-attribute of the form
  \code{td_<row>_<col>}.

}
\value{
  a character vector
}
\author{
Enrico Schumann
}
\seealso{
  \code{\link{toLatex}}
}
\examples{
x <- data.frame(a = 1:3, b = rnorm(3))
cat(toHTML(x,
       col.handlers   = list(b = function(x) round(x, 1)),
       class.handlers = list(integer = function(x) 100*x)))

## [  pretty-printed...  ]
## <tr>   <th>a</th>   <th>b</th> </tr>
## <tr> <td>100</td><td>-2.3</td> </tr>
## <tr> <td>200</td><td>-0.1</td> </tr>
## <tr> <td>300</td><td>-2.8</td> </tr>

}
