# textutils

Utilities for handling character vectors that store
human-readable text (either plain or with markup, such
as HTML or LaTeX). The package provides, in particular,
functions that help with the preparation of plain-text
reports (e.g. for expanding and aligning strings that
form the lines of such reports); the package also
provides generic functions for transforming R objects
to HTML and to plain text.

[ [More] ](http://enricoschumann.net/R/packages/textutils/)

## Installing the package

A stable version of the package is available from
[ CRAN ](https://CRAN.R-project.org/package=textutils).


The latest version is available from
http://enricoschumann.net. In an R session, just type:

    install.packages('textutils', type = 'source',
                     repos = c('http://enricoschumann.net/R', getOption('repos')))

For the latest development version, check out the Git
repository and build it. In a shell (e.g. `sh` or `bash`):

    ## first time: cd to _parent_ directory and ...
    $ git clone https://github.com/enricoschumann/textutils

    ## ... build and install the package
    $ R CMD build textutils
    $ R CMD INSTALL textutils_<0.2-0>.tar.gz  ## adjust version number

    ## optionally check
    $ R CMD check textutils_<0.2-0>.tar.gz    ## adjust version number


    ## update package: cd to _package_ directory and ...
    $ git pull
    $ cd ..
    $ R CMD build textutils  ## build/INSTALL/check as above
