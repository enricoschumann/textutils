# textutils

Utilities for handling character vectors that store human-readable
text (either plain or with markup, such as HTML).

[ [More] ](http://enricoschumann.net/R/packages/textutils/)

## Installing the package

The latest released version is available from
http://enricoschumann.net. In an R session, just type:

    install.packages('textutils', type = 'source',
                     repos = c('http://enricoschumann.net/R', getOption('repos')))

For the latest development version, check out the latest repository
and build it. In a shell (e.g. dash, bash):

    git clone https://github.com/enricoschumann/textutils.git
    R CMD build textutils
    R CMD INSTALL textutils_0.1-0.tar.gz  ## adapt version number

    ## optionally check
    R CMD check   textutils_0.1-0.tar.gz  ## adapt version number
    
