# textutils

Utilities for handling character vectors that store human-readable
text (either plain or with markup, such as HTML).

[ [More] ](http://enricoschumann.net/R/packages/textutils/)

## Installing the package

The latest released version is available from
http://enricoschumann.net. In an R session, just type:

    install.packages('textutils', type = 'source',
                     repos = c('http://enricoschumann.net/R', getOption('repos')))


For the latest development version, check out the Git repository and
build it. In a shell (e.g. sh or bash):

    ## first time: cd to directory and ...
    $ git clone https://github.com/enricoschumann/textutils.git

    ## later: cd to directory and ...
    $ git pull

    ## build and install the package
    $ R CMD build textutils
    $ R CMD INSTALL textutils_0.1-0.tar.gz  ## adjust version number

    ## optionally check
    $ R CMD check textutils_0.1-0.tar.gz    ## adjust version number
