# textutils

Utilities for handling character vectors that store human-readable
text (either plain or with markup, such as HTML).

[ [More] ](http://enricoschumann.net/R/packages/textutils/)

## Installing the package

A stable version of the package is available from
[ CRAN ](https://CRAN.R-project.org/package=textutils).


The latest version is available from
http://enricoschumann.net. In an R session, just type:

    install.packages('textutils', type = 'source',
                     repos = c('http://enricoschumann.net/R', getOption('repos')))

For the latest development version, check out the Git
repository and build it. In a shell (e.g. sh or bash):

    ## first time: cd to directory and ...
    $ git clone https://github.com/enricoschumann/textutils

    ## ... build and install the package
    $ R CMD build textutils
    $ R CMD INSTALL textutils_<0.1-0>.tar.gz  ## adjust version number

    ## optionally check
    $ R CMD check textutils_<0.1-0>.tar.gz    ## adjust version number


    ## update package: cd to package directory and ...
    $ git pull
    $ cd ..
    $ R CMD build textutils  # build/INSTALL/check as above
