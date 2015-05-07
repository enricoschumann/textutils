## -*- truncate-lines: t; -*-
## Time-stamp: <2015-05-07 17:03:58 CEST (es)>

test.TeXunits <- function() {

    ## TeXunits(c("1 cm", "0.7 in"), "in")
    ## TeXunits("1 cm", c("in", "cm"))

    require("textutils")
    require("RUnit")

    tmp <- TeXunits(c("1 cm", "0.7 in"), c("in", "cm"))
    checkEqualsNumeric(as.numeric(tmp), 
                       c(0.393700804026445, 1.77799992492009))
    checkEquals(names(tmp) , c("in", "cm"))
}
