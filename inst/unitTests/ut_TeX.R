## -*- truncate-lines: t; -*-

test.TeXunits <- function() {

    ## TeXunits(c("1 cm", "0.7 in"), "in")
    ## tmp <- TeXunits(c("1 cm", "0.7 in"), c("in", "cm"))

    require("textutils")
    require("RUnit")

    tmp <- TeXunits("1 cm", c("in", "cm"))
    checkEqualsNumeric(as.numeric(tmp), 
                       c(0.393700804026445, 1))
    checkEquals(names(tmp) , c("in", "cm"))
}
