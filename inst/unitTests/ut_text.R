## -*- truncate-lines: t; -*-

require("textutils")
require("RUnit")

test.strexp <- function() {

    s <- c("", "a  s")
    s1 <- strexp(s, after = "^", width = 20)
    checkEquals(nchar(s1), c(20L, 20L))

    s2 <- strexp(s, at = 0, width = 20)
    checkEquals(nchar(s2), c(20L, 20L))

}

test.spaces <- function() {

    checkEquals(spaces(0:3),
                c("", " ", "  ", "   "))

}

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
