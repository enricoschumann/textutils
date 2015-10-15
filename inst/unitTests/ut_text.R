## -*- truncate-lines: t; -*-
## Time-stamp: <2015-10-15 06:52:06 CEST (es)>

test.expstr <- function() {

    require("textutils")
    require("RUnit")

    s <- c("", "a  s")
    s1 <- strexp(s, after = "^", width = 20)
    checkEquals(nchar(s1), c(20L, 20L))

    s2 <- strexp(s, at = 0, width = 20)
    checkEquals(nchar(s2), c(20L, 20L))
    
}
