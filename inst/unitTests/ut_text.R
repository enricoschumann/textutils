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
