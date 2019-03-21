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

test.toHTML <- function() {
    col.handlers <- list(b = function(x) x,
                         x = function(x) round(x,1),
                         y = function(x) round(x,2))
    class.handlers <- list(numeric = function(x) round(x, 3))
    
    
    df <- data.frame(x = runif(10)*10000000, y = rnorm(10)*20000000)
    toHTML(df,
           col.handlers = list(x = round),
           class.handlers = list(numeric = function(x) round(x/1000000)))

}

test.HTMLencode <- function() {
    checkEquals(HTMLencode("test"), "test")
    checkEquals(HTMLencode("<"),    "&LT;")
    checkEquals(HTMLencode("&"),    "&amp;")
    checkEquals(HTMLencode("&&"),   "&amp;&amp;")
    checkEquals(HTMLencode("&amp ;"), "&amp;amp ;")

    checkEquals(HTMLencode(";"),    ";")
    checkEquals(HTMLencode("test &amp;"), "test &amp;")
}

test.HTMLdecode <- function() {
    checkEquals(HTMLdecode("&amp;"), "&")
    checkEquals(HTMLdecode("&semi;"), ";")
    checkEquals(HTMLdecode("&semi;;"), ";;")
}
