
s <- c("", "a  s")
s1 <- strexp(s, after = "^", width = 20)
expect_equal(nchar(s1), c(20L, 20L))

s2 <- strexp(s, at = 0, width = 20)
expect_equal(nchar(s2), c(20L, 20L))



##  -----------------------

expect_equal(spaces(0:3),
             c("", " ", "  ", "   "))



##  -----------------------

## TeXunits(c("1 cm", "0.7 in"), "in")
## tmp <- TeXunits(c("1 cm", "0.7 in"), c("in", "cm"))

tmp <- TeXunits("1 cm", c("in", "cm"))
expect_equivalent(as.numeric(tmp),
                  c(0.393700804026445, 1))
expect_equal(names(tmp) , c("in", "cm"))



##  -----------------------

col.handlers <- list(b = function(x) x,
                     x = function(x) round(x,1),
                     y = function(x) round(x,2))
class.handlers <- list(numeric = function(x) round(x, 3))


df <- data.frame(x = runif(10)*10000000, y = rnorm(10)*20000000)
toHTML(df,
       col.handlers = list(x = round),
       class.handlers = list(numeric = function(x) round(x/1000000)))



