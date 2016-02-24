## -*- truncate-lines: t; -*-

##
toText <- function(x, ...) 
    UseMethod("toText")

toText.default <- function(x, ...) {
    ans <- capture.output(write(as.character(x), ""))
    class(ans) <- "text"
    ans
}

print.text <- function(x, ...)
    cat(x, sep = "\n", ...)

toHTML <- function(x, ...)
    UseMethod("toHTML")

toHTML.default <- function(x, ...){
    ans <- capture.output(write(as.character(x), ""))
    c("<pre>", ans, "</pre>")
}

toHTML.text <- function(x, ...){
    ans <- capture.output(write(as.character(x), ""))
    c("<pre>", ans, "</pre>")
}

## remove space at beginning or end of string
rmspace <- function(s, leading = TRUE, trailing = TRUE) {
    .Deprecated("trim", "textutils",
                paste0(sQuote("rmspace"), " is deprecated. Use ",
                       sQuote("trim"), " instead.", collapse = ""))
    if (leading)
        s <- gsub("^\\s*", "", s)
    if (trailing)
        s <- gsub("\\s*$", "", s)
    s
}

trim <- function(s, leading = TRUE, trailing = TRUE, perl = TRUE, ...) {
    if (leading && trailing)
        gsub("^\\s+|\\s+$", "", s, perl = perl, ...)
    else if (leading)
        gsub("^\\s+", "", s, perl = perl, ...)
    else
        gsub("\\s+$", "", s, perl = perl, ...)
}

## s <- c("abc", "   abc", "abc   ", "  abc  ", "a b c")
## s <- c(s,s,s,s,s,s,s)
## require("rbenchmark")
## benchmark(gsub("^\\s\\s*|\\s*\\s$", "", s),
##           gsub("^\\s+|\\s+$", "", s),
##           gsub("^\\s*|\\s*$", "", s),
##           gsub("^\\s*|\\s*$", "", s, perl = TRUE),
##           gsub("^\\s\\s*|\\s*\\s$", "", s, perl = TRUE),
##           gsub("^\\s+|\\s+$", "", s, perl = TRUE),
##           rmspace(s),
##           trim(s),
##           replications = 10000, columns = c("test", "elapsed", "relative"),
##           order = "relative")

## takes a string like "12.000,23" and returns 12000.23
char2num <- function(s, dec = ",", big.mark = ".") {
    s <- gsub(big.mark, "", s, fixed = TRUE)
    as.numeric(sub(dec, Sys.localeconv()[["decimal_point"]], s, fixed = TRUE))
}

## TeXBook, p. 57
.TeXunit.table <- c("cm" = 1864680,
                    "in" = 4736287,
                    "pt" = 65536,
                    "pc" = 786432
                    )

## remove repeated pattern
rmrp <- function(s, pattern, ...) {
    i <- grep(pattern, s, ...)
    if (any(ii <- diff(i) == 1L))
        s <- s[-i[which(c(FALSE, ii))]]    
    s
}


## remove blank lines at beginning/end
rmbl <- function(s, pattern = "^$| +", ..., leading=TRUE , trailing=TRUE) {
s <- c("",""," ", "sahs", "jwhd", "", "", "", "", "", "")

    m <- grep(pattern, s)
    rm <- NULL
    if (leading && match(1, m, nomatch = 0L))
        rm <- seq_len(which(diff(m) > 1L)[1L])  
    if (trailing && match(length(s), m, nomatch = 0L))
        stop("to be written")
    s[-rm]    
}

## convert from one TeXunit to another
TeXunits <- function(from, to, from.unit = NULL) {
    if (!is.null(from.unit))
        frU <- from.unit
    else
        frU <- gsub("([-+0-9,. ])+([a-z]+) *", "\\2", from)
    fr <- gsub("([-+0-9,. ]+)([a-z]+) *", "\\1", from)
    fr <- as.numeric(gsub(",", ".", fr))
    ans <- fr * unname(.TeXunit.table[frU]) / .TeXunit.table[to]
    if (length(ans) > 1L && length(to) == 1L)
        names(ans) <- rep(to, length(from))
    ans
}

## expand string to given width
expstr <- function(s, after, width, fill = " ", at) {
    .Deprecated("strexp", "textutils",
                paste0(sQuote("expstr"), " is deprecated. Use ",
                       sQuote("strexp"), " instead.", collapse = ""))
    ns <- nchar(s)
    space <- character(length(s))
    for (i in seq_along(space))
        space[i] <- paste(rep(" ", width[1L] - ns[i]), collapse = "")
    if (missing(at)) {
        rx <- regexpr(after, s)
        at <- as.numeric(rx + attr(rx, "match.length"))
    }
    paste(substr(s, 1L, at - 1L), space,
          substr(s, at, ns), sep = "")    
}

strexp <- function(s, after, width, fill = " ", at) {
    ns <- nchar(s)
    space <- character(length(s))
    for (i in seq_along(space))
        space[i] <- paste(rep(" ", width[1L] - ns[i]), collapse = "")
    if (missing(at)) {
        rx <- regexpr(after, s)
        at <- as.numeric(rx + attr(rx, "match.length"))
    }
    ## if (is.character(at)) {}
    paste(substr(s, 1L, at - 1L), space,
          substr(s, at, ns), sep = "")    
}

## s <-  "test | 2"
## strexp(s, width = 10, at = "##")

## ## pretty print a csv file

## tmp <- gregexpr("##", c(s,s, "a"))
## unlist(tmp)

## unlist(lapply(tmp, attr, "match.length"))
align <- function(s, pattern, sep = " ", justify = "right", fixed = TRUE, at) {
    ans <- strsplit(s, pattern, fixed = fixed)
    nc <- lapply(ans, nchar)
    len <- max(unlist(lapply(nc, length)))
    res <- NULL
    if (length(justify) == 1)
        justify <- rep(justify, len)
    for (i in seq_len(len)) {
        width <- max(unlist(lapply(nc, `[`, i)), na.rm = TRUE)
        tmp <- unlist(lapply(ans, `[`, i))
        tmp[is.na(tmp)] <- ""
        res <- paste0(res, if (is.null(res)) "" else sep,
                      format(tmp,
                             width = width, justify = justify[i]))
    }
    res
}

nspace <- function(n) {
    ans <- character(length(n))
    for (i in seq_along(n))
        ans[i] <- paste(rep.int(" ", n[i]), collapse = "")
    ans[n == 0L] <- ""
    ans
}

valign <- function(s, align = "|", insert.at = "<>", replace = TRUE, fixed = TRUE) {
    pos <- regexpr(align, s, fixed = fixed)
    ns <- nspace(max(pos) - pos)

    for (i in seq_along(s)) 
         s[i] <- sub(insert.at, ns[i], s[i], fixed = TRUE)
    sub(align, "", s, fixed = TRUE)        
}
## TeXunits(c("1 cm", "0.7 in"), c("in", "cm"))
## TeXunits(c("1 cm", "0.7 in"), "in")
## TeXunits("1 cm", c("in", "cm"))

## align
## character vector, pattern, at

## s <- c("xxxxxxxxxxxxxxx",
##        "1",
##        "1.23|5.2|100000",
##        "100|2|100")

## cat(paste(align(s, "|", " | "), collapse = "\n"))

## s <- c("xxx <>  aa|aas",
##        "adsd dsdjd dd<>a|")


## s <- c("Price <>100.23|",
##        "in % <>1.1|")
## cat(s, sep = "\n")
## cat(valign(s), sep = "\n")

## x <- c(1,1,1)
## y <- c(.1,.1,.1)

latexrule <- function(x, y, col = NULL,
                     x.unit= "cm", y.unit = "cm", noindent = FALSE) {
    if (any(!grepl("[[:alpha:]]", x)))
        x <- paste0(x, x.unit)
    if (any(!grepl("[[:alpha:]]", y)))
        y <- paste0(y, y.unit)

    res <- paste("\\rule{", x, "}{", y, "}")
    if (!is.null(col))
        res <- paste0("{\\color{", col, "}", res, "}")
    res <- paste(res, collapse = "%\n")
    res
}

btable <- function(x, unit = "cm", before = "", after = "",
                   raise = "0.2ex", height = "1ex",...) {

    if (any(!grepl("[[:alpha:]]", x)))
        x <- paste0(x, unit)
    paste0(before,
           "\\raisebox{", raise, "}{\\rule{",x,"}{",height,"}}",
           after)
}
## \begin{tabular}{rrl}
## Dez 2001 & 1,0000 & \raisebox{0.1ex}{\rule{1.0000cm}{0.7ex}} \\
## Jan 2001 & 1,0210 & \raisebox{0.1ex}{\rule{1.0210cm}{0.7ex}}                                  \\
## \end{tabular}
