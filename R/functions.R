## -*- truncate-lines: t; -*-

toText <- function(x, ...)
    UseMethod("toText")


toText.default <- function(x, ...) {
    ans <- capture.output(write(as.character(x), ""))
    class(ans) <- "text"
    ans
}

toText.character <- function(x, ..., sep = ":") {
    if (is.atomic(x) && !is.null(names(x))) {
        ans <- paste0(names(x), sep, x)
        class(ans) <- "text"
        ans
    } else {
        NextMethod()
    }
}

toText.data.frame <- function(x, ...) {
    ans <- capture.output(print(x, ...))
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

toHTML.data.frame <- function(x, ...,
                              row.names = FALSE,
                              col.names = TRUE,
                              class.handlers = list(),
                              col.handlers = list(),
                              replace.NA = NULL,
                              td.id = FALSE) {

    row.names.header <- ""
    if (is.character(row.names)) {
        row.names.header <- row.names
        row.names <- TRUE
    }
    dfnames <- names(x)
    isna <- is.na(x)

    if (any(i <- names(col.handlers) %in% dfnames)) {
        elt <- which(i)
        for (e in elt)
            x[[ names(col.handlers)[e] ]] <- col.handlers[[e]](x[[ names(col.handlers)[e] ]])
    }
    cl <- sapply(x, class)
    for (j in seq_len(ncol(x))) {
        if (dfnames[j] %in% names(col.handlers))
            next
        if (cl[j] %in% names(class.handlers))
            x[[j]] <- class.handlers[[ cl[j] ]](x[[j]])

        if (!is.null(replace.NA))
            x[[j]][isna[, j]] <- replace.NA
    }

    header.row <- c(if (row.names) row.names.header,
                    if (is.character(col.names)) col.names else colnames(x))
    if (isFALSE(td.id)) {
        m <- apply(x, 2, function(x) as.matrix(paste0("<td>", x, "</td>")))
        if (dim(x)[[1]] == 1L)
            dim(m) <- dim(x)
    } else if (isTRUE(td.id)) {
        m <- array("", dim = dim(x))
        for (j in seq_len(ncol(x)))
            m[, j] <- paste0("<td id='td_", seq_len(nrow(x)), "_", j, "'>",
                             x[, j],
                             "</td>")
    }
    ans <- rbind(if (is.character(col.names) || col.names)
                     paste0("<th>", header.row, "</th>"),
                 cbind(if (row.names) paste0("<td>", row.names(x), "</td>"), m))

    paste0("<tr>", apply(ans, 1, paste, collapse = ""), "</tr>")
}

toLatex.data.frame <- function(object,
                               row.names = FALSE,
                               col.handlers = list(),
                               class.handlers = list(),
                               eol = "\\\\",
                               ...) {

    dfnames <- names(object)
    if (any(i <- names(col.handlers) %in% dfnames)) {
        elt <- which(i)
        for (e in elt)
            object[[ names(col.handlers)[e] ]] <-
                col.handlers[[e]](object[[ names(col.handlers)[e] ]])
    }
    cl <- sapply(object, class)
    for (j in seq_len(ncol(object))) {
        if (dfnames[j] %in% names(col.handlers))
            next
        if (cl[j] %in% names(class.handlers))
            object[[j]] <- class.handlers[[ cl[j] ]](object[[j]])
    }
    if (!isFALSE(row.names)) {
        nm <- if (is.character(row.names)) row.names else "row.names"
        object <- cbind(row.names(object), object)
        colnames(object)[1L] <- nm
    }
    paste(do.call(function(...) paste(..., sep = " & "), object), eol)
}

trim <- function(s, leading = TRUE, trailing = TRUE, perl = TRUE, ...) {
    if (leading && trailing)
        gsub("^\\s+|\\s+$", "", s, perl = perl, ...)
    else if (leading)
        gsub("^\\s+", "", s, perl = perl, ...)
    else
        gsub("\\s+$", "", s, perl = perl, ...)
}

rmrp <- function(s, pattern, ...) {
    i <- grep(pattern, s, ...)
    if (any(ii <- diff(i) == 1L))
        s <- s[-i[which(c(FALSE, ii))]]
    s
}


## TeXbook, p. 57
.TeXunit.table <- c("cm" = 1864680,
                    "in" = 4736287,
                    "pt" = 65536,
                    "pc" = 786432,
                    "bp" = 65536*72.27/72,
                    "mm" = 186468,
                    "dd" = 65536*1238/1157,
                    "cc" = 12*65536*1238/1157)

TeXunits <- function(from, to, from.unit = NULL) {
    if (length(from) > 1L && length(to) > 1L)
        to <- rep(to, each = length(from))
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

TeXencode <- function(s) {
    repl <- c("&", "\\&",
              "%", "\\%",
              "$", "\\$",
              "#", "\\#",
              "_", "\\_",
              "{", "\\{",
              "}", "\\}")
    ii <- seq(1, length(repl), by = 2)
    for (i in ii) {
        s <- gsub(repl[i], repl[i+1], s, fixed = TRUE)
    }
    s
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

.spaces <- "                                                                                                                                                                                                        "
spaces <- function(n) {
    if (max(n) > 200L)
        .spaces <- paste(rep.int(" ", max(n)), collapse = "")
    substring(.spaces, 0L, n)
}


valign <- function(s, align = "|", insert.at = "<>",
                   replace = TRUE, fixed = TRUE) {
    pos <- regexpr(align, s, fixed = fixed)
    ns <- spaces(max(pos) - pos)

    for (i in seq_along(s))
         s[i] <- sub(insert.at, ns[i], s[i], fixed = TRUE)
    sub(align, "", s, fixed = TRUE)
}


latexrule <- function(x, y, col = NULL,
                     x.unit = "cm", y.unit = "cm", noindent = FALSE) {
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

.map <- function (x, min = 0, max = 1,
                    omin = min(x), omax = max(x)) {
    new.range <- max - min
    old.range <- omax - omin
    (new.range * x + min * omax - max * omin)/old.range
}


dctable <- function(x,
                    unitlength = "1 cm",
                    width = 5,
                    y.offset = 0.07,
                    circle.size = 0.1,
                    xlim,
                    na.rm = FALSE) {

    if (missing(xlim))
        xlim <- range(x, na.rm = na.rm)
    pic <-
"\\begin{picture}(%width%,0)
\\put (%lstart%, %y.offset%) {\\color{gray!90}\\line(1, 0){%d%}}
\\put (    %x1%, %y.offset%) {\\color{gray!50}\\circle*{%circle.size%}}
\\put (    %x2%, %y.offset%) {\\color{gray!110}\\circle*{%circle.size%}}
\\end{picture}
"

    p <- character(nrow(x))
    for (i in seq_len(nrow(x))) {
        if (any(is.na(x[i, ])))
            next

        d <- diff(x[i, ])
        p.i <- pic
        p.i <- fill_in(pic,
                       width = width,
                       y.offset = y.offset,
                       circle.size = circle.size,
                       x1 = .map(x[i, 1], 0, width,
                                 xlim[1], xlim[2]),
                       x2 = .map(x[i, 2], 0, width,
                                 xlim[1], xlim[2]),
                       d  = .map(abs(d), 0, width,
                                 xlim[1], xlim[2]),
                       lstart =
                            .map(min(x[i, ]), 0, width,
                                xlim[1], xlim[2]),
                       delim = c("%", "%"))
        p[i] <- p.i
    }
    p
}


HTMLencode <- function(x, use.iconv = FALSE, encode.only = NULL) {
    ii <- seq.int(1, length(.html_entities), 2)
    if (!is.null(encode.only)) {
        m <- match(encode.only, .html_entities[ii+1], nomatch = 0L)
        entities <- list(char = encode.only[m > 0L],
                         ent  = .html_entities[ii][m])
    } else
        entities <- list(char = .html_entities[ii + 1L],
                         ent  = .html_entities[ii])
    nd <- !duplicated(entities[["char"]])
    entities[["char"]] <- entities[["char"]][nd]
    entities[["ent"]]  <- entities[["ent"]][nd]
    if (use.iconv)
        iconv(x, from = "", to = "UTF-8")
    for (i in seq_len(length(entities[["char"]])))
        if (entities[["char"]][i] == "&")
            x <- gsub("&(?![a-zA-Z]+;)",
                      "&amp;",
                      x,
                      perl = TRUE)
        else if (entities[["char"]][i] == ";")
            ## FIXME: PCRE does not allow backward
            ## assertions with variable length
            ## x <- gsub("(?<!&[a-zA-Z]+);",
            ##           "&semi;",
            ##           x,
            ##           perl = TRUE)
            next
        else
            x <- gsub(entities[["char"]][i],
                      entities[["ent"]][i],
                      x,
                      fixed = TRUE)
    x
}

HTMLdecode <- function(x, named = TRUE, hex = TRUE, decimal = TRUE) {
    if (named) {
        ii <- seq.int(1, length(.html_entities), 2)
        for (i in ii)
            x <- gsub(.html_entities[i], .html_entities[i + 1L], x, fixed = TRUE)
    }
    if (hex)
        x <- .replace_num_char_ref(x, hex = TRUE)
    if (decimal)
        x <- .replace_num_char_ref(x, hex = FALSE)
    x
}

.replace_num_char_ref <- function(s, hex = TRUE) {
    p <- if (hex) "&#x[0-9a-f]+;" else "&#[0-9a-f]+;"
    m <- gregexpr(p, s, perl = TRUE, ignore.case = TRUE)
    ii <- which(unlist(lapply(m, function(x) x[[1L]]) > 0))

    repl <- vector("list", length(ii))
    for (i in ii) {
        temp <- substring(s[i],
                          m[[i]] + if (hex) 3L else 2L,               ## skip '&#x?'
                          m[[i]] + attr(m[[i]], "match.length") - 2L) ## skip ';'
        for (t in temp)
            if (hex)
                temp[t == temp] <- intToUtf8(strtoi(t, 16L))
            else
                temp[t == temp] <- intToUtf8(t)

        repl[[which(i == ii)]] <- temp
    }
    regmatches(s[ii], m[ii]) <- repl
    Encoding(s) <- "UTF-8"
    s
}


## https://www.w3.org/TR/html5/syntax.html#named-character-references
## '.html_entities' is defined in file 'char_refs.R'
Encoding(.html_entities) <- "UTF-8"
## Encoding(.html_entities) <- "bytes"

title_case <- function(s, strict = FALSE, ignore = NULL) {
    spl <- strsplit(s, split = " ")
    for (i in seq_along(spl)) {
        if (!is.null(ignore)) {
            do <- !spl[[i]] %in% ignore
        } else
            do <- rep(TRUE, length(spl[[i]]))
        if (strict)
            spl[[i]][do] <- tolower(spl[[i]][do])
        substr(spl[[i]][do],1,1) <- toupper(substr(spl[[i]][do],1,1))
    }

    unlist(lapply(spl, paste0, collapse = " "),
           use.names = !is.null(names(s)))
}


fill_in <- function(s, ..., delim = c("{", "}"), replace.NA = TRUE) {
    val <- list(...)
    it <- if (is.null(names(val)))
              seq_along(val)
          else
              names(val)
    for (i in it) {
        repl <- if (isTRUE(replace.NA) && is.na(val[[i]]))
                    "NA"
                else if (is.character(replace.NA) && is.na(val[[i]]))
                    replace.NA
                else
                    val[[i]]

        s <- gsub(paste0(delim[1L], i, delim[2L]),
                  repl, s, fixed = TRUE)
    }
    s
}

here <- function(s, drop = TRUE, guess.type = TRUE,
                 sep = NULL, header = TRUE,
                 stringsAsFactors = FALSE,
                 trim = TRUE, ...) {
    ans <- readLines(con <- textConnection(s))
    close(con)

    if (drop && ans[len <- length(ans)] == "")
        ans <- ans[-len]
    if (drop && ans[1L] == "")
        ans <- ans[-1L]

    if (is.null(sep) && guess.type) {
        if (trim)
            ans <- trimws(ans)
        ans <- type.convert(ans, as.is = TRUE)
    } else {
        ans <- read.table(text = ans,
                          header = header, sep = sep,
                          stringsAsFactors = stringsAsFactors,
                          strip.white = trim,
                          colClasses = if (guess.type)
                                           NA else "character", ...)
    }
    ans
}

insert <- function(x, values, before.index) {
    if (!length(values) || !length(before.index))
        return(x)
    if (is.unsorted(before.index)) {
        ii <- order(before.index)
        values <- values[ii]
        before.index <- before.index[ii]
    }
    if (length(values) < length(before.index))
        values <- rep(values, length(before.index)/length(values))
    before.index <- before.index +
        seq(from = 0, to = length(values) - 1)
    ans <- vector(class(x), length(x) + length(values))
    ans[ before.index] <- values
    ans[-before.index] <- x
    ans
}


## Non-printable ASCII control characters, i.e. those up to
## octal 37 except \x09 (\t), \x0A (\n), \x0D (\r).
## Without the exceptions, character class [:cntrl:] would
## work as well and would also handle 000.
.ASCII.control.rx <- "\001|\002|\003|\004|\005|\006|\a|\b|\v|\f|\016|\017|\020|\021|\022|\023|\024|\025|\026|\027|\030|\031|\032|\033|\034|\035|\036|\037|\0177"



HTMLrm <- function(x, ..., tag.replace = "") {

    ignore.case <- TRUE
    x <- gsub("<style>.*?</style>", "", x, perl = TRUE, ignore.case = ignore.case)
    x <- gsub("<head>.*?</head>", "", x, perl = TRUE, ignore.case = ignore.case)
    x <- gsub("<style>.*?</style>", "", x, perl = TRUE, ignore.case = ignore.case)
    x <- gsub("<script>.*?</script>", "", x, perl = TRUE, ignore.case = ignore.case)
    x <- gsub("<!--.*?-->", "", x, perl = TRUE, ignore.case = ignore.case)
    x <- HTMLdecode(x)

    x <- gsub("<a [^>]*?href *= *['\"](.*?)['\"][^>]*>.*?</a>", "\\1", x)

    ## remove all tags like '<something>' and '</something>'
    x <- gsub("<[/]?[^>]+>", tag.replace, x,
              perl = TRUE, ignore.case = ignore.case)
    x
}



read_HTMLtable <- function(s, unlist1 = FALSE, guess.type = TRUE) {

    tables <- gregexpr("<table.*?</table>", s)
    tables <- regmatches(s, tables)

    ans <- vector("list", length(tables))

    i <- 0
    for (table in tables[[1L]]) {
        i <- i + 1
        m <- gregexpr("<tr.*?</tr>", table, perl = TRUE)
        rows <- regmatches(table, m)[[1]]

        h <- gregexpr("<th.*?</th>", rows, perl = TRUE)
        headers <- regmatches(rows, h)

        H <- headers[[1]]
        H <- sub("<th[^>]*?>", "", H)
        H <- sub("</th>", "", H)


        n <- gregexpr("<td.*?</td>", rows, perl = TRUE)
        cells <- regmatches(rows, n)

        fix.cells <- function(s) {
            txt <- gsub(r"(<a ("[^"]*"|'[^']*'|[^>"'])*>)", "", s, perl = TRUE)
            txt <- gsub("</a>", "", txt, fixed = TRUE)
            txt <- sub("<td[^>]*?>", "", txt, perl = TRUE)
            sub("</td>", "", txt, fixed = TRUE)
        }
        res <- lapply(cells, fix.cells)
        res <- do.call(rbind, res)
        res <- split(res, col(res))
        res <- lapply(res, type.convert, as.is = TRUE)
        res <- list2DF(res)
        colnames(res) <- H
        ans[[i]] <- res
    }
    ans
}
