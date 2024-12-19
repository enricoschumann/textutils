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
