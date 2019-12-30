## -*- truncate-lines: t; -*-

library("jsonlite")
data <- read_json("https://html.spec.whatwg.org/entities.json", TRUE)
readLines("https://html.spec.whatwg.org/entities.json")
res <- unlist(lapply(data,
                     function(x)
                         format(as.hexmode(x[[1]]),
                                width = 4,
                                upper.case = TRUE)))

chars <- unlist(lapply(data, `[[`, 2L))
Encoding(chars) <- "UTF-8"
