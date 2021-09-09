## -*- truncate-lines: t; -*-

library("jsonlite")
data <- read_json("https://html.spec.whatwg.org/entities.json", TRUE)
res <- unlist(lapply(data,
                     function(x)
                         format(as.hexmode(x[[1]]),
                                width = 4,
                                upper.case = TRUE)))

chars <- unlist(lapply(data, `[[`, 2L))
Encoding(chars) <- "UTF-8"





##  ------ prepare .html_entities ------
download.file("https://html.spec.whatwg.org/entities.json",
              temp.i <- tempfile(pattern = "file_input_"))
txt <- readLines(temp.i)
txt <- sub(': \\{ "codepoints": .*, "characters": ', ",", txt)
txt <- sub(" *},? *$", ",", txt)
txt[1] <- ".html_entities <- c("
txt[length(txt)] <- ")"
txt[length(txt)-1] <- substr(txt[length(txt)-1], 1, nchar(txt[length(txt)-1])-1)
writeLines(txt, temp.o <- tempfile(pattern = "file_output_"))

eval(parse(text = txt))
move <- NULL
for (i in seq(1, length(.html_entities), by = 2)) {

    ## all is fine if the entity ends with ";"
    if (grepl(";$", .html_entities[i]))
        next

    j <- grep(.html_entities[i], .html_entities, fixed = TRUE)
    j <- j[nchar(.html_entities[i]) < nchar(sub(";$", "", .html_entities[j]))]
    if (length(j)) {
        message(.html_entities[i],
                "\n  ",
                .html_entities[j])
        move <- c(move, i)
    }
}
lineno <- (move+1)/2 + 1

## add final ","
txt[length(txt) - 1] <- paste0(txt[length(txt) - 1], ",")

txt <- c(txt[-c(lineno, length(txt))],
         txt[ c(lineno, length(txt))])

## remove final ","
txt[length(txt)-1] <- substr(txt[length(txt)-1], 1, nchar(txt[length(txt)-1])-1)


## writeLines(txt, "~/Packages/textutils/R/char_refs.R")



##
eval(parse(text = txt))

## add final ","
txt[length(txt) - 1] <- paste0(txt[length(txt) - 1], ",")

for (i in seq(3, length(.html_entities), by = 2)) {
    if (endsWith(.html_entities[i], ";")  &&
        substr(.html_entities[i], 1, nchar(.html_entities[i])-1) ==
               .html_entities[i - 2]) {
        message(i, "--", .html_entities[i-2], "--", .html_entities[i])
        txt[(i+1)/2 - 1:0 + 1] <- txt[(i+1)/2 - 0:1 + 1]
        }
}

## remove final ","
txt[length(txt)-1] <- substr(txt[length(txt)-1], 1, nchar(txt[length(txt)-1])-1)
writeLines(txt, "~/Packages/textutils/R/char_refs.R")
