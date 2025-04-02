test <- "before <a href='https://enricoschumann.net'>LINK</a>  after"

expect_equal(HTMLrm(test),
             "before https://enricoschumann.net  after")

expect_equal(HTMLrm(paste0(test, test)),
             paste(rep("before https://enricoschumann.net  after", 2),
                   collapse = ""))

## ---

test <- "before <a>LINK</a> after"
expect_equal(HTMLrm("before <a>LINK</a> after"),
             "before LINK after")

## ---

test <- "before <a target = '_blank' href='https://enricoschumann.net'>LINK</a> after"
expect_equal(HTMLrm(test),
             "before https://enricoschumann.net after")

## --- comments

test <- "before <!-- comment --> after"
expect_equal(HTMLrm(test),
             "before  after")
