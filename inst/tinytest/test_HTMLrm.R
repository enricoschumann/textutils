test <- "before <a href='http://enricoschumann.net'>LINK</a>  after"

expect_equal(HTMLrm(test),
             "before http://enricoschumann.net  after")

expect_equal(HTMLrm(paste0(test, test)),
             paste(rep("before http://enricoschumann.net  after", 2),
                   collapse = ""))

## ---

test <- "before <a>LINK</a> after"
expect_equal(HTMLrm("before <a>LINK</a> after"),
             "before LINK after")

## ---

test <- "before <a target = '_blank' href='http://enricoschumann.net'>LINK</a> after"
expect_equal(HTMLrm(test),
             "before http://enricoschumann.net after")

## --- comments

test <- "before <!-- comment --> after"
expect_equal(HTMLrm(test),
             "before  after")
