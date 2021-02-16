##  -----------------------

expect_equal(HTMLencode("test"), "test")
expect_equal(HTMLencode("<"),    "&LT;")
expect_equal(HTMLencode("&"),    "&amp;")
expect_equal(HTMLencode("&&"),   "&amp;&amp;")
expect_equal(HTMLencode("&amp ;"), "&amp;amp ;")

expect_equal(HTMLencode(";"),    ";")
expect_equal(HTMLencode("test &amp;"), "test &amp;")

##  -----------------------

expect_equal(HTMLdecode("&amp;"), "&")
expect_equal(HTMLdecode("&semi;"), ";")
expect_equal(HTMLdecode("&semi;;"), ";;")


##  -----------------------
s <- c("a &#x26; ba &#x26; b",
       "test",
       "&#X26;",
       "a &#X26; ba",
       "&#X26; ba",
       "&#X26; aa &#X26;",
       "a &#X26;",
       "a &#X26; ba",
       "a &#38; ba"
       )

expect_equal(HTMLdecode(s),
             c("a & ba & b",
               "test",
               "&",
               "a & ba",
               "& ba",
               "& aa &",
               "a &",
               "a & ba",
               "a & ba"))


## no decoding
expect_equal(HTMLdecode("&#38;",  decimal = FALSE), "&#38;")
expect_equal(HTMLdecode("&#x99;", hex = FALSE),     "&#x99;")


## case x/X
expect_equal(HTMLdecode("&#x26;"), "&")
expect_equal(HTMLdecode("&#X26;"), "&")


## https://github.com/enricoschumann/textutils/issues/3
expect_equal(HTMLdecode("I'm &notit; I tell you"),
             "I'm \u00ACit; I tell you")
expect_equal(HTMLdecode("I'm &notin; I tell you"),
             "I'm \u2209 I tell you")


