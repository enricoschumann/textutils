## toHTML: data.frame

s <- toHTML(data.frame(a = 1:3, b = 4:6))
expect_equal(s,
             c("<tr><th>a</th><th>b</th></tr>",
               "<tr><td>1</td><td>4</td></tr>",
               "<tr><td>2</td><td>5</td></tr>",
               "<tr><td>3</td><td>6</td></tr>"))



s <- toHTML(data.frame(a = 1:3, b = 4:6), row.names = TRUE)
expect_equal(s,
             c("<tr><th></th><th>a</th><th>b</th></tr>",
               "<tr><td>1</td><td>1</td><td>4</td></tr>",
               "<tr><td>2</td><td>2</td><td>5</td></tr>",
               "<tr><td>3</td><td>3</td><td>6</td></tr>"))



s <- toHTML(data.frame(a = 1:3, b = 4:6), row.names = "Num")
expect_equal(s,
             c("<tr><th>Num</th><th>a</th><th>b</th></tr>",
               "<tr><td>1</td><td>1</td><td>4</td></tr>",
               "<tr><td>2</td><td>2</td><td>5</td></tr>",
               "<tr><td>3</td><td>3</td><td>6</td></tr>"))



s <- toHTML(data.frame(a = 1:3, b = 4:6, row.names = letters[24:26]),
            row.names = "Letter")
expect_equal(s,
             c("<tr><th>Letter</th><th>a</th><th>b</th></tr>",
               "<tr><td>x</td><td>1</td><td>4</td></tr>",
               "<tr><td>y</td><td>2</td><td>5</td></tr>",
               "<tr><td>z</td><td>3</td><td>6</td></tr>"))



s <- toHTML(data.frame(a = 1:3, b = 4:6), col.names = FALSE)
expect_equal(s,
             c("<tr><td>1</td><td>4</td></tr>",
               "<tr><td>2</td><td>5</td></tr>",
               "<tr><td>3</td><td>6</td></tr>"))



s <- toHTML(data.frame(a = 1:3, b = 4:6), col.names = FALSE, row.names = TRUE)
expect_equal(s,
             c("<tr><td>1</td><td>1</td><td>4</td></tr>",
               "<tr><td>2</td><td>2</td><td>5</td></tr>",
               "<tr><td>3</td><td>3</td><td>6</td></tr>"))


## single row data.frame
s <- toHTML(data.frame(a = 1, b = 4))
expect_equal(s,
             c("<tr><th>a</th><th>b</th></tr>",
               "<tr><td>1</td><td>4</td></tr>"))



## ---------------------------------------------------
## single row data.frame
## ---------------------------------------------------

s <- toHTML(data.frame(a = 1, b = 4))
expect_equal(s,
             c("<tr><th>a</th><th>b</th></tr>",
               "<tr><td>1</td><td>4</td></tr>"))

s <- toHTML(data.frame(a = 1, b = 4), row.names = TRUE)
expect_equal(s,
             c("<tr><th></th><th>a</th><th>b</th></tr>",
               "<tr><td>1</td><td>1</td><td>4</td></tr>"))

s <- toHTML(data.frame(a = 1, b = 4), row.names = "test")
expect_equal(s,
             c("<tr><th>test</th><th>a</th><th>b</th></tr>",
               "<tr><td>1</td><td>1</td><td>4</td></tr>"))

s <- toHTML(data.frame(a = 1, b = 4), col.names = FALSE)
expect_equal(s,
             c("<tr><td>1</td><td>4</td></tr>"))

s <- toHTML(data.frame(a = 1, b = 4),
            col.names = FALSE,
            row.names = TRUE)
expect_equal(s,
             c("<tr><td>1</td><td>1</td><td>4</td></tr>"))
