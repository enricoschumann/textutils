expect_equal(insert(1:5, 99, 1), c(99, 1:5))
expect_equal(insert(1:5, 99, 6), c(1:5, 99))
expect_equal(insert(1:5, 99, 3), c(1:2, 99, 3:5))

expect_equal(insert(1:5, 98:99, c(1,5)), c(98, 1:4, 99, 5))
expect_equal(insert(1:3, 101:103, 1:3), c(101,1,102,2,103,3))

## recycling
expect_equal(insert(letters[1:5], " ", c(2, 4)),
             c("a", " ", "b", "c", " ", "d", "e"))

expect_equal(insert(letters[1:5], c(" ", "Z"), c(2, 4)),
             c("a", " ", "b", "c", "Z", "d", "e"))

expect_equal(insert(letters[1:5], c(" ", "Z"), 2),
             c("a", " ", "Z", "b", "c", "d", "e"))

## if "values" is of length 0, nothing is inserted
expect_equal(insert(letters[1:5], character(0L), c(2, 4)),
             letters[1:5])

## if "before.index" is of length 0, nothing is inserted
expect_equal(insert(letters[1:5], "%%%", integer(0L)),
             letters[1:5])

