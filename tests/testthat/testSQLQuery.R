library(DBAbstractR)
context("SQLQuery Tests")

test_that("SQLQuery: Object correct instatiation", {
    expect_silent(SQLQuery$new("SELECT * FROM table"))
})

test_that("SQLQuery: Object incorrect instatiation", {
    expect_error(SQLQuery$new(), "The query cannot be an empty set of characters")
})

test_that("SQLQuery: $toString()", {
    sqlQuery <- SQLQuery$new("SELECT * FROM table")

    expect_equal(sqlQuery$toString(), "SELECT * FROM table")
})
