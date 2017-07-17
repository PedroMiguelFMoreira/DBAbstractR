library(DBAbstractR)
context("Database Tests")

test_that("Database: Object incorrect instatiation #1", {
    expect_error(Database$new(), "The object cannot be null.")
})

test_that("Database: Object incorrect instatiation #2", {
    expect_error(Database$new(""), "The object must be of DatabaseCredentials type.")
})

# Database class is further tested by testDBAbsctratR.R, since
# that class encapsulates this one. Please check it.
