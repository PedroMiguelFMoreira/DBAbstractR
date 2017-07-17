library(DBAbstractR)
context("DatabaseCredentials Tests")

test_that("DatabaseCredentials: Object correct instatiation", {
    expect_silent(DatabaseCredentials$new("connection", "user", "password"))
})

test_that("DatabaseCredentials: Object incorrect instatiation", {
    expect_error(DatabaseCredentials$new("", "user", "password"), "Data source name cannot be an empty field.")
})

test_that("DatabaseCredentials: $getDataSourceName()", {
    databaseCredentials <- DatabaseCredentials$new("connection", "user", "password")

    expect_equal(databaseCredentials$getDataSourceName(), "connection")
})

test_that("DatabaseCredentials: $getUserID()", {
    databaseCredentials <- DatabaseCredentials$new("connection", "user", "password")

    expect_equal(databaseCredentials$getUserID(), "user")
})

test_that("DatabaseCredentials: $getPassword()", {
    databaseCredentials <- DatabaseCredentials$new("connection", "user", "password")

    expect_equal(databaseCredentials$getPassword(), "password")
})
