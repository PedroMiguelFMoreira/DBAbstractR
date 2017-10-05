library(DBAbstractR)
context("SQLQueryFactory Tests")

test_that("SQLQueryFactory: Object correct instantiation", {
    expect_silent(SQLQueryFactory$new())
})

test_that("SQLQueryFactory: create select with null table map", {
    sqlQueryFactory <- SQLQueryFactory$new()

    expect_error(sqlQueryFactory$createSelect(), "The table map appears to be empty.")
})

test_that("SQLQueryFactory: create select with empty table map", {
    sqlQueryFactory <- SQLQueryFactory$new()

    expect_error(sqlQueryFactory$createSelect(), "The table map appears to be empty.")
})

test_that("SQLQueryFactory: create select with incorrect table map", {
    sqlQueryFactory <- SQLQueryFactory$new()
    tableMap <- data.frame(tableName = "user")

    expect_error(sqlQueryFactory$createSelect(tableMap), "The table map should contain both the table and the fields to select.")
})

test_that("SQLQueryFactory: create view without name", {
    sqlQueryFactory <- SQLQueryFactory$new()

    expect_error(sqlQueryFactory$createView(), "Specify a name for the view.")
})

test_that("SQLQueryFactory: create view without table map", {
    sqlQueryFactory <- SQLQueryFactory$new()

    expect_error(sqlQueryFactory$createView("viewname"), "The table map appears to be empty.")
})

test_that("SQLQueryFactory: View create query", {
    sqlQueryFactory <- SQLQueryFactory$new()
    tableMap <- data.frame(tableName = "user", columns = "*")
    query = "CREATE OR REPLACE VIEW viewname AS SELECT * FROM user"

    expect_equal(sqlQueryFactory$createView("viewname", tableMap), SQLQuery$new(query))
})

test_that("SQLQueryFactory: View delete query", {
    sqlQueryFactory <- SQLQueryFactory$new()
    query = "DROP VIEW viewname"

    expect_equal(sqlQueryFactory$deleteView("viewname"), SQLQuery$new(query))
})


# test_that("SQLQueryFactory: create select with null join map", {
#     sqlQueryFactory <- SQLQueryFactory$new()
#     tableMap <- data.frame(tableName = c("user", "role"), columns = c("*", "*"))
#
#     expect_error(sqlQueryFactory$createSelect(tableMap), "The table map should contain both the table and the fields to select.")
# })
#
# test_that("SQLQueryFactory: create select with empty join map", {
#     sqlQueryFactory <- SQLQueryFactory$new()
#     tableMap <- data.frame(tableName = c("user", "role"), columns = c("*", "*"))
#     joinMap <- data.frame(firstTable = "user", firstTablefield = "role_id", secondTable = "role", secondTableField = "id")
#
#     expect_error(sqlQueryFactory$createSelect(tableMap), "The table map should contain both the table and the fields to select.")
# })
#if (nrow(tableMap) > 1 && (ncol(joinMap != 2) || nrow(joinMap) != nrow(tableMap) - 1))
# stop("The join map appears to have the wrong format.")
#
# TODO: DEvELOP TESTS FOR SIMPLE SELECTS AND COMPLEX SELECTS
#       Contemplating the use of join, where, group by and having.
