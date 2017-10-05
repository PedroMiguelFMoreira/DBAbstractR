library(DBAbstractR)
context("Database Tests")

# All this tests will be skipped automatically unless you decide
# to activate them. To do so, please follow this instructions:
#
# Install MySQL on your machine or have access to a machine with
# MySQL already installed.
#
# Run the .sql file in /... to create the test database.
#
# Install the MySQL Connector/ODBC on the machine where you plan
# to run the tests.
# Link: https://dev.mysql.com/downloads/connector/odbc/
#
# Create an ODBC to the MySQL server and name it "test", without
# the quotes.
#
# Change the value hasODBC from the function hasODBCCreated from
# FALSE to TRUE.
#
# Run the tests.
#
hasODBCCreated = function() {
    hasODBC = FALSE # Change me! :)
    if (!hasODBC)
        skip("Assuming ODBC is not created.")
}

test_that("DBAbstractR: Object correct instantiation", {
    expect_silent(DBAbstractR$new())
})

test_that("DBAbstractR: Database connection with invalid ODBC", {
    dbAbstract <- DBAbstractR$new()

    expect_error(dbAbstract$connectToDatabase("ISoHopeYouDontHaveAndODBCWithThisName"), "a")
})

#test_that("DBAbstractR: Database connection with valid ODBC", {
#    hasODBCCreated()
#
#    dbAbstract <- DBAbstractR$new()
#    silent <- dbAbstract$connectToDatabase("test")
#})
#
# public = list(
#     listTables = function() {
#         private$database$list()
#     },
#
#     listTableInformation = function(tableName) {
#         private$database$listTableInformation(tableName)
#     },
#
#     createView = function(viewName, tableMap, distinct = FALSE,
#                           where = "", groupBy = "", having = "") {
#         query <- private$queryBuilder$createView(viewName, tableMap, distinct,
#                                                  where, groupBy, having)
#
#         private$database$query(query)
#     },
#
#     listViews = function() {
#         private$database$list("VIEW")
#     },
#
#     deleteView = function(viewName = "") {
#         query <- private$queryBuilder$deleteView(viewName)
#
#         private$database$query(query)
#     },
#
#     createDataset = function(tableMap, distinct = FALSE,
#                              where = "", groupBy = "", having = "") {
#         query <- private$queryBuilder$createDataset(tableMap, distinct,
#                                                     where, groupBy, having)
#
#         private$database$query(query)
#     }
#
# )
