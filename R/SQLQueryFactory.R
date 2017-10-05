#' SQLQueryFactory
#'
#' Represents an SQL query factory, responsible for building SQL queries. By simply calling
#' the available methods and supplying them with your needs, you get an SQLQuery object
#' containing a query that can bring the information from the database.
#'
#' @section Usage:
#' \preformatted{
#' sqlFactory <- SQLQueryFactory$new()
#'
#' sqlFactory$createSelect(tableMap, joinMap, distinct, where, groupBy, having)
#' sqlFactory$createView(viewName, tableMap, distinct, where, groupBy, having)
#' sqlFactory$deleteView(viewName)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{tableName}{Name of the table inside a database.}
#'   \item{viewName}{Name of the view inside a database.}
#'   \item{tableMap}{Dataframe specifing which tables and columns should be used to extract information.}
#'   \item{joinMap}{Dataframe specifing which tables to join and repective atributes.}
#'   \item{distinct}{Logical that dictates if the data retrieved should be distinct or not.}
#'   \item{where}{Filters the results by the specified conditions.}
#'   \item{groupBy}{Groups the results by the values of one or more columns.}
#'   \item{having}{Further filters the results, by allowing the use of agreggation functions (e.g. COUNT, SUM, ...)}
#' }
#'
#' @section Details:
#' \code{$new()} Creates an instance of DBAbstractR.
#'
#' \code{$connectToDatabase(dataSourceName = "", userID = "", password = "")} Performs the connection to a database, returning its information.
#'
#' \code{$listTables} Returns a dataframe with all the tables (name and description) in the database.
#'
#' \code{$listTableInformation(tableName = "")} Returns a dataframe with the information (names of the columns, their type and nullability) about a given table.
#'
#' @importFrom R6 R6Class
#' @name SQLQueryFactory
#' @examples
#' \dontrun{
#' # Building the SQLQueryFactory object.
#' sqlQueryFactory <- SQLQueryFactory$new()
#'
#' # Selecting both the username and last_login from the user table.
#' tableMap <- data.frame(tableName = "user", columns = "username", "last_login")
#'
#' sqlQueryFactory$createSelect(tableMap)
#'
#' # Creating a view based on the user table where all registered users date from under 2009.
#' tableMap <- data.frame(tablename = "user", columns = "*")
#' where <- "register_date < 2010-01-01"
#'
#' # Droping an existing view
#' sqlQueryFactory$deleteView("old_users")
#' }
#'
NULL
#' @export
SQLQueryFactory <- R6Class(
    classname = "SQLQueryFactory",

    lock_objects = FALSE,

    cloneable = FALSE,

    private = list(
        createSimpleSelect = function(tableMap = data.frame(), distinct = FALSE,
                                      where = data.frame(), groupBy = data.frame(),
                                      having = data.frame()) {
            query <- "SELECT"
            if (distinct) query <- paste(query, "DISTINCT", sep = " ")
            what <- tableMap$columns[1]
            from <- paste("FROM", tableMap$tableName[1], sep = " ")

            query <- paste(query, what, from, sep = " ")

            if (nrow(where) > 0) query <- paste(query, private$where(where), sep = "")
            if (nrow(groupBy) > 0) query <- paste(query, "GROUP BY", groupBy, sep = " ")
            if (nrow(having) > 0) query <- paste(query, "HAVING", having, sep = " ")

            query
        },

        createComplexSelect = function(tableMap = data.frame(), joinMap = data.frame(),
                                       distinct = FALSE, where = data.frame(),
                                       groupBy = data.frame(), having = data.frame()) {
            #### TODO ####

            query = "Test"

            query
        },

        where = function(whereClause) {
            query <- "WHERE"

            for(i in 1:nrow(whereClause)) {
                table <- whereClause[i, 1]
                operator <- whereClause[i, 2]
                value <- whereClause[i, 3]

                query <- paste()

            }

#            if (distinct) query <- paste(query, "DISTINCT", sep = " ")
#            what <- tableMap$columns[1]
#            from <- paste("FROM", tableMap$tableName[1], sep = " ")
#
#            query <- paste(query, what, from, sep = " ")


        },

        groupBy = function() {
            #### TODO ####
        },

        having = function() {
            #### TODO ####
        }

    ),

    public = list(
        initialize = function() {},

        createSelect = function(tableMap = data.frame(), joinMap = data.frame(),
                                distinct = FALSE, where = data.frame(),
                                groupBy = data.frame(), having = data.frame()) {
            if (nrow(tableMap) == 0) stop("The table map appears to be empty.")
            if (ncol(tableMap) != 2) stop("The table map should contain both the table and the fields to select.")
            if (nrow(tableMap) > 1 && (ncol(joinMap) != 4 || nrow(joinMap) != nrow(tableMap) - 1)) stop("The join map appears to have the wrong format.")

            query = ""

            if (nrow(tableMap) == 1) {
                query <- private$createSimpleSelect(tableMap, distinct, where, groupBy, having)
            } else {
                query <- private$createComplexSelect(tableMap, joinMap, distinct, where, groupBy, having)
            }

            SQLQuery$new(query)
        },

        createView = function(viewName = "", tableMap = data.frame(), joinMap = data.frame(),
                              distinct = FALSE, where = data.frame(), groupBy = data.frame(),
                              having = data.frame()) {
            if (nchar(viewName) == 0) stop("Specify a name for the view.")
            if (nrow(tableMap) == 0) stop("The table map appears to be empty.")
            if (ncol(tableMap) != 2) stop("The table map should contain both the table and the fields to select.")
            if (nrow(tableMap) > 1 && (ncol(joinMap != 4) || nrow(joinMap) != nrow(tableMap) - 1)) stop("The join map appears to have the wrong format.")

            query <- paste("CREATE OR REPLACE VIEW ", viewName, " AS", sep = "")
            query <- paste(query, self$createSelect(tableMap, joinMap, distinct, where, groupBy, having)$toString())

            SQLQuery$new(query)
        },

        deleteView = function(viewName = "") {
            query = paste("DROP VIEW", viewName, sep = " ")

            SQLQuery$new(query)
        }

    )
)
