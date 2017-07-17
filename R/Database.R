#' Database
#'
#' Represents a database, handling the connections itself. It allows you to perform
#' operations through the use of functions (e.g. list tables, query data).
#'
#' @section Usage:
#' \preformatted{
#' credentials <- DatabaseCredentials$new("connection", "user", "password")
#' database <- Database$new(credentials)
#'
#' database$list(tableType)
#' database$listTableInformation(tableName)
#' database$query(query)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{credentials}{Object containing all the information required to connect to a database.}
#'   \item{dataSourceName}{Data structure normally used to describe the connection to a database.}
#'   \item{userID}{Id of the user, usually necessary to perform database connections.}
#'   \item{password}{Password of the user, usually necessary to perform database connections.}
#'   \item{tableType}{Table type (e.g. TABLE, VIEW)}
#'   \item{tableName}{Name of the table inside a database.}
#'   \item{query}{SQL query to run against the database.}
#' }
#'
#' @section Details:
#' \code{$new(databaseCredentials = NULL)} Creates an instance of Database.
#'
#' \code{$list(tableType = "TABLE")} List all the tables (name and description) in the database according to the type given.
#'
#' \code{$listTableInformation(tableName = "")} List the information about a given table (names of the columns, their type and nullability).
#'
#' \code{$query(query = NuLL)} Queries the database when given a valid SQL Query (e.g. SELECT * FROM table)
#'
#' @import RODBC
#' @importFrom R6 R6Class
#' @name Database
#' @examples
#' \dontrun{
#' # Building the Database object.
#' dsn <- "connection"
#' uid <- "root"
#' pwd <- "password"
#'
#' databaseCredentials <- DatabaseCredentials$new(dsn, uid, pwd)
#' database <- Database$new(databaseCredentials)
#'
#' # Listing all the tables and views in the database.
#' database$list("TABLE")
#' database$list("VIEW")
#'
#' # List information about a sepecif table.
#' database$listTableInformation("tableName")
#'
#' # Query the database to get information.
#' database$query("SELECT * FROM table")
#' }
#'
NULL
#' @export
Database <- R6Class(
    classname = "Database",

    lock_objects = FALSE,

    cloneable = FALSE,

    private = list(
        databaseCredentials = NULL,
        # Cool mechanic to ensure that every connection that is open on the
        execute = function(FUN = NULL, ...) {
            if (is.null(FUN)) stop("The parameter can't be null.")
            if (class(FUN) != "function") stop("The parameter must be a function.")

            connection <- tryCatch(
                odbcConnect(
                    private$databaseCredentials$getDataSourceName(),
                    private$databaseCredentials$getUserID(),
                    private$databaseCredentials$getPassword()),
                warning = function(warning) {
                    stop("Unable to connect to the database, please verify your credentials (e.g. DSN)", call. = FALSE)
                }
            )

            tryCatch(
                FUN(connection, ...),
                error = function(error) {
                    if (grepl("table not found on channel", error)) {
                        stop("Table not found on database.", call. = FALSE)
                    } else {
                        stop(error, call. = FALSE)
                    }
                },
                finally = odbcClose(connection)
            )
        }
    ),

    public = list(
        initialize = function(databaseCredentials = NULL) {
            if (is.null(databaseCredentials)) stop("The object cannot be null.", call. = FALSE)
            if (class(databaseCredentials)[1] != 'DatabaseCredentials') stop("The object must be of DatabaseCredentials type.", call. = FALSE)

            private$databaseCredentials = databaseCredentials
            print(private$execute(odbcGetInfo))
        },

        list = function(tableType = "TABLE") {
            columns = c("TABLE_NAME", "REMARKS")

            tableListRaw <- private$execute(sqlTables, errors = TRUE, tableType = tableType)

            subset(tableListRaw, select = columns)
        },

        listTableInformation = function(tableName = "") {
            if (class(tableName) != "character") stop("The name of the table must be a set of characters.", call. = FALSE)
            if (nchar(tableName) == 0) stop("The name of the table can't be empty.", call. = FALSE)

            columns = c("COLUMN_NAME", "TYPE_NAME", "IS_NULLABLE")

            tableInformationRaw <- private$execute(sqlColumns, errors = TRUE, sqtable = tableName)

            subset(tableInformationRaw, select = columns)
        },

        query = function(query = NULL) {
            if (is.null(query)) stop("The object cannot be null.", call. = FALSE)
            if (class(query)[1] != 'SQLQuery') stop("The object must be of SQLQuery type.", call. = FALSE)

            queryString = query$toString()

            private$execute(RODBC::sqlQuery, query = queryString, errors = TRUE)
        }

    )
)
