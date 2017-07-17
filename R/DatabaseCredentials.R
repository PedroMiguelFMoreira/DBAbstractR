#' DatabaseCredentials
#'
#' Represents the credentials of a database which are necessary to perform a connection.
#'
#' @section Usage:
#' \preformatted{
#' databaseCredentials <- DBCredentials$new(dataSourceName = "", userID = "", password = "")
#'
#' databaseCredentials$getDataServiceName()
#' databaseCredentials$getUserID()
#' databaseCredentials$getPassword()
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{dataSourceName}{Data structure normally used to describe the connection to a database.}
#'   \item{userID}{Id of the user, usually necessary to perform database connections.}
#'   \item{password}{Password of the user, usually necessary to perform database connections.}
#' }
#'
#' @section Details:
#' \code{$new(dataSourceName = "", userID = "", password = "")} Creates an instance of DatabaseCredentials.
#'
#' \code{$getDataSourceName()} Returns the data source name.
#'
#' \code{$getUserID()} Returns the id of the user.
#'
#' \code{$getPassword()} Returns the password of the user.
#'
#' @importFrom R6 R6Class
#' @name DatabaseCredentials
#' @examples
#' # Building a simple DatabaseCredentials object and extracting each of its attributes.
#' dsn <- "connection"
#' uid <- "root"
#' pwd <- "password"
#'
#' databaseCredentials <- DatabaseCredentials$new(dsn, uid, pwd)
#'
#' databaseCredentials$getDataSourceName()
#' databaseCredentials$getUserID()
#' databaseCredentials$getPassword()
#'
NULL
#' @export
DatabaseCredentials <- R6Class(
    classname = "DatabaseCredentials",

    lock_objects = FALSE,

    cloneable = FALSE,

    private = list(
        dataSourceName = "",
        userID = "",
        password = ""
    ),

    public = list(
        initialize = function(dataSourceName = "", userID = "", password = "") {
            if (nchar(dataSourceName) == 0) stop("Data source name cannot be an empty field.")

            private$dataSourceName <- dataSourceName
            private$userID <- userID
            private$password <- password
        },

        getDataSourceName = function() {
            toString(private$dataSourceName)
        },

        getUserID = function() {
            toString(private$userID)
        },

        getPassword = function() {
            toString(private$password)
        }

    )
)
