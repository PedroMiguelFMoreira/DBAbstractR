#' SQLQuery
#'
#' This class represents an SQLQuery, storing inside it all the information regarding a
#' normal query. It can later be used to run against the database.
#'
#' @section Usage:
#' \preformatted{
#' sql <- SQLQuery$new(query = "")
#'
#' sql$toString()
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{query}{Character representation of an SQL query.}
#' }
#'
#' @section Details:
#' \code{$new(query = "")} Creates an instance of SQLQuery.
#'
#' \code{$toString()} Returns the SQL query inside the object as a string.
#'
#' @importFrom R6 R6Class
#' @name SQLQuery
#' @examples
#' # Building a simple SQLQuery object and printing out its content.
#' query <- "SELECT * FROM tableName"
#' sqlQuery <- SQLQuery$new(query)
#'
#' sqlQuery$toString()
#'
NULL
#' @export
SQLQuery <- R6Class(
    classname = "SQLQuery",

    lock_objects = FALSE,

    cloneable = FALSE,

    private = list(
        query = ""
    ),

    public = list(
        initialize = function(query = "") {
            if (nchar(query) == 0) stop("The query cannot be an empty set of characters")

            private$query = query
        },

        toString = function() {
            toString(private$query)
        }
    )
)
