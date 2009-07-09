
# A ViewerData class that allows viewing of an SQL query to MySQL

# For a small query, you might as well just use dbGetQuery and then
# view the result as a ViewerDataFrame
# The point of this class is to ONLY pull across the visible part
# of the query result, so should allow viewing of VERY large
# database tables

library(RMySQL)

setClass("ViewerDataMySQL",
         representation(dbcon="MySQLConnection",
                        query="character",
                        colnames="character",
                        paddednames="character",
                        widths="numeric",
                        nlines="numeric"),
         contains="ViewerData")


setMethod("colWidths",
          signature("ViewerDataMySQL"),
          function(data, which=NULL) {
              # These are pre-calculated so just return relevant ones
              widths <- data@widths
              if (is.null(which))
                  widths
              else
                  widths[which]
          })

setMethod("getText",
          signature("ViewerDataMySQL"),
          function(data, rows, cols) {
              # Run the query, but limit to only the relevant
              # columns and rows
              # Original query becomes subquery
              colspec <- paste(data@colnames[cols], collapse=", ")
              # LIMIT offset is ZERO-based
              rowspec <- paste(min(rows) - 1, max(rows), sep=",")
              fullQuery <- paste("SELECT ", colspec, " FROM (",
                                 data@query, ") AS query ",
                                 "LIMIT ", rowspec,
                                 sep="")
              result <- dbGetQuery(data@dbcon, fullQuery)
              names(result) <- data@paddednames[cols]
              ow <- options(width=10000)
              on.exit(ow)
              # Drop col names AFTER producing output
              capture.output(print(result, row.names=FALSE))[-1]
          })

setMethod("dimensions",
          signature("ViewerDataMySQL"),
          function(data) {
              c(data@nlines, length(data@widths))
          })

setMethod("colNames",
          signature("ViewerDataMySQL"),
          function(data, cols) {
              paste(" ",
                    paste(data@paddednames[cols], collapse=" "),
                    sep= "")
          })

# Get num rows by just doing a SELECT COUNT(*) on the query
# Get (conesrvative) column widths from dbColumnInfo() 
queryStats <- function(query, conn) {
    countQuery <- paste("SELECT COUNT(*) AS numrows FROM (",
                        query, ") AS query",
                        sep="")
    result <- dbGetQuery(conn, countQuery)
    nrows <- result$numrows
    oneRowQuery <- paste("SELECT * FROM (",
                        query, ") AS query LIMIT 1",
                        sep="")
    result <- dbSendQuery(conn, oneRowQuery)
    colInfo <- dbColumnInfo(result)
    colnames <- colInfo$name
    widths <- colInfo$len
    dbClearResult(result)
    list(colnames=colnames,
         # "plus one" because space is put in front of
         # each column when printing
         widths=widths + 1,
         nrows=nrows)
}

viewerDataMySQL <- function(query, dbname, username, password="", host=NULL) {
    # Establish connection
    conn <- dbConnect(dbDriver("MySQL"),
                      username=username, password=password,
                      host=host, dbname=dbname)
    # Calculate summary stats for query
    stats <- queryStats(query, conn)
    new("ViewerDataMySQL", dbcon=conn, query=query,
        colnames=stats$colnames,
        paddednames=padColNames(stats$colnames, stats$widths),
        widths=stats$widths, nlines=stats$nrows)
}

# FIXME:  Need a "finalizer" to break the db connection ?
# Should this use reg.finalizer() ?
close.ViewerDataMySQL <- function(con, ...) {
    dbDisconnect(con@dbcon)
}
