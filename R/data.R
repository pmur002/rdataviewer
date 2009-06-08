
# A simple data frame
# Other options might include another basic R data structure,
# a file (connection), or some "large data" interface,
# such as a database connection or something from the "ff" package
# or the 'bigmemory' package ...
setClass("ViewerDataFrame",
         representation(df="data.frame",
                        widths="numeric"),
         contains="ViewerData")

calcWidths <- function(data) {
    colWidth <- function(col) {
        text <- capture.output(print(col, row.names=FALSE))
        width <- max(nchar(text))
    }
    ow <- options(width=10000)
    on.exit(ow)
    widths <- numeric(length(data))
    for (i in seq_along(widths)) {
        widths[i] <- colWidth(data[i])
    }
    widths
}

setMethod("colWidths", signature(data="ViewerDataFrame"),
          function(data, which=NULL) {
              widths <- data@widths
              if (is.null(which))
                  widths
              else
                  widths[which]
          })

setMethod("getText",
          signature(data="ViewerDataFrame",
                    rows="numeric", cols="numeric"),
          function(data, rows, cols) {
              subset <- data@df[rows, cols, drop=FALSE]
              ow <- options(width=10000)
              on.exit(ow)
              # Drop col names AFTER producing output
              capture.output(print(subset, row.names=FALSE))[-1]
          })

setMethod("dimensions", signature(data="ViewerDataFrame"),
          function(data) {
              dim(data@df)
          })

setMethod("colNames",
          signature(data="ViewerDataFrame", cols="numeric"),
          function(data, cols) {
              ow <- options(width=10000)
              on.exit(ow)
              capture.output(print(data@df[1, cols, drop=FALSE],
                                   row.names=FALSE))[1]
          })

viewerDataFrame <- function(data) {
    widths <- calcWidths(data)    
    names(data) <- padColNames(names(data), widths)
    new("ViewerDataFrame",
        df=data,
        widths=widths)
}
