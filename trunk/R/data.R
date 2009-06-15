
# Method for S3 generic for all ViewerData classes
# (does nothing)
close.ViewerData <- function(con, ...) { }

# Default rowNames() method for all ViewerData classes
setMethod("rowNames",
          signature(data="ViewerData"),
          function(data, rows) {
              as.character(rows)
          })

setMethod("rowNameWidth",
          signature(data="ViewerData"),
          function(data) {
              nchar(as.character(dimensions(data)[1]))
          })

# A simple vector
# Other options might include another basic R data structure,
# a file (connection), or some "large data" interface,
# such as a database connection or something from the "ff" package
# or the 'bigmemory' package ...

# The basic idea is that atomic vectors will just convert the
# appropriate subset via as.character()
# Anything else will get print()ed and the result will
# be shown as a character vector
setClass("ViewerDataVector",
         representation(x="vector",
                        name="character",
                        width="numeric"),
         contains="ViewerData")

setMethod("colWidths", signature(data="ViewerDataVector"),
          function(data, which=NULL) {
              # ONLY EVER 1 column
              data@width
          })

setMethod("getText",
          signature(data="ViewerDataVector",
                    rows="numeric", cols="numeric"),
          function(data, rows, cols) {
              subset <- data@x[rows]
              # Drop col names AFTER producing output
              format(subset, width=data@width)
          })

setMethod("dimensions", signature(data="ViewerDataVector"),
          function(data) {
              c(length(data@x), 1)
          })

setMethod("colNames",
          signature(data="ViewerDataVector", cols="numeric"),
          function(data, cols) {
              format(data@name, width=data@width)
          })

viewerData <- function(x) {
    if (!is.atomic(x)) {
        x <- capture.output(print(x))
    }
    name <- deparse(substitute(x))
    new("ViewerDataVector", x=x, name=name,
        width=max(nchar(name),
          nchar(format(x))))
}
