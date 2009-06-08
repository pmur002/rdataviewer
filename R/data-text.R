
# A ViewerData class that allows viewing of a text file

# The idea is that the number of lines in the file and
# the width can be determined when the object is created,
# but only the required lines will be read from the file for
# viewing (hence text file can be large because it does not
# all have to fit in memory at once).

# For large text files, you can optionally generate an index
# to each line when reading the file in, to allow seek() 

setClass("ViewerDataText",
         representation(filename="character",
                        width="numeric",
                        nlines="numeric",
                        readArgs="list",
                        index="numeric"),
         contains="ViewerData")

# There is only one column, so this is just the max line width
setMethod("colWidths",
          signature(data="ViewerDataText"),
          function(data, which=NULL) {
              data@width
          })

padText <- function(text, width) {
    nchar <- nchar(text)
    padding <- sapply(nchar,
                      function(n) {
                          paste(rep(" ", width - n), collapse="")
                      })
    paste(text, padding, sep="")
}

# Lines get trimmed OR padded to data 'width'
# 'cols' should ONLY EVER be 1
setMethod("getText",
          signature(data="ViewerDataText"),
          function(data, rows, cols) {
              if (length(data@index) == 0) { # No index was generated
                  lines <- scan(data@filename,
                                what="character",
                                sep="\n",
                                quiet=TRUE,
                                skip=max(0, min(rows) - 1),
                                n=diff(range(rows)) + 1)
                  padText(substr(lines, 1, data@width), data@width)
              } else { # Use the index to seek to the right line first
                  con <- file(data@filename, open="r")
                  seek(con, data@index[min(rows)])
                  lines <- scan(con,
                                what="character",
                                sep="\n",
                                quiet=TRUE,
                                n=diff(range(rows)) + 1)
                  close(con)
                  padText(substr(lines, 1, data@width), data@width)
              }
          })

setMethod("dimensions",
          signature(data="ViewerDataText"),
          function(data) {
              c(data@nlines, 1)
          })

# Just return the filename
setMethod("colNames",
          signature(data="ViewerDataText"),
          function(data, cols) {
              padText(substr(data@filename, 1, data@width), data@width)
          })

# Determine how many lines in the file
# and optionally generate an index of line beginnings
# Based on countLines() in 'R.utils' package, but with
# the following changes:
# - uses which() rather than R.util's whichVector()
# - does not DELETE the CR from a CRLF, but replaces it with
#   a space character instead (so that file remains the same
#   size, so that the index is correct)
# - additional 'index' argument and if that is TRUE
#   an index to each line is generated
countLines <- function(file, index=FALSE, chunkSize=50e6, ...) {
    # Argument 'file':
    file <- as.character(file)
    con <- file(file, open="rb")
    on.exit(close(con))

    LF <- as.raw(0x0a)
    CR <- as.raw(0x0d)

    isLastCR <- FALSE
    nbrOfLines <- as.integer(0)
    lineIndex <- 0
    indexBase <- 0
    
    while(TRUE) {
        bfr <- readBin(con=con, what="raw", n=chunkSize)
        if (isLastCR) {
            # Don't count LF following a CR in previous chunk.
            if (bfr[1] == LF)
                bfr[1] <- as.raw(32)
        }
        
        n <- length(bfr)
        if (n == 0)
            break

        # Replace all CRLF:s to become <SPACE>LF:s
        idxsCR <- which(bfr == CR)
        whichCR <- idxsCR
        nCR <- length(idxsCR)
        if (nCR > 0) {
            idxsCRLF <- idxsCR[(bfr[idxsCR+as.integer(1)] == LF)]
            bfr[idxsCRLF] <- as.raw(32)
            rm(idxsCRLF)
            whichCR <- which(bfr == CR)
            nCR <- length(whichCR)
        }

        # Count all CR:s and LF:s
        whichLF <- which(bfr == LF)
        nLF <- length(whichLF)
        nbrOfLines <- nbrOfLines + (nCR + nLF)
        if (index) {
            # Make sure the line breaks are in order
            lineIndex <- c(lineIndex, indexBase + sort(c(whichCR, whichLF)))
            indexBase <- indexBase + chunkSize
        }

        # If last symbol is CR it might be followed by a LF in
        # the next chunk. If so, don't count that next LF.
        isLastCR <- (bfr[n] == CR)
    } # while()

    # Don't want that index printing out accidentally!
    invisible(list(n=nbrOfLines, index=lineIndex[-length(lineIndex)]))
}

# Constructor
# The max width of lines of text is either taken
# from 'width' as a fixed number of chars
# or from 'estimate' as a number of lines of the file
# to read to estimate the max width
# If both of those are NULL, the entire file is read
# ... are passed on to readLines() or scan()
viewerDataText <- function(filename, width=NULL, estimate=NULL, index=FALSE,
                           ...) {
    readArgs <- list(...)
    if (is.null(width)) {
        if (is.null(estimate)) {
            # If index is TRUE, we can get the maxWidth more
            # efficiently from the index, so don't do it here
            if (!index)
                maxWidth <- max(nchar(do.call("readLines",
                                              c(list(con=filename),
                                                readArgs))))
        } else {
            maxWidth <- max(nchar(do.call("readLines",
                                          c(list(con=filename, n=estimate),
                                            ...))))
        }
    } else {
        maxWidth <- as.numeric(width)
    }
    lineCount <- countLines(filename, index=index)
    # If we calculatede an index and did not specify 'width' or 'estimate'
    # then calculate the maxWidth from the index
    # FIXME:  this calculation is byte-based so not exactly i18n-proof
    if (index && is.null(width) && is.null(estimate)) 
        maxWidth <- max(diff(lineCount$index))
    new("ViewerDataText",
        filename=filename, width=maxWidth, nlines=lineCount$n,
        readArgs=readArgs, index=lineCount$index)
}
