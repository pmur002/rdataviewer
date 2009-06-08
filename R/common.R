
# Pad the column names with spaces.  That way, whenever we
# print the data frame, the columns stay the same width.
padColNames <- function(names, widths) {
    unlist(mapply(function(name, width) {
        nc <- nchar(name)
        if (nc + 1 < width) {
            name <- paste(paste(rep(" ", width - nc - 1), collapse=""),
                          name, sep="")
        } else {
            name
        }
    }, names, widths))
}

