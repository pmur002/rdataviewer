

setClass("ViewerSimple",
         representation(dev="ViewerDevice",
                        state="ViewerState",
                        data="ViewerData",
                        rows="numeric",
                        cols="numeric"),
         prototype(cols=1,
                   rows=1),
         contains="Viewer")

simpleViewer <- function(data,
                         state=new("ViewerStateSimple"),
                         dev=new("ViewerDeviceDefault")) {
    v <- new("ViewerSimple",
             dev=dev, state=state, data=data)
    dim <- dimensions(data)
    # Initialise rows and cols based on data
    # How many chars do we have to view?
    numChars <- numChars(v@dev, fontsize(v@state))
    cumWidths <- cumsum(colWidths(v@data))
    lastCol <- min(which(cumWidths > numChars))
    v@cols <- seq(1, min(lastCol, dim[2]))
    numRows <- numRows(v@dev, fontsize(v@state))
    v@rows <- seq(1, min(numRows, dim[1]))
    v
}

# Having added or fully revealed col on right, update which
# is the leftmost column
# If zooming, there can be drastic changes, so need to consider
# more columns
updateColsLeft <- function(v, zoom=FALSE) {
    right <- max(v@cols)
    # May have made columns on left-hand-side invisible
    # so recalculate visible cols
    # How many chars do we have?
    numChars <- numChars(v@dev, fontsize(v@state))
    # How many chars are in the visible columns?
    if (zoom)
        v@cols <- 1:max(v@cols)
    widths <- colWidths(v@data, v@cols)
    # What is the new leftmost column?
    cumWidths <- rev(cumsum(rev(widths)))
    # It is possible that we need all columns
    if (all(cumWidths < numChars))
        left <- 1
    else 
        left <- v@cols[max(which(cumWidths >
                                 numChars))]
    left:right
}

# Having added or fully revealed col on left, update which
# is the rightmost column
updateColsRight <- function(v, zoom=FALSE) {
    dim <- dimensions(v@data)
    left <- min(v@cols)
    # May have made columns on right-hand-side invisible
    # so recalculate visible cols
    # How many chars do we have?
    numChars <- numChars(v@dev, fontsize(v@state))
    # How many chars are in the visible columns?
    if (zoom) 
        v@cols <- min(v@cols):(dim[2])
    widths <- colWidths(v@data, v@cols)
    # What is the new rightmost column?
    cumWidths <- cumsum(widths)
    # It is possible that we need all columns
    if (all(cumWidths < numChars))
        right <- dim[2]
    else 
        right <- v@cols[min(which(cumWidths >
                                  numChars))]
    left:right
}

# Scroll right (or left) to (fully) view the next column on the right (or left)
setMethod("lrscroll", signature(v="ViewerSimple"),
          function(v, side="right") {
              if (side == "right") {
                  if (lrmode(v@state) == "left-to-right") {
                      lrmode(v@state) <- "right-to-left"
                      v@cols <- updateColsLeft(v)
                  } else {
                      # Have we got any more columns to show?
                      dim <- dimensions(v@data)
                      lastCol <- max(v@cols)
                      nextCol <- lastCol + 1
                      if (lastCol < dim[2]) {
                          v@cols <- c(v@cols, nextCol)
                          v@cols <- updateColsLeft(v)
                      }
                  }
              } else { # side == "left"                      
                  if (lrmode(v@state) == "right-to-left") {
                      lrmode(v@state) <- "left-to-right"
                      v@cols <- updateColsRight(v)
                  } else {
                      # Have we got any more columns to show?
                      dim <- dimensions(v@data)
                      firstCol <- min(v@cols)
                      nextCol <- firstCol - 1
                      if (firstCol > 1) {
                          v@cols <- c(nextCol, v@cols)
                          v@cols <- updateColsRight(v)
                      }
                  }
              }
              v
          })

# Scroll all the way to the left
setMethod("lrhome", signature(v="ViewerSimple"),
          function(v) {
              if (lrmode(v@state) == "right-to-left") {
                  lrmode(v@state) <- "left-to-right"
              }
              v@cols <- 1:max(v@cols)
              v@cols <- updateColsRight(v)
              v
          })

# Scroll all the way to the right
setMethod("lrend", signature(v="ViewerSimple"),
          function(v) {
              if (lrmode(v@state) == "left-to-right") {
                  lrmode(v@state) <- "right-to-left"
              }
              dim <- dimensions(v@data)
              v@cols <- min(v@cols):dim[2]
              v@cols <- updateColsLeft(v)
              v
          })

# Zoom right (or left) to (fully) view the next column on the right (or left)
setMethod("lrshrink", signature(v="ViewerSimple"),
          function(v, side="right") {
              # How many chars do we 
              if (side == "right") {
                  if (lrmode(v@state) == "left-to-right") {
                      lrmode(v@state) <- "right-to-left"
                      colChars <- sum(colWidths(v@data, v@cols))
                      fontsize(v@state) <- getFontForWidth(v@dev,
                                                           fontsize(v@state),
                                                           colChars)
                  } else {
                      # Have we got any more columns to show?
                      dim <- dimensions(v@data)
                      lastCol <- max(v@cols)
                      nextCol <- lastCol + 1
                      if (lastCol < dim[2]) {
                          v@cols <- c(v@cols, nextCol)
                          colChars <- sum(colWidths(v@data, v@cols))
                          fontsize(v@state) <-
                              getFontForWidth(v@dev,
                                              fontsize(v@state),
                                              colChars)
                      }
                  }
                  v@cols <- updateColsLeft(v, zoom=TRUE)
              } else { # side == "left"
                  if (lrmode(v@state) == "right-to-left") {
                      lrmode(v@state) <- "left-to-right"
                      colChars <- sum(colWidths(v@data, v@cols))
                      fontsize(v@state) <- getFontForWidth(v@dev,
                                                           fontsize(v@state),
                                                           colChars)
                  } else {
                      # Have we got any more columns to show?
                      firstCol <- min(v@cols)
                      nextCol <- firstCol - 1
                      if (firstCol > 1) {
                          v@cols <- c(nextCol, v@cols)
                          colChars <- sum(colWidths(v@data, v@cols))
                          fontsize(v@state) <-
                              getFontForWidth(v@dev,
                                              fontsize(v@state),
                                              colChars)
                      }
                  }
                  v@cols <- updateColsRight(v, zoom=TRUE)
              }
              v@rows <- switch(udmode(v@state),
                               "top-to-bottom"=updateRowsBottom(v),
                               "bottom-to-top"=updateRowsTop(v))
              v
          })

# Zoom OUT right (or left) to remove the column on the right (or left) 
setMethod("lrgrow", signature(v="ViewerSimple"),
          function(v, side="right") {
              if (side == "right") {
                  if (lrmode(v@state) == "left-to-right") {
                      lrmode(v@state) <- "right-to-left"
                  }
                  # Have we got any more columns to remove?
                  nCols <- length(v@cols)
                  if (nCols > 1) {
                      v@cols <- v@cols[-nCols]
                  }
                  colChars <- sum(colWidths(v@data, v@cols))
                  fontsize(v@state) <- getFontForWidth(v@dev,
                                                       fontsize(v@state),
                                                       colChars,
                                                       zoom="out")
                  v@cols <- updateColsLeft(v, zoom=TRUE)
              } else { # side == "left"
                  if (lrmode(v@state) == "right-to-left") {
                      lrmode(v@state) <- "left-to-right"
                  }
                  # Have we got any more columns to remove?
                  nCols <- length(v@cols)
                  if (nCols > 1) {
                      v@cols <- v@cols[-1]
                  }
                  colChars <- sum(colWidths(v@data, v@cols))
                  fontsize(v@state) <- getFontForWidth(v@dev,
                                                       fontsize(v@state),
                                                       colChars,
                                                       zoom="out")
                  v@cols <- updateColsRight(v, zoom=TRUE)
              }
              v@rows <- switch(udmode(v@state),
                               "top-to-bottom"=updateRowsBottom(v),
                               "bottom-to-top"=updateRowsTop(v))
              v
          })

# Having added or fully revealed a row at the bottom,
# determine which is the topmost row.
# Some of these calculations are a little over-the-top
# for just scrolling, but this allows code sharing
# for both scrolling and zooming.
updateRowsTop <- function(v) {
    bottom <- max(v@rows)
    # How many rows do we have?
    numRows <- ceiling(numRows(v@dev, fontsize(v@state)))
    # What is the new top-most row?
    top <- bottom - numRows + 1
    # It is possible that we need all rows
    if (top < 1)
        top <- 1
    top:bottom
}

updateRowsBottom <- function(v) {
    dim <- dimensions(v@data)
    top <- min(v@rows) 
    # How many rows do we have?
    numRows <- ceiling(numRows(v@dev, fontsize(v@state)))
    # What is the new bottom-most row?
    bottom <- top + numRows - 1
    # It is possible that we need all rows
    if (bottom > dim[1])
        bottom <- dim[1]
    top:bottom
}

# Scroll up (or down) to (fully) view the next row (or page) above (or below)
setMethod("udscroll", signature(v="ViewerSimple"),
          function(v, side="top", page=FALSE) {
              dim <- dimensions(v@data)
              numRows <- ceiling(numRows(v@dev, fontsize(v@state)))
              if (side == "top") {
                  if (page) {
                      if (udmode(v@state) == "bottom-to-top") {
                          udmode(v@state) <- "top-to-bottom"
                      }
                      topRow <- max(1, min(v@rows) - numRows + 1)
                  } else {
                      if (udmode(v@state) == "bottom-to-top") {
                          udmode(v@state) <- "top-to-bottom"
                          topRow <- min(v@rows)
                      } else {
                          topRow <- max(1, min(v@rows) - 1)
                      }
                  }
                  bottomRow <- topRow + numRows
                  v@rows <- topRow:bottomRow
                  v@rows <- updateRowsBottom(v)
              } else { # side == "bottom"
                  if (page) {
                      if (udmode(v@state) == "top-to-bottom") {
                          udmode(v@state) <- "bottom-to-top"
                      }
                      bottomRow <- min(dim[1], max(v@rows) + numRows - 1)
                  } else {
                      if (udmode(v@state) == "top-to-bottom") {
                          udmode(v@state) <- "bottom-to-top"
                          bottomRow <- max(v@rows)
                      } else {
                          bottomRow <- min(dim[1], max(v@rows) + 1)
                      }
                  }
                  topRow <- bottomRow - numRows
                  v@rows <- topRow:bottomRow
                  v@rows <- updateRowsTop(v)
              }
              v
          })

# Scroll all the way to the top
setMethod("udhome", signature(v="ViewerSimple"),
          function(v) {
              if (udmode(v@state) == "bottom-to-top") {
                  udmode(v@state) <- "top-to-bottom"
              }
              v@rows <- 1:max(v@rows)
              v@rows <- updateRowsBottom(v)
              v
          })

# Scroll all the way to the bottom
setMethod("udend", signature(v="ViewerSimple"),
          function(v) {
              if (udmode(v@state) == "top-to-bottom") {
                  udmode(v@state) <- "bottom-to-top"
              }
              dim <- dimensions(v@data)
              v@rows <- min(v@rows):dim[1]
              v@rows <- updateRowsTop(v)
              v
          })

# If page=TRUE, DOUBLE the number of rows on display
setMethod("udshrink", signature(v="ViewerSimple"),
          function(v, side="top", page=FALSE) {
              dim <- dimensions(v@data)
              bottom <- max(v@rows)
              top <- min(v@rows)
              if (side == "top") {
                  if (page) {
                      if (udmode(v@state) == "bottom-to-top") {
                          udmode(v@state) <- "top-to-bottom"
                      }
                      top <- max(1, trunc(top - (bottom - top)))
                  } else {
                      if (udmode(v@state) == "bottom-to-top") {
                          udmode(v@state) <- "top-to-bottom"
                      } else {
                          top <- max(1, top - 1)
                      }
                  }                      
                  fontsize(v@state) <- getFontForHeight(v@dev,
                                                        fontsize(v@state),
                                                        bottom - top + 1)
                  v@rows <- top:bottom
                  v@rows <- updateRowsBottom(v)
              } else { # side == "bottom"
                  if (page) {
                      if (udmode(v@state) == "top-to-bottom") {
                          udmode(v@state) <- "bottom-to-top"
                      }
                      bottom <- min(dim[1], bottom + (bottom - top))
                  } else {
                      if (udmode(v@state) == "top-to-bottom") {
                          udmode(v@state) <- "bottom-to-top"
                      } else {
                          bottom <- min(dim[1], bottom + 1)
                      }
                  }
                  fontsize(v@state) <- getFontForHeight(v@dev,
                                                        fontsize(v@state),
                                                        bottom - top + 1)
                  v@rows <- top:bottom
                  v@rows <- updateRowsTop(v)
              }
              v@cols <- switch(lrmode(v@state),
                               "left-to-right"=updateColsRight(v, zoom=TRUE),
                               "right-to-left"=updateColsLeft(v, zoom=TRUE))
              v
          })

# If page=TRUE, HALVE the number of rows on display
setMethod("udgrow", signature(v="ViewerSimple"),
          function(v, side="top", page=FALSE) {
              dim <- dimensions(v@data)
              bottom <- max(v@rows)
              top <- min(v@rows)
              if (side == "top") {
                  if (page) {
                      if (udmode(v@state) == "bottom-to-top") {
                          udmode(v@state) <- "top-to-bottom"
                      }
                      top <- min(bottom, trunc(top + (bottom - top)/2))
                  } else {
                      if (udmode(v@state) == "bottom-to-top") {
                          udmode(v@state) <- "top-to-bottom"
                      } else {
                          top <- min(bottom, top + 1)
                      }
                  }
                  fontsize(v@state) <- getFontForHeight(v@dev,
                                                        fontsize(v@state),
                                                        bottom - top + 1,
                                                        zoom="out")
                  v@rows <- top:bottom
                  v@rows <- updateRowsBottom(v)
              } else { # side == "bottom"
                  if (page) {
                      if (udmode(v@state) == "top-to-bottom") {
                          udmode(v@state) <- "bottom-to-top"
                      }
                      bottom <- ceiling(bottom - (bottom - top)/2)
                  } else {
                      if (udmode(v@state) == "top-to-bottom") {
                          udmode(v@state) <- "bottom-to-top"
                      } else {
                          bottom <- max(top, bottom - 1)
                      }
                  }
                  fontsize(v@state) <- getFontForHeight(v@dev,
                                                        fontsize(v@state),
                                                        bottom - top + 1,
                                                        zoom="out")
                  v@rows <- top:bottom
                  v@rows <- updateRowsTop(v)
                  
              }
              v@cols <- switch(lrmode(v@state),
                               "left-to-right"=updateColsRight(v, zoom=TRUE),
                               "right-to-left"=updateColsLeft(v, zoom=TRUE))
              v
          })

setMethod("renderData",
          signature(x="ViewerSimple"),
          function(x) {
              grid.rect(gp=gpar(fill="white", lwd=.2))
              grid.clip()
              text <- getText(x@data, x@rows, x@cols)
              nr <- length(text)
              hmode <- switch(lrmode(x@state),
                              "left-to-right"=list(hjust="left", xpos=0),
                              "right-to-left"=list(hjust="right", xpos=1))
              vmode <- switch(udmode(x@state),
                              "bottom-to-top"=list(vjust="bottom",
                                ypos=unit((nr - 1):0, "lines")),
                              "top-to-bottom"=list(vjust="top",
                                ypos=unit(1, "npc") -
                                unit(0:(nr - 1), "lines")))
              grid.text(text, x=hmode$xpos, y=vmode$ypos,
              just=c(hmode$hjust, vmode$vjust),
                        gp=gpar(fontsize=fontsize(x@state), fontfamily="mono"))
              # Marker to indicate mode
              grid.clip(width=2, height=2)
              xpos <- 0:1
              # xpos <- switch(lrmode(x@state),
              #                "left-to-right"=unit(0:1, "inches"),
              #                "right-to-left"=unit(1, "npc") -
              #                                unit(0:1, "inches"))
              ypos <- switch(udmode(x@state),
                             "top-to-bottom"=1,
                             "bottom-to-top"=0)
              grid.lines(xpos, ypos, gp=gpar(col=rgb(0, 0, 0, .5), lwd=2))
              xpos <- switch(lrmode(x@state),
                             "left-to-right"=0,
                             "right-to-left"=1)
              ypos <- 0:1
              # ypos <- switch(udmode(x@state),
              #                "top-to-bottom"=unit(1, "npc") -
              #                                unit(0:1, "inches"),
              #                "bottom-to-top"=unit(0:1, "inches"))
              grid.lines(xpos, ypos, gp=gpar(col=rgb(0, 0, 0, .5), lwd=2))
          })

setMethod("renderHead",
          signature(x="ViewerSimple"),
          function(x) {
              hmode <- switch(lrmode(x@state),
                              "left-to-right"=list(hjust="left", xpos=0),
                              "right-to-left"=list(hjust="right", xpos=1))
              grid.text(colNames(x@data, x@cols),
                        x=hmode$xpos, 
                        just=c(hmode$hjust, "bottom"),
                        gp=gpar(fontsize=fontsize(x@state), fontfamily="mono"))
          })

# Given mode information and the data to view and the current text size,
# draw the data
setMethod("draw", signature(v="ViewerSimple"),
          function(v) {
              grid.newpage()
              grid.rect(gp=gpar(fill="grey90"))
              drawData(v, v@dev, fontsize(v@state))
              drawHead(v, v@dev, fontsize(v@state))
          })
          
          
    
