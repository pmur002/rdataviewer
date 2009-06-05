

setClass("ViewerSimple",
         representation(dev="ViewerDevice",
                        state="ViewerState",
                        data="ViewerData",
                        startcol="numeric",
                        startrow="numeric",
                        # Size below which just draw lines
                        threshold="numeric"), 
         prototype(startcol=1,
                   startrow=1,
                   threshold=6),
         contains="Viewer")

simpleViewer <- function(data,
                         state=new("ViewerStateSimple"),
                         dev=new("ViewerDeviceDefault")) {
    v <- new("ViewerSimple",
             dev=dev, state=state, data=data)
    v
}

# In right-to-left mode, the startcol is the right-most column.
# Which is the left-most column?
viewerColsLeft <- function(v, numChars) {
    right <- v@startcol
    # How many chars are in the visible columns?
    cols <- 1:right
    widths <- colWidths(v@data, cols)
    # What is the new leftmost column?
    cumWidths <- rev(cumsum(rev(widths)))
    # It is possible that we need all columns
    if (all(cumWidths < numChars))
        left <- 1
    else 
        left <- cols[max(which(cumWidths >
                               numChars))]
    left
}

# In left-to-right mode, the startcol is the left-most column.
# Which is the right-most column?
viewerColsRight <- function(v, dim, numChars) {
    left <- v@startcol
    # How many chars are in the visible columns?
    cols <- left:(dim[2])
    widths <- colWidths(v@data, cols)
    # What is the new rightmost column?
    cumWidths <- cumsum(widths)
    # It is possible that we need all columns
    if (all(cumWidths < numChars))
        right <- dim[2]
    else 
        right <- cols[min(which(cumWidths >
                                numChars))]
    right
}

# In bottom-to-top mode, the startrow is the bottom-most-row.
# Which is the top-most row?
viewerRowsTop <- function(v, numRows) {
    bottom <- v@startrow
    # What is the new top-most row?
    max(1, bottom - numRows + 1)
}

# In top-to-bottom mode, the startrow is the top-most row.
# Which is the bottom-most row?
viewerRowsBottom <- function(v, dim, numRows) {
    top <- v@startrow
    # What is the new bottom-most row?
    min(dim[1], top + numRows - 1)
}

# Scroll right (or left) to (fully) view the next column on the right (or left)
setMethod("lrscroll", signature(v="ViewerSimple"),
          function(v, side="right", n=1) {
              dim <- dimensions(v@data)
              nChars <- numChars(v@dev, fontsize(v@state))
              if (side == "right") {
                  if (lrmode(v@state) == "left-to-right") {
                      lrmode(v@state) <- "right-to-left"
                      v@startcol <- viewerColsRight(v, dim, nChars) + n - 1
                  } else {
                      v@startcol <- min(dim[2], v@startcol + n)
                  }
              } else { # side == "left"                      
                  if (lrmode(v@state) == "right-to-left") {
                      lrmode(v@state) <- "left-to-right"
                      v@startcol <- viewerColsLeft(v, nChars) - n + 1
                  } else {
                      v@startcol <- max(1, v@startcol - n)
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
              v@startcol <- 1
              v
          })

# Scroll all the way to the right
setMethod("lrend", signature(v="ViewerSimple"),
          function(v) {
              if (lrmode(v@state) == "left-to-right") {
                  lrmode(v@state) <- "right-to-left"
              }
              dim <- dimensions(v@data)
              v@startcol <- dim[2]
              v
          })

# Zoom right (or left) to (fully) view the next column on the right (or left)
setMethod("lrshrink", signature(v="ViewerSimple"),
          function(v, side="right", n=1) {
              dim <- dimensions(v@data)
              nChars <- numChars(v@dev, fontsize(v@state))
              if (side == "right") {
                  if (lrmode(v@state) == "left-to-right") {
                      lrmode(v@state) <- "right-to-left"
                      right <- min(dim[2],
                                   viewerColsRight(v, dim, nChars) + n - 1)
                      colChars <- sum(colWidths(v@data, (v@startcol):right))
                      fontsize(v@state) <- getFontForWidth(v@dev,
                                                           fontsize(v@state),
                                                           colChars)
                      v@startcol <- right
                  } else {
                      nextCol <- min(dim[2], v@startcol + n)
                      # Have we got any more columns to show?
                      if (v@startcol < dim[2]) {
                          left <- viewerColsLeft(v, nChars)
                          v@startcol <- nextCol
                          colChars <- sum(colWidths(v@data, left:(v@startcol)))
                          fontsize(v@state) <-
                              getFontForWidth(v@dev,
                                              fontsize(v@state),
                                              colChars)
                      }
                  }
              } else { # side == "left"
                  if (lrmode(v@state) == "right-to-left") {
                      lrmode(v@state) <- "left-to-right"
                      left <- max(1, viewerColsLeft(v, nChars) - n + 1)
                      colChars <- sum(colWidths(v@data, left:(v@startcol)))
                      fontsize(v@state) <- getFontForWidth(v@dev,
                                                           fontsize(v@state),
                                                           colChars)
                      v@startcol <- left 
                  } else {
                      nextCol <- max(1, v@startcol - n)
                      # Have we got any more columns to show?
                      if (v@startcol > 1) {
                          right <- viewerColsRight(v, dim, nChars)
                          v@startcol <- nextCol
                          colChars <- sum(colWidths(v@data,
                                                    (v@startcol):right))
                          fontsize(v@state) <-
                              getFontForWidth(v@dev,
                                              fontsize(v@state),
                                              colChars)
                      }
                  }
              }
              v
          })

# Zoom OUT right (or left) to remove the column on the right (or left) 
setMethod("lrgrow", signature(v="ViewerSimple"),
          function(v, side="right", n=1) {
              dim <- dimensions(v@data)
              nChars <- numChars(v@dev, fontsize(v@state))
              if (side == "right") {
                  if (lrmode(v@state) == "left-to-right") {
                      lrmode(v@state) <- "right-to-left"
                      left <- v@startcol
                      right <- viewerColsRight(v, dim, nChars)
                  } else {
                      left <- viewerColsLeft(v, nChars)
                      right <- v@startcol
                  }
                  # Have we got any more columns to remove?
                  nCols <- right - left + 1
                  if (nCols > n) {
                      right <- right - n
                      v@startcol <- right
                      colChars <- sum(colWidths(v@data, left:right))
                      fontsize(v@state) <- getFontForWidth(v@dev,
                                                           fontsize(v@state),
                                                           colChars,
                                                           zoom="in")
                  }
              } else { # side == "left"
                  if (lrmode(v@state) == "right-to-left") {
                      lrmode(v@state) <- "left-to-right"
                      left <- viewerColsLeft(v, nChars)
                      right <- v@startcol
                  } else {
                      left <- v@startcol
                      right <- viewerColsRight(v, dim, nChars)
                  }
                  # Have we got any more columns to remove?
                  nCols <- right - left + 1
                  if (nCols > n) {
                      left <- left + n
                      v@startcol <- left
                      colChars <- sum(colWidths(v@data, left:right))
                      fontsize(v@state) <- getFontForWidth(v@dev,
                                                           fontsize(v@state),
                                                           colChars,
                                                           zoom="in")
                  }
              }
              v
          })

# Scroll up (or down) to (fully) view the next row (or page) above (or below)
setMethod("udscroll", signature(v="ViewerSimple"),
          function(v, side="top", page=FALSE, n=1) {
              dim <- dimensions(v@data)
              nRows <- ceiling(numRows(v@dev, fontsize(v@state)))
              if (page)
                  n <- nRows
              if (side == "top") {
                  if (udmode(v@state) == "bottom-to-top") {
                      udmode(v@state) <- "top-to-bottom"
                      v@startrow <- max(1,
                                        viewerRowsTop(v, nRows) -
                                        n + 1)
                  } else {
                      v@startrow <- max(1, v@startrow - n)
                  }
              } else { # side == "bottom"
                  if (udmode(v@state) == "top-to-bottom") {
                      udmode(v@state) <- "bottom-to-top"
                      v@startrow <- min(dim[1],
                                        viewerRowsBottom(v, dim, nRows) +
                                        n - 1)
                  } else {
                      v@startrow <- min(dim[1], v@startrow + n)
                  }
              }
              v
          })

# Scroll all the way to the top
setMethod("udhome", signature(v="ViewerSimple"),
          function(v) {
              if (udmode(v@state) == "bottom-to-top") {
                  udmode(v@state) <- "top-to-bottom"
              }
              v@startrow <- 1
              v
          })

# Scroll all the way to the bottom
setMethod("udend", signature(v="ViewerSimple"),
          function(v) {
              if (udmode(v@state) == "top-to-bottom") {
                  udmode(v@state) <- "bottom-to-top"
              }
              dim <- dimensions(v@data)
              v@startrow <- dim[1]
              v
          })

# If page=TRUE, DOUBLE the number of rows on display
setMethod("udshrink", signature(v="ViewerSimple"),
          function(v, side="top", page=FALSE, n=1) {
              dim <- dimensions(v@data)
              nRows <- ceiling(numRows(v@dev, fontsize(v@state)))
              if (page)
                  n <- nRows
              if (side == "top") {
                  if (udmode(v@state) == "bottom-to-top") {
                      udmode(v@state) <- "top-to-bottom"
                      v@startrow <- max(1,
                                        viewerRowsTop(v, nRows) - n + 1)
                  } else {
                      v@startrow <- max(1, v@startrow - n)
                  }
                  fontsize(v@state) <- getFontForHeight(v@dev,
                                                        fontsize(v@state),
                                                        min(dim[1],
                                                            nRows + n))
              } else { # side == "bottom"
                  if (udmode(v@state) == "top-to-bottom") {
                      udmode(v@state) <- "bottom-to-top"
                      v@startrow <- min(dim[1],
                                        viewerRowsBottom(v, dim, nRows) +
                                        n - 1)
                  } else {
                      v@startrow <- min(dim[1], v@startrow + n)
                  }
                  fontsize(v@state) <- getFontForHeight(v@dev,
                                                        fontsize(v@state),
                                                        min(dim[1],
                                                            nRows + n))
              }
              v
          })

# If page=TRUE, HALVE the number of rows on display
setMethod("udgrow", signature(v="ViewerSimple"),
          function(v, side="top", page=FALSE, n=1) {
              dim <- dimensions(v@data)
              nRows <- ceiling(numRows(v@dev, fontsize(v@state)))
              if (page)
                  n <- trunc(nRows/2)
              if (side == "top") {
                  if (udmode(v@state) == "bottom-to-top") {
                      udmode(v@state) <- "top-to-bottom"
                      v@startrow <- min(dim[1],
                                        viewerRowsTop(v, nRows) + n - 1)
                  } else {
                      v@startrow <- min(dim[1], v@startrow + n)
                  }
                  fontsize(v@state) <-
                      getFontForHeight(v@dev,
                                       fontsize(v@state),
                                       max(1, nRows - n),
                                       zoom="in")
              } else { # side == "bottom"
                  if (udmode(v@state) == "top-to-bottom") {
                      udmode(v@state) <- "bottom-to-top"
                      v@startrow <- max(1,
                                        viewerRowsBottom(v, dim, nRows) -
                                        n + 1)
                  } else {
                      v@startrow <- max(1, v@startrow - n)
                  }
                  fontsize(v@state) <-
                      getFontForHeight(v@dev,
                                       fontsize(v@state),
                                       max(1, nRows - n),
                                       zoom="in") 
              }
              v
          })

renderText <- function(x, rows, cols) {
    text <- getText(x@data, rows, cols)
    nr <- length(text)
    # Extra 0.1 so text is not right up against border
    hmode <- switch(lrmode(x@state),
                    "left-to-right"=list(hjust="left",
                      xpos=unit(0.1, "lines")),
                    "right-to-left"=list(hjust="right",
                      xpos=unit(1, "npc") - unit(0.1, "lines")))
    # Extra 0.1 so text is not right up against border
    vmode <- switch(udmode(x@state),
                    "bottom-to-top"=list(vjust="bottom",
                      ypos=unit(0.1 + (nr - 1):0, "lines")),
                    "top-to-bottom"=list(vjust="top",
                      ypos=unit(1, "npc") -
                      unit(0.1 + 0:(nr - 1), "lines")))
    grid.text(text, x=hmode$xpos, y=vmode$ypos,
              just=c(hmode$hjust, vmode$vjust),
              gp=gpar(fontsize=fontsize(x@state), fontfamily="mono"))
}

renderLines <- function(x, rows, cols) {
    text <- getText(x@data, rows, cols)
    nr <- length(text)
    nChars <- numChars(x@dev, fontsize(x@state))
    lines <- gregexpr("[^ ]+", text)
    nlines <- sapply(lines, length)
    first <- unlist(lines) - 1
    last <- first + unlist(lapply(lines, attr, "match.length")) 
    xstart <- switch(lrmode(x@state),
                     "left-to-right"=first/nChars,
                     "right-to-left"=1 - (max(last) - first)/nChars)
    xend <- switch(lrmode(x@state),
                   "left-to-right"=last/nChars,
                   "right-to-left"=1 - (max(last) - last)/nChars)
    ypos <- switch(udmode(x@state),
                   "bottom-to-top"=unit(rep(0.1 + (nr - 1):0 + 0.5,
                     nlines), "lines"),
                   "top-to-bottom"=unit(1, "npc") -
                     unit(rep(0.1 + 0:(nr - 1) + 0.5, nlines), "lines"))
    grid.segments(xstart, ypos, xend, ypos,
                  gp=gpar(col=rgb(.5, .5, .6, .5)))
}

setMethod("renderData",
          signature(x="ViewerSimple"),
          function(x, rows, cols) {
              grid.rect(gp=gpar(fill="white", lwd=.2))
              grid.clip()
              if (fontsize(x@state) >= x@threshold)
                  renderText(x, rows, cols)
              else
                  renderLines(x, rows, cols)
              # Marker to indicate mode
              grid.clip(width=2, height=2)
              xpos <- 0:1
              ypos <- switch(udmode(x@state),
                             "top-to-bottom"=1,
                             "bottom-to-top"=0)
              grid.lines(xpos, ypos, gp=gpar(col=rgb(0, 0, 0, .5), lwd=2))
              xpos <- switch(lrmode(x@state),
                             "left-to-right"=0,
                             "right-to-left"=1)
              ypos <- 0:1
              grid.lines(xpos, ypos, gp=gpar(col=rgb(0, 0, 0, .5), lwd=2))
          })

setMethod("renderHead",
          signature(x="ViewerSimple"),
          function(x, cols) {
              if (fontsize(x@state) >= x@threshold) {
                  hmode <- switch(lrmode(x@state),
                                  "left-to-right"=list(hjust="left", xpos=0),
                                  "right-to-left"=list(hjust="right", xpos=1))
                  grid.text(colNames(x@data, cols),
                            x=hmode$xpos, 
                            just=c(hmode$hjust, "bottom"),
                            gp=gpar(fontsize=fontsize(x@state),
                              fontfamily="mono"))
              } else {
                  text <- colNames(x@data, cols)
                  nChars <- numChars(x@dev, fontsize(x@state))
                  lines <- gregexpr("[^ ]+", text)
                  nlines <- sapply(lines, length)
                  first <- unlist(lines) - 1
                  last <- first + unlist(lapply(lines, attr, "match.length")) 
                  xstart <- switch(lrmode(x@state),
                                   "left-to-right"=first/nChars,
                                   "right-to-left"=1 - (max(last) -
                                     first)/nChars)
                  xend <- switch(lrmode(x@state),
                                 "left-to-right"=last/nChars,
                                 "right-to-left"=1 - (max(last) - last)/nChars)
                  grid.segments(xstart, unit(.5, "lines"),
                                xend, unit(.5, "lines"),
                                gp=gpar(col=rgb(.5, .5, .6, .5)))
              }
          })

viewerRowsAndCols <- function(v) {
    dim <- dimensions(v@data)
    nChars <- numChars(v@dev, fontsize(v@state))
    nRows <- ceiling(numRows(v@dev, fontsize(v@state)))
    if (lrmode(v@state) == "left-to-right")
        cols <- v@startcol:viewerColsRight(v, dim, nChars)
    else
        cols <- viewerColsLeft(v, nChars):v@startcol
    if (udmode(v@state) == "top-to-bottom")
        rows <- v@startrow:viewerRowsBottom(v, dim, nRows)
    else
        rows <- viewerRowsTop(v, nRows):v@startrow
    list(rows=rows, cols=cols)
}

drawDetails.ViewerDataGrob <- function(x, recording) {
    grid.rect(gp=gpar(fill="grey90"))
    rc <- viewerRowsAndCols(x$v)
    drawData(x$v, rc$rows, rc$cols, x$v@dev, fontsize(x$v@state))
    drawHead(x$v, rc$cols, x$v@dev, fontsize(x$v@state))
}

# Given mode information and the data to view and the current text size,
# draw the data
setMethod("draw", signature(v="ViewerSimple"),
          function(v) {
              grid.newpage()
              grid.draw(grob(v=v, cl="ViewerDataGrob"))
          })
          
          
    
