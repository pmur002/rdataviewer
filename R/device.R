
setClass("ViewerDeviceDefault",
         contains="ViewerDevice")

setMethod("numChars",
          signature(dev="ViewerDeviceDefault"),
          function(dev, fontsize) {
              charWidth <-
                  convertWidth(grobWidth(textGrob("M",
                                                  gp=gpar(fontfamily="mono",
                                                    fontsize=fontsize))),
                               "inches", valueOnly=TRUE)
              devWidth <- convertWidth(unit(1, "npc"), "inches",
                                       valueOnly=TRUE)
              devWidth / charWidth
          })

setMethod("numRows",
          signature(dev="ViewerDeviceDefault"),
          function(dev, fontsize) {
              rowHeight <-
                  convertHeight(grobHeight(rectGrob(height=unit(1, "lines"),
                                                    gp=gpar(fontfamily="mono",
                                                      fontsize=fontsize))),
                                "inches", valueOnly=TRUE)
              devHeight <- convertHeight(unit(1, "npc"),
                                         "inches", valueOnly=TRUE)
              devHeight / rowHeight
          })

setMethod("getFontForWidth",
          signature(dev="ViewerDeviceDefault"),
          function(dev, fontsize, numChars, zoom="out") {
              nChar <- numChars(dev, fontsize)
              if (zoom == "out") {
                  # Just calculating font size as a proporation
                  # does not guarantee the right char width,
                  # so iterate until actually get the right
                  # number of chars
                  while (nChar < numChars) {
                      # truncate to one decimal place
                      # (i.e., be conservative)
                      fontsize <- trunc(fontsize * nChar/numChars * 10)/10
                      nChar <- numChars(dev, fontsize)
                  }
              } else { # zoom == "in"
                  while (nChar > numChars) {
                      # "truncate up" to one decimal place
                      # (i.e., be conservative)
                      fontsize <- ceiling(fontsize * nChar/numChars * 10)/10
                      nChar <- numChars(dev, fontsize)
                  }                  
              }
              fontsize
          })           

setMethod("getFontForHeight",
          signature(dev="ViewerDeviceDefault"),
          function(dev, fontsize, numRows, zoom="out") {
              nRow <- numRows(dev, fontsize)
              if (zoom == "out") {
                  while (nRow < numRows) {
                      fontsize <- trunc(fontsize * nRow/numRows * 10)/10
                      nRow <- numRows(dev, fontsize)
                  }
              } else {
                  while (nRow > numRows) {
                      fontsize <- ceiling(fontsize * nRow/numRows * 10)/10
                      nRow <- numRows(dev, fontsize)
                  }
              }
              fontsize
          })
                  
setMethod("drawData",
          signature(x="ANY", dev="ViewerDeviceDefault"),
          function(x, rows, cols, dev, fontsize) {
              # Just draw data in entire device
              renderData(x, rows, cols)
          })

setMethod("drawHead",
          signature(x="ANY", dev="ViewerDeviceDefault"),
          function(x, cols, dev, fontsize) {
              # No room set aside for headings
          })


###################
# ViewerDeviceViewport class

library(grid)

setOldClass("viewport")

setClass("ViewerDeviceViewport",
         representation(datavp="viewport",
                        headvp="viewport"),
         contains="ViewerDeviceDefault")

    # NOTE that within these methods, whenever we call a
    # NextMethod, we need to convert the ViewerDeviceViewport
    # into a ViewerDeviceDefault because otherwise any
    # calls to generics in the next method will call
    # the ViewerDeviceViewport method
    # For example, the NextMethod for getFontForWidth()
    # call numChars(), which has a method for ViewerDeviceViewport
    # which will push viewports a second time!!!

setMethod("numChars",
          signature(dev="ViewerDeviceViewport"),
          function(dev, fontsize) {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@datavp)
              # Push the viewport
              pushViewport(dev@datavp)
              # Call inherited method
              numChars <- callNextMethod(new("ViewerDeviceDefault"),
                                         fontsize)
              # Pop the viewport
              popViewport(depth + 1)
              numChars
          })

setMethod("numRows",
          signature(dev="ViewerDeviceViewport"),
          function(dev, fontsize) {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@datavp)
              # Push the viewport
              pushViewport(dev@datavp)
              # Call inherited method
              numRows <- callNextMethod(new("ViewerDeviceDefault"),
                                        fontsize)
              # Pop the viewport
              popViewport(depth + 1)
              numRows
          })

setMethod("getFontForWidth",
          signature(dev="ViewerDeviceViewport"),
          function(dev, fontsize, numChars, zoom="out") {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@datavp)
              # Push the viewport
              pushViewport(dev@datavp)
              # Call inherited method
              fontsize <- callNextMethod(new("ViewerDeviceDefault"),
                                         fontsize, numChars, zoom)
              # Pop the viewport
              popViewport(depth + 1)
              fontsize
          })

setMethod("getFontForHeight",
          signature(dev="ViewerDeviceViewport"),
          function(dev, fontsize, numRows, zoom="out") {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@datavp)
              # Push the viewport
              pushViewport(dev@datavp)
              # Call inherited method
              fontsize <- callNextMethod(new("ViewerDeviceDefault"),
                                         fontsize, numRows, zoom)
              # Pop the viewport
              popViewport(depth + 1)
              fontsize
          })

setMethod("drawData",
          signature(x="ANY", dev="ViewerDeviceViewport"),
          function(x, rows, cols, dev, fontsize) {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@datavp)
              # Push the viewport
              pushViewport(dev@datavp)
              renderData(x, rows, cols)
              # Pop the viewport
              popViewport(depth + 1)
          })

setMethod("drawHead",
          signature(x="ANY", dev="ViewerDeviceViewport"),
          function(x, cols, dev, fontsize) {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@headvp)
              # Push the viewport
              pushViewport(dev@headvp)
              renderHead(x, cols)
              # Pop the viewport
              popViewport(depth + 1)
          })
