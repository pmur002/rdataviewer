
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
          function(dev, fontsize, numChars, zoom="in") {
              nChar <- numChars(dev, fontsize)
              if (zoom == "in") {
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
              } else { # zoom == "out"
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
          function(dev, fontsize, numRows, zoom="in") {
              nRow <- numRows(dev, fontsize)
              if (zoom == "in") {
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
          function(x, dev, fontsize) {
              # Just draw data in entire device
              renderData(x)
          })

setMethod("drawHead",
          signature(x="ANY", dev="ViewerDeviceDefault"),
          function(x, dev, fontsize) {
              # No room set aside for headings
          })


###################
# ViewerDeviceViewport class

setOldClass("viewport")

setClass("ViewerDeviceViewport",
         representation(datavp="viewport",
                        headvp="viewport"),
         contains="ViewerDeviceDefault")

setMethod("numChars",
          signature(dev="ViewerDeviceViewport"),
          function(dev, fontsize) {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@datavp)
              # Push the viewport
              pushViewport(dev@datavp)
              # Call inherited method
              numChars <- callNextMethod()
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
              numRows <- callNextMethod()
              # Pop the viewport
              popViewport(depth + 1)
              numRows
          })

setMethod("getFontForWidth",
          signature(dev="ViewerDeviceViewport"),
          function(dev, fontsize, numChars, zoom="in") {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@datavp)
              # Push the viewport
              pushViewport(dev@datavp)
              # Call inherited method
              fontsize <- callNextMethod()
              # Pop the viewport
              popViewport(depth + 1)
              fontsize
          })

setMethod("getFontForHeight",
          signature(dev="ViewerDeviceViewport"),
          function(dev, fontsize, numRows, zoom="in") {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@datavp)
              # Push the viewport
              pushViewport(dev@datavp)
              # Call inherited method
              fontsize <- callNextMethod()
              # Pop the viewport
              popViewport(depth + 1)
              fontsize
          })

setMethod("drawData",
          signature(x="ANY", dev="ViewerDeviceViewport"),
          function(x, dev, fontsize) {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@datavp)
              # Push the viewport
              pushViewport(dev@datavp)
              renderData(x)
              # Pop the viewport
              popViewport(depth + 1)
          })

setMethod("drawHead",
          signature(x="ANY", dev="ViewerDeviceViewport"),
          function(x, dev, fontsize) {
              pushViewport(viewport(gp=gpar(fontsize=fontsize)))
              depth <- grid:::depth(dev@headvp)
              # Push the viewport
              pushViewport(dev@headvp)
              renderHead(x)
              # Pop the viewport
              popViewport(depth + 1)
          })
