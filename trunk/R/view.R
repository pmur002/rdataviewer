
# Top-level convenience function

viewData <- function(datasrc) {
    tcltkViewer(simpleViewer(datasrc, dev=viewerDeviceVp(datasrc)))
}

setGeneric("view",
           function(datasrc) {
               standardGeneric("view")
           })

setMethod("view",
          signature(datasrc="ViewerData"),
          function(datasrc) {
              viewData(datasrc)
          })

# If we are given a raw R data structure, try to figure out
# something to do with it.
setMethod("view",
          signature(datasrc="ANY"),
          function(datasrc) {
              if (is.data.frame(datasrc))
                  viewData(viewerDataFrame(datasrc))
              else
                  viewData(viewerData(datasrc,
                                      name=deparse(substitute(datasrc))))
          })

