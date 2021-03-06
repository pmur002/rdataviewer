
###########
# Generics

######################
# For ViewerState classes
setGeneric("lrmode",
           function(state) {
               standardGeneric("lrmode")
           })

setGeneric("udmode",
           function(state) {
               standardGeneric("udmode")
           })

setGeneric("fontsize",
           function(state) {
               standardGeneric("fontsize")
           })

setGeneric("lrmode<-",
           function(state, value) {
               standardGeneric("lrmode<-")
           })

setGeneric("udmode<-",
           function(state, value) {
               standardGeneric("udmode<-")
           })

setGeneric("fontsize<-",
           function(state, value) {
               standardGeneric("fontsize<-")
           })

######################
# For ViewerData classes
setGeneric("colWidths",
           function(data, which=NULL) {
               standardGeneric("colWidths")
           })

# A ViewerData class might use caching to make this
# calculation fast (enough).
# It is important that the results of colWidths()
# and getText() are consistent for the same ViewerData class.
# For example, the column widths produced by geText()
# must remain the same no matter what row subset is being viewed.
setGeneric("getText",
           function(data, rows, cols) {
               standardGeneric("getText")
           })

setGeneric("dimensions",
           function(data) {
               standardGeneric("dimensions")
           })

setGeneric("colNames",
           function(data, cols) {
               standardGeneric("colNames")
           })

# The next two have sensible defaults so do NOT need specific methods
setGeneric("rowNames",
           function(data, rows) {
               standardGeneric("rowNames")
           })

setGeneric("rowNameWidth",
          function(data) {
               standardGeneric("rowNameWidth")
           })

######################
# For ViewerDevice classes
setGeneric("numChars",
           function(dev, fontsize) {
               standardGeneric("numChars")
           })

setGeneric("numRows",
           function(dev, fontsize) {
               standardGeneric("numRows")
           })
           
# If zooming "out" then need to fit AT LEAST numChars onto device
# If zooming "in" then need to fit AT MOST numChars onto device
# 'fontsize' gives a starting font size
# A device can set up a cache of fontsizes and precalculated
# widths if it needs the speed up.
setGeneric("getFontForWidth",
           function(dev, fontsize, numChars, zoom="out") {
               standardGeneric("getFontForWidth")
           })           

# If zooming "out" then need to fit AT LEAST numRows onto device
# If zooming "in" then need to fit AT MOST numRows onto device
# 'fontsize' gives a starting font size
setGeneric("getFontForHeight",
           function(dev, fontsize, numRows, zoom="out") {
               standardGeneric("getFontForHeight")
           })           

setGeneric("drawData",
           function(x, rows, cols, dev, fontsize) {
               standardGeneric("drawData")
           })
           
setGeneric("drawHead",
           function(x, cols, dev, fontsize) {
               standardGeneric("drawHead")
           })

setGeneric("drawRowNames",
           function(x, rows, dev, fontsize) {
               standardGeneric("drawRowNames")
           })

######################
# For Viewer classes
setGeneric("draw",
           function(v) {
               standardGeneric("draw")
           })

setGeneric("lrscroll",
           function(v, side="right", n=1) {
               standardGeneric("lrscroll")
           })

setGeneric("lrhome",
           function(v) {
               standardGeneric("lrhome")
           })
        
setGeneric("lrend",
           function(v) {
               standardGeneric("lrend")
           })
        
setGeneric("lrshrink",
           function(v, side="right", n=1) {
               standardGeneric("lrshrink")
           })

setGeneric("lrgrow",
           function(v, side="right", n=1) {
               standardGeneric("lrgrow")
           })

setGeneric("udscroll",
           function(v, side="top", page=FALSE, n=1) {
               standardGeneric("udscroll")
           })

setGeneric("udhome",
           function(v) {
               standardGeneric("udhome")
           })
        
setGeneric("udend",
           function(v) {
               standardGeneric("udend")
           })
        
setGeneric("udshrink",
           function(v, side="top", page=FALSE, n=1) {
               standardGeneric("udshrink")
           })

setGeneric("udgrow",
           function(v, side="top", page=FALSE, n=1) {
               standardGeneric("udgrow")
           })

setGeneric("renderData",
           function(x, rows, cols) {
               standardGeneric("renderData")
           })

setGeneric("renderHead",
           function(x, cols) {
               standardGeneric("renderHead")
           })

setGeneric("renderRowNames",
           function(x, rows) {
               standardGeneric("renderRowNames")
           })
