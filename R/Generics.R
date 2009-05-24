
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
           
# If zooming "in" then need to fit AT LEAST numChars onto device
# If zooming "out" then need to fit AT MOST numChars onto device
# 'fontsize' gives a starting font size
# A device can set up a cache of fontsizes and precalculated
# widths if it needs the speed up.
setGeneric("getFontForWidth",
           function(dev, fontsize, numChars, zoom="in") {
               standardGeneric("getFontForWidth")
           })           

# If zooming "in" then need to fit AT LEAST numRows onto device
# If zooming "out" then need to fit AT MOST numRows onto device
# 'fontsize' gives a starting font size
setGeneric("getFontForHeight",
           function(dev, fontsize, numRows, zoom="in") {
               standardGeneric("getFontForHeight")
           })           

setGeneric("drawData",
           function(x, dev, fontsize) {
               standardGeneric("drawData")
           })
           
setGeneric("drawHead",
           function(x, dev, fontsize) {
               standardGeneric("drawHead")
           })

######################
# For Viewer classes
setGeneric("draw",
           function(v) {
               standardGeneric("draw")
           })

setGeneric("lrscroll",
           function(v, side="right") {
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
           function(v, side="right") {
               standardGeneric("lrshrink")
           })

setGeneric("lrgrow",
           function(v, side="right") {
               standardGeneric("lrgrow")
           })

setGeneric("udscroll",
           function(v, side="top", page=FALSE) {
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
           function(v, side="top", page=FALSE) {
               standardGeneric("udshrink")
           })

setGeneric("udgrow",
           function(v, side="top", page=FALSE) {
               standardGeneric("udgrow")
           })

setGeneric("renderData",
           function(x) {
               standardGeneric("renderData")
           })

setGeneric("renderHead",
           function(x) {
               standardGeneric("renderHead")
           })
