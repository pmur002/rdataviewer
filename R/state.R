
# Just an S4 object
# Other options might include a closure to avoid copying?
setClass("ViewerStateSimple",
         representation(lrmode="character",
                        udmode="character",
                        fontsize="numeric"),
         prototype(lrmode="left-to-right",
                   udmode="top-to-bottom",
                   fontsize=10),
         contains="ViewerState")

viewerState <- function() {
    new("ViewerStateSimple")
}

setMethod("lrmode", signature(state="ViewerStateSimple"),
          function(state) {
              state@lrmode
          })

setMethod("udmode", signature(state="ViewerStateSimple"),
          function(state) {
              state@udmode
          })

setMethod("fontsize", signature(state="ViewerStateSimple"),
          function(state) {
              state@fontsize
          })

setMethod("lrmode<-", signature(state="ViewerStateSimple"),
          function(state, value) {
              state@lrmode <- value
              state
          })

setMethod("udmode<-", signature(state="ViewerStateSimple"),
          function(state, value) {
              state@udmode <- value
              state
          })

setMethod("fontsize<-", signature(state="ViewerStateSimple"),
          function(state, value) {
              state@fontsize <- value
              state
          })


