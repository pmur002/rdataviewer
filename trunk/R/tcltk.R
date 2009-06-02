library(tcltk)
library(tkrplot)

tcltkViewer <- function(v) {
    tt <- tktoplevel()
    # Instructions
    tl1a <- tklabel(tt, text="<n> <Left>:")
    tl1b <- tklabel(tt, text="scroll left <n> columns")
    tl2a <- tklabel(tt, text="<n> <Right>:")
    tl2b <- tklabel(tt, text="scroll right <n> columns")
    tkgrid(tl1a, column=1, row=1, sticky="e")
    tkgrid(tl1b, column=2, row=1, sticky="w")
    tkgrid(tl2a, column=1, row=2, sticky="e")
    tkgrid(tl2b, column=2, row=2, sticky="w")
    # Key bindings
    update <- function() {
        draw(v)
        nChars <<- numChars(v@dev, fontsize(v@state))
        nRows <<- numRows(v@dev, fontsize(v@state))
        tkrreplot(miniview)
    }
    scrollLeft <- function() {
        v <<- lrscroll(v, side="left")
        update()
    }
    growLeft <- function() {
        v <<- lrgrow(v, side="left")
        update()
    }
    shrinkLeft <- function() {
        v <<- lrshrink(v, side="left")
        update()
    }
    tkbind(tt, "<Left>", scrollLeft)
    tkbind(tt, "<Shift-Left>", growLeft)
    tkbind(tt, "<Control-Left>", shrinkLeft)
    scrollRight <- function() {
        v <<- lrscroll(v, side="right")
        update()
    }
    growRight <- function() {
        v <<- lrgrow(v, side="right")
        update()
    }
    shrinkRight <- function() {
        v <<- lrshrink(v, side="right")
        update()
    }
    tkbind(tt, "<Right>", scrollRight)
    tkbind(tt, "<Shift-Right>", growRight)
    tkbind(tt, "<Control-Right>", shrinkRight)
    scrollHome <- function() {
        v <<- lrhome(v)
        update()
    }
    scrollEnd <- function() {
        v <<- lrend(v)
        update()
    }
    tkbind(tt, "<Home>", scrollHome)
    tkbind(tt, "<End>", scrollEnd)
    scrollUp <- function() {
        v <<- udscroll(v, side="top")
        update()
    }
    growUp <- function() {
        v <<- udgrow(v, side="top")
        update()
    }
    shrinkUp <- function() {
        v <<- udshrink(v, side="top")
        update()
    }
    tkbind(tt, "<Up>", scrollUp)
    tkbind(tt, "<Shift-Up>", growUp)
    tkbind(tt, "<Control-Up>", shrinkUp)
    scrollDown <- function() {
        v <<- udscroll(v, side="bottom")
        update()
    }
    growDown <- function() {
        v <<- udgrow(v, side="bottom")
        update()
    }
    shrinkDown <- function() {
        v <<- udshrink(v, side="bottom")
        update()
    }
    tkbind(tt, "<Down>", scrollDown)
    tkbind(tt, "<Shift-Down>", growDown)
    tkbind(tt, "<Control-Down>", shrinkDown)
    scrollPgUp <- function() {
        v <<- udscroll(v, side="top", page=TRUE)
        update()
    }
    growPgUp <- function() {
        v <<- udgrow(v, side="top", page=TRUE)
        update()
    }
    shrinkPgUp <- function() {
        v <<- udshrink(v, side="top", page=TRUE)
        update()
    }
    tkbind(tt, "<Prior>", scrollPgUp)
    tkbind(tt, "<Shift-Prior>", growPgUp)
    tkbind(tt, "<Control-Prior>", shrinkPgUp)
    scrollPgDn <- function() {
        v <<- udscroll(v, side="bottom", page=TRUE)
        update()
    }
    growPgDn <- function() {
        v <<- udgrow(v, side="bottom", page=TRUE)
        update()
    }
    shrinkPgDn <- function() {
        v <<- udshrink(v, side="bottom", page=TRUE)
        update()
    }
    tkbind(tt, "<Next>", scrollPgDn)
    tkbind(tt, "<Shift-Next>", growPgDn)
    tkbind(tt, "<Control-Next>", shrinkPgDn)
    topLeft <- function() {
        v <<- lrhome(v)
        v <<- udhome(v)
        update()
    }
    botRight <- function() {
        v <<- lrend(v)
        v <<- udend(v)
        update()
    }
    tkbind(tt, "<Control-Home>", topLeft)
    tkbind(tt, "<Control-End>", botRight)
    # Mini overview
    # CANNOT calculate numChars or numRows in here
    # because we are dealing with a DIFFERENT DEVICE
    # from the one where the main view is being drawn
    drawMiniView <- function() {
        # FIXME: this code is specific to ViewerSimple class
        # Should use generic accessors so can work with other
        # classes derived from Viewer class
        dim <- dimensions(v@data)
        pushViewport(viewport(width=.99, height=.99))
        grid.rect(gp=gpar(fill="grey"))
        widths <- colWidths(v@data)
        fullWidth <- sum(widths)
        fullHeight <- dim[1]
        viewX <- switch(lrmode(v@state),
                        "left-to-right"=if (v@startcol == 1) 0
                        else sum(widths[1:(v@startcol - 1)]),
                        "right-to-left"=sum(widths[1:(v@startcol)]))
        hjust <- switch(lrmode(v@state),
                        "left-to-right"="left",
                        "right-to-left"="right")
        viewWidth <- nChars
        viewY <- switch(udmode(v@state),
                        "top-to-bottom"=v@startrow - 1,
                        "bottom-to-top"=v@startrow)
        vjust <- switch(udmode(v@state),
                        "top-to-bottom"="top",
                        "bottom-to-top"="bottom")
        viewHeight <- nRows
        grid.rect(viewX/fullWidth, 1 - viewY/fullHeight,
                  viewWidth/fullWidth, viewHeight/fullHeight,
                  just=c(hjust, vjust),
                  gp=gpar(col=NA))
    }
    nChars <- numChars(v@dev, fontsize(v@state))
    nRows <- numRows(v@dev, fontsize(v@state))
    width <- sum(colWidths(v@data))
    height <- dimensions(v@data)[1]
    if (height > width) {
        vscale <- .5
        hscale <- .5 * width/height
    } else {
        hscale <- .5
        vscale <- .5 * height/width
    }
    miniview <- tkrplot(tt, drawMiniView, hscale, vscale)
    tkgrid(miniview, column=1, columnspan=2)
    # Initialise the main view
    draw(v)
}
