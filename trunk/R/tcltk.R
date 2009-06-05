library(tcltk)
library(tkrplot)

tcltkViewer <- function(v, bg="grey90", region="red") {
    tt <- tktoplevel(background=bg)
    tktitle(tt) <- "rdataviewer"
    N <- tclVar("-")
    update <- function() {
        draw(v)
        nChars <<- numChars(v@dev, fontsize(v@state))
        nRows <<- numRows(v@dev, fontsize(v@state))
        tkrreplot(miniview)
        tclvalue(N) <- "-"
    }
    # Instructions
    tkrow <- 1
    addLabel <- function(tt, left, right) {
        tla <- tklabel(tt, text=left, background=bg)
        tlb <- tklabel(tt, text=paste(" ", right), background=bg)
        tkgrid(tla, column=1, row=tkrow, sticky="e")
        tkgrid(tlb, column=2, row=tkrow, sticky="w")
        tkrow <<- tkrow + 1
    }
    addLabel(tt, "<Left>:", "scroll left <n> columns")
    addLabel(tt, "<Right>:", "scroll right <n> columns")
    addLabel(tt, "<Up>:", "scroll up <n> rows")
    addLabel(tt, "<Down>:", "scroll down <n> rows")
    addLabel(tt, "<Shift>+<KEY>:", "zoom in <n> columns or rows")
    addLabel(tt, "<Control>+<KEY>:", "zoom out <n> columns or rows")
    addLabel(tt, "0-9:", "enter <n>")
    addLabel(tt, "<Escape>:", "clear <n>")
    addLabel(tt, "g:", "go to column <n>")
    addLabel(tt, "G:", "go to row <n>")
    # Alphanumeric key bindings
    keyFun <- function(n) {
        num <- tclvalue(N)
        if (num == "-")
            tclvalue(N) <- as.character(n)
        else
            tclvalue(N) <- paste(tclvalue(N), n, sep="")
    }
    key1 <- function() { keyFun(1) }
    key2 <- function() { keyFun(2) }
    key3 <- function() { keyFun(3) }
    key4 <- function() { keyFun(4) }
    key5 <- function() { keyFun(5) }
    key6 <- function() { keyFun(6) }
    key7 <- function() { keyFun(7) }
    key8 <- function() { keyFun(8) }
    key9 <- function() { keyFun(9) }
    key0 <- function() { keyFun(0) }
    tkbind(tt, "<KeyPress-1>", key1)
    tkbind(tt, "<KeyPress-2>", key2)
    tkbind(tt, "<KeyPress-3>", key3)
    tkbind(tt, "<KeyPress-4>", key4)
    tkbind(tt, "<KeyPress-5>", key5)
    tkbind(tt, "<KeyPress-6>", key6)
    tkbind(tt, "<KeyPress-7>", key7)
    tkbind(tt, "<KeyPress-8>", key8)
    tkbind(tt, "<KeyPress-9>", key9)
    tkbind(tt, "<KeyPress-0>", key0)
    blankN <- function() {
        tclvalue(N) <- "-"
    }
    tkbind(tt, "<Escape>", blankN)
    deleteN <- function() {
        num <- tclvalue(N)
        if (num != "-") {
            num <- substr(num, 1, nchar(num) - 1)
            if (nchar(num) == 0)
                num <- "-"
        }
        tclvalue(N) <- num
    }
    tkbind(tt, "<BackSpace>", deleteN)
    gotoRow <- function() {
        num <- tclvalue(N)
        if (num == "-")
            num <- dimensions(v@data)[1]
        else
            num <- as.numeric(num)
        v@startrow <<- num
        update()
    }
    gotoCol <- function() {
        num <- tclvalue(N)
        if (num == "-")
            num <- dimensions(v@data)[2]
        else
            num <- as.numeric(num)
        v@startcol <<- num
        update()
    }
    tkbind(tt, "<g>", gotoCol)
    tkbind(tt, "<G>", gotoRow)
    # Navigation key bindings
    getN <- function() {
        num <- tclvalue(N)
        if (num == "-")
            1
        else
            as.numeric(num)
    }
    scrollLeft <- function() {
        v <<- lrscroll(v, side="left", n=getN())
        update()
    }
    growLeft <- function() {
        v <<- lrgrow(v, side="left", n=getN())
        update()
    }
    shrinkLeft <- function() {
        v <<- lrshrink(v, side="left", n=getN())
        update()
    }
    tkbind(tt, "<Left>", scrollLeft)
    tkbind(tt, "<Shift-Left>", growLeft)
    tkbind(tt, "<Control-Left>", shrinkLeft)
    scrollRight <- function() {
        v <<- lrscroll(v, side="right", n=getN())
        update()
    }
    growRight <- function() {
        v <<- lrgrow(v, side="right", n=getN())
        update()
    }
    shrinkRight <- function() {
        v <<- lrshrink(v, side="right", n=getN())
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
        v <<- udscroll(v, side="top", n=getN())
        update()
    }
    growUp <- function() {
        v <<- udgrow(v, side="top", n=getN())
        update()
    }
    shrinkUp <- function() {
        v <<- udshrink(v, side="top", n=getN())
        update()
    }
    tkbind(tt, "<Up>", scrollUp)
    tkbind(tt, "<Shift-Up>", growUp)
    tkbind(tt, "<Control-Up>", shrinkUp)
    scrollDown <- function() {
        v <<- udscroll(v, side="bottom", n=getN())
        update()
    }
    growDown <- function() {
        v <<- udgrow(v, side="bottom", n=getN())
        update()
    }
    shrinkDown <- function() {
        v <<- udshrink(v, side="bottom", n=getN())
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
        # Draw every second column light grey
        grid.rect(x=cumsum(widths)[seq(1, dim[2], 2)]/fullWidth,
                  width=widths[seq(1, dim[2], 2)]/fullWidth,
                  just="right",
                  gp=gpar(col=NA, fill="white"))
        # Draw a rectangle representing the current view
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
        # Have a minimum width & height so that can still see
        # where the current region is in VERY large files
        grid.rect(viewX/fullWidth, 1 - viewY/fullHeight,
                  max(0.01, viewWidth/fullWidth),
                  max(0.01, viewHeight/fullHeight),
                  just=c(hjust, vjust),
                  gp=gpar(col=NA, fill=region))
        grid.rect(gp=gpar(fill=NA))
    }
    nChars <- numChars(v@dev, fontsize(v@state))
    nRows <- numRows(v@dev, fontsize(v@state))
    width <- sum(colWidths(v@data))
    height <- dimensions(v@data)[1]
    if (height > width) {
        vscale <- .5
        # Avoid VERY small width or height because tkrplot can't handle it
        hscale <- max(0.01, .5 * width/height)
    } else {
        hscale <- .5
        vscale <- max(0.01, .5 * height/width)
    }
    miniview <- tkrplot(tt, drawMiniView, hscale, vscale)
    tkconfigure(miniview, background=bg)
    tkgrid(miniview, column=1, columnspan=2)
    tkrow <- tkrow + 1
    tlNa <- tklabel(tt, text="<n> =   ", background=bg)
    tlNb <- tklabel(tt, textvariable=N, relief="sunken", background=bg)
    tkgrid(tlNa, column=1, row=tkrow, sticky="e")
    tkgrid(tlNb, column=2, row=tkrow, sticky="w")
    tkrow <- tkrow + 1
    # Initialise the main view
    draw(v)
}
