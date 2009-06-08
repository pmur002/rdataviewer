
source("rdataviewer/R/Classes.R")
source("rdataviewer/R/Generics.R")
source("rdataviewer/R/common.R")
source("rdataviewer/R/data.R")
source("rdataviewer/R/data-text.R")
source("rdataviewer/R/data-mysql.R")
source("rdataviewer/R/state.R")
source("rdataviewer/R/device.R")
source("rdataviewer/R/viewer.R")
source("rdataviewer/R/tcltk.R")

#########################
# ViewerDataFrame examples
# mtcars
temp <- mtcars
rownames(temp) <- NULL
cars <- cbind(name=rownames(mtcars), temp)
data <- viewerDataFrame(cars)

# exoplanets
data <- viewerDataFrame(read.csv("exoplanets.csv"))

#########################
# ViewerDataText
data <- viewerDataText("exoplanets.csv")

# Metrix
# Of course this only works on stat18
data <- viewerDataText("/scratch/Metrix/Data/MEENDL10092008_001-lines.XML",
                       estimate=100)
# Takes about 20s to count the number of lines and generate an index! 
data <- viewerDataText("/scratch/Metrix/Data/MEENDL30102008_001-lines.XML",
                       index=TRUE)

# Large test file
# writeLines(as.character(1:4000000), "/scratch/large.txt")
data <- viewerDataText("/scratch/large.txt", index=TRUE)

#########################
# ViewerDataMySQL examples
data <- viewerDataMySQL("select * from pottery_table",
                        "china", "stat220", "2202009")

# Do a larger STATS 220 data set ?
data <- viewerDataMySQL("select * from innings_tbl",
                        "cricket", "stat220", "2202009")

# Do a large public database ?
data <- viewerDataMySQL("select * from seq_region",
                        "homo_sapiens_core_46_36h", "anonymous",
                        host="ensembldb.ensembl.org")

vdv <- new("ViewerDeviceViewport",
           datavp=viewport(x=unit(2, "mm"), y=unit(2, "mm"),
             width=unit(1, "npc") - unit(4, "mm"),
             # 1.5 lines for col headings
             height=unit(1, "npc") - unit(1.5, "lines") -
             unit(4, "mm"),
             just=c("left", "bottom")),
           headvp=viewport(x=unit(2, "mm"),
             y=unit(1, "npc") - unit(2, "mm"),
             width=unit(1, "npc") - unit(4, "mm"),
             height=unit(1.5, "lines"),
             just=c("left", "top")))

           
v <- simpleViewer(data, dev=vdv)
         
tcltkViewer(v)


#############
# Testing by hand
draw(v)

v <- lrscroll(v)
draw(v)

v <- lrscroll(v, side="left")
draw(v)

v <- lrshrink(v)
draw(v)

v <- lrshrink(v, side="left")
draw(v)

v <- lrgrow(v)
draw(v)

v <- lrgrow(v, side="left")
draw(v)

v <- udscroll(v)
draw(v)

v <- udscroll(v, side="bottom")
draw(v)

v <- udscroll(v, page=TRUE)
draw(v)

v <- udscroll(v, side="bottom", page=TRUE)
draw(v)

v <- udgrow(v)
draw(v)

v <- udgrow(v, side="bottom")
draw(v)

v <- udgrow(v, page=TRUE)
draw(v)

v <- udgrow(v, side="bottom", page=TRUE)
draw(v)

v <- udshrink(v)
draw(v)

v <- udshrink(v, side="bottom")
draw(v)

v <- udshrink(v, page=TRUE)
draw(v)

v <- udshrink(v, side="bottom", page=TRUE)
draw(v)


