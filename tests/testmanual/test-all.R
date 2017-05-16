# Test Script
# 17-May-2017

library(testthat)
library(pophelper)
#devtools::test()

# Start ------------------------------------------------------------------------

#Preparation
deleteoutput <- TRUE
testfont <- FALSE
testtiff <- FALSE

if(testfont)
{
  library(extrafont)
  font_import(pattern="^Verdana",prompt=F)
  loadfonts()
}

#create a new folder and set as wd
currwd <- getwd()
if(basename(currwd) != "pophelper-demo") 
{
  dir.create(paste(currwd,"/pophelper-demo",sep=""))
  setwd(paste(currwd,"/pophelper-demo",sep=""))
}

#read sample STRUCTURE files from R package
sfiles <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
sfiles1 <- list.files(path=system.file("files/structure-ci",package="pophelper"),full.names=TRUE)
#read sample TESS files from R package
tfiles <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#read sample ADMIXTURE files from R package
afiles <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#read sample fastSTRUCTURE files from R package
ffiles <- list.files(path=system.file("files/faststructure",package="pophelper"),full.names=TRUE)
#read sample MATRIX files from R package
mcfiles <- list.files(path=system.file("files/basic/comma",package="pophelper"),full.names=TRUE)
mtfiles <- list.files(path=system.file("files/basic/tab",package="pophelper"),full.names=TRUE)
msfiles <- list.files(path=system.file("files/basic/space",package="pophelper"),full.names=TRUE)
tabs1 <- c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))
grps1 <- read.delim(system.file("files/structuregrplabels.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
grps2 <- read.delim(system.file("files/structuregrplabels2.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)

# checkQ --------------------------------------------------------------------

#checkQ
context("Check runs")
cat("checkQ ---------------------------------------------------------------\n")
cr1 <- pophelper:::checkQ(sfiles)
cr2 <- pophelper:::checkQ(tfiles)
cr3 <- pophelper:::checkQ(afiles)
cr4 <- pophelper:::checkQ(ffiles)
cr5 <- pophelper:::checkQ(mcfiles)
cr6 <- pophelper:::checkQ(mtfiles)
cr7 <- pophelper:::checkQ(msfiles)
cr8 <- pophelper:::checkQ(tabs1)
cr9 <- pophelper:::checkQ(sfiles1)
cr10 <- pophelper:::checkQ(readQ(sfiles))

test_that("check runs output",{
  expect_equal(unique(cr1$type),"STRUCTURE")
  expect_equal(unique(cr1$subtype),NA)
  expect_equal(unique(cr2$type),"TESS")
  expect_equal(unique(cr2$subtype),NA)
  expect_equal(unique(cr3$type),"BASIC")
  expect_equal(unique(cr3$subtype),"SPACE")
  expect_equal(unique(cr4$type),"BASIC")
  expect_equal(unique(cr4$subtype),"SPACE")
  expect_equal(unique(cr5$type),"BASIC")
  expect_equal(unique(cr5$subtype),"COMMA")
  expect_equal(unique(cr6$type),"BASIC")
  expect_equal(unique(cr6$subtype),"SPACE")
  expect_equal(unique(cr7$type),"BASIC")
  expect_equal(unique(cr7$subtype),"SPACE")
  expect_equal(unique(cr8$type),"CLUMPP")
  expect_equal(unique(cr8$subtype),NA)
  expect_equal(unique(cr9$type),"STRUCTURE")
  expect_equal(unique(cr10$type),"data.frame")
})

# readQ ------------------------------------------------------------------------

context("readQ")
cat("readQ ---------------------------------------------------------------\n")

test_that("Is output dataframe or list?",{
  expect_equal(class(readQ(sfiles)),"list")
  expect_equal(class(readQ(tfiles)),"list")
  expect_equal(class(readQ(afiles)),"list")
  expect_equal(class(readQ(ffiles)),"list")
  expect_equal(class(readQ(mcfiles)),"list")
  expect_equal(class(readQ(msfiles)),"list")
  expect_equal(class(readQ(mtfiles)),"list")
  expect_equal(class(readQ(tabs1)),"list")
})

test_that("error",{
  expect_error(readQ(sfiles,filetype="tess"))
  expect_error(readQ(tfiles,filetype="structure"))
  expect_error(readQ(sfiles,filetype="basic"))
  expect_error(readQ(afiles,filetype="tess"))
  expect_error(readQ(afiles,filetype="clumpp"))
  expect_error(readQ(sfiles,filetype="tess"))
  expect_error(readQ(tabs1,filetype="structure"))
  expect_error(readQ(tabs1,filetype="tess"))
})

test_that("Is output dataframe or list?",{
  expect_equal(class(pophelper:::readQStructure(sfiles)),"list")
  expect_equal(class(pophelper:::readQStructure(sfiles[1])),"list")
  expect_equal(any(is.na(as.numeric(row.names(pophelper:::readQStructure(sfiles[1],indlabfromfile = F))))),FALSE)
  expect_warning(any(is.na(as.numeric(row.names(pophelper:::readQStructure(sfiles[1],indlabfromfile = T)[[1]])))))
  expect_equal(class(pophelper:::readQTess(tfiles)),"list")
  expect_equal(class(pophelper:::readQTess(tfiles[1])),"list")
  expect_equal(class(pophelper:::readQTess(tfiles[1])[[1]]),"data.frame")
  expect_error(class(suppressWarnings(runsToDfStructure(sfiles))))
  expect_equal(class(pophelper:::readQBasic(afiles)),"list")
  expect_equal(class(pophelper:::readQBasic(afiles[1])),"list")
  expect_equal(class(pophelper:::readQBasic(afiles[1])[[1]]),"data.frame")
  expect_equal(class(pophelper:::readQBasic(ffiles)),"list")
  expect_equal(class(pophelper:::readQBasic(ffiles[1])),"list")
  expect_equal(class(pophelper:::readQBasic(ffiles[1])[[1]]),"data.frame")
  expect_equal(class(pophelper:::readQBasic(mcfiles)),"list")
  expect_equal(class(pophelper:::readQBasic(mcfiles[1])),"list")
  expect_equal(class(pophelper:::readQBasic(mcfiles[1])[[1]]),"data.frame")
  expect_equal(class(pophelper:::readQBasic(mtfiles)),"list")
  expect_equal(class(pophelper:::readQBasic(mtfiles[1])),"list")
  expect_equal(class(pophelper:::readQBasic(mtfiles[1])[[1]]),"data.frame")
  expect_equal(class(pophelper:::readQBasic(msfiles)),"list")
  expect_equal(class(pophelper:::readQBasic(msfiles[1])),"list")
  expect_equal(class(pophelper:::readQBasic(msfiles[1])[[1]]),"data.frame")
  expect_equal(class(pophelper:::readQStructure(sfiles1)),"list")
  expect_equal(class(pophelper:::readQStructure(sfiles1[1])),"list")
  expect_equal(class(pophelper:::readQStructure(sfiles1[1])[[1]]),"data.frame")
  expect_equal(class(pophelper:::readQClumpp(tabs1)),"list")
  expect_equal(class(pophelper:::readQClumpp(tabs1[1])),"list")
  expect_equal(class(pophelper:::readQClumpp(tabs1[1])[[1]]),"data.frame")
})

test_that("Error: no input",{
  expect_error(pophelper:::readQStructure())
  expect_error(pophelper:::readQTess())
  expect_error(pophelper:::readQBasic())
  expect_error(pophelper:::readQClumpp())
  expect_error(readQ())
})

test_that("Error: wrong input format",{
  expect_error(pophelper:::readQStructure(tfiles))
  expect_error(pophelper:::readQTess(sfiles))
  expect_error(pophelper:::readQBasic(sfiles))
  expect_error(pophelper:::readQClumpp(sfiles))
  expect_error(pophelper:::readQTess(tabs1))
  expect_error(readQ(tfiles,filetype="structure"))
  expect_error(readQ(sfiles,filetype="tess"))
  expect_error(readQ(sfiles,filetype="basic"))
  expect_error(readQ(sfiles,filetype="clumpp"))
  expect_error(readQ(tabs1,filetype="structure"))
})

# is.qlist ---------------------------------------------------------------------

context("is.qlist")
cat("is.qlist ---------------------------------------------------------------\n")
q1 <- readQ(sfiles)

expect_silent(is.qlist(q1))
names(q1)[2] <- NA
expect_error(is.qlist(q1))
names(q1)[2] <- ""
expect_error(is.qlist(q1))
names(q1)[2] <- "trial"
names(q1)[3] <- "trial"
expect_error(is.qlist(q1))
q1[3] <- "new"
expect_error(is.qlist(q1))

# tabulateQ -----------------------------------------------------------------

context("Tabulate")
cat("tabulateQ ---------------------------------------------------------------\n")

tr1 <- tabulateQ(qlist=readQ(sfiles))
tr2 <- tabulateQ(qlist=readQ(tfiles))
tr3 <- suppressWarnings(tabulateQ(qlist=readQ(afiles)))
tr4 <- tabulateQ(qlist=readQ(afiles))
tr5 <- tabulateQ(qlist=readQ(ffiles))
tr6 <- tabulateQ(qlist=readQ(mcfiles))
tr7 <- tabulateQ(qlist=readQ(mtfiles))
tr8 <- tabulateQ(qlist=readQ(msfiles))
tr9 <- tabulateQ(qlist=readQ(sfiles1))
tr10 <- tabulateQ(qlist=readQ(sfiles))
tr11 <- tabulateQ(qlist=readQ(tfiles))
tr12 <- tabulateQ(qlist=readQ(afiles))
tr13 <- tabulateQ(qlist=readQ(tabs1))

test_that("Is output dataframe?",{
  expect_equal(class(tr1),"data.frame")
  expect_equal(class(tr2),"data.frame")
  expect_equal(class(tr3),"data.frame")
  expect_equal(class(tr4),"data.frame")
  expect_equal(class(tr5),"data.frame")
  expect_equal(class(tr6),"data.frame")
  expect_equal(class(tr7),"data.frame")
  expect_equal(class(tr8),"data.frame")
  expect_equal(class(tr9),"data.frame")
  expect_equal(class(tr10),"data.frame")
  expect_equal(class(tr11),"data.frame")
  expect_equal(class(tr12),"data.frame")
  expect_equal(class(tr13),"data.frame")
})

test_that("sorttable=FALSE",{
  expect_equal(row.names(tabulateQ(qlist=readQ(sfiles),sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
  expect_equal(row.names(tabulateQ(qlist=readQ(tfiles),sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21"))
  expect_equal(row.names(tabulateQ(qlist=readQ(afiles),sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10"))

  expect_equal(row.names(tabulateQ(qlist=readQ(ffiles),sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10","11"))
  expect_equal(row.names(tabulateQ(qlist=readQ(mcfiles),sorttable = FALSE)),
               c("1","2","3"))
  expect_equal(row.names(tabulateQ(qlist=readQ(mtfiles),sorttable = FALSE)),
               c("1","2","3"))
  expect_equal(row.names(tabulateQ(qlist=readQ(msfiles),sorttable = FALSE)),
               c("1","2","3"))
  expect_equal(row.names(tabulateQ(qlist=readQ(sfiles),sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
})

test_that("sorttable=TRUE",{
  expect_equal(row.names(tabulateQ(qlist=readQ(sfiles),sorttable = TRUE)), c("1","2","9","3","4","10","5","11","15","6","12","16","7","13","17","8","14"))
  expect_equal(row.names(tabulateQ(qlist=readQ(tfiles),sorttable = TRUE)), c("1","8","15","2","9","16","3","10","17","4","11","18","5","12","19","6","13","20","7","14","21"))
  expect_equal(row.names(tabulateQ(qlist=readQ(afiles),sorttable = TRUE)), c("1","2","3","4","5","6","7","8","9","10"))
  expect_equal(row.names(tabulateQ(qlist=readQ(ffiles),sorttable = TRUE)), c("1","2","3","4","5","6","7","8","9","10","11"))
  expect_equal(row.names(tabulateQ(qlist=readQ(mcfiles),sorttable = TRUE)), c("1","2","3"))
  expect_equal(row.names(tabulateQ(qlist=readQ(mtfiles),sorttable = TRUE)), c("1","2","3"))
  expect_equal(row.names(tabulateQ(qlist=readQ(msfiles),sorttable = TRUE)), c("1","2","3"))
  expect_equal(row.names(tabulateQ(qlist=readQ(sfiles),sorttable = TRUE)), c("1","2","9","3","4","10","5","11","15","6","12","16","7","13","17","8","14"))
})

test_that("writetable=TRUE",{
  tabulateQ(qlist=readQ(sfiles),writetable=TRUE)
  expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateQ.txt")
  
  tabulateQ(qlist=readQ(tfiles),writetable=TRUE)
  expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateQ.txt")
  
  tabulateQ(qlist=readQ(afiles),writetable=TRUE)
  expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateQ.txt")
  
  tabulateQ(qlist=readQ(afiles),writetable=TRUE)
  expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateQ.txt")
  
  tabulateQ(qlist=readQ(ffiles),writetable=TRUE)
  expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateQ.txt")
  
  tabulateQ(qlist=readQ(mcfiles),writetable=TRUE)
  expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateQ.txt")
  
  tabulateQ(qlist=readQ(mtfiles),writetable=TRUE)
  expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateQ.txt")
  
  tabulateQ(qlist=readQ(msfiles),writetable=TRUE)
  expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateQ.txt")
  
  tabulateQ(qlist=readQ(sfiles),writetable=TRUE)
  expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateQ.txt")
})

test_that("Error: no input",{
  expect_error(tabulateQ())
  expect_error(tabulateQ(sfiles))
  expect_error(tabulateQ(afiles))
  expect_error(tabulateQ(mcfiles))
  expect_error(tabulateQ(mtfiles))
  expect_error(tabulateQ(msfiles))
  expect_error(tabulateQ(ffiles))
})

# summariseQ ----------------------------------------------------------------

context("Summarise")
cat("summariseQ ---------------------------------------------------------------\n")

test_that("writetable=TRUE",{
  sr1 <- summariseQ(tr1,writetable=TRUE)
  expect_equal(class(sr1),"data.frame")
  expect_equal("summariseQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseQ.txt")
  
  sr2 <- summariseQ(tr2,writetable=TRUE)
  expect_equal(class(sr2),"data.frame")
  expect_equal("summariseQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseQ.txt")
  
  sr3 <- suppressWarnings(summariseQ(tr3,writetable=TRUE))
  expect_equal(class(sr3),"data.frame")
  expect_equal("summariseQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseQ.txt")
  
  sr4 <- summariseQ(tr4,writetable=TRUE)
  expect_equal(class(sr4),"data.frame")
  expect_equal("summariseQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseQ.txt")
  
  sr5 <- summariseQ(tr5,writetable=TRUE)
  expect_equal(class(sr5),"data.frame")
  expect_equal("summariseQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseQ.txt")
  
  sr6 <- summariseQ(tr6,writetable=TRUE)
  expect_equal(class(sr6),"data.frame")
  expect_equal("summariseQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseQ.txt")
  
  sr7 <- summariseQ(tr7,writetable=TRUE)
  expect_equal(class(sr7),"data.frame")
  expect_equal("summariseQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseQ.txt")
  
  sr8 <- summariseQ(tr8,writetable=TRUE)
  expect_equal(class(sr8),"data.frame")
  expect_equal("summariseQ.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseQ.txt")
})

test_that("Error: no input",{
  expect_error(summariseQ())
  expect_error(summariseQ(sfiles))
  expect_error(summariseQ(afiles))
  expect_error(summariseQ(mcfiles))
})

# evannoMethodStucture ---------------------------------------------------------

context("Evanno method")
cat("evannoMethodStructure ----------------------------------------------------\n")

#returns dataframe
sr1 <- summariseQ(tr1,writetable=FALSE)
expect_equal(class(evannoMethodStructure(sr1)),"data.frame")

#export text ouput
evannoMethodStructure(sr1,writetable=TRUE)
expect_equal("evannoMethodStructure.txt" %in% list.files(),TRUE)
if(deleteoutput) file.remove("evannoMethodStructure.txt")

#PLOTS
#export plot png
evannoMethodStructure(sr1,exportplot=TRUE)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#export plot jpeg
evannoMethodStructure(sr1,exportplot=TRUE,imgtype="jpeg")
if(deleteoutput) file.remove("evannoMethodStructure.jpg")

if(testtiff)
{
  #export plot tiff
  evannoMethodStructure(sr1,exportplot=TRUE,imgtype="tiff")
  if(deleteoutput) file.remove("evannoMethodStructure.tiff")
}

#export plot pdf
evannoMethodStructure(sr1,exportplot=TRUE,imgtype="pdf")
if(deleteoutput) file.remove("evannoMethodStructure.pdf")

#change errorbar features, pointcol, linecol
evannoMethodStructure(sr1,exportplot=TRUE,ebwidth=0.1,
                      ebcol="coral",pointcol="firebrick",
                      linecol="green",textcol="blue",gridsize=0.6)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#change plot linesize, pointsize
evannoMethodStructure(sr1,exportplot=TRUE,linesize=0.9,pointsize=8)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#plot change dim, dpi, units, basesize for web plot
evannoMethodStructure(sr1,exportplot=TRUE,height=800,width=800,dpi=72,units="px",basesize=20)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#change font
if(testfont)
{
  evannoMethodStructure(sr1,exportplot=TRUE,font="Verdana")
  if(deleteoutput) file.remove("evannoMethodStructure.png")
  
  #change theme
  evannoMethodStructure(sr1,exportplot=TRUE,font="Verdana",theme="theme_grey")
  if(deleteoutput) file.remove("evannoMethodStructure.png")
}

#ERRORS
#error only 2 values of k
expect_error(evannoMethodStructure(sr1[1:2,],exportplot=TRUE))
expect_equal("kPlot.png" %in% list.files(),TRUE)
if(deleteoutput) file.remove("kPlot.png")

#error test kplot features
expect_error(evannoMethodStructure(sr1[1:2,],exportplot=TRUE,linesize=1,
                                   pointsize=8))
expect_equal("kPlot.png" %in% list.files(),TRUE)
if(deleteoutput) file.remove("kPlot.png")

#error test kplot features
expect_error(evannoMethodStructure(sr1[1:2,],exportplot=TRUE,ebwidth=0.05,
                                   ebcol="coral",pointcol="firebrick",
                                   linecol="green",textcol="blue",
                                   gridsize=0.6,theme="theme_grey",font="Verdana"))
expect_equal("kPlot.png" %in% list.files(),TRUE)
if(deleteoutput) file.remove("kPlot.png")

if(testfont)
{
  expect_error(evannoMethodStructure(sr1[1:2,],exportplot=TRUE,font="Verdana"))
  expect_equal("kPlot.png" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("kPlot.png")  
}

#error non sequential values of k
expect_error(evannoMethodStructure(sr1[c(1,2,4),],exportplot=TRUE))
expect_equal("kPlot.png" %in% list.files(),TRUE)
if(deleteoutput) file.remove("kPlot.png")

#warning less than 2 runs
expect_warning(evannoMethodStructure(summariseQ(tr1[1:16,])[4:6,]))

test_that("Errors",{
  expect_error(evannoMethodStructure())
  expect_error(evannoMethodStructure(sfiles))
  expect_error(evannoMethodStructure(readQ(tfiles)))
  expect_error(evannoMethodStructure(readQ(sfiles)))
})

# clumppExport -----------------------------------------------------------------

#clumppExport
context("Clumpp Output")
cat("clumppExport -------------------------------------------------------------\n")

clumppExport(readQ(sfiles))
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

if(FALSE) {
  
#structure clumpp export check prefix
clumppExport(readQ(sfiles),prefix="Boom")
expect_equal(all(grepl("Boom",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("Boom*", recursive = TRUE, force = TRUE)

#structure clumpp export useexe
clumppExport(readQ(sfiles),useexe=T)
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#tess clumpp export check
clumppExport(readQ(tfiles))
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#tess clumpp export check prefix
clumppExport(readQ(tfiles),prefix="Hahaha")
expect_equal(all(grepl("Hahaha",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("Hahaha*", recursive = TRUE, force = TRUE)

#tess clumpp export check
clumppExport(readQ(tfiles),useexe=TRUE)
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#matrix clumpp export check
clumppExport(readQ(afiles),useexe=TRUE)
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#tructure clumpp export check prefix
clumppExport(readQ(sfiles),prefix="Nanana")
expect_equal(all(grepl("Nanana",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("Nanana*", recursive = TRUE, force = TRUE)

#matrix clumpp export check
clumppExport(readQ(afiles))
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#matrix clumpp export check
clumppExport(readQ(ffiles))
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#structure clumpp list export check
clumppExport(readQ(sfiles))
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

context("Clumpp Output useexe")

#structure clumpp export useexe
clumppExport(readQ(sfiles),useexe=T)
expect_equal(sum(grepl("aligned",list.files(recursive=T))),6)
expect_equal(sum(grepl("merged",list.files(recursive=T))),6)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#tess clumpp export useexe
clumppExport(readQ(tfiles),useexe=T)
expect_equal(sum(grepl("aligned",list.files(recursive=T))),7)
expect_equal(sum(grepl("merged",list.files(recursive=T))),7)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#matrix admixture clumpp export useexe
clumppExport(readQ(afiles),useexe=T)
expect_equal(sum(grepl("aligned",list.files(recursive=T))),1)
expect_equal(sum(grepl("merged",list.files(recursive=T))),1)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

}

# collectClumppOutput ----------------------------------------------------------

context("Collect clumpp output")
cat("collectClumppOutput ------------------------------------------------------\n")

clumppExport(readQ(sfiles),useexe=T)

collectClumppOutput(filetype = "aligned")
expect_equal(sum(grepl("aligned",list.dirs())),1)

collectClumppOutput(filetype = "merged")
expect_equal(sum(grepl("merged",list.dirs())),1)

collectClumppOutput(filetype = "both")
expect_equal(sum(grepl("both",list.dirs())),1)

if(deleteoutput) unlink("pop*", recursive=TRUE, force=TRUE)

# getPlotParams ----------------------------------------------------------------

context("getPlotParams")
cat("getPlotParams ------------------------------------------------------------\n")

pophelper:::getPlotParams(grplab=grps1$V1, plotnum=1)
pophelper:::getPlotParams(grplab=grps1$V1, plotnum=2)
pophelper:::getPlotParams(grplab=grps1$V1, plotnum=1,grplabsize=5,grplabangle=90,grplabjust=0.5,pointsize=2,linesize=1)

# grpLabels --------------------------------------------------------------------

context("grpLabels")
cat("grpLabels ---------------------------------------------------------------\n")

grps1 <- read.delim(system.file("files/structuregrplabels.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
test_that("check if grps df",{expect_equal(class(grps1),"data.frame")})

grps2 <- read.delim(system.file("files/structuregrplabels2.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
test_that("check if grps df",{expect_equal(class(grps2),"data.frame")})

slist <- readQ(sfiles[1])

test_that("grplab data.frame error",{
expect_error(pophelper:::grpLabels(df=slist[[1]],grplab=grps1$V1))
})

test_that("return list",{
expect_equal(class(pophelper:::grpLabels(df=slist[[1]],grplab=grps1)),"list")
})

test_that("subsetgrp not present in grplabel",{
expect_error(pophelper:::grpLabels(df=slist[[1]],grplab=grps1,subsetgrp="popk"))
})

test_that("subsetgrp Pop B",{
  expect_equal(unique(pophelper:::grpLabels(df=slist[[1]],grplab=grps1,subsetgrp="Pop B")$grplab)$V1,"Pop B")
})

test_that("subsetgrp change order",{
  expect_equal(unique(pophelper:::grpLabels(df=slist[[1]],grplab=grps1,subsetgrp=c("Pop B","Pop A"))$grplab)$V1,c("Pop B","Pop A"))
})

test_that("ordergrp",{
  expect_equal(unique(pophelper:::grpLabels(df=slist[[1]],grplab=grps1)$grplab)$V1,c("Pop A","Pop B"))
})

# sortInd ----------------------------------------------------------------------

test_that("return NA",{
expect_equal(pophelper:::sortInd(slist[[1]])$grplab,NA)
})

grplabs <- read.delim(system.file("files/grplab.txt",package="pophelper"),stringsAsFactors=F)

test_that("original order",{
expect_equal(rownames(pophelper:::sortInd(slist[[1]],grplab=grplabs)$dframe),as.character(1:149))
})

test_that("error sortind",{
expect_error(pophelper:::sortInd(slist[[1]],grplab=grplabs,sortind="blue"))
})

pophelper:::sortInd(slist[[1]],grplab=grplabs,sortind="all")
pophelper:::sortInd(slist[[1]],grplab=grplabs,sortind="label")
pophelper:::sortInd(slist[[1]],grplab=grplabs,sortind="Cluster2")
pophelper:::sortInd(slist[[1]],grplab=grplabs,selgrp="mixed",sortind="all")

# plotQ  --------------------------------------------------------------

context("plotQ")
cat("plotQ ----------------------------------------------------------\n")

slist <- readQ(sfiles)
grpsrep <- read.delim(system.file("files/structuregrplabels-rep.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
grplabs <- read.delim(system.file("files/grplab.txt",package="pophelper"),stringsAsFactors=F)

imgout <- c("sep","join")

for(i in 1:length(imgout))
{
  
  imgo <- imgout[i]
  
  #plot one sep
  plotQ(slist[1])
  
  #plot many
  plotQ(slist[1:2],imgoutput=imgo)
  
  #plot many outputfilename
  plotQ(slist[1:2],imgoutput="sep",outputfilename=c("this","that"))
  
  #showsp
  plotQ(slist[1:2],imgoutput=imgo,showsp=F)
  
  #showsp
  plotQ(slist[1:2],imgoutput=imgo,splab=c("this","that"),splabsize=8,splabcol="blue",splabface="bold",spbgcol="green")
  
  #change barsize
  plotQ(slist[1:2],imgoutput=imgo,barsize=0.8)
  
  #one ind plot
  t1 <- slist[1:2]
  t1[[1]] <- t1[[1]][1,]
  t1[[2]] <- t1[[2]][1,]
  plotQ(t1[1:2],imgoutput=imgo)
  
  #1 ind with lab
  plotQ(t1[1:2],imgoutput=imgo,grplab=data.frame("lab"="A",stringsAsFactors=F))
  
  #check orderind cluster
  plotQ(slist[1:2],imgoutput=imgo,sortind="Cluster1")
  
  #check orderind all
  plotQ(slist[1:2],imgoutput=imgo,sortind="all")
  
  #check orderind label
  plotQ(slist[1:2],imgoutput=imgo,sortind="label")
  
  #clustercol
  plotQ(slist[1:2],imgoutput=imgo,clustercol=c("red","green"))
  
  #single labels
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE])
  
  #single labels showsp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],splab=c("this","that"),splabsize=8,splabcol="blue",splabface="bold",spbgcol="green")
  
  #showsp pos
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],splab=c("this","that"),sppos="left")
  
  #single labels ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE)
  
  #single labels ordergrp sortind all
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="all")
  
  #single labels ordergrp sortind Cluster1
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="Cluster1")
  
  #single labels ordergrp sortind label
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="label")
  
  #single labels ordergrp sortind label show indlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="label",showindlab=TRUE)
  
  #multiple labels
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs)
  
  #multiple labels grpmean
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T)
  
  #multiple labels grpmean ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T,ordergrp=T)
  
  #multiple labels ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE)
  
  #multiple labels selgrp ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",ordergrp=TRUE)
  
  #multiple labels selgrp ordergrp divgrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",ordergrp=TRUE,divgrp="lab2")
  
  #multiple labels selgrp ordergrp divgrp double
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",ordergrp=TRUE,divgrp=c("lab2","lab1"))
  
  #multiple labels selgrp ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="lab2",ordergrp=TRUE)
  
  #multiple labels ordergrp sortind all
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE,sortind="all")
  
  #multiple labels selgrp ordergrp sortind all
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="lab2",ordergrp=TRUE,sortind="all")
  
  #multiple labels sortind Cluster1
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,sortind="Cluster1")
  
  #multiple labels ordergrp sortind label
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE,sortind="label")
  
  #multiple labels ordergrp sortind label show indlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE,sortind="label",showindlab=TRUE)
  
  #multiple labels grpmean
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T)
  
  #multiple labels grpmean showindlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T,showindlab=TRUE,width=15)
  
  #multiple labels ordergrp subsetgrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=T,subsetgrp="b")
  
  #multiple labels ordergrp reorder
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=T,subsetgrp=c("c","b"))
  
  #multiple labels selgrp ordergrp reorder
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",ordergrp=T,subsetgrp=c("fds","erd"))
  
  #multiple labels selgrp ordergrp reorder showindlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",ordergrp=T,subsetgrp=c("fds","erd"),showindlab=TRUE,width=15)
  
  #single label line size
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,linesize=2,linecol="green",linetype="22",linealpha=0.6,pointsize=0.6,pointtype=16,pointalpha=0.6,pointcol="red")
  
  #imgtype jpeg
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,imgtype="jpeg")
  
  #imgtype pdf
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,imgtype="pdf")
  
  if(testtiff)
  {
    #imgtype tiff
    plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,imgtype="tiff")
  }
  
  #check output sep with rep labels
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep)
  
  #check grpmean with rep labels
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,grpmean=T)
  
  #check grpmean with rep labels sortind
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,sortind="all")
  
  #check grpmean with rep labels sortind
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,sortind="label")
  
  #check grpmean with rep labels sortind showindlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,sortind="label",showindlab=T,width=15)
  
  expect_error(plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,subsetgrp="Pop A"))
  
  #use ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,subsetgrp="Pop A",ordergrp=T)
}

#ERRORS

#input not list
expect_error(plotQ(slist[[1]]))

#check less colours
expect_error(plotQ(slist[1],clustercol="red"))

#showsp
expect_error(plotQ(slist[1],showsp="a"))

#splab
expect_error(plotQ(slist[1],splab=NULL))

if(testfont)
{
  #change font
  plotQ(qlist=slist[1:2],grplab=grplabs,font="Verdana")
  
  #join change font
  plotQ(qlist=slist[1:2],imgoutput="join",grplab=grplabs,font="Verdana")
  
  if(testtiff)
  {
    #tiff output single
    plotQ(qlist=slist[1],grplab=grplabs,font="Verdana",imgtype="tiff")
  }
}

if(deleteoutput) file.remove(list.files())

# plotQ Clumpp --------------------------------------------------------------

context("plotQ Clumpp")
cat("plotQ Clumpp --------------------------------------------------------------\n")

tabs1 <- c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))

#check output table
plotQ(qlist=readQ(tabs1))

#check output table
plotQ(qlist=readQ(tabs1),grpmean=T)

#check output table sort cluster
plotQ(qlist=readQ(tabs1),sortind="Cluster1")

#check output table sort cluster
plotQ(qlist=readQ(tabs1),imgoutput="join",sortind="Cluster1")

#check output table pop lab
plotQ(qlist=readQ(tabs1),grplab=grps1)

#check output table pop lab sort all
plotQ(qlist=readQ(tabs1),imgoutput="sep",grplab=grplabs,sortind="all")

#check output table pop lab sort cluster
plotQ(qlist=readQ(tabs1),imgoutput="sep",grplab=grplabs,sortind="Cluster1")

#check output table pop lab pop mean
plotQ(qlist=readQ(tabs1),imgoutput="sep",grplab=grplabs,grpmean=T)

#check output table pop lab rep
plotQ(qlist=readQ(tabs1),imgoutput="sep",grplab=grpsrep)

#check output table pop lab rep sort
plotQ(qlist=readQ(tabs1),imgoutput="sep",grplab=grpsrep,sortind="all")

#check output table pop lab rep sort pop mean
plotQ(qlist=readQ(tabs1),imgoutput="sep",grplab=grpsrep,sortind="all",grpmean=T)

if(deleteoutput) file.remove(list.files())

expect_error(plotQ(qlist=readQ(tabs1),imgoutput="sep",grplab=grpsrep,subsetgrp="Pop B"))

# plotQMultiline ---------------------------------------------------------------
cat("plotQMultiline -----------------------------------------------------------\n")

slist <- readQ(list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE))
grpsrep <- read.delim(system.file("files/structuregrplabels-rep.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
grplabs <- read.delim(system.file("files/grplab.txt",package="pophelper"),stringsAsFactors=F)

#plotQMultiline
context("plotQMultiline")

#sfiles 1 check output
plotQMultiline(slist[1])

#sfiles 1 check output sort cluster
plotQMultiline(slist[1],sortind="Cluster1")

#sfiles 1 check output sort all
plotQMultiline(slist[1],sortind="all")

#sfiles >1 check output
plotQMultiline(slist[1:2])

#sfiles >1 sort cluster
plotQMultiline(slist[1:2],sortind="Cluster1")

#sfiles >1 sort all
plotQMultiline(slist[1:2],sortind="all")

#sfiles 1 sort NA indlabfromfile
plotQMultiline(slist[1:2],sortind=NA,useindlab=T)

#sfiles 1 sort all indlabfromfile=T
plotQMultiline(slist[1:2],sortind="all",useindlab=T)

#jpeg output
plotQMultiline(slist[1],imgtype="jpeg")

if(testtiff)
{
  #tiff output
  plotQMultiline(slist[1],imgtype="tiff")
}

#pdf output
plotQMultiline(slist[1],imgtype="pdf")

inds <- read.delim(system.file("files/structureindlabels.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)

#files 1 indlab
rownames(slist[[1]]) <- inds$V1
plotQMultiline(slist[1],useindlab=T)

#sfiles 1 check output sort cluster1 indlab
plotQMultiline(slist[1],sortind="Cluster1",useindlab=T)

#grplab with useind one lab
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs[,1,drop=F],useindlab=T)

#grplab with useind two lab
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs[,1:2],useindlab=T)

#grplab with useind
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T)

#grplab with useind and selgrp
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T,selgrp="lab2")

#grplab with useind and selgrp, ordergrp
plotQMultiline(slist[1],barsize=1,lpp=6,grplab=grplabs,useindlab=T,selgrp="mixed",ordergrp=T)

#grplab with useind, sortind cluster1
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T, sortind="Cluster1")

#grplab with useind, sortind all
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T, sortind="all")

#grplab with useind, sortind label
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T, sortind="label")

#grplab without useind, sortind label
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=F, sortind="label")

#grplab without useind, subset
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=F, subsetgrp="b")

#grplab without useind, reorder
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=F, subsetgrp=c("b","a"))

#grplab without useind, reorder, selgrp, orgergrp
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=F, selgrp="mixed",ordergrp=TRUE,subsetgrp=c("fds","rga"))

#grplab without useind, sort cluster, subset
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=F, sortind="Cluster1", subsetgrp=c("c"))

#grplab with useind, sort cluster, reorder
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T, sortind="Cluster1", subsetgrp=c("b","c"))

#grplab without useind, sort label, reorder
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs, grpmean=T)

#modify options
plotQMultiline(slist[1],lpp=7,grplab=grplabs,indlabsize=5,indlabangle=45,indlabvjust=1,indlabhjust=1,indlabcol="darkgreen",
               grplabsize=4,grplabbgcol="skyblue",grplabcol="red",clustercol=c("coral","steelblue"),showticks=T,showyaxis=T,
               mar=c(0.1,0.5,0.1,0.5),theme="theme_bw")

#output file type
plotQMultiline(slist[1],grplab=grplabs,imgtype="jpeg")

#output file type
plotQMultiline(slist[1],grplab=grplabs,imgtype="pdf")

#output file type
if(testtiff) plotQMultiline(slist[1],grplab=grplabs,imgtype="tiff")

#grplab theme font
if(testfont) plotQMultiline(slist[1],theme="theme_bw",font="Verdana")

if(deleteoutput) file.remove(list.files())

# collectTessRuns --------------------------------------------------------------

context("Collect Tess runs ----------------------------------------------------\n")

# analyseQ ------------------------------------------------------------------

context("analyseQ Structure")
cat("analyseQ Structure ------------------------------------------------------\n")

analyseQ(sfiles)
test_that("check output",{
  expect_equal(any(grepl("evannoMethodStructure.png",list.files())),TRUE)
  expect_equal(any(grepl("evannoMethodStructure.txt",list.files())),TRUE)
  expect_equal(any(grepl("tabulateQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("structure_01.png",list.files())),TRUE)
  expect_equal(any(grepl("structure_17.png",list.files())),TRUE)
  expect_equal(any(grepl("./pop_K2",list.dirs())),TRUE)
  expect_equal(any(grepl("./pop_K7",list.dirs())),TRUE)
})
if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
if(deleteoutput) file.remove(list.files())

context("analyseQ Tess")
cat("analyseQ Tess\n")

analyseQ(tfiles)
test_that("check output",{
  expect_equal(any(grepl("tabulateQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("tess_01.png",list.files())),TRUE)
  expect_equal(any(grepl("tess_21.png",list.files())),TRUE)
  expect_equal(any(grepl("./pop_K2",list.dirs())),TRUE)
  expect_equal(any(grepl("./pop_K8",list.dirs())),TRUE)
})
if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
if(deleteoutput) file.remove(list.files())

context("analyseQ Admixture")
cat("analyseQ Admixture\n")

analyseQ(afiles)
test_that("check output",{
  expect_equal(any(grepl("tabulateQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("admixture_01.png",list.files())),TRUE)
  expect_equal(any(grepl("admixture_10.png",list.files())),TRUE)
  expect_equal(any(grepl("./pop_K2",list.dirs())),TRUE)
})
if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
if(deleteoutput) file.remove(list.files())

# distructExport Structure -----------------------------------------------------

context("distructExport Structure")
cat("distructExport Structure ------------------------------------------------\n")

grps1 <- read.delim(system.file("files/structuregrplabels.txt",package="pophelper"),header=FALSE)

#ind one file
distructExport(qlist = readQ(sfiles)[1])
test_that("destructExport structure one file",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#both top and bottom labels
distructExport(readQ(sfiles)[1],grplabbottom=as.character(grps1$V1),grplabtop=as.character(grps1$V1),grpmean=T)
test_that("destructExport structure top and bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#both top and bottom labels quiet
distructExport(readQ(sfiles)[1],grplabbottom=as.character(grps1$V1),grplabtop=as.character(grps1$V1),quiet=T)
test_that("destructExport structure top and botoom labels quiet",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#no bottom labels
distructExport(readQ(sfiles)[1],grplabbottom=NA,grplabtop=as.character(grps1$V1))
test_that("destructExport structure no bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#no top labels
distructExport(readQ(sfiles)[1],grplabbottom=as.character(grps1$V1),grplabtop=NA)
test_that("destructExport structure no top labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#no both labels
distructExport(readQ(sfiles)[1],grplabbottom=NA,grplabtop=NA)
test_that("destructExport structure both labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files
distructExport(readQ(sfiles)[2:5],grplabbottom=as.character(grps1$V1))
test_that("destructExport structure multiple files",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#pop mean true
distructExport(readQ(sfiles)[1],grplabbottom=as.character(grps1$V1),grpmean=T)
test_that("destructExport structure grpmean true",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files with grpmean
distructExport(readQ(sfiles)[2:5],grplabbottom=as.character(grps1$V1),grpmean=T)
test_that("destructExport structure multiple files",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#other colours
distructExport(readQ(sfiles)[1],grplabbottom=as.character(grps1$V1),clustercol=pophelper:::distructColours()[43:44])
test_that("destructExport structure other colours",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files use exe
if(pophelper:::getOS()!="unix64")
{
  distructExport(readQ(sfiles)[2:5],grplabbottom=as.character(grps1$V1),useexe=T)
  test_that("destructExport structure use exe",{
    expect_equal(length(list.files()),4)
  })
  if(deleteoutput) unlink(list.files(),recursive=T,force=T)
}

#multiple files grpmean use exe
if(pophelper:::getOS()!="unix64")
{
  distructExport(readQ(sfiles)[2:5],grplabbottom=as.character(grps1$V1),grpmean=T,useexe=T)
  test_that("destructExport structure grpmean use exe",{
    expect_equal(length(list.files()),4)
  })
  if(deleteoutput) unlink(list.files(),recursive=T,force=T)
}

#other colours use exe
if(pophelper:::getOS()!="unix64")
{
  distructExport(readQ(sfiles)[1],grplabbottom=as.character(grps1$V1),clustercol=pophelper:::distructColours()[43:44],useexe=T)
  test_that("destructExport structure other colours use exe",{
    expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
  })
  if(deleteoutput) unlink(list.files(),recursive=T,force=T)
}

# distructExport Tess ----------------------------------------------------------

context("exportDistruct Tess")
cat("distructExport Tess ------------------------------------------------------\n")

grps1 <- read.delim(system.file("files/tessgrplabels.txt",package="pophelper"),header=FALSE)

#both top and bottom labels
distructExport(readQ(tfiles)[3],grplabbottom=as.character(grps1$V1),grplabtop=as.character(grps1$V1))
test_that("exportDistruct Tess top and bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./tess_03-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#both top and bottom labels quiet
distructExport(readQ(tfiles)[3],grplabbottom=as.character(grps1$V1),grplabtop=as.character(grps1$V1),quiet=T)
test_that("exportDistruct Tess top and bottom labels quiet",{
  expect_equal(any(grepl("drawparams",list.files("./tess_03-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#no bottom labels
distructExport(readQ(tfiles)[3],grplabbottom=NA,grplabtop=as.character(grps1$V1))
test_that("exportDistruct Tess no bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./tess_03-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files
distructExport(readQ(tfiles)[2:5],grplabbottom=as.character(grps1$V1))
test_that("exportDistruct Tess multiple files",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files with grpmean
distructExport(readQ(tfiles)[2:5],grplabbottom=as.character(grps1$V1),grpmean=T)
test_that("exportDistruct Tess pop mean",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

# distructExport basic --------------------------------------------------------

if(deleteoutput) unlink(list.files(),recursive=T,force=T)
context("distructExport Other formats")
cat("distructExport Basic -----------------------------------------------------\n")

#admixture multiple
distructExport(qlist = readQ(afiles)[1:4],grpmean=F)
test_that("exportDistruct Admixture",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#admixture multiple grpmean
distructExport(qlist = readQ(afiles)[1:4],grpmean=T)
test_that("exportDistruct Admixture pop mean",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#faststructure
distructExport(qlist = readQ(ffiles)[2:3],grpmean=F)
test_that("exportDistruct faststructure",{
  expect_equal(length(list.files()),2)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#basic files comma
distructExport(qlist = readQ(mcfiles)[2:3],grpmean=F)
test_that("exportDistruct basic Comma",{
  expect_equal(length(list.files()),2)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#basic files tab
distructExport(qlist = readQ(mtfiles)[2:3],grpmean=F)
test_that("exportDistruct basic tab",{
  expect_equal(length(list.files()),2)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#basic files space
distructExport(qlist = readQ(msfiles)[2:3],grpmean=F)
test_that("exportDistruct basic space",{
  expect_equal(length(list.files()),2)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#tab no grpmean
distructExport(qlist = readQ(tabs1),grpmean=F)
test_that("exportDistruct Admixture",{
  expect_equal(length(list.files()),7)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#tab grpmean
distructExport(qlist = readQ(tabs1),grpmean=T)
test_that("exportDistruct Admixture",{
  expect_equal(length(list.files()),7)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#tab no grpmean use exe
if(pophelper:::getOS()!="unix64")
{
  distructExport(qlist = readQ(tabs1),useexe=T)
  test_that("exportDistruct Admixture",{
    expect_equal(length(list.files()),7)
  })
  if(deleteoutput) unlink(list.files(),recursive=T,force=T)
}

# End --------------------------------------------------------------------------

setwd(currwd)
if(deleteoutput) unlink("pophelper-demo",recursive = T,force = T)

