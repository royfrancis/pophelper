# Test Script
# 25-Jul-2016

library(testthat)
library(pophelper)

#devtools::test()

#Preparation
deleteoutput = TRUE
#create a new folder and set as wd
currwd <- getwd()
dir.create(paste(currwd,"/pophelper-demo",sep=""))
setwd(paste(currwd,"/pophelper-demo",sep=""))
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
mcfiles <- list.files(path=system.file("files/matrix/comma",package="pophelper"),full.names=TRUE)
mtfiles <- list.files(path=system.file("files/matrix/tab",package="pophelper"),full.names=TRUE)
msfiles <- list.files(path=system.file("files/matrix/space",package="pophelper"),full.names=TRUE)
tabs1 <- c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))
pops <- read.delim(system.file("files/structurepoplabels.txt",package="pophelper"),header=FALSE)
#-------------------------------------------------------------------------------

#checkRuns
context("Check runs")
cr1 <- pophelper:::checkRuns(sfiles)
cr2 <- pophelper:::checkRuns(tfiles)
cr3 <- pophelper:::checkRuns(afiles)
cr4 <- pophelper:::checkRuns(ffiles)
cr5 <- pophelper:::checkRuns(mcfiles)
cr6 <- pophelper:::checkRuns(mtfiles)
cr7 <- pophelper:::checkRuns(msfiles)
cr8 <- pophelper:::checkRuns(tabs1)
cr9 <- pophelper:::checkRuns(sfiles1)

test_that("check runs output",{
  expect_equal(unique(cr1$type),"STRUCTURE")
  expect_equal(unique(cr1$subtype),NA)
  expect_equal(unique(cr2$type),"TESS")
  expect_equal(unique(cr2$subtype),NA)
  expect_equal(unique(cr3$type),"MATRIX")
  expect_equal(unique(cr3$subtype),"SPACE")
  expect_equal(unique(cr4$type),"MATRIX")
  expect_equal(unique(cr4$subtype),"SPACE")
  expect_equal(unique(cr5$type),"MATRIX")
  expect_equal(unique(cr5$subtype),"COMMA")
  expect_equal(unique(cr6$type),"MATRIX")
  expect_equal(unique(cr6$subtype),"SPACE")
  expect_equal(unique(cr7$type),"MATRIX")
  expect_equal(unique(cr7$subtype),"SPACE")
  expect_equal(unique(cr8$type),"TAB")
  expect_equal(unique(cr8$subtype),NA)
  expect_equal(unique(cr9$type),"STRUCTURE")
})

#tabulateRuns
context("Tabulate")
tr1 <- tabulateRunsStructure(files=sfiles)
tr2 <- tabulateRunsTess(files=tfiles)
tr3 <- suppressWarnings(tabulateRunsAdmixture(files=afiles))
tr4 <- tabulateRunsMatrix(files=afiles)
tr5 <- tabulateRunsMatrix(files=ffiles)
tr6 <- tabulateRunsMatrix(files=mcfiles)
tr7 <- tabulateRunsMatrix(files=mtfiles)
tr8 <- tabulateRunsMatrix(files=msfiles)
tr9 <- tabulateRunsStructure(files=sfiles1)

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
})

test_that("quiet=FALSE",{
  expect_output(tabulateRunsStructure(files=sfiles, quiet=FALSE),"Number of files selected: 17")
  expect_output(tabulateRunsTess(files=tfiles, quiet=FALSE),"Number of files selected: 21")
  expect_output(suppressWarnings(tabulateRunsAdmixture(files=afiles, quiet=FALSE)),"Number of files selected: 10")
  expect_output(tabulateRunsMatrix(files=afiles, quiet=FALSE),"Number of files selected: 10")
  expect_output(tabulateRunsMatrix(files=ffiles, quiet=FALSE),"Number of files selected: 11")
  expect_output(tabulateRunsMatrix(files=mcfiles, quiet=FALSE),"Number of files selected: 3")
  expect_output(tabulateRunsMatrix(files=mtfiles, quiet=FALSE),"Number of files selected: 3")
  expect_output(tabulateRunsMatrix(files=msfiles, quiet=FALSE),"Number of files selected: 3")
})

test_that("quiet=TRUE",{
  expect_silent(tabulateRunsStructure(files=sfiles, quiet=TRUE))
  expect_silent(tabulateRunsTess(files=tfiles, quiet=TRUE))
  expect_silent(suppressWarnings(tabulateRunsAdmixture(files=afiles, quiet=TRUE)))
  expect_silent(tabulateRunsMatrix(files=afiles, quiet=TRUE))
  expect_silent(tabulateRunsMatrix(files=ffiles, quiet=TRUE))
  expect_silent(tabulateRunsMatrix(files=mcfiles, quiet=TRUE))
  expect_silent(tabulateRunsMatrix(files=mtfiles, quiet=TRUE))
  expect_silent(tabulateRunsMatrix(files=msfiles, quiet=TRUE))
})

test_that("sorttable=FALSE",{
  expect_equal(row.names(tabulateRunsStructure(files=sfiles, quiet=FALSE,sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
  expect_equal(row.names(tabulateRunsTess(files=tfiles, quiet=FALSE,sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21"))
  tr3 <- suppressWarnings(tabulateRunsAdmixture(files=afiles, quiet=FALSE,sorttable = FALSE))
  expect_equal(row.names(tr3),
               c("1","2","3","4","5","6","7","8","9","10"))
  expect_equal(row.names(tabulateRunsMatrix(files=afiles, quiet=FALSE,sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10"))
  expect_equal(row.names(tabulateRunsMatrix(files=ffiles, quiet=FALSE,sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10","11"))
  expect_equal(row.names(tabulateRunsMatrix(files=mcfiles, quiet=FALSE,sorttable = FALSE)),
               c("1","2","3"))
  expect_equal(row.names(tabulateRunsMatrix(files=mtfiles, quiet=FALSE,sorttable = FALSE)),
               c("1","2","3"))
  expect_equal(row.names(tabulateRunsMatrix(files=msfiles, quiet=FALSE,sorttable = FALSE)),
               c("1","2","3"))
})

test_that("sorttable=TRUE",{
  expect_equal(row.names(tabulateRunsStructure(files=sfiles,sorttable = TRUE)), c("1","2","9","3","4","10","5","11","15","6","12","16","7","13","17","8","14"))
  expect_equal(row.names(tabulateRunsTess(files=tfiles,sorttable = TRUE)), c("1","8","15","2","9","16","3","10","17","4","11","18","5","12","19","6","13","20","7","14","21"))
  expect_equal(row.names(suppressWarnings(tabulateRunsAdmixture(files=afiles,sorttable = TRUE))), c("1","2","3","4","5","6","7","8","9","10"))
  expect_equal(row.names(tabulateRunsMatrix(files=afiles,sorttable = TRUE)), c("1","2","3","4","5","6","7","8","9","10"))
  expect_equal(row.names(tabulateRunsMatrix(files=ffiles,sorttable = TRUE)), c("1","2","3","4","5","6","7","8","9","10","11"))
  expect_equal(row.names(tabulateRunsMatrix(files=mcfiles,sorttable = TRUE)), c("1","2","3"))
  expect_equal(row.names(tabulateRunsMatrix(files=mtfiles,sorttable = TRUE)), c("1","2","3"))
  expect_equal(row.names(tabulateRunsMatrix(files=msfiles,sorttable = TRUE)), c("1","2","3"))
})

test_that("writetable=TRUE",{
  tabulateRunsStructure(files=sfiles,writetable=TRUE)
  expect_equal("tabulateRunsStructure.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateRunsStructure.txt")
  
  tabulateRunsTess(files=tfiles,writetable=TRUE)
  expect_equal("tabulateRunsTess.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateRunsTess.txt")
  
  suppressWarnings(tabulateRunsAdmixture(files=afiles,writetable=TRUE))
  expect_equal("tabulateRunsAdmixture.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateRunsAdmixture.txt")
  
  tabulateRunsMatrix(files=afiles,writetable=TRUE)
  expect_equal("tabulateRunsMatrix.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateRunsMatrix.txt")
  
  tabulateRunsMatrix(files=ffiles,writetable=TRUE)
  expect_equal("tabulateRunsMatrix.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateRunsMatrix.txt")
  
  tabulateRunsMatrix(files=mcfiles,writetable=TRUE)
  expect_equal("tabulateRunsMatrix.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateRunsMatrix.txt")
  
  tabulateRunsMatrix(files=mtfiles,writetable=TRUE)
  expect_equal("tabulateRunsMatrix.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateRunsMatrix.txt")
  
  tabulateRunsMatrix(files=msfiles,writetable=TRUE)
  expect_equal("tabulateRunsMatrix.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("tabulateRunsMatrix.txt")
})

test_that("Error: no input",{
  expect_error(tabulateRunsStructure())
  expect_error(tabulateRunsTess())
  expect_error(suppressWarnings(tabulateRunsAdmixture()))
  expect_error(tabulateRunsMatrix())
})

#-------------------------------------------------------------------------------

#summariseRuns
context("Summarise")

test_that("writetable=TRUE",{
  sr1 <- summariseRunsStructure(tr1,writetable=TRUE)
  expect_equal(class(sr1),"data.frame")
  expect_equal("summariseRunsStructure.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseRunsStructure.txt")
  
  sr2 <- summariseRunsTess(tr2,writetable=TRUE)
  expect_equal(class(sr2),"data.frame")
  expect_equal("summariseRunsTess.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseRunsTess.txt")
  
  sr3 <- suppressWarnings(summariseRunsAdmixture(tr3,writetable=TRUE))
  expect_equal(class(sr3),"data.frame")
  expect_equal("summariseRunsAdmixture.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseRunsAdmixture.txt")
  
  sr4 <- summariseRunsMatrix(tr4,writetable=TRUE)
  expect_equal(class(sr4),"data.frame")
  expect_equal("summariseRunsMatrix.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseRunsMatrix.txt")
  
  sr5 <- summariseRunsMatrix(tr5,writetable=TRUE)
  expect_equal(class(sr5),"data.frame")
  expect_equal("summariseRunsMatrix.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseRunsMatrix.txt")
  
  sr6 <- summariseRunsMatrix(tr6,writetable=TRUE)
  expect_equal(class(sr6),"data.frame")
  expect_equal("summariseRunsMatrix.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseRunsMatrix.txt")
  
  sr7 <- summariseRunsMatrix(tr7,writetable=TRUE)
  expect_equal(class(sr7),"data.frame")
  expect_equal("summariseRunsMatrix.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseRunsMatrix.txt")
  
  sr8 <- summariseRunsMatrix(tr8,writetable=TRUE)
  expect_equal(class(sr8),"data.frame")
  expect_equal("summariseRunsMatrix.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("summariseRunsMatrix.txt")
})

test_that("Error: no input",{
  expect_error(summariseRunsStructure())
  expect_error(summariseRunsTess())
  expect_error(suppressWarnings(summariseRunsAdmixture()))
  expect_error(summariseRunsMatrix())
})

#-------------------------------------------------------------------------------

#evannoMethodStucture
context("Evanno method")
sr1 <- summariseRunsStructure(tr1,writetable=FALSE)
test_that("Is output dataframe?",{
  expect_equal(class(evannoMethodStructure(sr1)),"data.frame")
})
evannoMethodStructure(sr1,writetable=TRUE)
test_that("writetable=TRUE",{
  expect_equal("evannoMethodStructure.txt" %in% list.files(),TRUE)
})
if(deleteoutput) file.remove("evannoMethodStructure.txt")

evannoMethodStructure(sr1,exportplot=TRUE)
test_that("exportplot=TRUE",{
  expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
})
if(deleteoutput) file.remove("evannoMethodStructure.png")

evannoMethodStructure(sr1,exportplot=TRUE,imgtype="jpeg")
test_that("exportplot=TRUE jpeg",{
  expect_equal("evannoMethodStructure.jpg" %in% list.files(),TRUE)
})
if(deleteoutput) file.remove("evannoMethodStructure.jpg")

evannoMethodStructure(sr1,exportplot=TRUE,imgtype="pdf")
test_that("exportplot=TRUE pdf",{
  expect_equal("evannoMethodStructure.pdf" %in% list.files(),TRUE)
})
if(deleteoutput) file.remove("evannoMethodStructure.pdf")

evannoMethodStructure(sr1,exportplot=TRUE,height=4,width=4, res=400)
test_that("exportplot=TRUE change dim res",{
  expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
})
if(deleteoutput) file.remove("evannoMethodStructure.png")

test_that("Error: Two input K",{
  expect_error(evannoMethodStructure(sr1[1:2,]))
})

test_that("Error: Non sequential K",{
  expect_error(evannoMethodStructure(sr1[c(1,2,4),],exportplot=TRUE))
})

test_that("exportplot=TRUE with error",{
  expect_equal("kPlot.png" %in% list.files(),TRUE)
})
if(deleteoutput) file.remove("kPlot.png")

test_that("Warning: runs < 2",{
  expect_warning(evannoMethodStructure(summariseRunsStructure(tr1[1:16,])[4:6,]))
})

test_that("Error: no input",{
  expect_error(evannoMethodStructure())
})

#-------------------------------------------------------------------------------

#runsToDf
context("runsToDf")
test_that("Is output dataframe or list?",{
  expect_equal(class(runsToDfStructure(sfiles)),"list")
  expect_equal(class(runsToDfStructure(sfiles[1])),"data.frame")
  expect_equal(any(is.na(as.numeric(row.names(runsToDfStructure(sfiles[1],indlabfromfile = F))))),FALSE)
  expect_equal(any(is.na(as.numeric(row.names(runsToDfStructure(sfiles[1],indlabfromfile = T))))),TRUE)
  expect_equal(class(runsToDfTess(tfiles)),"list")
  expect_equal(class(runsToDfTess(tfiles[1])),"data.frame")
  expect_equal(class(suppressWarnings(runsToDfAdmixture(afiles))),"list")
  expect_equal(class(suppressWarnings(runsToDfAdmixture(afiles[1]))),"data.frame")
  expect_equal(class(runsToDfMatrix(afiles)),"list")
  expect_equal(class(runsToDfMatrix(afiles[1])),"data.frame")
  expect_equal(class(runsToDfMatrix(ffiles)),"list")
  expect_equal(class(runsToDfMatrix(ffiles[1])),"data.frame")
  expect_equal(class(runsToDfMatrix(mcfiles)),"list")
  expect_equal(class(runsToDfMatrix(mcfiles[1])),"data.frame")
  expect_equal(class(runsToDfMatrix(mtfiles)),"list")
  expect_equal(class(runsToDfMatrix(mtfiles[1])),"data.frame")
  expect_equal(class(runsToDfMatrix(msfiles)),"list")
  expect_equal(class(runsToDfMatrix(msfiles[1])),"data.frame")
  expect_equal(class(runsToDfStructure(sfiles1)),"list")
  expect_equal(class(runsToDfStructure(sfiles1[1])),"data.frame")
})

test_that("Error: no input",{
  expect_error(runsToDfStructure())
  expect_error(runsToDfTess())
  expect_error(suppressWarnings(runsToDfAdmixture()))
  expect_error(runsToDfMatrix())
})

#-------------------------------------------------------------------------------
if(FALSE) {
#clumppExport
context("Clumpp Output")
clumppExportStructure(sfiles)
test_that("structure clumpp export check",{
  expect_equal(all(grepl("STRUCTUREpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("STRUCTUREpop*", recursive = TRUE, force = TRUE)

clumppExportStructure(sfiles,prefix="Boom")
test_that("structure clumpp export check prefix",{
  expect_equal(all(grepl("Boom",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("Boom*", recursive = TRUE, force = TRUE)

clumppExportStructure(sfiles,useexe=T)
test_that("structure clumpp export useexe",{
  expect_equal(all(grepl("STRUCTUREpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("STRUCTUREpop*", recursive = TRUE, force = TRUE)

clumppExportTess(tfiles)
test_that("tess clumpp export check",{
  expect_equal(all(grepl("TESSpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("TESSpop*", recursive = TRUE, force = TRUE)

clumppExportTess(tfiles,prefix="Hahaha")
test_that("tess clumpp export check prefix",{
  expect_equal(all(grepl("Hahaha",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("Hahaha*", recursive = TRUE, force = TRUE)

clumppExportTess(tfiles,useexe=TRUE)
test_that("tess clumpp export check",{
  expect_equal(all(grepl("TESSpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("TESSpop*", recursive = TRUE, force = TRUE)

suppressWarnings(clumppExportMatrix(afiles))
test_that("matrix clumpp export check",{
  expect_equal(all(grepl("MATRIXpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("MATRIXpop*", recursive = TRUE, force = TRUE)

clumppExportMatrix(afiles,useexe=TRUE)
test_that("matrix clumpp export check",{
  expect_equal(all(grepl("MATRIXpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("MATRIXpop*", recursive = TRUE, force = TRUE)

clumppExportStructure(sfiles,prefix="Nanana")
test_that("structure clumpp export check prefix",{
  expect_equal(all(grepl("Nanana",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("Nanana*", recursive = TRUE, force = TRUE)

clumppExportMatrix(afiles)
test_that("matrix clumpp export check",{
  expect_equal(all(grepl("MATRIXpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("MATRIXpop*", recursive = TRUE, force = TRUE)

clumppExportMatrix(ffiles)
test_that("matrix clumpp export check",{
  expect_equal(all(grepl("MATRIXpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("MATRIXpop*", recursive = TRUE, force = TRUE)
}


#-------------------------------------------------------------------------------

#collectClumppOutput
context("Collect clumpp output")
# clumppExportStructure(sfiles)
# collectClumppOutput()
# clumppExportStructure(tfiles)
# collectClumppOutput(prefix="TESSpop")
# clumppExportStructure(afiles)
# collectClumppOutput(prefix="ADMIXTUREpop")

#-------------------------------------------------------------------------------

#plotRuns
context("plotRuns Structure")
plotRuns(sfiles[1])
test_that("check output",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles)
test_that("check output",{
  expect_equal(length(grep("structure\\w+.png$",list.files())),17)
})
if(deleteoutput) file.remove(paste0(basename(sfiles),".png"))

plotRuns(sfiles[1],sortind="Cluster1")
test_that("check orderind cluster",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles[1],sortind="all")
test_that("check orderind all",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles[1:2],imgoutput="join")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(sfiles[1:2],imgoutput="join",sortind="Cluster1")
test_that("check join orderind cluster1",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined",list.files())])

pops <- read.delim(system.file("files/structurepoplabels.txt",package="pophelper"),header=FALSE)
test_that("check if pops df",{
  expect_equal(class(pops),"data.frame")
})

plotRuns(sfiles[1],poplab=pops$V1)
test_that("check output sep with labels",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles[1],poplab=pops$V1,popmean=T)
test_that("check popmean",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles[1:4],poplab=pops$V1,popmean=T)
test_that("check popmean",{
  expect_equal(all(paste0(basename(sfiles[1:4]),".png") %in% list.files()),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1:4]),".png"))

plotRuns(sfiles[1],poplab=pops$V1,sortind="Cluster1")
test_that("check output sep with labels sort cluster",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles[1],poplab=pops$V1,sortind="all")
test_that("check output sep with labels sort all",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles[1:4],poplab=pops$V1,popmean=T,sortind="all")
test_that("check popmean with sortind",{
  expect_equal(all(paste0(basename(sfiles[1:4]),".png") %in% list.files()),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1:4]),".png"))

plotRuns(sfiles[1:2],imgoutput="join",poplab=pops$V1)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(sfiles[1:4],poplab=pops$V1,popmean=T,imgoutput="join")
test_that("check popmean with join",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

popsrep <- read.delim(system.file("files/structurepoplabels-rep.txt",package="pophelper"),header=FALSE)

plotRuns(sfiles[1],poplab=popsrep$V1)
test_that("check output sep with rep labels",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles[1],poplab=popsrep$V1,popmean=T)
test_that("check popmean with rep labels",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles[1],poplab=popsrep$V1,sort="all")
test_that("check output sep with rep labels sort all",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles[1],poplab=popsrep$V1,sort="Cluster2")
test_that("check output sep with rep labels sort cluster",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

expect_error(plotRuns(sfiles[1],poplab=popsrep$V1,subsetpops="Pop A"))
expect_error(plotRuns(sfiles[1],poplab=popsrep$V1,subsetpops=c("Pop B","Pop A")))

test_that("check less colours",{
  expect_error(plotRuns(sfiles[4],popcol=c("red","green")))
})

plotRuns(sfiles[4],popcol=c("red","green","blue"))
test_that("check custom colours",{
  expect_equal(paste0(basename(sfiles[4]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[4]),".png"))

#-------------------------------------------------------------------------------

context("plotRuns Tess")
plotRuns(tfiles[1])
test_that("check output",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

plotRuns(tfiles[1],sortind="Cluster1")
test_that("check orderind cluster",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

plotRuns(tfiles[1],sortind="all")
test_that("check orderind all",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

plotRuns(tfiles[1:2],imgoutput="join")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(tfiles[1:2],imgoutput="join",sortind="Cluster1")
test_that("check output joined sortind cluster",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(tfiles[1:2],imgoutput="join",sortind="all")
test_that("check output joined sortind all",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

labs1 <- factor(c(rep("PopA",30),rep("PopB",45)))

plotRuns(tfiles[1],poplab=labs1)
test_that("check output sep with labels",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

plotRuns(tfiles[1],poplab=labs1,popmean = T)
test_that("check output sep with labels and popmean",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

plotRuns(tfiles[1],poplab=labs1,sortind="Cluster1")
test_that("check output sep with labels sortind cluster1",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

plotRuns(tfiles[1],poplab=labs1,sortind="all")
test_that("check output sep with labels sortind all",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

plotRuns(tfiles[1:2],imgoutput="join",poplab=labs1)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(tfiles[1:4],imgoutput="join",poplab=labs1, popmean=T)
test_that("check output joined with labels and popmean",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

#-------------------------------------------------------------------------------

context("plotRuns Admixture")
plotRuns(afiles[1])
test_that("check output",{
  expect_equal(any(grepl("adm",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

plotRuns(afiles[1],sortind="Cluster1")
test_that("check orderind cluster",{
  expect_equal(any(grepl("admixture",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("admixture",list.files())])

plotRuns(afiles[1],sortind="all")
test_that("check orderind all",{
  expect_equal(any(grepl("admixture",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("admixture",list.files())])

plotRuns(afiles[1:2],imgoutput="join")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

admpops<-read.delim(system.file("files/admixturepoplabels.txt",package="pophelper"),header=FALSE)

plotRuns(afiles[1],poplab=admpops$V1)
test_that("check output sep with labels",{
  expect_equal(any(grepl("adm",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

plotRuns(afiles[1],poplab=admpops$V1, popmean=T)
test_that("check output sep with labels and popmean",{
  expect_equal(any(grepl("adm",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

plotRuns(afiles[1],poplab=admpops$V1,sortind="Cluster1")
test_that("check orderind cluster",{
  expect_equal(any(grepl("admixture",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("admixture",list.files())])

plotRuns(afiles[1],poplab=admpops$V1,sortind="all")
test_that("check orderind all",{
  expect_equal(any(grepl("admixture",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("admixture",list.files())])

plotRuns(afiles[1:2],imgoutput="join",poplab=admpops$V1)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(afiles[1:3],imgoutput="join",poplab=admpops$V1,popmean=T)
test_that("check output joined with labels and popmean",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(afiles[1:2],imgoutput="join",poplab=admpops$V1,sortind="Cluster1")
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(afiles[1:2],imgoutput="join",poplab=admpops$V1,sortind="all")
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

#-------------------------------------------------------------------------------

context("plotRuns Matrix fastSTRUCTURE")
plotRuns(ffiles[2])
test_that("check output",{
  expect_equal(any(grepl("fast",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("fast",list.files())])

plotRuns(ffiles[2],sortind="Cluster1")
test_that("check output",{
  expect_equal(any(grepl("fast",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("fast",list.files())])

plotRuns(ffiles[2],sortind="all")
test_that("check output",{
  expect_equal(any(grepl("fast",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("fast",list.files())])

plotRuns(ffiles[1:2],imgoutput="join")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

fastpops <- read.delim(system.file("files/faststructurepoplabels.txt",package="pophelper"),header=FALSE)

plotRuns(ffiles[2],poplab=fastpops$V1)
test_that("check output sep with labels",{
  expect_equal(any(grepl("fast",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

plotRuns(ffiles[2],poplab=fastpops$V1,popmean=T)
test_that("check output sep with labels and pop mean",{
  expect_equal(any(grepl("fast",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

plotRuns(ffiles[2:3],imgoutput="join",poplab=fastpops$V1)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(ffiles[2:4],imgoutput="join",poplab=fastpops$V1,popmean=T)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

#-------------------------------------------------------------------------------

context("plotRuns Matrix mcfiles")
plotRuns(mcfiles[1])
test_that("check output",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mcfiles[1],sortind="Cluster1")
test_that("check output sort cluster1",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mcfiles[1],sortind="all")
test_that("check output sort all",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mcfiles[1:2],imgoutput="join")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(mcfiles[1:2],imgoutput="join",sortind="Cluster1")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(mcfiles[1:2],imgoutput="join",sortind="all")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

matpops<-read.delim(system.file("files/matrixpoplabels.txt",package="pophelper"),header=FALSE)

plotRuns(mcfiles[1],poplab=matpops$V1)
test_that("check output sep with labels",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mcfiles[1],poplab=matpops$V1,popmean=T)
test_that("check output sep with labels",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mcfiles[1],poplab=matpops$V1,sortind="Cluster1")
test_that("check output sep with labels",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mcfiles[1],poplab=matpops$V1,sortind="all")
test_that("check output sep with labels",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mcfiles[1:2],imgoutput="join",poplab=matpops$V1)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(mcfiles[1:2],imgoutput="join",poplab=matpops$V1,sortind="Cluster1")
test_that("check output joined with labels sort cluster1",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(mcfiles[1:2],imgoutput="join",poplab=matpops$V1,sortind="all")
test_that("check output joined with labels sort cluster1",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(mcfiles[1:3],imgoutput="join",poplab=matpops$V1,sortind="all",popmean=T)
test_that("check output joined with labels sort cluster1",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

#-------------------------------------------------------------------------------

context("plotRuns Matrix mtfiles")
plotRuns(mtfiles[1])
test_that("check output",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mtfiles[1],sortind="Cluster1")
test_that("check output sort cluster",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mtfiles[1],sortind="all")
test_that("check output sort cluster",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mtfiles[1:2],imgoutput="join")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(mtfiles[1:2],imgoutput="join",sortind="Cluster1")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(mtfiles[1:2],imgoutput="join",sortind="all")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(mtfiles[1],poplab=matpops$V1)
test_that("check output sep with labels",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mtfiles[1],poplab=matpops$V1,sortind="Cluster1")
test_that("check output sep with labels sort cluster",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mtfiles[1],poplab=matpops$V1,sortind="all")
test_that("check output sep with labels sort all",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(mtfiles[1:2],imgoutput="join",poplab=matpops$V1)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(mtfiles[1:2],imgoutput="join",poplab=matpops$V1,sortind="Cluster1")
test_that("check output joined with labels sort cluster",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

#-------------------------------------------------------------------------------

context("plotRuns Matrix mtfiles")
plotRuns(msfiles[1])
test_that("check output",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(msfiles[1],sortind="Cluster1")
test_that("check output sort cluster",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(msfiles[1],sortind="all")
test_that("check output sort all",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(msfiles[1:2],imgoutput="join")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(msfiles[1:2],imgoutput="join",sortind="Cluster1")
test_that("check output joined sort cluster",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(msfiles[1:2],imgoutput="join",sortind="all")
test_that("check output joined sort all",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(msfiles[1],poplab=matpops$V1)
test_that("check output sep with labels",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(msfiles[1],poplab=matpops$V1,sortind="Cluster1")
test_that("check output sep with labels",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(msfiles[1],poplab=matpops$V1,sortind="all")
test_that("check output sep with labels",{
  expect_equal(any(grepl("matrix",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("matrix",list.files())])

plotRuns(msfiles[1:2],imgoutput="join",poplab=matpops$V1)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(msfiles[1:2],imgoutput="join",poplab=matpops$V1,sortind="Cluster1")
test_that("check output joined with labels sort cluster",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

plotRuns(msfiles[1:2],imgoutput="join",poplab=matpops$V1,sortind="all")
test_that("check output joined with labels sort all",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

#-------------------------------------------------------------------------------

context("plotRuns Table")
tabs1 <- c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))

plotRuns(files=tabs1,imgoutput="tab")
test_that("check output table",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])

plotRuns(files=tabs1,imgoutput="tab",popmean=T)
test_that("check output table",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])

plotRuns(files=tabs1,imgoutput="tab",sortind="Cluster1")
test_that("check output table sort cluster",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])

plotRuns(files=tabs1,imgoutput="tab",poplab=pops$V1)
test_that("check output table pop lab",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])

plotRuns(files=tabs1,imgoutput="tab",poplab=pops$V1,sortind="all")
test_that("check output table pop lab sort all",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])

plotRuns(files=tabs1,imgoutput="tab",poplab=pops$V1,sortind="Cluster1")
test_that("check output table pop lab sort cluster",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])

plotRuns(files=tabs1,imgoutput="tab",poplab=pops$V1,popmean=T)
test_that("check output table pop lab pop mean",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])

plotRuns(files=tabs1,imgoutput="tab",poplab=popsrep$V1)
test_that("check output table pop lab rep",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])

plotRuns(files=tabs1,imgoutput="tab",poplab=popsrep$V1,sortind="all")
test_that("check output table pop lab rep sort",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])

plotRuns(files=tabs1,imgoutput="tab",poplab=popsrep$V1,sortind="all",popmean=T)
test_that("check output table pop lab rep sort pop mean",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])

expect_error(plotRuns(files=tabs1,imgoutput="tab",poplab=popsrep$V1,subsetpops="Pop B"))
#-------------------------------------------------------------------------------

#plotMultiline
context("plotMultiline STRUCTURE")
plotMultiline(sfiles[1])
test_that("sfiles 1 check output",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

plotMultiline(sfiles[1],sortind="Cluster1")
test_that("sfiles 1 check output sort cluster",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

plotMultiline(sfiles[1],sortind="all")
test_that("sfiles 1 check output sort all",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

plotMultiline(sfiles[1:2])
test_that("sfiles >1 check output",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

plotMultiline(sfiles[1:2],sortind="Cluster1")
test_that("sfiles >1 sort cluster",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

plotMultiline(sfiles[1:2],sortind="all")
test_that("sfiles >1 sort all",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

plotMultiline(sfiles[1:2],sortind=NA,indlabfromfile=T)
test_that("sfiles 1 sort NA indlabfromfile",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

plotMultiline(sfiles[1:2],sortind="all",indlabfromfile=T)
test_that("sfiles 1 sort all indlabfromfile=T",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

inds <- read.delim(system.file("files/structureindlabels.txt",package="pophelper"),header=FALSE)

plotMultiline(sfiles[1:2],indlab=inds$V1)
test_that("sfiles 1 indlab",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

plotMultiline(sfiles[1:2],sortind="Cluster1",indlab=inds$V1)
test_that("sfiles 1 check output sort cluster1 indlab",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

context("plotMultiline TESS")
plotMultiline(tfiles[1])
test_that("tfiles 1 check output",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

plotMultiline(tfiles[1:2])
test_that("tfiles >1 check output",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

plotMultiline(tfiles[1:2],sortind="Cluster1")
test_that("tfiles >1 check output sort cluster",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

plotMultiline(tfiles[1:2],sortind="all")
test_that("tfiles >1 check output",{
  expect_equal(any(grepl("tess",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("tess",list.files())])

context("plotMultiline ADMIXTURE")
plotMultiline(afiles[1])
test_that("afiles 1 check output",{
  expect_equal(any(grepl("adm",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

plotMultiline(afiles[1:2])
test_that("afiles >1 check output",{
  expect_equal(any(grepl("adm",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

plotMultiline(afiles[1:2],sortind="Cluster1")
test_that("afiles >1 check output join sort cluster",{
  expect_equal(any(grepl("adm",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

plotMultiline(afiles[1:2],sortind="all")
test_that("afiles >1 check output join sort all",{
  expect_equal(any(grepl("adm",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

context("plotMultiline FF")
plotMultiline(ffiles[1])
test_that("ffiles 1 check output",{
  expect_equal(any(grepl("fast",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("fast",list.files())])

plotMultiline(ffiles[1:2])
test_that("ffiles >1 check output",{
  expect_equal(any(grepl("fast",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("fast",list.files())])

plotMultiline(ffiles[1:2],sortind="Cluster1")
test_that("ffiles >1 check output sort cluster",{
  expect_equal(any(grepl("fast",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("fast",list.files())])

plotMultiline(ffiles[1:2],sortind="all")
test_that("ffiles >1 check output sort all",{
  expect_equal(any(grepl("fast",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("fast",list.files())])

context("plotMultiline MC")
plotMultiline(mcfiles[1])
test_that("mcfiles 1 check output",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

plotMultiline(mcfiles[1:2])
test_that("mcfiles >1 check output",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

plotMultiline(mcfiles[1:2], sortind="Cluster1")
test_that("mcfiles >1 check output sort cluster",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

plotMultiline(mcfiles[1:2], sortind="all")
test_that("mcfiles >1 check output sort all",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

context("plotMultiline MT")
plotMultiline(mtfiles[1])
test_that("mtfiles 1 check output",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

plotMultiline(mtfiles[1:2])
test_that("mtfiles >1 check output",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

plotMultiline(mtfiles[1:2],sortind="Cluster1")
test_that("mtfiles >1 check output sort cluster",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

plotMultiline(mtfiles[1:2],sortind="all")
test_that("mtfiles >1 check output sort all",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

context("plotMultiline MS")
plotMultiline(msfiles[1])
test_that("msfiles 1 check output",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

plotMultiline(msfiles[1:2])
test_that("msfiles >1 check output",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

plotMultiline(msfiles[1:2],sortind="Cluster1")
test_that("msfiles >1 check output sort cluster",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

plotMultiline(msfiles[1:2],sortind="all")
test_that("msfiles >1 check output sort all",{
  expect_equal(any(grepl("mat",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("mat",list.files())])

#-------------------------------------------------------------------------------

context("Collect Tess runs")

#-------------------------------------------------------------------------------

context("plotRunsInterpolate Structure")
cfile239 <- read.delim(system.file("files/coords239.txt",package="pophelper"),header=FALSE)
sfile239 <- system.file("files/Structure239_4",package="pophelper")

plotRunsInterpolate(datafile=sfile239,coordsfile=cfile239)
test_that("check output krig",{
  expect_equal(any(grepl("Interpolation-krig",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-krig",list.files())])

plotRunsInterpolate(datafile=sfile239,coordsfile=cfile239,method = "bilinear")
test_that("check output bilinear",{
  expect_equal(any(grepl("Interpolation-bilinear",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bilinear",list.files())])

plotRunsInterpolate(datafile=sfile239,coordsfile=cfile239,method = "bicubic")
test_that("check output bicubic",{
  expect_equal(any(grepl("Interpolation-bicubic",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bicubic",list.files())])

plotRunsInterpolate(datafile=sfile239,coordsfile=cfile239,method = "idw")
test_that("check output idw",{
  expect_equal(any(grepl("Interpolation-idw",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-idw",list.files())])

plotRunsInterpolate(datafile=sfile239,coordsfile=cfile239,method = "nn")
test_that("check output nn",{
  expect_equal(any(grepl("Interpolation-nn",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-nn",list.files())])

#-------------------------------------------------------------------------------

context("plotRunsInterpolate Tess")
cd2 <- system.file("/files/coords75.txt",package="pophelper")
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2)
test_that("check output krig",{
  expect_equal(any(grepl("Interpolation-krig",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-krig",list.files())])

plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,imgtype="jpeg")
test_that("check output krig jpeg",{
  expect_equal(any(grepl("Clusters.jpg",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.jpg",list.files())])

plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,imgtype="pdf")
test_that("check output krig pdf",{
  expect_equal(any(grepl("Clusters.pdf",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.pdf",list.files())])

plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,method = "bilinear")
test_that("check output bilinear",{
  expect_equal(any(grepl("Interpolation-bilinear",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bilinear",list.files())])

plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,method = "bicubic")
test_that("check output bicubic",{
  expect_equal(any(grepl("Interpolation-bicubic",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bicubic",list.files())])

plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,method = "idw")
test_that("check output idw",{
  expect_equal(any(grepl("Interpolation-idw",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-idw",list.files())])

plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,method = "nn")
test_that("check output nn",{
  expect_equal(any(grepl("Interpolation-nn",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-nn",list.files())])

#-------------------------------------------------------------------------------

context("plotRunsInterpolate Admixture")
cd2 <- system.file("/files/coords1592.txt",package="pophelper")
plotRunsInterpolate(datafile=afiles[2],coordsfile=cd2)
test_that("check output krig",{
  expect_equal(any(grepl("Interpolation-krig",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-krig",list.files())])

plotRunsInterpolate(datafile=afiles[2],coordsfile=cd2,imgtype="jpeg")
test_that("check output krig jpeg",{
  expect_equal(any(grepl("Clusters.jpg",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.jpg",list.files())])

plotRunsInterpolate(datafile=afiles[2],coordsfile=cd2,imgtype="pdf")
test_that("check output krig pdf",{
  expect_equal(any(grepl("Clusters.pdf",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.pdf",list.files())])

plotRunsInterpolate(datafile=afiles[2],coordsfile=cd2,method = "bilinear")
test_that("check output bilinear",{
  expect_equal(any(grepl("Interpolation-bilinear",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bilinear",list.files())])

plotRunsInterpolate(datafile=afiles[2],coordsfile=cd2,method = "bicubic")
test_that("check output bicubic",{
  expect_equal(any(grepl("Interpolation-bicubic",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bicubic",list.files())])

plotRunsInterpolate(datafile=afiles[2],coordsfile=cd2,method = "idw")
test_that("check output idw",{
  expect_equal(any(grepl("Interpolation-idw",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-idw",list.files())])

plotRunsInterpolate(datafile=afiles[2],coordsfile=cd2,method = "nn")
test_that("check output nn",{
  expect_equal(any(grepl("Interpolation-nn",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-nn",list.files())])

#-------------------------------------------------------------------------------

context("plotRunsInterpolate fastStructure")
cd2 <- system.file("/files/coords22.txt",package="pophelper")
plotRunsInterpolate(datafile=ffiles[2],coordsfile=cd2)
test_that("check output krig",{
  expect_equal(any(grepl("Interpolation-krig",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-krig",list.files())])

plotRunsInterpolate(datafile=ffiles[2],coordsfile=cd2,imgtype="jpeg")
test_that("check output krig jpeg",{
  expect_equal(any(grepl("Clusters.jpg",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.jpg",list.files())])

plotRunsInterpolate(datafile=ffiles[2],coordsfile=cd2,imgtype="pdf")
test_that("check output krig pdf",{
  expect_equal(any(grepl("Clusters.pdf",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Clusters.pdf",list.files())])

plotRunsInterpolate(datafile=ffiles[2],coordsfile=cd2,method = "bilinear")
test_that("check output bilinear",{
  expect_equal(any(grepl("Interpolation-bilinear",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-bilinear",list.files())])

# plotRunsInterpolate(datafile=ffiles[2],coordsfile=cd2,method = "bicubic")
# test_that("check output bicubic",{
#   expect_equal(any(grepl("Interpolation-bicubic",list.files())),TRUE)
# })
# if(deleteoutput) file.remove(list.files()[grep("Interpolation-bicubic",list.files())])

plotRunsInterpolate(datafile=ffiles[2],coordsfile=cd2,method = "idw")
test_that("check output idw",{
  expect_equal(any(grepl("Interpolation-idw",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-idw",list.files())])

plotRunsInterpolate(datafile=ffiles[2],coordsfile=cd2,method = "nn")
test_that("check output nn",{
  expect_equal(any(grepl("Interpolation-nn",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Interpolation-nn",list.files())])

#-------------------------------------------------------------------------------

context("plotRunsSpatial")
cfile239 <- read.delim(system.file("files/coords239.txt",package="pophelper"),header=FALSE)
sfile239 <- system.file("files/Structure239_4",package="pophelper")
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239)
test_that("check output",{
  expect_equal(any(grepl("Spatial.png",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Spatial.png",list.files())])

cd2 <- system.file("/files/coords75.txt",package="pophelper")
plotRunsSpatial(datafile=tfiles[2],coordsfile=cd2)
test_that("check output",{
  expect_equal(any(grepl("Spatial.png",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Spatial.png",list.files())])

cd2 <- system.file("/files/coords1592.txt",package="pophelper")
plotRunsSpatial(datafile=afiles[2],coordsfile=cd2)
test_that("check output",{
  expect_equal(any(grepl("Spatial.png",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Spatial.png",list.files())])

cd2 <- system.file("/files/coords22.txt",package="pophelper")
plotRunsSpatial(datafile=ffiles[2],coordsfile=cd2)
test_that("check output",{
  expect_equal(any(grepl("Spatial.png",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Spatial.png",list.files())])


#-------------------------------------------------------------------------------

context("analyseRuns Structure")
analyseRuns(sfiles)
test_that("check output",{
  expect_equal(any(grepl("evannoMethodStructure.png",list.files())),TRUE)
  expect_equal(any(grepl("evannoMethodStructure.txt",list.files())),TRUE)
  expect_equal(any(grepl("tabulateRunsStructure.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseRunsStructure.txt",list.files())),TRUE)
  expect_equal(any(grepl("structure_01.png",list.files())),TRUE)
  expect_equal(any(grepl("structure_17.png",list.files())),TRUE)
  expect_equal(any(grepl("./STRUCTUREpop_K2",list.dirs())),TRUE)
  expect_equal(any(grepl("./STRUCTUREpop_K7",list.dirs())),TRUE)
})
if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
if(deleteoutput) file.remove(list.files())

#-------------------------------------------------------------------------------

context("analyseRuns Tess")
analyseRuns(tfiles)
test_that("check output",{
  expect_equal(any(grepl("tabulateRunsTess.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseRunsTess.txt",list.files())),TRUE)
  expect_equal(any(grepl("tess_01.png",list.files())),TRUE)
  expect_equal(any(grepl("tess_21.png",list.files())),TRUE)
  expect_equal(any(grepl("./TESSpop_K2",list.dirs())),TRUE)
  expect_equal(any(grepl("./TESSpop_K8",list.dirs())),TRUE)
})
if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
if(deleteoutput) file.remove(list.files())

#-------------------------------------------------------------------------------

context("analyseRuns Admixture")
analyseRuns(afiles)
test_that("check output",{
  expect_equal(any(grepl("tabulateRunsMatrix.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseRunsMatrix.txt",list.files())),TRUE)
  expect_equal(any(grepl("admixture_01.png",list.files())),TRUE)
  expect_equal(any(grepl("admixture_10.png",list.files())),TRUE)
  expect_equal(any(grepl("./MATRIXpop_K2",list.dirs())),TRUE)
})
if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
if(deleteoutput) file.remove(list.files())

#-------------------------------------------------------------------------------

context("distructExport Structure")

pops <- read.delim(system.file("files/structurepoplabels.txt",package="pophelper"),header=FALSE)

#ind one file
distructExport(files = sfiles[1])
test_that("destructExport structure one file",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#both top and bottom labels
distructExport(sfiles[1],poplabbottom=as.character(pops$V1),poplabtop=as.character(pops$V1),popmean=T)
test_that("destructExport structure top and bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#both top and bottom labels quiet
distructExport(sfiles[1],poplabbottom=as.character(pops$V1),poplabtop=as.character(pops$V1),quiet=T)
test_that("destructExport structure top and botoom labels quiet",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#no bottom labels
distructExport(sfiles[1],poplabbottom=NA,poplabtop=as.character(pops$V1))
test_that("destructExport structure no bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#no top labels
distructExport(sfiles[1],poplabbottom=as.character(pops$V1),poplabtop=NA)
test_that("destructExport structure no top labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#no both labels
distructExport(sfiles[1],poplabbottom=NA,poplabtop=NA)
test_that("destructExport structure both labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files
distructExport(sfiles[2:5],poplabbottom=as.character(pops$V1))
test_that("destructExport structure multiple files",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#pop mean true
distructExport(sfiles[1],poplabbottom=as.character(pops$V1),popmean=T)
test_that("destructExport structure popmean true",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files with popmean
distructExport(sfiles[2:5],poplabbottom=as.character(pops$V1),popmean=T)
test_that("destructExport structure multiple files",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#other colours
distructExport(sfiles[1],poplabbottom=as.character(pops$V1),popcol=pophelper:::distructColours()[43:44])
test_that("destructExport structure other colours",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files use exe
distructExport(sfiles[2:5],poplabbottom=as.character(pops$V1),useexe=T)
test_that("destructExport structure use exe",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files popmean use exe
distructExport(sfiles[2:5],poplabbottom=as.character(pops$V1),popmean=T,useexe=T)
test_that("destructExport structure popmean use exe",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#other colours use exe
distructExport(sfiles[1],poplabbottom=as.character(pops$V1),popcol=pophelper:::distructColours()[43:44],useexe=T)
test_that("destructExport structure other colours use exe",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

context("exportDistruct Tess")
pops <- read.delim(system.file("files/tesspoplabels.txt",package="pophelper"),header=FALSE)

#both top and bottom labels
distructExport(tfiles[3],poplabbottom=as.character(pops$V1),poplabtop=as.character(pops$V1))
test_that("exportDistruct Tess top and bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./tess_03-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#both top and bottom labels quiet
distructExport(tfiles[3],poplabbottom=as.character(pops$V1),poplabtop=as.character(pops$V1),quiet=T)
test_that("exportDistruct Tess top and bottom labels quiet",{
  expect_equal(any(grepl("drawparams",list.files("./tess_03-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#no bottom labels
distructExport(tfiles[3],poplabbottom=NA,poplabtop=as.character(pops$V1))
test_that("exportDistruct Tess no bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./tess_03-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files
distructExport(tfiles[2:5],poplabbottom=as.character(pops$V1))
test_that("exportDistruct Tess multiple files",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#multiple files with popmean
distructExport(tfiles[2:5],poplabbottom=as.character(pops$V1),popmean=T)
test_that("exportDistruct Tess pop mean",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

context("distructExport Other formats")

#admixture multiple
distructExport(files = afiles[1:4],popmean=F)
test_that("exportDistruct Admixture",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#admixture multiple popmean
distructExport(files = afiles[1:4],popmean=T)
test_that("exportDistruct Admixture pop mean",{
  expect_equal(length(list.files()),4)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#faststructure
distructExport(files = ffiles[2:3],popmean=F)
test_that("exportDistruct faststructure",{
  expect_equal(length(list.files()),2)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#matrix files comma
distructExport(files = mcfiles[2:3],popmean=F)
test_that("exportDistruct Matrix Comma",{
  expect_equal(length(list.files()),2)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#matrix files tab
distructExport(files = mtfiles[2:3],popmean=F)
test_that("exportDistruct Matrix tab",{
  expect_equal(length(list.files()),2)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#matrix files space
distructExport(files = msfiles[2:3],popmean=F)
test_that("exportDistruct Matrix space",{
  expect_equal(length(list.files()),2)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#tab no popmean
distructExport(files = tabs1,popmean=F)
test_that("exportDistruct Admixture",{
  expect_equal(length(list.files()),7)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#tab popmean
distructExport(files = tabs1,popmean=T)
test_that("exportDistruct Admixture",{
  expect_equal(length(list.files()),7)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#tab no popmean use exe
distructExport(files = tabs1,useexe=T)
test_that("exportDistruct Admixture",{
  expect_equal(length(list.files()),7)
})
if(deleteoutput) unlink(list.files(),recursive=T,force=T)

#-------------------------------------------------------------------------------

setwd(currwd)
if(deleteoutput) unlink("pophelper-demo",recursive = T,force = T)

