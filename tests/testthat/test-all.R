library(testthat)
library(pophelper)

#devtools::test()

#Preparation
deleteoutput = TRUE
#create a new folder and set as wd
#currwd <- getwd()
#dir.create(paste(currwd,"/PophelperDemo",sep=""))
#setwd(paste(currwd,"/PophelperDemo",sep=""))
#read sample STRUCTURE files from R package
sfiles <- list.files(path=system.file("/files/structure",package="pophelper"),full.names=TRUE)
#read sample TESS files from R package
tfiles <- list.files(path=system.file("/files/tess",package="pophelper"),full.names=TRUE)
#read sample ADMIXTURE files from R package
afiles <- list.files(path=system.file("/files/admixture",package="pophelper"),full.names=TRUE)

#-------------------------------------------------------------------------------

#tabulateRuns
context("Tabulate")
tr1 <- tabulateRunsStructure(files=sfiles)
tr2 <- tabulateRunsTess(files=tfiles)
tr3 <- tabulateRunsAdmixture(files=afiles)

test_that("Is output dataframe?",{
  expect_equal(class(tr1),"data.frame")
  expect_equal(class(tr2),"data.frame")
  expect_equal(class(tr3),"data.frame")
})

test_that("quiet=FALSE",{
  expect_output(tabulateRunsStructure(files=sfiles, quiet=FALSE),"Number of files selected: 17")
  expect_output(tabulateRunsTess(files=tfiles, quiet=FALSE),"Number of files selected: 21")
  expect_output(tabulateRunsAdmixture(files=afiles, quiet=FALSE),"Number of files selected: 10")
})

test_that("quiet=TRUE",{
  expect_output(tabulateRunsStructure(files=sfiles, quiet=TRUE),"")
  expect_output(tabulateRunsTess(files=tfiles, quiet=TRUE),"")
  expect_output(tabulateRunsAdmixture(files=afiles, quiet=TRUE),"")
})

test_that("sorttable=FALSE",{
  expect_equal(row.names(tabulateRunsStructure(files=sfiles, quiet=FALSE,sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
  expect_equal(row.names(tabulateRunsTess(files=tfiles, quiet=FALSE,sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21"))
  expect_equal(row.names(tabulateRunsAdmixture(files=afiles, quiet=FALSE,sorttable = FALSE)),
               c("1","2","3","4","5","6","7","8","9","10"))
})

# test_that("sorttable=TRUE",{
#   expect_equal(row.names(tabulateRunsStructure(files=sfiles,sorttable = TRUE)), c("1","10","17","2","11","12","3","7","13","4","8","14","5","9","15","6","16"))
#   expect_equal(row.names(tabulateRunsTess(files=tfiles,sorttable = TRUE)), c("1","8","15","2","9","16","3","10","17","4","11","18","5","12","19","6","13","20","7","14","21"))
#   expect_equal(row.names(tabulateRunsAdmixture(files=afiles,sorttable = TRUE)), c("1","2","3","4","5","6","7","8","9","10"))
# })

tabulateRunsStructure(files=sfiles,writetable=TRUE)
tabulateRunsTess(files=tfiles,writetable=TRUE)
tabulateRunsAdmixture(files=afiles,writetable=TRUE)

test_that("writetable=TRUE",{
  expect_equal("tabulateRunsStructure.txt" %in% list.files(),TRUE)
  expect_equal("tabulateRunsTess.txt" %in% list.files(),TRUE)
  expect_equal("tabulateRunsAdmixture.txt" %in% list.files(),TRUE)
})
if(deleteoutput) file.remove("tabulateRunsStructure.txt")
if(deleteoutput) file.remove("tabulateRunsTess.txt")
if(deleteoutput) file.remove("tabulateRunsAdmixture.txt")

test_that("Error: no input",{
  expect_error(tabulateRunsStructure())
  expect_error(tabulateRunsTess())
  expect_error(tabulateRunsAdmixture())
})

#-------------------------------------------------------------------------------

#summariseRuns
context("Summarise")
test_that("Is output dataframe?",{
  expect_equal(class(summariseRunsStructure(tr1)),"data.frame")
  expect_equal(class(summariseRunsTess(tr2)),"data.frame")
  expect_equal(class(summariseRunsAdmixture(tr3)),"data.frame")
})

sr1 <- summariseRunsStructure(tr1,writetable=TRUE)
sr2 <- summariseRunsTess(tr2,writetable=TRUE)
sr3 <- summariseRunsAdmixture(tr3,writetable=TRUE)

test_that("writetable=TRUE",{
  expect_equal("summariseRunsStructure.txt" %in% list.files(),TRUE)
  expect_equal("summariseRunsTess.txt" %in% list.files(),TRUE)
  expect_equal("summariseRunsAdmixture.txt" %in% list.files(),TRUE)
})
if(deleteoutput) file.remove("summariseRunsStructure.txt")
if(deleteoutput) file.remove("summariseRunsTess.txt")
if(deleteoutput) file.remove("summariseRunsAdmixture.txt")

test_that("Error: no input",{
  expect_error(summariseRunsStructure())
  expect_error(summariseRunsTess())
  expect_error(summariseRunsAdmixture())
})

#-------------------------------------------------------------------------------

#evannoMethodStucture
context("Evanno method")
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
  expect_equal(class(runsToDfTess(tfiles)),"list")
  expect_equal(class(runsToDfTess(tfiles[1])),"data.frame")
  expect_equal(class(runsToDfAdmixture(afiles)),"list")
  expect_equal(class(runsToDfAdmixture(afiles[1])),"data.frame")
})

test_that("Error: no input",{
  expect_error(runsToDfStructure())
  expect_error(runsToDfTess())
  expect_error(runsToDfAdmixture())
})
#-------------------------------------------------------------------------------

#clumppExport
context("Clumpp Output")
clumppExportStructure(sfiles)
test_that("clumpp export check",{
  expect_equal(all(grepl("STRUCTUREpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("STRUCTUREpop*", recursive = TRUE, force = TRUE)

clumppExportStructure(sfiles,prefix="Boom")
test_that("clumpp export check prefix",{
  expect_equal(all(grepl("Boom",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("Boom*", recursive = TRUE, force = TRUE)

clumppExportTess(tfiles)
test_that("clumpp export check",{
  expect_equal(all(grepl("TESSpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("TESSpop*", recursive = TRUE, force = TRUE)

clumppExportTess(tfiles,prefix="Hahaha")
test_that("clumpp export check prefix",{
  expect_equal(all(grepl("Hahaha",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("Hahaha*", recursive = TRUE, force = TRUE)

clumppExportAdmixture(afiles)
test_that("clumpp export check",{
  expect_equal(all(grepl("ADMIXTUREpop",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("ADMIXTUREpop*", recursive = TRUE, force = TRUE)

clumppExportStructure(sfiles,prefix="Nanana")
test_that("clumpp export check prefix",{
  expect_equal(all(grepl("Nanana",list.dirs()[-1])),TRUE)
})
if(deleteoutput) unlink("Nanana*", recursive = TRUE, force = TRUE)

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

plotRuns(sfiles[1:2],imgoutput="join")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

pops <- read.delim(system.file("files/structurepoplabels.txt",package="pophelper"),header=FALSE)
test_that("check if pops df",{
  expect_equal(class(pops),"data.frame")
})

plotRuns(sfiles[1],poplab=pops$V1)
test_that("check output sep with labels",{
  expect_equal(paste0(basename(sfiles[1]),".png") %in% list.files(),TRUE)
})
if(deleteoutput) file.remove(paste0(basename(sfiles[1]),".png"))

plotRuns(sfiles[1:2],imgoutput="join",poplab=pops$V1)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

context("plotRuns Tess")
plotRuns(tfiles[1])
test_that("check output",{
  expect_equal(any(grepl("TESS",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("TESS",list.files())])

plotRuns(tfiles[1:2],imgoutput="join")
test_that("check output joined",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

labs1 <- factor(c(rep("PopA",30),rep("PopB",45)))

plotRuns(tfiles[1],poplab=labs1)
test_that("check output sep with labels",{
  expect_equal(any(grepl("TESS",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("TESS",list.files())])

plotRuns(tfiles[1:2],imgoutput="join",poplab=labs1)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

context("plotRuns Admixture")
plotRuns(afiles[1])
test_that("check output",{
  expect_equal(any(grepl("adm",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

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

plotRuns(afiles[1:2],imgoutput="join",poplab=admpops$V1)
test_that("check output joined with labels",{
  expect_equal(length(grep("Joined.+",list.files())),1)
})
if(deleteoutput) file.remove(list.files()[grep("Joined*",list.files())])

context("plotRuns Table")

tabs1 <- c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))

plotRuns(files=tabs1,imgoutput="tab")

test_that("check output table",{
  expect_equal(length(grep("STRUCTUREpop",list.files())),3)
})
if(deleteoutput) file.remove(list.files()[grep("STRUCTUREpop",list.files())])
#-------------------------------------------------------------------------------

#plotMultiline
context("plotMultiline")
plotMultiline(sfiles[1])
test_that("check output",{
  expect_equal(any(grepl("structure",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("structure",list.files())])

plotMultiline(tfiles[1])
test_that("check output",{
  expect_equal(any(grepl("TESS",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("TESS",list.files())])

plotMultiline(afiles[1])
test_that("check output",{
  expect_equal(any(grepl("adm",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("adm",list.files())])

#-------------------------------------------------------------------------------

context("Collect Tess runs")

#-------------------------------------------------------------------------------

context("plotRunsInterpolate")
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

#-------------------------------------------------------------------------------

context("plotRunsSpatial")
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239)
test_that("check output",{
  expect_equal(any(grepl("Spatial.png",list.files())),TRUE)
})
if(deleteoutput) file.remove(list.files()[grep("Spatial.png",list.files())])

