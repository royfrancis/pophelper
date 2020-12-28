# Test Script

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
  library(showtext)
  font_add_google("Patrick Hand","patrick")
  showtext_auto()
}

#create a new folder and set as wd
currwd <- tempdir()
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
#read sample BAPS files from R package
bfiles <- list.files(path=system.file("files/baps",package="pophelper"),full.names=TRUE)
#read sample MATRIX files from R package
mcfiles <- list.files(path=system.file("files/basic/comma",package="pophelper"),full.names=TRUE)
mtfiles <- list.files(path=system.file("files/basic/tab",package="pophelper"),full.names=TRUE)
msfiles <- list.files(path=system.file("files/basic/space",package="pophelper"),full.names=TRUE)
cfiles <- c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))

# labels
grps1 <- read.delim(system.file("files/structuregrplabels.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
grps2 <- read.delim(system.file("files/structuregrplabels2.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
grplabs <- read.delim(system.file("files/grplab.txt",package="pophelper"),stringsAsFactors=F)

ftype <- c("sfiles","sfiles1","tfiles","afiles","ffiles",
            "mcfiles","mtfiles","msfiles","cfiles","bfiles")

# LOOP -------------------------------------------------------------------------

testthat::context("LOOP")

len <- length(ftype)
for(i in seq_along(ftype))
{
  #cat(paste0("\nRunning ",i," of ",len,"\n"))
  files <- get(ftype[i])
  cr <- pophelper:::checkQ(files)
  
  # checkQ ---------------------------------------------------------------------
  
  test_that("checkQ",{
    
    # check length
    expect_equal(length(cr$type),length(files))
    
    # check type
    if(i==1|i==2) {
      expect_equal(unique(cr$type),"STRUCTURE")
      expect_equal(unique(cr$subtype),NA)
    }
    
    if(i==3) {
      expect_equal(unique(cr$type),"TESS")
      expect_equal(unique(cr$subtype),NA)
    }
    if(i>3 && i<9) {
      expect_equal(unique(cr$type),"BASIC")
      if(i!=6) expect_equal(unique(cr$subtype),"SPACE")
      if(i==6) expect_equal(unique(cr$subtype),"COMMA")
    }
    
    if(i==9) {
      expect_equal(unique(cr$type),"CLUMPP")
      expect_equal(unique(cr$subtype),NA)
    }
    
    if(i==10) {
      expect_equal(unique(cr$type),"BAPS")
      expect_equal(unique(cr$subtype),NA)
    }
  })
  
  test_that("checkQ errors",{
    expect_error(checkQ())
  })
  
  # readQ ----------------------------------------------------------------------
  
  test_that("readQ",{
    expect_equal(class(readQ(files)),"list")
    
    if(unique(cr$type) == "STRUCTURE")
    {
      expect_equal(class(readQStructure(files)),"list")
      expect_error(readQ(files,filetype="tess"))
      expect_error(readQ(files,filetype="basic"))
      expect_error(readQ(files,filetype="baps"))
      expect_error(readQ(files,filetype="clumpp"))
    }
    
    if(unique(cr$type) == "TESS")
    {
      expect_equal(class(readQTess(files)),"list")
      expect_error(readQ(files,filetype="structure"))
      expect_error(readQ(files,filetype="basic"))
      expect_error(readQ(files,filetype="baps"))
      expect_error(readQ(files,filetype="clumpp"))
    }
    
    if(unique(cr$type) == "BASIC")
    {
      expect_equal(class(readQBasic(files)),"list")
      expect_error(readQ(files,filetype="structure"))
      #expect_warning(readQ(files,filetype="tess"))
      expect_error(readQ(files,filetype="baps"))
      #expect_warning(readQ(files,filetype="clumpp"))
    }
    
    if(unique(cr$type) == "BAPS")
    {
      expect_equal(class(readQBaps(files)),"list")
      expect_error(readQ(files,filetype="structure"))
      expect_error(readQ(files,filetype="tess"))
      expect_error(readQ(files,filetype="basic"))
      expect_error(readQ(files,filetype="clumpp"))
    }
    
    if(unique(cr$type) == "CLUMPP")
    {
      expect_equal(class(readQClumpp(files)),"list")
      expect_error(readQ(files,filetype="structure"))
      expect_error(readQ(files,filetype="basic"))
      expect_error(readQ(files,filetype="baps"))
      expect_error(readQ(files,filetype="tess"))
    }
  })
  
  xlist <- readQ(files)
  
  # qlist ----------------------------------------------------------------------
  
  test_that("qlist",{
    expect_equal(class(xlist),"list")
    expect_equal(class(xlist[1]),"list")
    expect_equal(class(xlist[[1]]),"data.frame")
    
    expect_equal(xlist,as.qlist(xlist))
    
    q1 <- list(data.frame(Cluster1=c(0.2,0.4,0.6,0.2), Cluster2=c(0.8,0.6,0.4,0.8)),data.frame(Cluster1=c(0.3,0.1,0.5,0.6), Cluster2=c(0.7,0.9,0.5,0.4)))
    expect_error(is.qlist(q1))
    expect_silent(as.qlist(q1))
    expect_equal(c("run1","run2"),names(as.qlist(q1)))
    rm(q1)
  })
  
  # sortQ ----------------------------------------------------------------------

  test_that("sortQ",{
    expect_equal(names(sortQ(xlist)),tabulateQ(xlist)$file)
  })
  
  # splitQ ---------------------------------------------------------------------

  test_that("splitQ",{
    expect_equal(length(unique(tabulateQ(xlist)$k)),length(splitQ(xlist)))
    expect_equal(length(unique(tabulateQ(xlist)$ind)),length(splitQ(xlist,by="ind")))
  })
  
  # alignK ---------------------------------------------------------------------

  test_that("alignK",{
    expect_equal(class(alignK(xlist)),"list")
  })
  
  # mergeQ ------------------------------------------------------------------

  test_that("mergeQ",{
    expect_equal(summariseQ(tabulateQ(xlist))$k,as.numeric(names(mergeQ(xlist))))
  })
  
  # tabulateQ ---------------------------------------------------------------- 
  
  tr <- tabulateQ(qlist=xlist,writetable=TRUE,exportpath=getwd())
  
  test_that("tabulateQ",{
    
    # check if file has been exported
    expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
    
    if(deleteoutput) file.remove("tabulateQ.txt")
    
    expect_equal(class(tr),"data.frame")
    
    # except clumpp, num of samples must match tabulated rows
    if(tolower(unique(cr$type)) != "clumpp") expect_equal(nrow(tr),length(files))
  })
  
  test_that("tabulateQ errors",{
    expect_error(tabulateQ())
    expect_error(tabulateQ(xlist,writetable="T"))
    expect_error(tabulateQ(xlist,writetable=T))
    expect_error(tabulateQ(xlist,sorttable="F"))
  })
  
  # summariseQ ----------------------------------------------------------------

  sr <- summariseQ(tr,writetable=TRUE,exportpath=getwd())
  
  test_that("summariseQ",{
    
    # check if file has been exported
    expect_equal("summariseQ.txt" %in% list.files(),TRUE)
  
    if(deleteoutput) file.remove("summariseQ.txt")
    
    # check if data.frame
    expect_equal(class(sr),"data.frame")
  })
  
  test_that("summariseQ errors",{
    expect_error(summariseQ())
    expect_error(summariseQ(sr))
    expect_error(summariseQ("hello"))
    expect_error(summariseQ(tr,writetable=T))
    expect_error(summariseQ(tr,writetable="T"))
    
    trtemp <- tr
    trtemp$k <- NULL
    expect_error(summariseQ(trtemp))
    
    trtemp <- tr
    trtemp$ind <- NULL
    expect_error(summariseQ(trtemp))
    
    rm(trtemp)
  })
  
  rm(cr)
}

# readQ ------------------------------------------------------------

test_that("readQ errors",{
  # readq confidence intervals
  # no ci readci false
  q1 <- readQ(sfiles)
  expect_equal(any("ci" %in% unlist(lapply(q1,function(x) names(attributes(x))))),FALSE)
  # no ci readci true
  expect_warning(q1 <- readQ(sfiles,readci=T))
  expect_equal(any("ci" %in% unlist(lapply(q1,function(x) names(attributes(x))))),FALSE)
  # ci readci false
  q1 <- readQ(sfiles1)
  expect_equal(any("ci" %in% unlist(lapply(q1,function(x) names(attributes(x))))),FALSE)
  # ci readci true
  q1 <- readQ(sfiles1,readci=T)
  expect_equal(any("ci" %in% unlist(lapply(q1,function(x) names(attributes(x))))),TRUE)
  
  # readq structure confidence intervals
  # no ci readci false
  q1 <- readQStructure(sfiles)
  expect_equal(any("ci" %in% unlist(lapply(q1,function(x) names(attributes(x))))),FALSE)
  # no ci readci true
  expect_warning(q1 <- readQStructure(sfiles,readci=T))
  expect_equal(any("ci" %in% unlist(lapply(q1,function(x) names(attributes(x))))),FALSE)
  # ci readci false
  q1 <- readQStructure(sfiles1)
  expect_equal(any("ci" %in% unlist(lapply(q1,function(x) names(attributes(x))))),FALSE)
  # ci readci true
  q1 <- readQStructure(sfiles1,readci=T)
  expect_equal(any("ci" %in% unlist(lapply(q1,function(x) names(attributes(x))))),TRUE)
  
  # ci dimensions
  c1 <- attributes(q1[[1]])$ci
  expect_equal(nrow(c1),551)
  expect_equal(ncol(c1),4)
  rm(c1)
})

test_that("readQ errors",{
  expect_error(readQStructure())
  expect_error(readQTess())
  expect_error(readQBasic())
  expect_error(readQClumpp())
  expect_error(readQTess3())
  expect_error(readQBaps())
  expect_error(readQ())
  expect_error(readQ(1))
})

# alignK -----------------------------------------------------------

test_that("alignK",{
  x <- readQ(sfiles)
  expect_equal(class(alignK(x)),"list")
  expect_equal(class(alignK(x,type="across")),"list")
  expect_equal(class(alignK(x,type="within")),"list")
  expect_error(alignK(x[1],type="across"))
  expect_error(alignK())
  
  # one run differs in num of indiv
  x[[1]] <- x[[1]][-c(149),,drop=FALSE]
  expect_error(alignK(x))
})

# readQTess3 -------------------------------------------------------------------

test_that("readQTess3",{
  # tess3 files removed due to large size
  #t3obj <- readRDS(system.file("files/tess3.rds",package="pophelper"))
  #t3list <- readQTess3(t3obj)
})

# is.qlist ---------------------------------------------------------------------

testthat::context("is.qlist")
test_that("is.qlist",{
  q1 <- readQ(sfiles)
  
  expect_silent(is.qlist(q1))
  names(q1)[2] <- NA
  expect_error(is.qlist(q1))
  
  q1 <- readQ(sfiles)
  q1[[1]][1,] <- NA
  expect_warning(is.qlist(q1))
  
  q1 <- readQ(sfiles)
  names(q1)[2] <- ""
  expect_error(is.qlist(q1))
  names(q1)[2] <- "trial"
  names(q1)[3] <- "trial"
  expect_error(is.qlist(q1))
  q1[3] <- "new"
  expect_error(is.qlist(q1))
})

# evannoMethodStucture ---------------------------------------------------------

slist <- readQ(sfiles)
sr <- summariseQ(tabulateQ(slist),writetable=FALSE)

test_that("evannoMethodStructure",{

  #returns dataframe
  expect_equal(class(evannoMethodStructure(sr)),"data.frame")
  
  #export text output
  evannoMethodStructure(sr,writetable=TRUE,exportpath=getwd())
  expect_equal("evannoMethodStructure.txt" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("evannoMethodStructure.txt")
  
  #return text
  ev <- evannoMethodStructure(sr,writetable=F,returndata=T)
  expect_equal(class(ev),"data.frame")
  
  #return plot
  ev <- evannoMethodStructure(sr,writetable=F,returnplot=T,returndata=F)
  expect_equal(class(ev)[1],"gtable")
  
  #return plot and text
  ev <- evannoMethodStructure(sr,writetable=F,returnplot=T,returndata=T)
  expect_equal(class(ev),"list")
  expect_equal(class(ev[[1]]),"data.frame")
  expect_equal(class(ev[[2]])[1],"gtable")
  
  #outputfilename
  evannoMethodStructure(sr,writetable=TRUE,exportplot=T,outputfilename="candy",exportpath=getwd())
  expect_equal("candy.txt" %in% list.files(),TRUE)
  expect_equal("candy.png" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("candy.txt")
  if(deleteoutput) file.remove("candy.png")
  
  #PLOTS
  #export plot png
  evannoMethodStructure(sr,exportplot=TRUE,exportpath=getwd())
  if(deleteoutput) file.remove("evannoMethodStructure.png")
  
  #export plot jpeg
  evannoMethodStructure(sr,exportplot=TRUE,imgtype="jpeg",exportpath=getwd())
  if(deleteoutput) file.remove("evannoMethodStructure.jpg")
  
  if(testtiff)
  {
    #export plot tiff
    evannoMethodStructure(sr,exportplot=TRUE,imgtype="tiff",exportpath=getwd())
    if(deleteoutput) file.remove("evannoMethodStructure.tiff")
  }
  
  #export plot pdf
  evannoMethodStructure(sr,exportplot=TRUE,imgtype="pdf",exportpath=getwd())
  if(deleteoutput) file.remove("evannoMethodStructure.pdf")
  
  #change errorbar features, pointcol, linecol
  evannoMethodStructure(sr,exportplot=TRUE,ebwidth=0.1,
                        ebcol="coral",pointcol="firebrick",
                        linecol="green",textcol="blue",gridsize=0.6,exportpath=getwd())
  if(deleteoutput) file.remove("evannoMethodStructure.png")
  
  #change plot linesize, pointsize
  evannoMethodStructure(sr,exportplot=TRUE,linesize=0.9,pointsize=8,exportpath=getwd())
  if(deleteoutput) file.remove("evannoMethodStructure.png")
  
  #plot change dim, dpi, units, basesize for web plot
  evannoMethodStructure(sr,exportplot=TRUE,height=800,width=800,dpi=72,units="px",basesize=20,exportpath=getwd())
  if(deleteoutput) file.remove("evannoMethodStructure.png")
  
  #change font
  if(testfont)
  {
    evannoMethodStructure(sr,exportplot=TRUE,font="patrick",exportpath=getwd())
    if(deleteoutput) file.remove("evannoMethodStructure.png")
    
    #change theme
    evannoMethodStructure(sr1,exportplot=TRUE,font="patrik",theme="theme_grey",exportpath=getwd())
    if(deleteoutput) file.remove("evannoMethodStructure.png")
  }
  
  # xaxisbreaks
  ev <- evannoMethodStructure(sr,exportplot=FALSE,returndata=F,returnplot=T,xaxisbreaks=c(2,8,10))
})

test_that("evannoMethodStructure error kplot",{
  #error only 2 values of k
  expect_error(evannoMethodStructure(sr[1:2,],exportplot=TRUE,exportpath=getwd()))
  expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("evannoMethodStructure.png")
  
  #error test kplot features
  expect_error(evannoMethodStructure(sr[1:2,],exportplot=TRUE,linesize=1,pointsize=8,exportpath=getwd()))
  expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("evannoMethodStructure.png")
  
  #error test kplot features
  expect_error(evannoMethodStructure(sr[1:2,],exportplot=TRUE,ebwidth=0.05,
                                     ebcol="coral",pointcol="firebrick",
                                     linecol="green",textcol="blue",
                                     gridsize=0.6,theme="theme_grey",font="Verdana",exportpath=getwd()))
  expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("evannoMethodStructure.png")
  
  if(testfont)
  {
    expect_error(evannoMethodStructure(sr[1:2,],exportplot=TRUE,font="patrick",exportpath=getwd()))
    expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
    if(deleteoutput) file.remove("evannoMethodStructure.png")  
  }
})

test_that("evannoMethodStructure error",{
  expect_error(evannoMethodStructure())
  expect_error(evannoMethodStructure("hello"))
  expect_error(evannoMethodStructure(tr))
  expect_error(evannoMethodStructure(sr,writetable="T"))
  expect_error(evannoMethodStructure(sr,writetable=T))
  expect_error(evannoMethodStructure(sr,exportplot="F"))
  expect_error(evannoMethodStructure(sr,exportplot=T))
  expect_error(evannoMethodStructure(sr,exportplot=T,exportpath=getwd(),imgtype="hello"))
  expect_error(evannoMethodStructure(sfiles))
  expect_error(evannoMethodStructure(slist))
  # warning less than 2 runs
  expect_error(evannoMethodStructure(sr[4:5,]))
})

test_that("evannoMethodStructure error non-sequential k png",{
  expect_error(evannoMethodStructure(sr[c(1,2,4),],exportplot=TRUE,exportpath=getwd()))
  expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("evannoMethodStructure.png")
})

test_that("evannoMethodStructure error non-sequential k pdf",{
  expect_error(evannoMethodStructure(sr[c(1,2,4),],exportplot=TRUE,exportpath=getwd(),imgtype="pdf"))
  expect_equal("evannoMethodStructure.pdf" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("evannoMethodStructure.pdf")
})

test_that("evannoMethodStructure error non-sequential k jpeg",{
  expect_error(evannoMethodStructure(sr[c(1,2,4),],exportplot=TRUE,exportpath=getwd(),imgtype="jpeg"))
  expect_equal("evannoMethodStructure.jpg" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("evannoMethodStructure.jpg")
})

test_that("evannoMethodStructure error missing loci column",{
  srtemp <- sr
  srtemp$loci <- NULL
  expect_error(evannoMethodStructure(srtemp))
})

test_that("evannoMethodStructure warning loci column varies",{
  srtemp <- sr
  srtemp$loci[2] <- 20
  expect_error(evannoMethodStructure(srtemp))
})

test_that("evannoMethodStructure warning ind column varies",{
  srtemp <- sr
  srtemp$ind[2] <- 20
  expect_error(evannoMethodStructure(srtemp))
})

# clumppExport -----------------------------------------------------------------

testthat::context("clumppExport")
test_that("clumppExport",{
  clumppExport(slist,exportpath=getwd())
  expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
  if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)
  
  if(TRUE) {
    
    #structure clumpp export check prefix
    clumppExport(slist,prefix="Boom",exportpath=getwd())
    expect_equal(all(grepl("Boom",list.dirs()[-1])),TRUE)
    if(deleteoutput) unlink("Boom*", recursive = TRUE, force = TRUE)
    
    #structure clumpp export check prefix
    clumppExport(slist,prefix="Nanana",exportpath=getwd())
    expect_equal(all(grepl("Nanana",list.dirs()[-1])),TRUE)
    if(deleteoutput) unlink("Nanana*", recursive = TRUE, force = TRUE)
    
    #structure clumpp list export check
    clumppExport(slist,exportpath=getwd())
    expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
    if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)
  }
})

test_that("clumppExport errors",{
  expect_error(clumppExport())
  expect_error(clumppExport(slist))
})

# collectClumppOutput ----------------------------------------------------------

testthat::context("collectClumppOutput")
test_that("collectClumppOutput",{

  file.copy(from=list.dirs(path=system.file("files/clumpp",package="pophelper"),full.names=TRUE)[-1],to=getwd(),recursive = T)
  
  collectClumppOutput(runsdir=getwd(), newdir="aligned", filetype = "aligned")
  expect_equal(sum(grepl("aligned",list.dirs())),1)
  
  collectClumppOutput(filetype = "merged",runsdir=getwd(), newdir="merged")
  expect_equal(sum(grepl("merged",list.dirs())),1)
  
  collectClumppOutput(filetype = "both",runsdir=getwd(), newdir="both")
  expect_equal(sum(grepl("both",list.dirs())),1)
})

test_that("collectClumppOutput errors",{
  expect_error(collectClumppOutput())
  expect_error(collectClumppOutput(runsdir=getwd()))
  expect_error(collectClumppOutput(filetype="hello"))
  expect_error(collectClumppOutput(filetype=1))
  expect_error(collectClumppOutput(prefix=1))
})

if(deleteoutput) {
  unlink("pop*", recursive=TRUE, force=TRUE)
  unlink("aligned", recursive=TRUE, force=TRUE)
  unlink("merged", recursive=TRUE, force=TRUE)
  unlink("both", recursive=TRUE, force=TRUE)
}

# getDim --------------------------------------------------------

testthat::context("getDim")
test_that("getDim",{
  expect_error(pophelper:::getDim())
  expect_error(pophelper:::getDim(2))
  expect_error(getDim(imgtype="pdf"))
  expect_equal(class(getDim(2,imgtype="pdf")),"list")
  expect_equal(class(getDim(2,imgtype="png")),"list")
  expect_equal(class(getDim(2,imgtype="pdf",units="in")),"list")
  expect_equal(class(getDim(2,imgtype="pdf",units="cm")),"list")
  expect_equal(class(getDim(2,imgtype="pdf",units="mm")),"list")
  expect_equal(class(getDim(2,imgtype="pdf",units="px")),"list")
  expect_equal(class(getDim(2,imgtype="png",units="in")),"list")
  expect_equal(class(getDim(2,imgtype="png",units="cm")),"list")
  expect_equal(class(getDim(2,imgtype="png",units="mm")),"list")
  expect_equal(class(getDim(2,imgtype="png",units="px")),"list")
  
  expect_equal(class(getDim(2,imgtype="pdf",units="in",grplabheight=0.6)),"list")
  expect_equal(class(getDim(2,imgtype="pdf",units="cm",grplabheight=0.6)),"list")
  expect_equal(class(getDim(2,imgtype="pdf",units="mm",grplabheight=0.6)),"list")
  expect_equal(class(getDim(2,imgtype="pdf",units="px",grplabheight=0.6)),"list")
  expect_equal(class(getDim(2,imgtype="png",units="in",grplabheight=0.6)),"list")
  expect_equal(class(getDim(2,imgtype="png",units="cm",grplabheight=0.6)),"list")
  expect_equal(class(getDim(2,imgtype="png",units="mm",grplabheight=0.6)),"list")
  expect_equal(class(getDim(2,imgtype="png",units="px",grplabheight=0.6)),"list")
})

# getPlotParams ----------------------------------------------------------------

testthat::context("getPlotParams")
test_that("getPlotParams",{
  expect_equal(class(pophelper:::getPlotParams(grplab=grps1$V1, plotnum=1)),"list")
  expect_equal(class(pophelper:::getPlotParams(grplab=grps1$V1, plotnum=2)),"list")
  expect_equal(class(pophelper:::getPlotParams(grplab=grps1$V1, plotnum=1,grplabsize=5,grplabangle=90,grplabjust=0.5,pointsize=2,linesize=1)),"list")
  expect_error(pophelper:::getPlotParams())
})

# grpLabels --------------------------------------------------------------------

testthat::context("grpLabels")

#grps1 <- read.delim(system.file("files/structuregrplabels.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)

test_that("grpLabels output df",{
  expect_equal(class(grps1),"data.frame")
})

#grps2 <- read.delim(system.file("files/structuregrplabels2.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)

test_that("grpLabels check if grps df",{
  expect_equal(class(grps2),"data.frame")
})

slist <- readQ(sfiles)

test_that("grpLabels grplab data.frame error",{
expect_error(pophelper:::grpLabels(df=slist[[1]],grplab=grps1$V1))
})

test_that("grpLabels return list",{
expect_equal(class(pophelper:::grpLabels(df=slist[[1]],grplab=grps1)),"list")
})

test_that("grpLabels subsetgrp not present in grplabel",{
expect_error(pophelper:::grpLabels(df=slist[[1]],grplab=grps1,subsetgrp="popk"))
})

test_that("grpLabels subsetgrp Pop B",{
  expect_equal(unique(pophelper:::grpLabels(df=slist[[1]],grplab=grps1,subsetgrp="Pop B")$grplab)$V1,"Pop B")
})

test_that("grpLabels subsetgrp change order",{
  expect_equal(unique(pophelper:::grpLabels(df=slist[[1]],grplab=grps1,subsetgrp=c("Pop B","Pop A"))$grplab)$V1,c("Pop B","Pop A"))
})

test_that("grpLabels ordergrp",{
  expect_equal(unique(pophelper:::grpLabels(df=slist[[1]],grplab=grps1)$grplab)$V1,c("Pop A","Pop B"))
})

test_that("grpLabels errors",{
  expect_error(pophelper:::grpLabels())
  expect_error(pophelper:::grpLabels("hello"))
  expect_error(pophelper:::grpLabels(df=slist[[1]]))
})

# sortInd ----------------------------------------------------------------------

testthat::context("sortInd")

test_that("sortInd return NA",{
expect_equal(pophelper:::sortInd(slist[[1]])$grplab,NA)
})

#grplabs <- read.delim(system.file("files/grplab.txt",package="pophelper"),stringsAsFactors=F)

test_that("sortInd check if grps df",{
  expect_equal(class(grplabs),"data.frame")
})

test_that("sortInd original order",{
expect_equal(rownames(pophelper:::sortInd(slist[[1]],grplab=grplabs)$dframe),as.character(1:149))
})

test_that("sortInd error sortind",{
expect_error(pophelper:::sortInd(slist[[1]],grplab=grplabs,sortind="blue"))
})

pophelper:::sortInd(slist[[1]],grplab=grplabs,sortind="all")
pophelper:::sortInd(slist[[1]],grplab=grplabs,sortind="label")
pophelper:::sortInd(slist[[1]],grplab=grplabs,sortind="Cluster2")
pophelper:::sortInd(slist[[1]],grplab=grplabs,selgrp="mixed",sortind="all")

test_that("sortInd errors",{
  expect_error(pophelper:::sortInd())
  expect_error(pophelper:::sortInd("hello"))
})

# plotQ  --------------------------------------------------------------

testthat::context("plotQ")

grpsrep <- read.delim(system.file("files/structuregrplabels-rep.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
grplabs <- read.delim(system.file("files/grplab.txt",package="pophelper"),stringsAsFactors=F)

slist <- readQ(sfiles)
imgout <- c("sep","join")

# LOOP ----------------------------------------------------

for(i in seq_along(imgout))
{
  imgo <- imgout[i]
  
  #plot one sep
  plotQ(slist[1],exportpath=getwd())
  
  #plot many
  plotQ(slist[1:2],imgoutput=imgo,exportpath=getwd())
  
  #plot many outputfilename
  plotQ(slist[1:2],imgoutput="sep",outputfilename=c("this","that"),exportpath=getwd())
  
  #showsp
  plotQ(slist[1:2],imgoutput=imgo,showsp=F,exportpath=getwd())
  
  #showsp
  plotQ(slist[1:2],imgoutput=imgo,splab=c("this","that"),splabsize=8,splabcol="blue",splabface="bold",spbgcol="green",exportpath=getwd())
  
  #change barsize
  plotQ(slist[1:2],imgoutput=imgo,barsize=0.8,exportpath=getwd())
  
  #one ind plot
  t1 <- slist[1:2]
  t1[[1]] <- t1[[1]][1,]
  t1[[2]] <- t1[[2]][1,]
  plotQ(t1[1:2],imgoutput=imgo,exportpath=getwd())
  
  #1 ind with lab
  plotQ(t1[1:2],imgoutput=imgo,grplab=data.frame("lab"="A",stringsAsFactors=F),exportpath=getwd())
  
  #check orderind cluster
  plotQ(slist[1:2],imgoutput=imgo,sortind="Cluster1",sharedindlab=F,exportpath=getwd())
  
  #check orderind all
  plotQ(slist[1:2],imgoutput=imgo,sortind="all",sharedindlab=F,exportpath=getwd())
  
  #check orderind label
  plotQ(slist[1:2],imgoutput=imgo,sortind="label",exportpath=getwd())
  
  #clustercol
  plotQ(slist[1:2],imgoutput=imgo,clustercol=c("red","green"),exportpath=getwd())
  
  #single labels
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],exportpath=getwd())
  
  #single labels showsp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],splab=c("this","that"),splabsize=8,splabcol="blue",splabface="bold",spbgcol="green",exportpath=getwd())
  
  #showsp pos
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],splab=c("this","that"),sppos="left",exportpath=getwd())
  
  #single labels ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,exportpath=getwd())
  
  #single labels ordergrp sortind all
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="all",sharedindlab=F,exportpath=getwd())
  
  #single labels ordergrp sortind Cluster1
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="Cluster1",sharedindlab=F,exportpath=getwd())
  
  #single labels ordergrp sortind label
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="label",exportpath=getwd())
  
  #single labels ordergrp sortind label show indlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="label",showindlab=TRUE,exportpath=getwd())
  
  #multiple labels
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,exportpath=getwd())
  
  #multiple labels change selgrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",exportpath=getwd())
  
  #multiple labels grpmean
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T,exportpath=getwd())
  
  #multiple labels grpmean change selgrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T,selgrp="mixed",exportpath=getwd())
  
  #multiple labels grpmean ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T,ordergrp=T,exportpath=getwd())
  
  #multiple labels ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE,exportpath=getwd())
  
  #multiple labels selgrp ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",ordergrp=TRUE,exportpath=getwd())
  
  #multiple labels selgrp ordergrp divgrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",ordergrp=TRUE,divgrp="lab2",exportpath=getwd())
  
  #multiple labels selgrp ordergrp divgrp double
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",ordergrp=TRUE,divgrp=c("lab2","lab1"),exportpath=getwd())
  
  #multiple labels selgrp ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="lab2",ordergrp=TRUE,exportpath=getwd())
  
  #multiple labels ordergrp sortind all
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE,sortind="all",sharedindlab=F,exportpath=getwd())
  
  #multiple labels selgrp ordergrp sortind all
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="lab2",ordergrp=TRUE,sortind="all",sharedindlab=F,exportpath=getwd())
  
  #multiple labels sortind Cluster1
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,sortind="Cluster1",sharedindlab=F,ordergrp=TRUE,exportpath=getwd())
  
  #multiple labels ordergrp sortind label
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE,sortind="label",exportpath=getwd())
  
  #multiple labels ordergrp sortind label show indlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE,sortind="label",showindlab=TRUE,exportpath=getwd())
  
  #multiple labels grpmean
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T,showindlab=TRUE,exportpath=getwd())
  
  #multiple labels grpmean showindlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T,showindlab=TRUE,width=15,exportpath=getwd())
  
  #multiple labels ordergrp subsetgrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=T,subsetgrp="b",exportpath=getwd())
  
  #multiple labels ordergrp reorder
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=T,subsetgrp=c("c","b"),exportpath=getwd())
  
  #multiple labels selgrp ordergrp reorder
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",ordergrp=T,subsetgrp=c("fds","erd"),exportpath=getwd())
  
  #multiple labels selgrp ordergrp reorder showindlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed",ordergrp=T,subsetgrp=c("fds","erd"),showindlab=TRUE,width=15,exportpath=getwd())
  
  #single label line size
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,linesize=2,linecol="green",linetype="22",linealpha=0.6,pointsize=0.6,pointtype=16,pointalpha=0.6,pointcol="red",exportpath=getwd())
  
  #imgtype jpeg
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,imgtype="jpeg",exportpath=getwd())
  
  #imgtype pdf
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,imgtype="pdf",exportpath=getwd())
  
  if(testtiff)
  {
    #imgtype tiff
    plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,imgtype="tiff",exportpath=getwd())
  }
  
  #check output sep with rep labels
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,exportpath=getwd())
  
  #check grpmean with rep labels
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,grpmean=T,exportpath=getwd())
  
  #check grpmean with rep labels sortind
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,sortind="all",sharedindlab=F,ordergrp=TRUE,exportpath=getwd())
  
  #check grpmean with rep labels sortind
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,sortind="label",ordergrp=TRUE,exportpath=getwd())
  
  #check grpmean with rep labels sortind showindlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,sortind="label",showindlab=T,width=15,ordergrp=TRUE,exportpath=getwd())
  
  expect_error(plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,subsetgrp="Pop A"),exportpath=getwd())
  
  #use ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,subsetgrp="Pop A",ordergrp=T,exportpath=getwd())
  
  #indlabwithgrplab F
  plotQ(slist[1:2],imgoutput=imgo,showindlab=T,useindlab=F,indlabwithgrplab=F,grplab=grplabs,exportpath=getwd())
  
  #indlabwithgrplab T
  plotQ(slist[1:2],imgoutput=imgo,showindlab=T,useindlab=F,indlabwithgrplab=T,grplab=grplabs,exportpath=getwd())
  
  #sharedindlab
  if(imgo=="join")
  {
    #sharedindlab off
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=F,outputfilename="joined",exportpath=getwd())
    #sharedindlab on
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,outputfilename="joined",exportpath=getwd())
    #sharedindlab on with sort label
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,sort="label",outputfilename="joined",exportpath=getwd())
    #sharedindlab with sort all
    expect_error(plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sortind="all"),exportpath=getwd())
    #sharedindlab with sort cluster
    expect_error(plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sortind="Cluster1"),exportpath=getwd())
    #sharedindlab off with grplab
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=F,grplab=grplabs,outputfilename="joined",exportpath=getwd())
    #sharedindlab on with grplab
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,grplab=grplabs,width=16,outputfilename="joined",exportpath=getwd())
    #shared indlab with repeated grplab
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,grplab=grpsrep,outputfilename="joined",exportpath=getwd())
    #sharedindlab with grplab subset
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,grplab=grplabs,subsetgrp="a",width=16,outputfilename="joined",exportpath=getwd())
    #sharedindlab with reorder
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,grplab=grplabs,subsetgrp=c("b","a"),width=16,outputfilename="joined",exportpath=getwd())
    #error sharedindlab error
    expect_error(plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,showindlab=T,sortind="Cluster1",exportpath=getwd()))
  }
  
  #showyaxis
  plotQ(slist[1:2],imgoutput=imgo,showyaxis=T,exportpath=getwd())
  
  #showyaxis grplab
  plotQ(slist[1:2],imgoutput=imgo,showyaxis=T,grplab=grplabs,exportpath=getwd())
  
  #showyaxis ticks
  plotQ(slist[1:2],imgoutput=imgo,showyaxis=T,showticks=T,showindlab=T,exportpath=getwd())
  
  #showyaxis ticks ticksize ticklength
  plotQ(slist[1:2],imgoutput=imgo,showyaxis=T,showticks=T,showindlab=T,ticksize=0.5,ticklength=0.5,exportpath=getwd())
  
  #basesize
  plotQ(slist[1:2],imgoutput=imgo,showyaxis=T,showindlab=T,basesize=12,exportpath=getwd())
  
  #panelratio
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,panelratio=c(2,4),exportpath=getwd())
  
  #legend
  plotQ(slist[1:2],imgoutput=imgo,showlegend=T,exportpath=getwd())
  
  #legendmargin
  plotQ(slist[1:2],imgoutput=imgo,showlegend=T,legendmargin=c(4,4,4,4),exportpath=getwd())
  
  #exportplot
  plotQ(slist[1:2],imgoutput=imgo,exportplot=F)
  
  #returnplot
  p <- plotQ(slist[1:2],imgoutput=imgo,exportplot=F,returnplot=T)
  expect_equal(class(p),"list")
  expect_equal(names(p),c("plot","data"))
  expect_equal(class(p[[1]][[1]])[1],"gg")
  
  #returnplot returndata
  p <- plotQ(slist[1:2],imgoutput=imgo,exportplot=F,returnplot=T,returndata=T,grplab=grplabs)
  expect_equal(class(p),"list")
  expect_equal(names(p),c("plot","data"))
  expect_equal(class(p[[1]][[1]])[1],"gtable")
}

#ERRORS

test_that("plotQ errors",{
  # exportpath not specified
  expect_error(plotQ(slist))
  
  expect_error(plotQ(slist,exportpath=getwd(),imgoutput="hello"))
  expect_error(plotQ(slist,exportpath=getwd(),clustercol="red"))
  expect_error(plotQ(slist,exportpath=getwd(),useindlab="T"))
  expect_error(plotQ(slist,exportpath=getwd(),grpmean="T"))
  expect_error(plotQ(slist,exportpath=getwd(),splabface=TRUE))
  expect_error(plotQ(slist,exportpath=getwd(),sppos="test"))
  expect_error(plotQ(slist,exportpath=getwd(),showdiv="T"))
  expect_error(plotQ(slist,exportpath=getwd(),legendpos="outside"))
  expect_error(plotQ(slist,exportpath=getwd(),legendkeysize="T"))
  expect_error(plotQ(slist,exportpath=getwd(),legendtextsize="T"))
  expect_error(plotQ(slist,exportpath=getwd(),font=1))
  expect_error(plotQ(slist,exportpath=getwd(),barsize="T"))
  expect_error(plotQ(slist,exportpath=getwd(),panelratio="T"))
  expect_error(plotQ(slist,exportpath=getwd(),grplabpos=2))
  expect_error(plotQ(slist,exportpath=getwd(),linepos=2))
  
  #input not list
  expect_error(plotQ(slist[[1]],exportpath=getwd()))
  
  #check less colours
  expect_error(plotQ(slist[1],clustercol="red",exportpath=getwd()))
  
  #showsp
  expect_error(plotQ(slist[1],showsp="a",exportpath=getwd()))
  
  #splab
  expect_error(plotQ(slist[1],splab=NULL,exportpath=getwd()))
})


if(testfont)
{
  #change font
  plotQ(qlist=slist[1:2],grplab=grplabs,font="patrick",exportpath=getwd())
  
  #join change font
  plotQ(qlist=slist[1:2],imgoutput="join",grplab=grplabs,font="Verdana",exportpath=getwd())
  
  if(testtiff)
  {
    #tiff output single
    plotQ(qlist=slist[1],grplab=grplabs,font="Verdana",imgtype="tiff",exportpath=getwd())
  }
}

if(deleteoutput) file.remove(list.files())

#barbordersize
#barbordercolour
plotQ(slist[1],barbordersize=0.1,barbordercolour="black",exportpath=getwd())

#splabangle
plotQ(slist[1],exportpath=getwd())
plotQ(slist[1],splabangle=-90,exportpath=getwd())
plotQ(slist[1],sppos="left",exportpath=getwd())
plotQ(slist[1],sppos="left",splabangle=90,exportpath=getwd())
plotQ(slist[1],showsp=F,exportpath=getwd())
plotQ(slist[1:2],imgoutput = "join",outputfilename = "structure_01",exportpath=getwd())
plotQ(slist[1:2],imgoutput="join",splabangle=90,outputfilename = "structure_01",exportpath=getwd())
plotQ(slist[1:2],imgoutput="join",grplab=grplabs,splabangle=90,outputfilename = "structure_01",exportpath=getwd())

#title and subtitle
plotQ(slist[1],exportpath=getwd())
plotQ(slist[1],showtitle=T,exportpath=getwd())
plotQ(slist[1],showsubtitle=T,exportpath=getwd())
plotQ(slist[1],showtitle=T,showsubtitle=T,exportpath=getwd())
plotQ(slist[1],showtitle=T,showsubtitle=T,titlelab="Galapagos",subtitlelab="Sample from the region of Galapagos",exportpath=getwd())
plotQ(slist[1],showtitle=T,showsubtitle=T,titlecol="green",subtitlecol="blue",titlehjust=1,subtitlehjust=1,exportpath=getwd())
plotQ(slist[1],showtitle=T,showsubtitle=T,titlevjust=0,subtitlevjust=0,exportpath=getwd())
plotQ(slist[1],showtitle=T,showsubtitle=T,titleangle=45,subtitleangle=45,exportpath=getwd())
plotQ(slist[1],showtitle=T,showsubtitle=T,grplab=grplabs,exportpath=getwd())
plotQ(slist[1],showtitle=T,showsubtitle=T,titlelab="Galapagos",
      subtitlelab="Sample from the region of Galapagos",grplab=grplabs,exportpath=getwd())
plotQ(slist[1:2],imgoutput="join",exportpath=getwd())
plotQ(slist[1:2],imgoutput="join",showtitle=T,showsubtitle=T,exportpath=getwd())
plotQ(slist[1:2],imgoutput="join",outputfilename="structure_01",exportpath=getwd())
plotQ(slist[1:2],imgoutput="join",outputfilename="structure_01",showtitle=T,showsubtitle=T,
      titlelab="Galapagos",subtitlelab="Sample from the region of Galapagos",exportpath=getwd())
plotQ(slist[1:2],imgoutput="join",outputfilename="structure_01",showtitle=T,showsubtitle=T,
      titlelab="Galapagos",subtitlelab="Sample from the region of Galapagos",grplab=grplabs,sppos="left",exportpath=getwd())
plotQ(slist[1:2],imgoutput="join",outputfilename="structure_01",showtitle=T,showsubtitle=T,
      titlelab="Galapagos",subtitlelab="Sample from the region of Galapagos",
      showlegend=F,legendpos="right",exportpath=getwd())
plotQ(slist[1:2],imgoutput="join",outputfilename="structure_01",showtitle=T,
      titlelab="Galapagos",showlegend=T,legendpos="right",exportpath=getwd())

#indlabspacer
plotQ(slist[1],showindlab=T,exportpath=getwd())
plotQ(slist[1],showindlab=T,indlabspacer=5,exportpath=getwd())
plotQ(slist[1],showindlab=T,indlabspacer=-1,exportpath=getwd())
plotQ(slist[1:2],showindlab=T,indlabspacer=5,exportpath=getwd())
plotQ(slist[1:2],imgoutput="join",showindlab=T,indlabspace=-1,outputfilename="structure_01",exportpath=getwd())

# showgrplab
plotQ(slist[1],exportpath=getwd(),grplab=grplabs,showgrplab=FALSE)
plotQ(slist[1],exportpath=getwd(),grplab=grplabs,showgrplab=FALSE,imgtype="jpeg")
plotQ(slist[1],exportpath=getwd(),grplab=grplabs,showgrplab=FALSE,imgtype="pdf")
plotQ(slist[1:2],exportpath=getwd(),grplab=grplabs,showgrplab=FALSE,imgoutput="join")
plotQ(slist[1:2],exportpath=getwd(),grplab=grplabs,showgrplab=FALSE,imgtype="jpeg",imgoutput="join")
plotQ(slist[1:2],exportpath=getwd(),grplab=grplabs,showgrplab=FALSE,imgtype="pdf",imgoutput="join")

if(deleteoutput) file.remove(list.files())

# plotQ Clumpp --------------------------------------------------------------

testthat::context("plotQ Clumpp")
cfiles <- c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))
clist <- readQ(cfiles)

#check output table
plotQ(qlist=clist,exportpath=getwd())

#check output table
plotQ(qlist=clist,grpmean=T,exportpath=getwd())

#check output table sort cluster
plotQ(qlist=clist,sortind="Cluster1",exportpath=getwd())

#check output table sort cluster
plotQ(qlist=clist,imgoutput="join",sortind="Cluster1",sharedindlab=F,exportpath=getwd())

#check output table pop lab
plotQ(qlist=clist,grplab=grps1,exportpath=getwd())

#check output table pop lab sort all
plotQ(qlist=clist,imgoutput="sep",grplab=grplabs,sortind="all",exportpath=getwd())

#check output table pop lab sort cluster
plotQ(qlist=clist,imgoutput="sep",grplab=grplabs,sortind="Cluster1",exportpath=getwd())

#check output table pop lab pop mean
plotQ(qlist=clist,imgoutput="sep",grplab=grplabs,grpmean=T,exportpath=getwd())

#check output table pop lab rep
plotQ(qlist=clist,imgoutput="sep",grplab=grpsrep,exportpath=getwd())

#check output table pop lab rep sort
plotQ(qlist=clist,imgoutput="sep",grplab=grpsrep,sortind="all",exportpath=getwd())

#check output table pop lab rep sort pop mean
plotQ(qlist=clist,imgoutput="sep",grplab=grpsrep,sortind="all",grpmean=T,exportpath=getwd())

if(deleteoutput) file.remove(list.files())

expect_error(plotQ(qlist=clist,imgoutput="sep",grplab=grpsrep,subsetgrp="Pop B",exportpath=getwd()))

# plotQMultiline ---------------------------------------------------------------

testthat::context("plotQMultiline")

slist <- readQ(list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE))
grpsrep <- read.delim(system.file("files/structuregrplabels-rep.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
grplabs <- read.delim(system.file("files/grplab.txt",package="pophelper"),stringsAsFactors=F)

#plotQMultiline

#sfiles 1 check output
plotQMultiline(slist[1],exportpath=getwd())

#sfiles 1 check output sort cluster
plotQMultiline(slist[1],sortind="Cluster1",exportpath=getwd())

#sfiles 1 check output sort all
plotQMultiline(slist[1],sortind="all",exportpath=getwd())

#sfiles >1 check output
plotQMultiline(slist[1:2],exportpath=getwd())

#sfiles >1 sort cluster
plotQMultiline(slist[1:2],sortind="Cluster1",exportpath=getwd())

#sfiles >1 sort all
plotQMultiline(slist[1:2],sortind="all",exportpath=getwd())

#sfiles 1 sort NA indlabfromfile
plotQMultiline(slist[1:2],sortind=NA,useindlab=T,exportpath=getwd())

#sfiles 1 sort all indlabfromfile=T
plotQMultiline(slist[1:2],sortind="all",useindlab=T,exportpath=getwd())

#jpeg output
plotQMultiline(slist[1],imgtype="jpeg",exportpath=getwd())

if(testtiff)
{
  #tiff output
  plotQMultiline(slist[1],imgtype="tiff",exportpath=getwd())
}

#pdf output
plotQMultiline(slist[1],imgtype="pdf",exportpath=getwd())

inds <- read.delim(system.file("files/structureindlabels.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)

#files 1 indlab
rownames(slist[[1]]) <- inds$V1
plotQMultiline(slist[1],useindlab=T,exportpath=getwd())

#sfiles 1 check output sort cluster1 indlab
plotQMultiline(slist[1],sortind="Cluster1",useindlab=T,exportpath=getwd())

#grplab with useind one lab
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs[,1,drop=F],useindlab=T,exportpath=getwd())

#grplab with useind two lab
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs[,1:2],useindlab=T,exportpath=getwd())

#grplab with useind
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T,exportpath=getwd())

#grplab with useind and selgrp
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T,selgrp="lab2",exportpath=getwd())

#grplab with useind and selgrp, ordergrp
plotQMultiline(slist[1],barsize=1,lpp=6,grplab=grplabs,useindlab=T,selgrp="mixed",ordergrp=T,exportpath=getwd())

#grplab with useind, sortind cluster1
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T, sortind="Cluster1",exportpath=getwd())

#grplab with useind, sortind all
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T, sortind="all",exportpath=getwd())

#grplab with useind, sortind label
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T, sortind="label",exportpath=getwd())

#grplab without useind, sortind label
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=F, sortind="label",exportpath=getwd())

#grplab without useind, subset
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=F, subsetgrp="b",exportpath=getwd())

#grplab without useind, reorder
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=F, subsetgrp=c("b","a"),exportpath=getwd())

#grplab without useind, reorder, selgrp, orgergrp
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=F, selgrp="mixed",ordergrp=TRUE,subsetgrp=c("fds","rga"),exportpath=getwd())

#grplab without useind, sort cluster, subset
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=F, sortind="Cluster1", subsetgrp=c("c"),exportpath=getwd())

#grplab with useind, sort cluster, reorder
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs,useindlab=T, sortind="Cluster1", subsetgrp=c("b","c"),exportpath=getwd())

#grplab without useind, sort label, reorder
plotQMultiline(slist[1],barsize=1,lpp=7,grplab=grplabs, grpmean=T,exportpath=getwd())

#modify options
plotQMultiline(slist[1],lpp=7,grplab=grplabs,indlabsize=5,indlabangle=45,indlabvjust=1,indlabhjust=1,indlabcol="darkgreen",
               grplabsize=4,grplabbgcol="skyblue",grplabcol="red",clustercol=c("coral","steelblue"),showticks=T,showyaxis=T,
               mar=c(0.1,0.5,0.1,0.5),theme="theme_bw",exportpath=getwd())

#output file type
plotQMultiline(slist[1],grplab=grplabs,imgtype="jpeg",exportpath=getwd())

#output file type
plotQMultiline(slist[1],grplab=grplabs,imgtype="pdf",exportpath=getwd())

#output file type
if(testtiff) plotQMultiline(slist[1],grplab=grplabs,imgtype="tiff",exportpath=getwd())

#grplab theme font
if(testfont) plotQMultiline(slist[1],theme="theme_bw",font="patrick",exportpath=getwd())

#barbordersize
#barbordercolour
plotQMultiline(slist[1],barbordercolour="red",barbordersize=0.2,exportpath=getwd())
plotQMultiline(slist[1],barbordercolour="black",barbordersize=0.2,barsize=1,exportpath=getwd())

#indlabspacer
plotQMultiline(slist[1],exportpath=getwd())
plotQMultiline(slist[1],indlabspacer=3,exportpath=getwd())
plotQMultiline(slist[1],indlabspacer=-1,exportpath=getwd())

#title and subtitle
plotQMultiline(slist[1],exportpath=getwd())
plotQMultiline(slist[1],showtitle=T,showsubtitle=T,exportpath=getwd())
plotQMultiline(slist[1],showtitle=T,showsubtitle=T,lpp=6,
               titlelab="Galapagos",subtitlelab="Sample from the region of Galapagos",exportpath=getwd())
plotQMultiline(slist[1],showtitle=T,showsubtitle=T,lpp=6,titlehjust=1,titlecol="blue",exportpath=getwd())

#indlabwithgrplab
plotQMultiline(slist[1],grplab=grplabs,indlabwithgrplab=T,exportpath=getwd())

#show legend
plotQMultiline(slist[1],showlegend=T,exportpath=getwd())

#show legend grplab
plotQMultiline(slist[1],showlegend=T,grplab=grplabs,exportpath=getwd())

#show legend legend size
plotQMultiline(slist[1],showlegend=T,legendkeysize=15,legendtextsize=10,exportpath=getwd())

#show legend legend margin
plotQMultiline(slist[1],showlegend=T,legendmargin=c(15,15,15,15),exportpath=getwd())

#ticks
plotQMultiline(slist[1],showyaxis=T,showticks=T,ticklength=2,exportpath=getwd())


#exportplot
plotQMultiline(slist[1],exportplot=F,exportpath=getwd())

#returnplot
p <- plotQMultiline(slist[1],exportplot=F,returnplot=T)
expect_equal(class(p),"list")
expect_equal(names(p),c("plot","data"))
expect_equal(class(p[[1]][[1]][[1]])[1],"gtable")

#returnplot returndata
p <- plotQMultiline(slist[1],exportplot=F,returnplot=T,returndata=T,grplab=grplabs)
expect_equal(class(p),"list")
expect_equal(names(p),c("plot","data"))
expect_equal(class(p[[1]][[1]][[1]])[1],"gtable")

#many runs
plotQMultiline(slist[1:3],spl=20,lpp=4,exportpath=getwd())

test_that("plotQMultiline errors",{
  # exportpath not specified
  expect_error(plotQMultiline(slist))
  expect_error(plotQMultiline(slist,exportpath=getwd(),imgoutput="hello"))
  expect_error(plotQMultiline(slist,exportpath=getwd(),clustercol="red"))
  expect_error(plotQMultiline(slist,exportpath=getwd(),useindlab="T"))
  expect_error(plotQMultiline(slist,exportpath=getwd(),showticks="T"))
  expect_error(plotQMultiline(slist,exportpath=getwd(),showyaxis="T"))
  expect_error(plotQMultiline(slist,exportpath=getwd(),font=1))
  expect_error(plotQMultiline(slist,exportpath=getwd(),barsize="T"))
})

if(deleteoutput) file.remove(list.files())

# collectTessRuns --------------------------------------------------------------

test_that("collectTessRuns errors",{
  expect_error(collectRunsTess())
  expect_error(collectRunsTess(runsdir = getwd()))
  expect_error(collectRunsTess(newdir = getwd()))
})

dir.create("run1")
dir.create("run2")
dir.create("run3")
write("1234",file="run1/bla-TR.txt")
write("546",file="run2/bla-TR.txt")
write("23.6",file="run3/bla-TR.txt")

collectRunsTess(runsdir=getwd(),newdir="output")

expect_equal(any(grepl("output",list.dirs()[-1])),TRUE)

if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
if(deleteoutput) file.remove(list.files())

# analyseQ ---------------------------------------------------------------------

testthat::context("analyseQ")

test_that("analyseQ Structure check output",{
  analyseQ(sfiles,exportpath=getwd())
  expect_equal(any(grepl("evannoMethodStructure.png",list.files())),TRUE)
  expect_equal(any(grepl("evannoMethodStructure.txt",list.files())),TRUE)
  expect_equal(any(grepl("tabulateQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("structure_01.png",list.files())),TRUE)
  expect_equal(any(grepl("structure_17.png",list.files())),TRUE)
  if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
  if(deleteoutput) file.remove(list.files())
})

test_that("analyseQ TESS check output",{
  analyseQ(tfiles,exportpath=getwd())
  expect_equal(any(grepl("tabulateQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("tess_01.png",list.files())),TRUE)
  expect_equal(any(grepl("tess_21.png",list.files())),TRUE)
  if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
  if(deleteoutput) file.remove(list.files())
})

test_that("analyseQ Admixture check output",{
  analyseQ(afiles,exportpath=getwd())
  expect_equal(any(grepl("tabulateQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("admixture_01.png",list.files())),TRUE)
  expect_equal(any(grepl("admixture_10.png",list.files())),TRUE)
  if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
  if(deleteoutput) file.remove(list.files())
})

test_that("analyseQ BAPS check output",{
  analyseQ(bfiles,exportpath=getwd())
  expect_equal(any(grepl("tabulateQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("baps_k2.png",list.files())),TRUE)
  if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
  if(deleteoutput) file.remove(list.files())
})

test_that("analyseQ errors",{
  expect_error(analyseQ())
  expect_error(analyseQ(sfiles))
  expect_error(analyseQ(sfiles,evannomethod="T"))
  expect_error(analyseQ(sfiles,align="T"))
  expect_error(analyseQ(sfiles,plotruns="T"))
  expect_error(analyseQ(sfiles,writetable="T"))
  expect_error(analyseQ(sfiles,sorttable="T"))
})

# distructExport -----------------------------------------------------

testthat::context("distructExport")
#grps1 <- read.delim(system.file("files/structuregrplabels.txt",package="pophelper"),header=FALSE)

test_that("distructExport Structure one file",{
  distructExport(qlist = slist[1],exportpath=getwd())
    expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
  if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
})

test_that("destructExport Structure top and bottom labels",{
  distructExport(slist[1],grplabbottom=as.character(grps1$V1),grplabtop=as.character(grps1$V1),grpmean=T,exportpath=getwd())
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
  if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
})

test_that("destructExport Structure top and botoom labels",{
  distructExport(slist[1],grplabbottom=as.character(grps1$V1),grplabtop=as.character(grps1$V1),exportpath=getwd())
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
  if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
})

test_that("destructExport structure no bottom labels",{
  distructExport(slist[1],grplabbottom=NA,grplabtop=as.character(grps1$V1),exportpath=getwd())
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
  if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
})

test_that("destructExport Structure no top labels",{
  distructExport(slist[1],grplabbottom=as.character(grps1$V1),grplabtop=NA,exportpath=getwd())
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
  if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
})

test_that("destructExport Structure both labels",{
  distructExport(slist[1],grplabbottom=NA,grplabtop=NA,exportpath=getwd())
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
  if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
})

test_that("destructExport Structure multiple files",{
  distructExport(slist[2:5],grplabbottom=as.character(grps1$V1),exportpath=getwd())
  expect_equal(length(list.dirs(recursive=F)),4)
  if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
})

test_that("destructExport Structure grpmean true",{
  distructExport(slist[1],grplabbottom=as.character(grps1$V1),grpmean=T,exportpath=getwd())
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
  if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
})

test_that("destructExport Structure multiple files",{
  distructExport(slist[2:5],grplabbottom=as.character(grps1$V1),grpmean=T,exportpath=getwd())
  expect_equal(length(list.dirs(recursive=F)),4)
  if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
})

test_that("destructExport Structure other colours",{
  distructExport(slist[1],grplabbottom=as.character(grps1$V1),clustercol=pophelper:::distructColours()[43:44],exportpath=getwd())
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
  if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
})

if(deleteoutput) file.remove(list.dirs(recursive=F))

test_that("distructExport errors",{
  expect_error(distructExport())
  expect_error(distructExport(slist))
  expect_error(distructExport(slist,exportpath=getwd(),grpmean="T"))
  expect_error(distructExport(slist,exportpath=getwd(),overwritedirs="T"))
  expect_error(distructExport(slist,exportpath=getwd(),printtitle="T"))
  expect_error(distructExport(slist,exportpath=getwd(),sepline="T"))
  expect_error(distructExport(slist,exportpath=getwd(),grayscale="T"))
  expect_error(distructExport(slist,exportpath=getwd(),printcolorbrewer="T"))
  expect_error(distructExport(slist,exportpath=getwd(),echodata="T"))
  expect_error(distructExport(slist,exportpath=getwd(),printdata="T"))
})

# getColours -------------------------------------------------------------------

testthat::context("getColours")

test_that("getColours character input",{
  expect_error(pophelper:::getColours("10"))
})

test_that("getColours 10 colours",{
  x <- pophelper:::getColours(10)
  expect_length(x,10)
  expect_vector(x)
  expect_equal(class(x),"character")
})

test_that("getColours 13 colours",{
  x <- pophelper:::getColours(13)
  expect_length(x,13)
  expect_vector(x)
  expect_equal(class(x),"character")
})

# unitConverter ----------------------------------------------------------------

testthat::context("unitConverter")

test_that("UnitConverter output",{
  expect_equal(pophelper:::unitConverter(2,"cm","cm"),2)
  expect_equal(pophelper:::unitConverter(2,"cm","mm"),20)
  expect_equal(pophelper:::unitConverter(2,"cm","in"),0.79)
  expect_equal(pophelper:::unitConverter(2,"cm","px",dpi=250),197)
  expect_equal(pophelper:::unitConverter(2,"in","in"),2)
  expect_equal(pophelper:::unitConverter(2,"in","cm"),5.08)
  expect_equal(pophelper:::unitConverter(2,"in","mm"),0.51)
  expect_equal(pophelper:::unitConverter(2,"in","px",dpi=250),500)
  expect_equal(pophelper:::unitConverter(2,"mm","mm"),2)
  expect_equal(pophelper:::unitConverter(2,"mm","cm",dpi=250),0.2)
  expect_equal(pophelper:::unitConverter(2,"mm","in",dpi=250),0.08)
  expect_equal(pophelper:::unitConverter(2,"mm","px",dpi=250),20)
  expect_equal(pophelper:::unitConverter(2,"px","px",dpi=250),2)
  expect_equal(pophelper:::unitConverter(2,"px","cm",dpi=250),0.02032)
  expect_equal(pophelper:::unitConverter(2,"px","in",dpi=250),0.008)
  expect_equal(pophelper:::unitConverter(2,"px","mm",dpi=250),0.2032)
})

test_that("errors",{
  expect_error(pophelper:::unitConverter(2,"cm","px"))
  expect_error(pophelper:::unitConverter(2,"mm","px"))
  expect_error(pophelper:::unitConverter(2,"in","px"))
  expect_error(pophelper:::unitConverter(2))
  expect_error(pophelper:::unitConverter("cm","px"))
  expect_error(pophelper:::unitConverter())
})

# verifyGrplab -----------------------------------------------------------------

testthat::context("verifyGrplab")

test_that("grplab class",{
  expect_equal(class(grps1),"data.frame")
  expect_equal(class(grps2),"data.frame")
  expect_equal(class(grplabs),"data.frame")
})

test_that("verifyGrplab",{
  expect_silent(pophelper:::verifyGrplab(grps1))
  expect_silent(pophelper:::verifyGrplab(grps2))
  expect_silent(pophelper:::verifyGrplab(grplabs))
})

test_that("verifyGrplab errors",{
  expect_error(pophelper:::verifyGrplab())
  expect_error(pophelper:::verifyGrplab(grps1$V1))
  expect_error(pophelper:::verifyGrplab(grplabs$lab1))
  
  grptemp <- grplabs
  grptemp$lab2 <- as.factor(grptemp$lab2)
  expect_error(pophelper:::verifyGrplab(grptemp))
  
  grptemp <- grplabs
  grptemp$lab2[3] <- NA
  expect_error(pophelper:::verifyGrplab(grptemp))
  rm(grptemp)
})

# distructColours --------------------------------------------------------------

test_that("distructColours",{
  expect_vector(distructColours())
  expect_length(distructColours(),90)
})

# End --------------------------------------------------------------------------

if(deleteoutput) unlink("pophelper-demo",recursive=T,force=T)

