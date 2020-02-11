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
}else{
  file.remove(list.files())
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
#read sample BAPS files from R package
bfiles <- list.files(path=system.file("files/baps",package="pophelper"),full.names=TRUE)
#read sample MATRIX files from R package
mcfiles <- list.files(path=system.file("files/basic/comma",package="pophelper"),full.names=TRUE)
mtfiles <- list.files(path=system.file("files/basic/tab",package="pophelper"),full.names=TRUE)
msfiles <- list.files(path=system.file("files/basic/space",package="pophelper"),full.names=TRUE)
cfiles <- c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))
grps1 <- read.delim(system.file("files/structuregrplabels.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
grps2 <- read.delim(system.file("files/structuregrplabels2.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)

ftype <- c("sfiles","sfiles1","tfiles","afiles","ffiles",
            "mcfiles","mtfiles","msfiles","cfiles","bfiles")

slist <- readQ(sfiles)
slist1 <- readQ(sfiles1)
tlist <- readQ(tfiles)
alist <- readQ(afiles)
flist <- readQ(ffiles)
blist <- readQ(bfiles)
mclist <- readQ(mcfiles)
mtlist <- readQ(mtfiles)
mslist <- readQ(msfiles)
clist <- readQ(cfiles)

# checkQ --------------------------------------------------------------------

#checkQ
context("Check runs")
#cat("\ncheckQ ---------------------------------------------------------------\n")

len <- length(ftype)
for(i in seq_along(ftype))
{
  cat(paste0("Running ",i," of ",len,". "))
  files <- get(ftype[i])
  
  # check file type ------------------------------------------------------------
  cat(paste0("filetype; "))
  cr <- pophelper:::checkQ(files)
  
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
    
    # readq --------------------------------------------------------------------
    cat(paste0("readq; "))
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
    
    # error for empty input
    expect_error(readQStructure())
    expect_error(readQTess())
    expect_error(readQBasic())
    expect_error(readQClumpp())
    expect_error(readQTess3())
    expect_error(readQBaps())
    expect_error(readQ())
    
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
    
    xlist <- readQ(files)
    
    # qlist structure ----------------------------------------------------------
    cat(paste0("qlist; "))
    
    expect_equal(class(slist),"list")
    expect_equal(class(slist[1]),"list")
    expect_equal(class(slist[[1]]),"data.frame")
    
    # as.qlist ---------------------------------------------------------------
    
    context("as.qlist")
    #cat(paste0("as.qlist; "))
    
    expect_equal(slist,as.qlist(slist))
    q1 <- list(data.frame(Cluster1=c(0.2,0.4,0.6,0.2), Cluster2=c(0.8,0.6,0.4,0.8)),data.frame(Cluster1=c(0.3,0.1,0.5,0.6), Cluster2=c(0.7,0.9,0.5,0.4)))
    expect_error(is.qlist(q1))
    expect_silent(as.qlist(q1))
    expect_equal(c("run1","run2"),names(as.qlist(q1)))
    
    # sortQ ------------------------------------------------------------------
    
    context("sortQ")
    #cat(paste0("sortQ; "))
    
    expect_equal(names(sortQ(xlist)),tabulateQ(xlist)$file)
    
    # splitQ ------------------------------------------------------------------
    
    context("splitQ")
    #cat(paste0("splitQ; "))
    
    expect_equal(length(unique(tabulateQ(xlist)$k)),length(splitQ(xlist)))
    expect_equal(length(unique(tabulateQ(xlist)$ind)),length(splitQ(xlist,by="ind")))
    
    # alignK ------------------------------------------------------------------
    
    context("alignK")
    #cat(paste0("alignK; "))
    
    x <- alignK(xlist)
    
    # mergeQ ------------------------------------------------------------------

    context("mergeQ")
    #cat(paste0("mergeQ; "))
    
    expect_equal(summariseQ(tabulateQ(xlist))$k,as.numeric(names(mergeQ(xlist))))
    
    # tabulate -----------------------------------------------------------------
    context("tabulateQ")
    #cat(paste0("tabulateq; "))
    
    tr <- tabulateQ(qlist=xlist,writetable=TRUE)
    # check if file has been exported
    expect_equal("tabulateQ.txt" %in% list.files(),TRUE)
    if(deleteoutput) file.remove("tabulateQ.txt")
    
    expect_equal(class(tr),"data.frame")
    # except clumpp, num of samples must match tabulated rows
    if(tolower(unique(cr$type)) != "clumpp") expect_equal(nrow(tr),length(files))
    # error for empty input
    expect_error(tabulateQ())
    
    
    # summarise ----------------------------------------------------------------
    context("summariseQ")
    #cat(paste0("summarise; "))
    
    sr <- summariseQ(tr,writetable=TRUE)
    # check if file has been exported
    expect_equal("summariseQ.txt" %in% list.files(),TRUE)
    if(deleteoutput) file.remove("summariseQ.txt")
    
    # check if data.frame
    expect_equal(class(sr),"data.frame")
    # error for empty output
    expect_error(summariseQ())
}

# readQTess3 -------------------------------------------------------------------

context("readQTess3")
#cat("readQTess3 ---------------------------------------------------------------\n")

# tess3 files removed due to large size
#t3obj <- readRDS(system.file("files/tess3.rds",package="pophelper"))
#t3list <- readQTess3(t3obj)

# is.qlist ---------------------------------------------------------------------

context("is.qlist")
#cat("is.qlist ---------------------------------------------------------------\n")
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

# evannoMethodStucture ---------------------------------------------------------

context("Evanno method")
#cat("evannoMethodStructure ----------------------------------------------------\n")

#returns dataframe
sr <- summariseQ(tabulateQ(slist),writetable=FALSE)
expect_equal(class(evannoMethodStructure(sr)),"data.frame")

#export text output
evannoMethodStructure(sr,writetable=TRUE)
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
evannoMethodStructure(sr,writetable=TRUE,exportplot=T,outputfilename="candy")
expect_equal("candy.txt" %in% list.files(),TRUE)
expect_equal("candy.png" %in% list.files(),TRUE)
if(deleteoutput) file.remove("candy.txt")
if(deleteoutput) file.remove("candy.png")

#PLOTS
#export plot png
evannoMethodStructure(sr,exportplot=TRUE)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#export plot jpeg
evannoMethodStructure(sr,exportplot=TRUE,imgtype="jpeg")
if(deleteoutput) file.remove("evannoMethodStructure.jpg")

if(testtiff)
{
  #export plot tiff
  evannoMethodStructure(sr,exportplot=TRUE,imgtype="tiff")
  if(deleteoutput) file.remove("evannoMethodStructure.tiff")
}

#export plot pdf
evannoMethodStructure(sr,exportplot=TRUE,imgtype="pdf")
if(deleteoutput) file.remove("evannoMethodStructure.pdf")

#change errorbar features, pointcol, linecol
evannoMethodStructure(sr,exportplot=TRUE,ebwidth=0.1,
                      ebcol="coral",pointcol="firebrick",
                      linecol="green",textcol="blue",gridsize=0.6)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#change plot linesize, pointsize
evannoMethodStructure(sr,exportplot=TRUE,linesize=0.9,pointsize=8)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#plot change dim, dpi, units, basesize for web plot
evannoMethodStructure(sr,exportplot=TRUE,height=800,width=800,dpi=72,units="px",basesize=20)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#change font
if(testfont)
{
  evannoMethodStructure(sr,exportplot=TRUE,font="Verdana")
  if(deleteoutput) file.remove("evannoMethodStructure.png")
  
  #change theme
  evannoMethodStructure(sr1,exportplot=TRUE,font="Verdana",theme="theme_grey")
  if(deleteoutput) file.remove("evannoMethodStructure.png")
}

# xaxisbreaks
ev <- evannoMethodStructure(sr,exportplot=FALSE,returndata=F,returnplot=T,xaxisbreaks=c(2,8,10))

#ERRORS
#error only 2 values of k
expect_error(evannoMethodStructure(sr[1:2,],exportplot=TRUE))
expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#error test kplot features
expect_error(evannoMethodStructure(sr[1:2,],exportplot=TRUE,linesize=1,pointsize=8))
expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#error test kplot features
expect_error(evannoMethodStructure(sr[1:2,],exportplot=TRUE,ebwidth=0.05,
                                   ebcol="coral",pointcol="firebrick",
                                   linecol="green",textcol="blue",
                                   gridsize=0.6,theme="theme_grey",font="Verdana"))
expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
if(deleteoutput) file.remove("evannoMethodStructure.png")

if(testfont)
{
  expect_error(evannoMethodStructure(sr[1:2,],exportplot=TRUE,font="Verdana"))
  expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
  if(deleteoutput) file.remove("evannoMethodStructure.png")  
}

#error non sequential values of k
expect_error(evannoMethodStructure(sr[c(1,2,4),],exportplot=TRUE))
expect_equal("evannoMethodStructure.png" %in% list.files(),TRUE)
if(deleteoutput) file.remove("evannoMethodStructure.png")

#warning less than 2 runs
expect_error(evannoMethodStructure(sr[4:5,]))

test_that("Errors",{
  expect_error(evannoMethodStructure())
  expect_error(evannoMethodStructure(sfiles))
  expect_error(evannoMethodStructure(tlist))
  expect_error(evannoMethodStructure(slist))
})

# clumppExport -----------------------------------------------------------------

#clumppExport
context("Clumpp Output")
#cat("clumppExport -------------------------------------------------------------\n")

clumppExport(slist)
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

if(TRUE) {
  
#structure clumpp export check prefix
clumppExport(slist,prefix="Boom")
expect_equal(all(grepl("Boom",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("Boom*", recursive = TRUE, force = TRUE)

#tess clumpp export check
clumppExport(tlist)
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#tess clumpp export check prefix
clumppExport(tlist,prefix="Hahaha")
expect_equal(all(grepl("Hahaha",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("Hahaha*", recursive = TRUE, force = TRUE)

#tructure clumpp export check prefix
clumppExport(slist,prefix="Nanana")
expect_equal(all(grepl("Nanana",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("Nanana*", recursive = TRUE, force = TRUE)

#matrix clumpp export check
clumppExport(alist)
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#matrix clumpp export check
clumppExport(flist)
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

#structure clumpp list export check
clumppExport(slist)
expect_equal(all(grepl("pop",list.dirs()[-1])),TRUE)
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

context("Clumpp Output useexe")

#useexe warning
expect_warning(clumppExport(slist,useexe=T))
if(deleteoutput) unlink("pop*", recursive = TRUE, force = TRUE)

}

# collectClumppOutput ----------------------------------------------------------

context("Collect clumpp output")
#cat("collectClumppOutput ------------------------------------------------------\n")

collectClumppOutput(filetype = "aligned")
expect_equal(sum(grepl("aligned",list.dirs())),1)

collectClumppOutput(filetype = "merged")
expect_equal(sum(grepl("merged",list.dirs())),1)

collectClumppOutput(filetype = "both")
expect_equal(sum(grepl("both",list.dirs())),1)

if(deleteoutput) unlink("pop*", recursive=TRUE, force=TRUE)

# getPlotParams ----------------------------------------------------------------

context("getPlotParams")
#cat("getPlotParams ------------------------------------------------------------\n")

pophelper:::getPlotParams(grplab=grps1$V1, plotnum=1)
pophelper:::getPlotParams(grplab=grps1$V1, plotnum=2)
pophelper:::getPlotParams(grplab=grps1$V1, plotnum=1,grplabsize=5,grplabangle=90,grplabjust=0.5,pointsize=2,linesize=1)

# grpLabels --------------------------------------------------------------------

context("grpLabels")
#cat("grpLabels ---------------------------------------------------------------\n")

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

context("sortInd")

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
#cat("plotQ ----------------------------------------------------------\n")

grpsrep <- read.delim(system.file("files/structuregrplabels-rep.txt",package="pophelper"),header=FALSE,stringsAsFactors=F)
grplabs <- read.delim(system.file("files/grplab.txt",package="pophelper"),stringsAsFactors=F)

slist <- readQ(sfiles)
imgout <- c("sep","join")

for(i in seq_along(imgout))
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
  plotQ(slist[1:2],imgoutput=imgo,sortind="Cluster1",sharedindlab=F)
  
  #check orderind all
  plotQ(slist[1:2],imgoutput=imgo,sortind="all",sharedindlab=F)
  
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
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="all",sharedindlab=F)
  
  #single labels ordergrp sortind Cluster1
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="Cluster1",sharedindlab=F)
  
  #single labels ordergrp sortind label
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="label")
  
  #single labels ordergrp sortind label show indlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs[,1,drop=FALSE],ordergrp=TRUE,sortind="label",showindlab=TRUE)
  
  #multiple labels
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs)
  
  #multiple labels change selgrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="mixed")
  
  #multiple labels grpmean
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T)
  
  #multiple labels grpmean change selgrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T,selgrp="mixed")
  
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
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE,sortind="all",sharedindlab=F)
  
  #multiple labels selgrp ordergrp sortind all
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,selgrp="lab2",ordergrp=TRUE,sortind="all",sharedindlab=F)
  
  #multiple labels sortind Cluster1
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,sortind="Cluster1",sharedindlab=F,ordergrp=TRUE)
  
  #multiple labels ordergrp sortind label
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE,sortind="label")
  
  #multiple labels ordergrp sortind label show indlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,ordergrp=TRUE,sortind="label",showindlab=TRUE)
  
  #multiple labels grpmean
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,grpmean=T,showindlab=TRUE)
  
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
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,sortind="all",sharedindlab=F,ordergrp=TRUE)
  
  #check grpmean with rep labels sortind
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,sortind="label",ordergrp=TRUE)
  
  #check grpmean with rep labels sortind showindlab
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,sortind="label",showindlab=T,width=15,ordergrp=TRUE)
  
  expect_error(plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,subsetgrp="Pop A"))
  
  #use ordergrp
  plotQ(slist[1:2],imgoutput=imgo,grplab=grpsrep,subsetgrp="Pop A",ordergrp=T)
  
  #indlabwithgrplab F
  plotQ(slist[1:2],imgoutput=imgo,showindlab=T,useindlab=F,indlabwithgrplab=F,grplab=grplabs)
  
  #indlabwithgrplab T
  plotQ(slist[1:2],imgoutput=imgo,showindlab=T,useindlab=F,indlabwithgrplab=T,grplab=grplabs)
  
  #sharedindlab
  if(imgo=="join")
  {
    #sharedindlab off
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=F,outputfilename="joined")
    #sharedindlab on
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,outputfilename="joined")
    #sharedindlab on with sort label
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,sort="label",outputfilename="joined")
    #sharedindlab with sort all
    expect_error(plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sortind="all"))
    #sharedindlab with sort cluster
    expect_error(plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sortind="Cluster1"))
    #sharedindlab off with grplab
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=F,grplab=grplabs,outputfilename="joined")
    #sharedindlab on with grplab
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,grplab=grplabs,width=16,outputfilename="joined")
    #shared indlab with repeated grplab
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,grplab=grpsrep,outputfilename="joined")
    #sharedindlab with grplab subset
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,grplab=grplabs,subsetgrp="a",width=16,outputfilename="joined")
    #sharedindlab with reorder
    plotQ(slist[1:2],imgoutput=imgo,showindlab=T,sharedindlab=T,grplab=grplabs,subsetgrp=c("b","a"),width=16,outputfilename="joined")
    #error sharedindlab error
    expect_error(plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,showindlab=T,sortind="Cluster1"))
  }
  
  #showyaxis
  plotQ(slist[1:2],imgoutput=imgo,showyaxis=T)
  
  #showyaxis grplab
  plotQ(slist[1:2],imgoutput=imgo,showyaxis=T,grplab=grplabs)
  
  #showyaxis ticks
  plotQ(slist[1:2],imgoutput=imgo,showyaxis=T,showticks=T,showindlab=T)
  
  #showyaxis ticks ticksize ticklength
  plotQ(slist[1:2],imgoutput=imgo,showyaxis=T,showticks=T,showindlab=T,ticksize=0.5,ticklength=0.5)
  
  #basesize
  plotQ(slist[1:2],imgoutput=imgo,showyaxis=T,showindlab=T,basesize=12)
  
  #panelratio
  plotQ(slist[1:2],imgoutput=imgo,grplab=grplabs,panelratio=c(2,4))
  
  #legend
  plotQ(slist[1:2],imgoutput=imgo,showlegend=T)
  
  #legendmargin
  plotQ(slist[1:2],imgoutput=imgo,showlegend=T,legendmargin=c(4,4,4,4))
  
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

#barbordersize
#barbordercolour
plotQ(slist[1],barbordersize=0.1,barbordercolour="black")

#splabangle
plotQ(slist[1])
plotQ(slist[1],splabangle=-90)
plotQ(slist[1],sppos="left")
plotQ(slist[1],sppos="left",splabangle=90)
plotQ(slist[1],showsp=F)
plotQ(slist[1:2],imgoutput = "join",outputfilename = "structure_01")
plotQ(slist[1:2],imgoutput="join",splabangle=90,outputfilename = "structure_01")
plotQ(slist[1:2],imgoutput="join",grplab=grplabs,splabangle=90,outputfilename = "structure_01")

#title and subtitle
plotQ(slist[1])
plotQ(slist[1],showtitle=T)
plotQ(slist[1],showsubtitle=T)
plotQ(slist[1],showtitle=T,showsubtitle=T)
plotQ(slist[1],showtitle=T,showsubtitle=T,titlelab="Galapagos",subtitlelab="Sample from the region of Galapagos")
plotQ(slist[1],showtitle=T,showsubtitle=T,titlecol="green",subtitlecol="blue",titlehjust=1,subtitlehjust=1)
plotQ(slist[1],showtitle=T,showsubtitle=T,titlevjust=0,subtitlevjust=0)
plotQ(slist[1],showtitle=T,showsubtitle=T,titleangle=45,subtitleangle=45)
plotQ(slist[1],showtitle=T,showsubtitle=T,grplab=grplabs)
plotQ(slist[1],showtitle=T,showsubtitle=T,titlelab="Galapagos",
      subtitlelab="Sample from the region of Galapagos",grplab=grplabs)
plotQ(slist[1:2],imgoutput="join")
plotQ(slist[1:2],imgoutput="join",showtitle=T,showsubtitle=T)
plotQ(slist[1:2],imgoutput="join",outputfilename="structure_01")
plotQ(slist[1:2],imgoutput="join",outputfilename="structure_01",showtitle=T,showsubtitle=T,
      titlelab="Galapagos",subtitlelab="Sample from the region of Galapagos")
plotQ(slist[1:2],imgoutput="join",outputfilename="structure_01",showtitle=T,showsubtitle=T,
      titlelab="Galapagos",subtitlelab="Sample from the region of Galapagos",grplab=grplabs,sppos="left")
plotQ(slist[1:2],imgoutput="join",outputfilename="structure_01",showtitle=T,showsubtitle=T,
      titlelab="Galapagos",subtitlelab="Sample from the region of Galapagos",
      showlegend=F,legendpos="right")
plotQ(slist[1:2],imgoutput="join",outputfilename="structure_01",showtitle=T,
      titlelab="Galapagos",showlegend=T,legendpos="right")

#indlabspacer
plotQ(slist[1],showindlab=T)
plotQ(slist[1],showindlab=T,indlabspacer=5)
plotQ(slist[1],showindlab=T,indlabspacer=-1)
plotQ(slist[1:2],showindlab=T,indlabspacer=5)
plotQ(slist[1:2],imgoutput="join",showindlab=T,indlabspace=-1,outputfilename="structure_01")

if(deleteoutput) file.remove(list.files())

# plotQ Clumpp --------------------------------------------------------------

context("plotQ Clumpp")
#cat("plotQ Clumpp --------------------------------------------------------------\n")

cfiles <- c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))

#check output table
plotQ(qlist=clist)

#check output table
plotQ(qlist=clist,grpmean=T)

#check output table sort cluster
plotQ(qlist=clist,sortind="Cluster1")

#check output table sort cluster
plotQ(qlist=clist,imgoutput="join",sortind="Cluster1",sharedindlab=F)

#check output table pop lab
plotQ(qlist=clist,grplab=grps1)

#check output table pop lab sort all
plotQ(qlist=clist,imgoutput="sep",grplab=grplabs,sortind="all")

#check output table pop lab sort cluster
plotQ(qlist=clist,imgoutput="sep",grplab=grplabs,sortind="Cluster1")

#check output table pop lab pop mean
plotQ(qlist=clist,imgoutput="sep",grplab=grplabs,grpmean=T)

#check output table pop lab rep
plotQ(qlist=clist,imgoutput="sep",grplab=grpsrep)

#check output table pop lab rep sort
plotQ(qlist=clist,imgoutput="sep",grplab=grpsrep,sortind="all")

#check output table pop lab rep sort pop mean
plotQ(qlist=clist,imgoutput="sep",grplab=grpsrep,sortind="all",grpmean=T)

if(deleteoutput) file.remove(list.files())

expect_error(plotQ(qlist=clist,imgoutput="sep",grplab=grpsrep,subsetgrp="Pop B"))

# plotQMultiline ---------------------------------------------------------------
context("plotQMultiline")
#cat("plotQMultiline -----------------------------------------------------------\n")

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

#barbordersize
#barbordercolour
plotQMultiline(slist[1],barbordercolour="red",barbordersize=0.2)
plotQMultiline(slist[1],barbordercolour="black",barbordersize=0.2,barsize=1)

#indlabspacer
plotQMultiline(slist[1])
plotQMultiline(slist[1],indlabspacer=3)
plotQMultiline(slist[1],indlabspacer=-1)

#title and subtitle
plotQMultiline(slist[1])
plotQMultiline(slist[1],showtitle=T,showsubtitle=T)
plotQMultiline(slist[1],showtitle=T,showsubtitle=T,lpp=6,
               titlelab="Galapagos",subtitlelab="Sample from the region of Galapagos")
plotQMultiline(slist[1],showtitle=T,showsubtitle=T,lpp=6,titlehjust=1,titlecol="blue")

#indlabwithgrplab
plotQMultiline(slist[1],grplab=grplabs,indlabwithgrplab=T)

#show legend
plotQMultiline(slist[1],showlegend=T)

#show legend grplab
plotQMultiline(slist[1],showlegend=T,grplab=grplabs)

#show legend legend size
plotQMultiline(slist[1],showlegend=T,legendkeysize=15,legendtextsize=10)

#show legend legend margin
plotQMultiline(slist[1],showlegend=T,legendmargin=c(15,15,15,15))

#ticks
plotQMultiline(slist[1],showyaxis=T,showticks=T,ticklength=2)

#quiet
plotQMultiline(slist[1],quiet=T)

#exportplot
plotQMultiline(slist[1],exportplot=F)

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
plotQMultiline(slist[1:3],spl=20,lpp=4)

if(deleteoutput) file.remove(list.files())

# collectTessRuns --------------------------------------------------------------
context("CollectTessRuns")
#cat("Collect Tess runs ----------------------------------------------------\n")

# analyseQ ------------------------------------------------------------------

context("analyseQ Structure")
#cat("analyseQ Structure ------------------------------------------------------\n")

analyseQ(sfiles)
test_that("check output",{
  expect_equal(any(grepl("evannoMethodStructure.png",list.files())),TRUE)
  expect_equal(any(grepl("evannoMethodStructure.txt",list.files())),TRUE)
  expect_equal(any(grepl("tabulateQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("structure_01.png",list.files())),TRUE)
  expect_equal(any(grepl("structure_17.png",list.files())),TRUE)
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
})
if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
if(deleteoutput) file.remove(list.files())

context("analyseQ BAPS")
cat("analyseQ BAPS\n")

analyseQ(bfiles)
test_that("check output",{
  expect_equal(any(grepl("tabulateQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("summariseQ.txt",list.files())),TRUE)
  expect_equal(any(grepl("baps_k2.png",list.files())),TRUE)
})
if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
if(deleteoutput) file.remove(list.files())

# distructExport Structure -----------------------------------------------------

context("distructExport Structure")
#cat("distructExport Structure ------------------------------------------------\n")

grps1 <- read.delim(system.file("files/structuregrplabels.txt",package="pophelper"),header=FALSE)

#ind one file
distructExport(qlist = slist[1])
test_that("destructExport structure one file",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#both top and bottom labels
distructExport(slist[1],grplabbottom=as.character(grps1$V1),grplabtop=as.character(grps1$V1),grpmean=T)
test_that("destructExport structure top and bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#both top and bottom labels quiet
distructExport(slist[1],grplabbottom=as.character(grps1$V1),grplabtop=as.character(grps1$V1),quiet=T)
test_that("destructExport structure top and botoom labels quiet",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#no bottom labels
distructExport(slist[1],grplabbottom=NA,grplabtop=as.character(grps1$V1))
test_that("destructExport structure no bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#no top labels
distructExport(slist[1],grplabbottom=as.character(grps1$V1),grplabtop=NA)
test_that("destructExport structure no top labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#no both labels
distructExport(slist[1],grplabbottom=NA,grplabtop=NA)
test_that("destructExport structure both labels",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#multiple files
distructExport(slist[2:5],grplabbottom=as.character(grps1$V1))
test_that("destructExport structure multiple files",{
  expect_equal(length(list.dirs(recursive=F)),4)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#pop mean true
distructExport(slist[1],grplabbottom=as.character(grps1$V1),grpmean=T)
test_that("destructExport structure grpmean true",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#multiple files with grpmean
distructExport(slist[2:5],grplabbottom=as.character(grps1$V1),grpmean=T)
test_that("destructExport structure multiple files",{
  expect_equal(length(list.dirs(recursive=F)),4)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#other colours
distructExport(slist[1],grplabbottom=as.character(grps1$V1),clustercol=pophelper:::distructColours()[43:44])
test_that("destructExport structure other colours",{
  expect_equal(any(grepl("drawparams",list.files("./structure_01-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#use exe
expect_warning(distructExport(slist[2:5],grplabbottom=as.character(grps1$V1),useexe=T))

if(deleteoutput) unlink(list.dirs()[-1],recursive = T,force = T)
if(deleteoutput) file.remove(list.dirs(recursive=F))

# distructExport Tess ----------------------------------------------------------

context("exportDistruct Tess")
#cat("distructExport Tess ------------------------------------------------------\n")

grps1 <- read.delim(system.file("files/tessgrplabels.txt",package="pophelper"),header=FALSE)

#both top and bottom labels
distructExport(tlist[3],grplabbottom=as.character(grps1$V1),grplabtop=as.character(grps1$V1))
test_that("exportDistruct Tess top and bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./tess_03-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#both top and bottom labels quiet
distructExport(tlist[3],grplabbottom=as.character(grps1$V1),grplabtop=as.character(grps1$V1),quiet=T)
test_that("exportDistruct Tess top and bottom labels quiet",{
  expect_equal(any(grepl("drawparams",list.files("./tess_03-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#no bottom labels
distructExport(tlist[3],grplabbottom=NA,grplabtop=as.character(grps1$V1))
test_that("exportDistruct Tess no bottom labels",{
  expect_equal(any(grepl("drawparams",list.files("./tess_03-distruct"))),TRUE)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#multiple files
distructExport(tlist[2:5],grplabbottom=as.character(grps1$V1))
test_that("exportDistruct Tess multiple files",{
  expect_equal(length(list.dirs(recursive=F)),4)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#multiple files with grpmean
distructExport(tlist[2:5],grplabbottom=as.character(grps1$V1),grpmean=T)
test_that("exportDistruct Tess pop mean",{
  expect_equal(length(list.dirs(recursive=F)),4)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

# distructExport basic --------------------------------------------------------

if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)
context("distructExport Other formats")
#cat("distructExport Basic -----------------------------------------------------\n")

#admixture multiple
distructExport(qlist = alist[1:4],grpmean=F)
test_that("exportDistruct Admixture",{
  expect_equal(length(list.dirs(recursive=F)),4)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#admixture multiple grpmean
distructExport(qlist = alist[1:4],grpmean=T)
test_that("exportDistruct Admixture pop mean",{
  expect_equal(length(list.dirs(recursive=F)),4)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#faststructure
distructExport(qlist = flist[2:3],grpmean=F)
test_that("exportDistruct faststructure",{
  expect_equal(length(list.dirs(recursive=F)),2)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#basic files comma
distructExport(qlist = mclist[2:3],grpmean=F)
test_that("exportDistruct basic Comma",{
  expect_equal(length(list.dirs(recursive=F)),2)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#basic files tab
distructExport(qlist = mtlist[2:3],grpmean=F)
test_that("exportDistruct basic tab",{
  expect_equal(length(list.dirs(recursive=F)),2)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#basic files space
distructExport(qlist = mslist[2:3],grpmean=F)
test_that("exportDistruct basic space",{
  expect_equal(length(list.dirs(recursive=F)),2)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#tab no grpmean
distructExport(qlist = clist,grpmean=F)
test_that("exportDistruct Admixture",{
  expect_equal(length(list.dirs(recursive=F)),7)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

#tab grpmean
distructExport(qlist = clist,grpmean=T)
test_that("exportDistruct Admixture",{
  expect_equal(length(list.dirs(recursive=F)),7)
})
if(deleteoutput) unlink(list.dirs(recursive=F),recursive=T,force=T)

# End --------------------------------------------------------------------------

setwd(currwd)
if(deleteoutput) unlink("pophelper-demo",recursive=T,force=T)

