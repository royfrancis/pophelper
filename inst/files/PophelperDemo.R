#POPHELPER 1.1.4
#27-Sep-2015
#DEMO SCRIPT

library(pophelper)
#create a new folder and set as wd
currwd <- getwd()
dir.create(paste(currwd,"/PophelperDemo",sep=""))
setwd(paste(currwd,"/PophelperDemo",sep=""))
#read sample STRUCTURE files from R package
sfiles <- list.files(path=system.file("/files/structure",package="pophelper"),full.names=T)
#read sample TESS files from R package
tfiles <- list.files(path=system.file("/files/tess",package="pophelper"),full.names=T)
#read sample ADMIXTURE files from R package
afiles <- list.files(path=system.file("/files/admixture",package="pophelper"),full.names=T)
#tabulate STRUCTURE runs
sf1 <- tabulateRunsStructure(files=sfiles, quiet=F)
sf1 <- tabulateRunsStructure(files=sfiles, writetable = T)
sf1 <- tabulateRunsStructure(files=sfiles, writetable = T, sorttable = F, quiet = T)
sf1
#tabulate TESS runs
tf1 <- tabulateRunsTess(files=tfiles, writetable = T)
tf1 <- tabulateRunsTess(files=tfiles, writetable = T, sorttable = F, quiet = T)
tf1
#tabulate ADMIXTURE runs
af1 <- tabulateRunsAdmixture(files=afiles, writetable = T)
af1 <- tabulateRunsAdmixture(files=afiles, writetable = T, sorttable = F, quiet = T)
af1
#summarise STRUCTURE runs
sf2 <- summariseRunsStructure(sf1)
sf2 <- summariseRunsStructure(sf1, writetable = T)
sf2
#summarise TESS runs
tf2 <- summariseRunsTess(tf1, writetable = F)
tf2 <- summariseRunsTess(tf1, writetable = T)
tf2
#summarise ADMIXTURE runs
af2 <- summariseRunsAdmixture(af1, writetable = F)
af2 <- summariseRunsAdmixture(af1, writetable = T)
af2
#perform evanno method
evannoMethodStructure(sf2)
evannoMethodStructure(sf2, writetable = T, exportplot = T)
evannoMethodStructure(sf2, writetable = T, exportplot = T, height = 15, width = 15)
evannoMethodStructure(sf2, height = 20, width = 20, basesize=8)
#convert STRUCTURE run files to R dataframe
runsToDfStructure(files=sfiles)
#convert TESS run files to R dataframe
runsToDfTess(files=tfiles)
#convert ADMIXTURE run files to R dataframe
runsToDfAdmixture(files=afiles)
#convert STRUCTURE runs for CLUMPP
clumppExportStructure(files=sfiles)
clumppExportStructure(files=sfiles, prefix = "Boom", parammode = 3, paramrep = 200)
#convert TESS runs for CLUMPP
clumppExportTess(files=tfiles)
#convert ADMIXTURE runs for CLUMPP
clumppExportAdmixture(files=afiles)
#collect CLUMPP outputs into single directory
#collectClumppOutput(prefix="STRUCTUREpop", filetype="both")  
#collectClumppOutput(prefix="TESSpop", filetype="both")
#CANNOT TEST ABOVE WITHOUT CLUMPP.EXE
#plot separate figures STRUCTURE
plotRuns(files=sfiles[1:2])
plotRuns(files=sfiles[1:2], imgoutput = "sep", height = 5, width = 15) 
#plot separate figures TESS
plotRuns(files=tfiles[1:2]) 
#plot joined figures STRUCTURE
plotRuns(files=sfiles[1:2], imgoutput="join",na.rm=T)
#plot joined figures TESS
plotRuns(files=tfiles[1:2], imgoutput="join")
#read labels for STRUCTURE
pops<-read.delim(system.file("files/structurepoplabels.txt",package="pophelper"),header=F)
#plot separately with labels
plotRuns(files=sfiles[1:2], imgoutput="sep", poplab=pops$V1) 
#plot joined with labels
plotRuns(files=sfiles[1:2], imgoutput="join", poplab=pops$V1) 
#create TESS labels
labs1 <- factor(c(rep("PopA",30),rep("PopB",45)))
#plot TESS runs with labels
plotRuns(files=tfiles[1:2], imgoutput="sep", poplab=labs1)
plotRuns(files=tfiles[1:2], imgoutput="join", poplab=labs1)
#plot ADMIXTURE runs 
plotRuns(files=afiles[1:2], imgoutput="sep")
plotRuns(files=afiles[1:2], imgoutput="join")
#read labels for ADMIXTURE
admpops<-read.delim(system.file("files/admixturepoplabels.txt",package="pophelper"),header=F)
plotRuns(files=afiles[1:2], imgoutput="sep", poplab=admpops$V1)
plotRuns(files=afiles[1:2], imgoutput="join", poplab=admpops$V1)
#plot TABLE files
tabs1 <- c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
           system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))
#plot one TABLE file
plotRuns(files=tabs1[1],imgoutput="tab")
#plot all TABLE files
plotRuns(files=tabs1,imgoutput="tab")
#plot all TABLE files with labels
plotRuns(files=tabs1,imgoutput="tab",poplab=pops$V1)
#plot multiline STRUCTURE
plotMultiline(files=sfiles[1])
#plot multiline TESS
plotMultiline(files=tfiles[1])
#plot multiline ADMIXTURE
plotMultiline(files=afiles[1])
#plot multiline ADMIXTURE multiple
plotMultiline(files=afiles[2:3])
#plotmultiline with custom setting
plotMultiline(files=sfiles[1], spl=75, lpp=10)
#plotmultiline with custom setting
plotMultiline(files=sfiles[1],barspace = 0,barwidth = 1,height = 6,width=16)
#plotmultiline with custom setting
plotMultiline(files=sfiles[1],barspace = 0,barwidth = 1,height = 6,width=16,yaxislabs = T,indlabs = F)
#plotmultiline with custom setting
plotMultiline(files=sfiles[1],barspace = 0,barwidth = 1,height = 6,width=16,yaxislabs = T,labangle = 45,imgtype = "jpeg")
#plotMultiline with one TABLE file
plotMultiline(files=tabs1[1])
#plotMultiline with all TABLE file
plotMultiline(files=tabs1)
#read coordinate file
cd2 <- system.file("/files/coords75.txt",package="pophelper")
#basic usage
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2)
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,imgtype="jpeg")
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,imgtype="pdf")
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,method="bicubic")
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,method="krig")
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,clusters=2)
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,pointcol = "red")
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,clusters=c(1,3))
#adjusting legend size and legend text
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,legendsize=0.4,legendtextsize=6,method="bilinear")
#removing legend and changing method
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,legend=FALSE,method="idw")
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cd2,legend=FALSE,method="idw",idwpower=5)
#read coordinate file length 239
cfile239 <- read.delim(system.file("files/coords239.txt",package="pophelper"),header=F)
#select path to STRUCTURE file length 239
sfile239 <- system.file("files/Structure239_4",package="pophelper")
#basic usage
plotRunsInterpolate(datafile=sfile239,coordsfile=cfile239,cluster=1:2,height=8)
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239)
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,imgtype="jpeg")
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,imgtype="pdf")
#needs more height. set height and width as required in cm.
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,height=12)
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,height=12,showaxis = T)
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,height=12,showaxis = T, popcol=c("blue","green","red","steelblue","yellow"))
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,height=12,showaxis = T, pointcol ="red")
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,height=12,pointtype=12)
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,height=12,chull=T,chullsize=2,ellipse=F,)
#set UTM coordinates. Better geographic distance representation over a scale such as normal countries.
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,height=12,setutm=T)
#without ellipses
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,height=12,ellipse=F)

#Create a 2x2 montage with varying parameters
#don't export, export data, add title
p1 <- plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,exportplot=F,dataout=T, plottitle="Fig 1")
#without ellipse, with square points and transparency added
p2 <- plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,exportplot=F,dataout=T, plottitle="Fig 2",ellipse=F,pointtype=15,pointtransp=0.4)
#without ellipse, with convex hulls, coordinates in UTM
p3 <- plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,exportplot=F,dataout=T, plottitle="Fig 3",ellipse=F,chull=T, setutm=T)
#no ellipse, with convex hull, decreased convexhull transparency, change cluster labels
p4 <- plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,exportplot=F,dataout=T, plottitle="Fig 4",ellipse=F,chull=T,chulltransp=0.2,chulltype=3,legendlabels=c("PopA","PopB","PopC","PopD","PopE"))

#install gridExtra first
#library(gridExtra)
#png("PlotRunsSpatial.png",height=20,width=20,res=250,units="cm",type="cairo")
#grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)
#dev.off()
setwd(currwd)
dir.create(paste(currwd,"/AnalyseDemo",sep=""))
setwd(paste(currwd,"/AnalyseDemo",sep=""))

#perform a routine analysis using a wrapper function
analyseRuns(sfiles)
analyseRuns(tfiles)
analyseRuns(afiles)

setwd(currwd)
