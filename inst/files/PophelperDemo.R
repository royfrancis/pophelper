#POPHELPER 1.0.4
#DEMO SCRIPT
require(pophelper)
#create a new folder and set as wd
currwd <- getwd()
dir.create(paste(currwd,"/PophelperDemo",sep=""))
setwd(paste(currwd,"/PophelperDemo",sep=""))
#read sample STRUCTURE files from R package
sfiles <- list.files(path=system.file("/files/structure",package="pophelper"),full.names=T)
#read sample TESS files from R package
tfiles <- list.files(path=system.file("/files/tess",package="pophelper"),full.names=T)
#tabulate STRUCTURE runs
sf1 <- tabulateRunsStructure(files=sfiles)
sf1
#tabulate TESS runs
tf1 <- tabulateRunsTess(files=tfiles)
tf1
#summarise STRUCTURE runs
sf2 <- summariseRunsStructure(sf1)
tf2 <- summariseRunsTess(tf1)
#perform evanno method
evannoMethodStructure(sf2)
#convert STRUCTURE run files to R dataframe
runsToDfStructure(files=sfiles)
#convert TESS run files to R dataframe
runsToDfTess(files=tfiles)
#convert STRUCTURE runs for CLUMPP
clumppExportStructure(files=sfiles)
#convert TESS runs for CLUMPP
clumppExportTess(files=tfiles)
#collect CLUMPP outputs into single directory
#collectClumppOutput(prefix="STRUCTUREpop", filetype="both")  
#collectClumppOutput(prefix="TESSpop", filetype="both")
#CANNOT TEST ABOVE WITHOUT CLUMPP.EXE
#plot separate figures STRUCTURE
plotRuns(files=sfiles[1:2]) 
#plot separate figures TESS
plotRuns(files=tfiles[1:2]) 
#plot joined figures STRUCTURE
plotRuns(files=sfiles[1:2], imgoutput="join")
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
#plot multiline STRUCTURE
plotMultiline(files=sfiles[1])
#plot multiline TESS
plotMultiline(files=tfiles[1]) 
#plotmultiline with custom setting
plotMultiline(files=sfiles[1], spl=75, lpp=10)
#read coordinate file
cfile75 <- system.file("/files/coords75.txt",package="pophelper")
#basic usage
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cfile75)
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cfile75,imgtype="jpeg")
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cfile75,imgtype="pdf")
#adjusting legend size and legend text
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cfile75,legendsize=0.4,legendtextsize=6,method="bilinear")
#removing legend and changing method
plotRunsInterpolate(datafile=tfiles[2],coordsfile=cfile75,legend=FALSE,method="idw")
#read coordinate file length 239
cfile239 <- read.delim(system.file("files/coords239.txt",package="pophelper"),header=F)
#select path to STRUCTURE file length 239
sfile239 <- system.file("files/Structure239_4",package="pophelper")
#basic usage
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239)
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,imgtype="jpeg")
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,imgtype="pdf")
#needs more height. set height and width as required in cm.
plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,height=12)
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
#no ellipse, with convex hull, decreased convexhull transparency, change cluster labels, custom colours
p4 <- plotRunsSpatial(datafile=sfile239,coordsfile=cfile239,exportplot=F,dataout=T, plottitle="Fig 4",ellipse=F,chull=T,chulltransp=0.2,chulltype=3,legendlabels=c("PopA","PopB","PopC","PopD","PopE"),popcol=brewer.pal(5,"Set1"))

png("PlotRunsSpatial.png",height=20,width=20,res=250,units="cm",type="cairo")
grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)
dev.off()



