#Functions

#load or install required libraries
require(utils)
i=1
pakvec <- c("Hmisc", "gplots", "Cairo", "ggplot2", "reshape", "grid", "gridExtra")
for (i in 1:length(pakvec))
{
  if (!pakvec[i] %in% installed.packages()) install.packages(pakvec[i], repos="http://cran.us.r-project.org", dependencies=TRUE)
  require(pakvec[i], character.only=TRUE, quiet=TRUE)
}

#FUNCTION getColours
#' Internal: Get Colours
#' @description Generate colours based on number of K. This is an internal function to generate colours based on number of populations.
#' @param k Number of colours required
#' @return Returns a vector of k colours in hexadecimal format
#' @details Colours 1 to 12 are custom unique colours. Colours beyond 15 are generated from colour ramp \code{rich.colors()}.
#' 
getColours <- function(k)
{
  if (!is.numeric(k)) stop("Non-numeric input passed to getColours()")
  col1 <- c("#2121D9", "#9999FF", "#DF0101", "#04B404", "#FFFB23", "#FF9326", "#A945FF", "#0089B2", "#B26314", "#610B5E", "#FE2E9A", "#BFF217")
  if (k <= 12) return(col1[1:k])
  if (k > 12) return(rich.colors(k))
}


#FUNCTION tabulateRunsStructure
#' Tabulate STRUCTURE runs
#' @description Creates a table from STRUCTURE output files with various STRUCTURE parameters. Refer to return for detailed list of columns.
#' @param files A character vector of STRUCTURE output files to be tabulated. Use \code{choose.files(multi=TRUE)} for interactive selection.
#' @param writetable Set to FALSE by default. Setting to TRUE writes the output table as a tab-delimited text file in the working directory.
#' @param quiet Set to FALSE by default to print number of selected files. If set to TRUE, then number of selected files are not printed.
#' @return Returns a dataframe with all runs sorted by K. The table has 10 columns namely file name, value of K, Number of individuals, Number of loci, Estimated ln probability of data, Mean value of ln likelihood, Variance of ln likelihood, Mean value of alpha, Number of burn-in and Number of repeats. Missing values are given NA.
#' @details The row numbers of the output table denotes the file number selected. This is helpful if a particular file from the table needs to be identified in the selection vector.
#' @export

tabulateRunsStructure <- function(files=NULL, writetable=FALSE, quiet=FALSE)
{
  quiet <- toupper(quiet)
  #if no files chosen, stop excecution, give error message
  if (length(files) == 0) stop("No input files")
  #get filenames from selection
  filenames <- basename(files)
  #number of files selected
  number <- length(filenames)
  if (quiet == FALSE | quiet == "F" | quiet == "FALSE") cat(paste("Number of files selected: ", number, "\n", sep=""))
  
  #make dataframe container
  main <- data.frame(file=filenames, k=1:number, ind=1:number,loci=1:number,
                     elpd=1:number, mvll=1:number, vll=1:number,
                     mva=1:number, burnin=1:number, reps=1:number)
  
  vll <- NA
  mva <- NA
  
  #loop to make dataframe with filenames and other variables
  i=1
  for (i in i:number)
  {
    #read STRUCTURE file & error check
    file1 <- readLines(files[i], warn=FALSE)
    
    #read files
    chk1 <- grep("STRUCTURE", toupper(file1[4]))
    if (length(chk1) == 0) stop("Input not suitable STRUCTURE file/Incorrect input format.")
    
    #find individuals and get number of individuals
    ind <- as.numeric(gsub("\\D", "", grep("\\d individuals", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1]))
    if (is.na(ind)) cat(paste("Number of individuals is NA in file: ", filenames[i], sep=""))
    #get value of k & error check
    k <- as.numeric(gsub("\\D", "", grep("\\d populations assumed", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1]))
    if (is.na(k)) cat(paste("Value of K is NA in file: ", filenames[i], sep=""))
    #get number of loci & error check
    loci <- as.numeric(gsub("\\D", "", grep("\\d loci", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1]))
    if (is.na(loci)) cat(paste("Number of Loci is NA in file: ", filenames[i], "\n", sep=""))
    #get burn-in value & error check
    burnin <- as.numeric(gsub("\\D", "", grep("\\d Burn-in period", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1]))
    if (is.na(burnin)) cat(paste("Burn-in value is NA in file: ", filenames[i], "\n", sep=""))
    #get burn-in value & error check
    reps <- as.numeric(gsub("\\D", "", grep("\\d Reps", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1]))
    if (is.na(reps)) cat(paste("Reps value is NA in file: ", filenames[i], "\n", sep=""))
    #get est ln prob of data & error check
    elpd <- as.numeric(gsub("=", "", gsub("Estimated Ln Prob of Data", "", grep("Estimated Ln Prob of Data", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    if (is.na(elpd)) cat(paste("Estimated Ln Prob of Data is NA in file: ", filenames[i], sep=""))
    #get mn value of ln likelihood & error check
    mvll <- as.numeric(gsub("=", "", gsub("Mean value of ln likelihood", "", grep("Mean value of ln likelihood", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    if (is.na(mvll)) cat(paste("Mean value of ln likelihood is NA in file: ", filenames[i], sep=""))
    #get Variance of ln likelihood else NA
    vll <- as.numeric(gsub("=", "", gsub("Variance of ln likelihood", "", grep("Variance of ln likelihood", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    #get Mean value of alpha
    mva <- as.numeric(gsub("=", "", gsub("Mean value of alpha", "", grep("Mean value of alpha", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    #add values to rows in main table
    main[i, ] <- c(filenames[i], k, ind, loci, elpd, mvll, vll, mva, burnin, reps)
    i = i+1
  }
  
  #convert to numeric
  main$k <- as.numeric(main$k)
  main$ind <- as.numeric(main$ind)
  main$elpd <- as.numeric(main$elpd)
  main$mvll <- as.numeric(main$mvll)
  main$vll <- as.numeric(main$vll)
  main$mva <- as.numeric(main$mva)
  main$loci <- as.numeric(main$loci)
  main$burnin <- as.numeric(main$burnin)
  main$reps <- as.numeric(main$reps)
  
  #sort table on K
  main1 <- main[with(main, order(k)), ]
  #rownames(main1) <- 1:length(main1[, 1])
  
  #write table if opted
  if (writetable == TRUE | writetable == "T" | writetable == "TRUE")
  {
    write.table(prettyNum(main1, preserve.width="common"), "tabulateRunsStructure.txt", quote=FALSE, row.names=FALSE)
    cat("tabulateRunsStructure.txt exported\n") 
  }
  
  return(main1)
}

#FUNCTION tabulateRunsTess
#' Tabulate TESS runs
#' @description Creates a table from TESS output files with filenames, K and number of individuals.
#' @param files A character vector of TESS cluster files to be tabulated. Use \code{choose.files(multi=TRUE)} for interactive selection. Use \code{collectRunsTess()} to collect TESS runs from multiple folders into one.
#' @param writetable Set to FALSE by default. Setting to TRUE writes the output table as a tab-delimited text file in the same folder as the STRUCTURE run files.
#' @param quiet If set to TRUE, then number of selected files are not printed.
#' @return Returns a dataframe with filenames, K and number of individuals of all runs sorted by K.
#' @details The row numbers of the output table denotes the file number selected. This is helpful if a particular file from the table needs to be identified in the selection vector.
#' @export
#'
tabulateRunsTess <- function(files=NULL, writetable=FALSE, quiet=FALSE)
{
  quiet <- toupper(quiet)
  #choose output files
  filename <- files
  #if no files chosen, stop excecution, give error message
  if (length(filename) == 0) stop("No input files")
  #get filenames from selection
  filenames <- basename(filename)
  #number of files selected
  number <- length(filenames)
  if (quiet == FALSE | quiet == "F" | quiet == "FALSE") cat(paste("Number of files selected: ", number, "\n", sep=""))
  #make dataframe container
  main <- data.frame(file=filenames, k=1:number, ind=1:number)
  
  #loop to make dataframe with filenames and other variables
  i = 1
  for (i in i:number)
  {
    #read file & error check
    df1 <- runsToDfTess(files[i])
    #add values to rows in main table
    main[i, ] <- c(filenames[i], k=ncol(df1), ind=nrow(df1))
  }
  
  #convert to numeric
  main$k <- as.numeric(main$k)
  main$ind <- as.numeric(main$ind)
  #sort table on K
  main1 <- main[with(main, order(k)), ]
  #rownames(main1) <- 1:length(main1[, 1])
  
  #write table if opted
  if (writetable == TRUE | writetable == "T" | writetable == "TRUE")
  {
    write.table(prettyNum(main1, preserve.width="common"), "tabulateRunsTess.txt", quote=FALSE, row.names=FALSE)
    cat("tabulateRunsTess.txt exported\n") 
  }
  
  return(main1)
}

#FUNCTION summariseRunsStructure
#' Summarise STRUCTURE runs
#' @description Creates a summary table of several STRUCTURE runs with means and std deviation. Refer to return for detailed list of columns.
#' @param data A dataframe with tabulated runs. An output from \code{tabulateRunsStructure()}. Must have minimum 4 columns named k, ind, loci and elpd.
#' @param writetable Set to FALSE by default. Setting to TRUE writes the output table as a tab-delimited text file in the working directory.
#' @return Returns a dataframe with all values of K sorted by K. The table has 6 columns namely Mean estimated ln probability of data, Standard deviation, Value of K, Number of runs for each K, Estimated ln probability of data plus standard deviation, Estimated ln probability of data minus standard deviation.
#' @export

summariseRunsStructure <- function(data=NULL, writetable=FALSE)
{
  #does df data contain any data?
  if (length(data) == 0) stop("No input files")
  #make sure dataframe
  data <- as.data.frame(data)
  #convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  #is column k available?
  if (length(grep("k", colnames(data))) == 0) stop("Column k not available.")
  #is column ind available?
  if (length(grep("ind", colnames(data))) == 0) stop("Column ind not available.")
  #is column loci available?
  if (length(grep("loci", colnames(data))) == 0) stop("Column loci not available.")
  #is column elpd available?
  if (length(grep("elpd", colnames(data))) == 0) stop("Column elpd not available.")
  #is column elpd available?
  if (nrow(data) < 2) stop("At least 2 runs are required for this function.")
  
  #Generate Graph
  elpd2 <- vector()
  elpd3 <- vector()
  sd1 <- vector()
  sd2 <- vector()
  kval <- vector()
  lenk <- vector()
  indvec <- vector()
  locivec <- vector()
  e = 1
  while(e<=length(data[, 1]))
  {
    elpd1 <- vector()
    o <- data$k[e]
    p <- data$ind[e]
    q <- data$loci[e]
    while(o == data$k[e])
    {
      if (p == data$ind[e])
      {
        if (q == data$loci[e])
        {
          ind <- data$ind[e]
          loci <- data$loci[e]
          elpd1 <- c(elpd1, data$elpd[e])
        }else
        {break}
      }else
      {break}
      e = e + 1
      if (e > nrow(data)) break;
    }
    if (length(elpd1) > 1)
    {
      elpd2 <- mean(elpd1)
      sd1 <- sd(elpd1)
      lenkt <- length(elpd1)
      
    }else
    {
      elpd2 <- elpd1
      sd1 <- 0
      lenkt <- length(elpd1)
      
    }
    elpd3 <- c(elpd3, elpd2)
    sd2 <- c(sd2, sd1)
    kval <- c(kval, o)
    lenk <- c(lenk, lenkt)
    indvec <- c(indvec, ind)
    locivec <- c(locivec, loci)
  }
  
  #results into a dataframe
  data1 <- data.frame(meanelpd=elpd3, sd=sd2, k=kval, runs=lenk, ind=indvec, loci=locivec, maxelpd=elpd3+sd2,  minelpd=elpd3-sd2)
  data1$meanelpd <- as.numeric(as.character(data1$meanelpd))
  data1$sd <- as.numeric(as.character(data1$sd))
  data1$k <- as.integer(as.character(data1$k))
  data1$runs <- as.integer(as.character(data1$runs))
  data1$ind <- as.integer(as.character(data1$ind))
  data1$loci <- as.integer(as.character(data1$loci))
  data1$maxelpd <- as.numeric(as.character(data1$maxelpd))
  data1$minelpd <- as.numeric(as.character(data1$minelpd))
  
  #write table if opted
  if (writetable == TRUE | writetable == "T" | writetable == "TRUE")
  {
    write.table(prettyNum(data1, preserve.width="common"), "summariseRunsStructure.txt", quote=FALSE, row.names=FALSE)
    cat("summariseRunsStructure.txt exported\n") 
  }
  
  return(data1)
}

#FUNCTION evannoMethodStructure
#' Perform the Evanno method
#' @description The Evanno method for detecting the appropriate number of population clusters from STRUCTURE results. Creates table and figure with Evanno method derivatives. Refer to return for detailed list of columns. See details for Evanno method reference.
#' @param data A dataframe with summarised runs. An output from \code{summariseRunsStructure()}. Must have minimum 7 columns named meanelpd, sd, k, runs, loci, maxelpd and minelpd.
#' @param writetable Set to FALSE by default. Setting to TRUE writes the output table as a tab-delimited text file in the working directory.
#' @param showplot If set to TRUE, shows the Evanno plots in the graphical device. If Evanno method cannot be computed, a kPlot (elpd over k) is shown instead.
#' @param exportplot If set to TRUE, exports the Evanno plots as image in the working directory. If Evanno method cannot be computed, a kPlot (elpd over k) is exported instead.
#' @param imgtype Type of exported image. Default set to png. Other possible options are jpeg or pdf.
#' @param height Height of exported image. Default units in px. If imgtype is pdf, height must be in inches.
#' @param width Width of exported image. Default units in px. If imgtype is pdf, height must be in inches.
#' @param res Resolution of exported image. Default set to 200. If imgtype is pdf, this option does not apply.
#' @param units Units of measure of the export image. Some options include 'px','in','cm','mm' etc. By default, units is set to 'px' for png and jpeg and to 'in' if imgtype is 'pdf'.
#' @return Returns a dataframe with all values sorted by K. The table has 16 columns namely Mean estimated ln probability of data, Standard deviation, Value of K, Number of runs for each K, Number of runs for each K, Number of individuals for each K, Number of loci for each K, Estimated ln probability of data plus standard deviation, Estimated ln probability of data minus standard deviation, First derivative, Max error of first derivative, Min error of first derivative, Second derivative, Max error of second derivative, Min error of second derivative, Third derivative and Best value of K.
#' @details The Evanno method is based on the paper: Evanno, G., Regnaut, S., & Goudet, J. (2005). Detecting the number of clusters of individuals using the software STRUCTURE: a simulation study. Molecular ecology, 14(8), 2611-2620. The Evanno plot generated from this function can be recreated from the returned dataframe if furthur customisation is required.
#' @export

evannoMethodStructure <- function(data=NULL, writetable=FALSE, showplot=TRUE, exportplot=FALSE, imgtype="png", height=NA, width=NA, res=NA, units=NA)
{
  #checks
  exportplot <- toupper(exportplot)
  imgtype <- tolower(imgtype)
  if (imgtype != "png" && imgtype != "pdf" && imgtype != "jpeg") stop("Argument 'imgtype' set incorrectly. Options are 'png', 'jpeg' or 'pdf'.")
  
  if (is.na(height) && imgtype == "pdf") height <- 8;
  if (is.na(width) && imgtype =="pdf") width <- 8;
  if (is.na(height)) height <- 1500;
  if (is.na(width)) width <- 1500;
  if (is.na(units)) units <- "px";
  if (is.na(res)) res <- 200;
  
  #does df data contain any data?
  if (length(data) == 0) stop("No input files")
  #make sure dataframe
  data <- as.data.frame(data)
  #convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  #is column meanelpd available?
  if (length(grep("meanelpd", colnames(data))) == 0) stop("Column meanelpd not available")
  #is column sd available?
  if (length(grep("sd", colnames(data))) == 0) stop("Column sd not available")
  #is column k available?
  if (length(grep("k", colnames(data))) == 0) stop("Column k not available")
  #is column runs available?
  if (length(grep("runs", colnames(data))) == 0) stop("Column runs not available")
  #is column ind available?
  if (length(grep("ind", colnames(data))) == 0) stop("Column ind not available")
  #is column loci available?
  if (length(grep("loci", colnames(data))) == 0) stop("Column loci not available")
  #is column maxelpd available?
  if (length(grep("maxelpd", colnames(data))) == 0) stop("Column maxelpd not available")
  #is column minelpd available?
  if (length(grep("minelpd", colnames(data))) == 0) stop("Column minelpd not available")
  
  err <- 0
  #atleast 3 values of K?
  if (length(data$k) < 3) {cat("Error: The Evanno method not possible. Requires at least 3 values of K"); err <- 1;}
  #are k values sequential?
  is.sequential <- function(x) all(abs(diff(x)) == 1)
  if (is.sequential(data$k) == FALSE) {cat("Error: The Evanno method not possible. Requires sequential values of K"); err <- 1;}
  #do ind vary?
  if (all(data$ind[1] == data$ind) != TRUE) {cat("Error: The Evanno method not possible. Number of individuals vary between runs"); err <- 1;}
  #do loci vary?
  if (all(data$loci[1] == data$loci) != TRUE) {cat("Error: The Evanno method not possible. Number of loci vary between runs"); err <- 1;}
  #repeats of k<2
  if (all(data$runs < 2)) warning("Results may not be meaningful if repeats (runs) for any value of K is less than 2")
  
  if (err == 1)
  {
    errbar(x=data$k, y=data$meanelpd, yplus=data$maxelpd, yminus=data$minelpd, type="b", pch=19, cap=0.01, xlab=expression(paste(italic(K))), ylab=expression(paste("Mean L(", italic(K), ") " %+-% " SD", sep="")), main="kPlot", cex.main=0.8)
    if (exportplot == TRUE | exportplot == "T" | exportplot == "TRUE")
    {
      #check image imgtype
      if (imgtype == "pdf") pdf("kPlot.pdf", height=height, width=width)
      if (imgtype == "png") png("kPlot.png", height=height, width=width, res=res, units=units, type="cairo")
      if (imgtype == "jpeg") jpeg("kPlot.jpg", height=height, width=width, res=res, quality=100, units=units, type="cairo")
      errbar(x=data$k, y=data$meanelpd, yplus=data$maxelpd, yminus=data$minelpd, type="b", pch=19 , cap=0.01, xlab=expression(paste(italic(K))), ylab=expression(paste("Mean L(", italic(K), ") " %+-% " SD", sep="")), main="kPlot", cex.main=0.8)
      dev.off()
      
      if (imgtype == "pdf") cat("kPlot.pdf exported\n")
      if (imgtype == "png") cat("kPlot.png exported\n")
      if (imgtype == "jpeg") cat("kPlot.jpg exported\n")
    }
    stop("Evanno method not possible")
  }
  
  #Loop to get first derivative of l(K) and its sd
  drv1 <- vector()
  drv1sd <- vector()
  i <- 1
  while (i < length(data$meanelpd))
  {
    drvt1 <- data$meanelpd[i+1]-data$meanelpd[i]
    drv1 <- c(drv1, drvt1)
    drv1sdt <- data$sd[i+1]-data$sd[i]
    drv1sd <- c(abs(drv1sd), abs(drv1sdt))
    i=i+1
  }
  
  #Loop to get second derivative of l(K) and its sd
  drv2 <- vector()
  drv2sd <- vector()
  i <- 1
  while (i < length(drv1))
  {
    drvt2 <- abs(drv1[i+1]-drv1[i])
    drv2 <- c(drv2, drvt2) 
    drv2sdt <- abs(drv1sd[i+1]-drv1sd[i])
    drv2sd <- c(drv2sd, abs(drv2sdt))
    i=i+1
  }
  #add NA to SD vector 1 and 2
  drv1sdf <- c(NA, drv1sd)
  drv2sdf <- c(NA, drv2sd, NA)
  
  data$drv1 <- c(NA, drv1)
  data$drv1max <- data$drv1+drv1sdf
  data$drv1min <- data$drv1-drv1sdf
  data$drv2 <- c(NA, drv2, NA)
  data$drv2max <- data$drv2+drv2sdf
  data$drv2min <- data$drv2-drv2sdf
  data$drv3 <- abs(data$drv2)/data$sd
  data$BestK <- ""
  bestpos <- (1:length(data$drv3))[(data$drv3) == max(data$drv3, na.rm=TRUE)]
  bestpos <- bestpos[!is.na(bestpos)]
  data$BestK[bestpos] <- "*"
  
  #write table if opted
  if (writetable == TRUE | writetable == "T" | writetable == "TRUE")
  {
    write.table(prettyNum(data, preserve.width="common"), "evannoMethodStructure.txt", quote=FALSE, row.names=FALSE)
    cat("evannoMethodStructure.txt exported\n") 
  }
  
  if (exportplot == TRUE | exportplot == "T" | exportplot == "TRUE") showplot=FALSE
  #show plot
  if (showplot == TRUE | showplot == "T" | showplot == "TRUE")
  { 
    #change par settings
    par(mfrow=c(2, 2))
    par(mar=c(3.5, 3.5, 1, 1))
    par(oma=c(1, 1, 1, 0.5))
    par(mgp=c(2.2, 1, 0))
    #plot images
    errbar(x=data$k, y=data$meanelpd, yplus=data$maxelpd, yminus=data$minelpd, type="b", pch=19, cap=0.01, xlab=expression(paste(italic(K))), ylab=expression(paste("Mean L(", italic(K), ") " %+-% " SD", sep="")))
    mtext(side=3, text="A", line=0.2, cex=0.8, adj=0)
    errbar(x=data$k, y=data$drv1, yplus=data$drv1max, yminus=data$drv1min, type="b", pch=19, cap=0.01, xlab=expression(paste(italic(K))), ylab=expression(paste("L'(", italic(K), ") " %+-% " SD", sep="")))
    mtext(side=3, text="B", line=0.2, cex=0.8, adj=0)
    errbar(x=data$k, y=data$drv2, yplus=data$drv2max, yminus=data$drv2min, type="b", pch=19, cap=0.01, xlab=expression(paste(italic(K))), ylab=expression(paste("L''(", italic(K), ") " %+-% " SD", sep="")))
    mtext(side=3, text="C", line=0.2, cex=0.8, adj=0)
    if (is.finite(sum(data$drv3, na.rm=TRUE))) plot(x=data$k, y=data$drv3, type="b", pch=19, xlab=expression(paste(italic(K))), ylab=expression(paste(Delta, italic(K), sep="")))
    if (is.finite(sum(data$drv3, na.rm=TRUE))) mtext(side=3, text="D", line=0.2, cex=0.8, adj=0)
    #mtext(side = 1, outer=TRUE, text = expression(paste("Detecting Cluster Number. (Evanno ", italic(et~al.), ", 2005)")), line = 0, cex=0.4, adj=1, padj=1)
    #mtext(side = 1, outer=TRUE, text = paste("Analysed: ", date(), sep=""), line = 0, cex=0.4, adj=0, padj=1)
  }
  
  #export image
  if (exportplot == TRUE | exportplot == "T" | exportplot == "TRUE")
  {
    #check image imgtype
    if (imgtype == "pdf") pdf("evannoMethodStructure.pdf", height=height, width=width)
    if (imgtype == "png") png("evannoMethodStructure.png", height=height, width=width, res=res, units=units, type="cairo")
    if (imgtype == "jpeg") jpeg("evannoMethodStructure.jpg", height=height, width=width, res=res, quality=100, units=units, type="cairo")
    #change par settings
    par(mfrow=c(2, 2))
    par(mar=c(3.5, 3.5, 1, 1))
    par(oma=c(1, 1, 1, 0.5))
    par(mgp=c(2.2, 1, 0))
    errbar(x=data$k, y=data$meanelpd, yplus=data$maxelpd, yminus=data$minelpd, type="b", pch=19, cap=0.01, xlab=expression(paste(italic(K))), ylab=expression(paste("Mean L(", italic(K), ") " %+-% " SD", sep="")))
    mtext(side=3, text="A", line=0.2, cex=1.2, adj=0)
    errbar(x=data$k, y=data$drv1, yplus=data$drv1max, yminus=data$drv1min, type="b", pch=19, cap=0.01, xlab=expression(paste(italic(K))), ylab=expression(paste("L'(", italic(K), ") " %+-% " SD", sep="")))
    mtext(side=3, text="B", line=0.2, cex=1.2, adj=0)
    errbar(x=data$k, y=data$drv2, yplus=data$drv2max, yminus=data$drv2min, type="b", pch=19, cap=0.01, xlab=expression(paste(italic(K))), ylab=expression(paste("L''(", italic(K), ") " %+-% " SD", sep="")))
    mtext(side=3, text="C", line=0.2, cex=1.2, adj=0)
    if (is.finite(sum(data$drv3, na.rm=TRUE))) plot(x=data$k, y=data$drv3, type="b", pch=19, xlab=expression(paste(italic(K))), ylab=expression(paste(Delta, italic(K), sep="")))
    if (is.finite(sum(data$drv3, na.rm=TRUE))) mtext(side=3, text="D", line=0.2, cex=1.2, adj=0)
    #mtext(side = 1, outer=TRUE, text = expression(paste("Detecting Cluster Number. (Evanno ",italic(et~al.),", 2005)")), line = 0, cex=0.4, adj=1, padj=1)
    #mtext(side = 1, outer=TRUE, text = paste("Analysed: ",date(),sep=""), line = 0, cex=0.4, adj=0, padj=1)
    dev.off()
    
    if (imgtype == "pdf") cat("evannoMethodStructure.pdf exported\n")
    if (imgtype == "png") cat("evannoMethodStructure.png exported\n")
    if (imgtype == "jpeg") cat("evannoMethodStructure.jpg exported\n")
  }
  
  #return table
  return(data)
}

#FUNCTION clumppExportStructure
#' Combine STRUCTURE runs and export files for use with software CLUMPP
#' @description Takes multiple STRUCTURE runs and combines several repeats for each K into a single file along with a parameter file. The two output files are organised into folders by K. The CLUMPP.exe file can simply be copied to this folder and run to reorder the clusters for each K.
#' @param files A character vector of STRUCTURE output files to be tabulated. Use \code{choose.files(multi=TRUE)} for interactive selection.
#' @param prefix A character prefix for folder names.
#' @return The combined file and paramfile are written into respective folders named by population. A tabulated table is returned from function same as \code{tabulateRunsStructure()}.
#' @details When multiple repeats are run for each K in STRUCTURE, the order of clusters may be jumbled for each run. Therefore, when plotting multiple runs within each K, the colours cannot be assigned correctly. The software CLUMPP helps to overcome this issue by reordering the clusters correctly. This function clumppExportStructure() takes multiple runs for each K and combines them into a single file and generates a parameter file for easy use with CLUMPP. Further details for CLUMPP can be found here: Jakobsson, M., & Rosenberg, N. A. (2007). CLUMPP: a cluster matching and permutation program for dealing with label switching and multimodality in analysis of population structure. Bioinformatics, 23(14), 1801-1806.
#' @export
#'
clumppExportStructure <- function(files=NULL, prefix=NA)
{
  if (length(files) == 0) stop("No input files")
  if (is.na(prefix)) prefix <- "STRUCTUREpop"
  prefix <- paste(prefix, "_K", sep="")
  
  #get tabulated runs
  df <- tabulateRunsStructure(files=files)
  
  #Loop to move through k
  e=1
  while(e <= nrow(df))
  {
    #loop to extract and combine tables
    o <- df$k[e]
    p <- df$ind[e]
    q <- df$loci[e]
    n <- 0
    ldata <- list()
    while(o == df$k[e])
    {
      if (p == df$ind[e])
      {
        if (q == df$loci[e])
        {
          k <- df$k[e]
          ind <- df$ind[e]
          loci <- df$loci[e]
          n <- n+1
          
          sel <- grep(as.character(df[e, 1]), files)
          dframe1 <- runsToDfStructure(files[sel])
          
          #generate first and last line
          v <- character();v1 <- vector();
          for (z in 1:p)
          {
            v <- (paste(z, ":", sep=""))
            v1 <- c(v1, v)
          }
          dframe2 <- data.frame(V1=v1, dframe1, last=as.character(rep(1, p)))
          dframe3 <- as.matrix(dframe2)
          
          #add dataframes to list
          ldata[[n]] <- dframe3
          if (e>nrow(df))break;
          
        }else
        {break;}
      }else
      {break;}
      e=e+1
      if (e>nrow(df))break;
    }
    if (n>1)
    {
      currwd <- getwd()
      dir.create(paste(currwd, "/", prefix, k, sep=""))
      setwd(paste(currwd, "/", prefix, k, sep=""))
      cat(paste("Folder created: ", basename(getwd()), "\n", sep=""))  
      out <- paste(prefix, k, "-combined.txt", sep="")
      
      #File Output block
      
      #make 2 line space
      sp <- rep("  ", k+2)
      space <- t(data.frame(sp, sp))
      
      #Write file
      write(t(format(ldata[[1]], nsmall=15)), paste(out), ncolumns=k+2)
      i=2;
      for (i in 2:length(ldata))
      {
        write(t(format(space, nsmall=15)), paste(out), ncolumns=k+2, append=TRUE)
        write(t(format(ldata[[i]], nsmall=15)), append=TRUE, paste(out), ncolumns=k+2)
      }
      cat(paste(out), "exported\n")
      
      
      T1 <- factorial(k)*((length(ldata)*(length(ldata)-1))/2)*k*ind
      if (T1 <= 100000000)
      {
        mode <- 2
        repeats <- 20
      }else{
        mode <- 3
        repeats <- 500
      }
      out1 <- gsub(".txt","",out)
      params <- c("DATATYPE 1 ",
                  "INDFILE NOTNEEDED.indfile ",
                  paste("POPFILE ",out," ",sep=""),
                  paste("OUTFILE ",out1,"-merged.txt ",sep=""),
                  paste("MISCFILE ",out1,"-miscfile.txt ",sep=""),
                  paste("K ",k," ",sep=""),
                  paste("C ",ind," ",sep=""),
                  paste("R ",length(ldata)," ",sep=""),
                  paste("M ",mode," ",sep=""),
                  "W 0 ",
                  "S 2 ",
                  "GREEDY_OPTION 2 ",
                  paste("REPEATS ", repeats," ",sep=""),
                  "PERMUTATIONFILE NOTNEEDED.permutationfile ",
                  "PRINT_PERMUTED_DATA 1 ",
                  paste("PERMUTED_DATAFILE ",out1,"-aligned.txt ",sep=""),
                  "PRINT_EVERY_PERM 0 ",
                  paste("EVERY_PERMFILE ",out1,".every_permfile ",sep=""),
                  "PRINT_RANDOM_INPUTORDER 0 ",
                  paste("RANDOM_INPUTORDERFILE ",out1,".random_inputorderfile ",sep=""),
                  "OVERRIDE_WARNINGS 0 ",
                  "ORDER_BY_RUN 0 ")
      
      write(params, "paramfile")
      cat(paste("paramfile exported\n", sep=""))
      
      setwd(paste(currwd))
      cat("***********************\n")
    }else
    {
      cat(paste(prefix, k, " not exported. Repeats less than 2\n", sep=""))
      cat("***********************\n")
    }
  }
  cat("Run completed.\n")
  return(df)
}


#FUNCTION clumppExportTess
#' Combine TESS runs and export files for use with software CLUMPP
#' @description Takes multiple TESS runs and combines several repeats for each K into a single file along with a parameter file. The two output files are organised into folders by K. The CLUMPP.exe file can simply be copied to this folder and run to reorder the clusters for each K.
#' @param files A character vector of TESS cluster run files to be tabulated. Use \code{choose.files(multi=TRUE)} for interactive selection.
#' @param prefix A character prefix for folder names.
#' @return The combined file and paramfile are written into respective folders named by population. A tabulated table is returned from function same as \code{tabulateRunsTess()}.
#' @details When multiple repeats are run for each K in TESS, the order of clusters may be jumbled for each run. Therefore, when plotting multiple runs within each K, the colours cannot be assigned correctly. The software CLUMPP helps to overcome this issue by reordering the clusters correctly. This function clumppExportTess() takes multiple runs for each K and combines them into a single file and generates a parameter file for easy use with CLUMPP. Further details for CLUMPP can be found here: Jakobsson, M., & Rosenberg, N. A. (2007). CLUMPP: a cluster matching and permutation program for dealing with label switching and multimodality in analysis of population structure. Bioinformatics, 23(14), 1801-1806.
#' @export
#'
clumppExportTess <- function(files=NULL, prefix=NA)
{
  if (length(files) == 0) stop("No input files")
  if (is.na(prefix)) prefix <- "TESSpop"
  prefix <- paste(prefix, "_K", sep="")
  
  #get tabulated runs
  df <- tabulateRunsTess(files=files)
  
  #Loop to move through k
  e=1
  while(e <= nrow(df))
  {
    #loop to extract and combine tables
    o <- df$k[e]
    p <- df$ind[e]
    n <- 0
    ldata <- list()
    while(o == df$k[e])
    {
      if (p == df$ind[e])
      {
        k <- df$k[e]
        ind <- df$ind[e]
        n <- n+1
        
        sel <- grep(as.character(df[e, 1]), files)
        dframe1 <- runsToDfTess(files[sel])
        
        #generate first and last line
        v <- character();v1 <- vector();
        for (z in 1:p)
        {
          v <- (paste(z, ":", sep=""))
          v1 <- c(v1, v)
        }
        
        dframe2 <- data.frame(V1=v1, dframe1, last=as.character(rep(1, p)))
        dframe3 <- as.matrix(dframe2)
        
        #add dataframes to list
        ldata[[n]] <- dframe3
        if (e>nrow(df))break;
        
      }else
      {break;}
      e=e+1
      if (e>nrow(df))break;
    }
    if (n>1)
    {
      currwd <- getwd()
      dir.create(paste(currwd, "/", prefix, k, sep=""))
      setwd(paste(currwd, "/", prefix, k, sep=""))
      cat(paste("Folder created: ", basename(getwd()), "\n", sep=""))  
      out <- paste(prefix, k, "-combined.txt", sep="")
      
      #File Output block
      
      #make 2 line space
      sp <- rep("  ", k+2)
      space <- t(data.frame(sp, sp))
      
      #Write file
      write(t(format(ldata[[1]], nsmall=15)), paste(out), ncolumns=k+2)
      i=2;
      for (i in 2:length(ldata))
      {
        write(t(format(space, nsmall=15)), paste(out), ncolumns=k+2, append=TRUE)
        write(t(format(ldata[[i]], nsmall=15)), append=TRUE, paste(out), ncolumns=k+2)
      }
      cat(paste(out), "exported\n")
      
      
      T1 <- factorial(k)*((length(ldata)*(length(ldata)-1))/2)*k*ind
      if (T1 <= 100000000)
      {
        mode <- 2
        repeats <- 20
      }else{
        mode <- 3
        repeats <- 500
      }
      out1 <- gsub(".txt","",out)
      params <- c("DATATYPE 1 ",
                  "INDFILE NOTNEEDED.indfile ",
                  paste("POPFILE ",out," ",sep=""),
                  paste("OUTFILE ",out1,"-merged.txt ",sep=""),
                  paste("MISCFILE ",out1,"-miscfile.txt ",sep=""),
                  paste("K ",k," ",sep=""),
                  paste("C ",ind," ",sep=""),
                  paste("R ",length(ldata)," ",sep=""),
                  paste("M ",mode," ",sep=""),
                  "W 0 ",
                  "S 2 ",
                  "GREEDY_OPTION 2 ",
                  paste("REPEATS ", repeats," ",sep=""),
                  "PERMUTATIONFILE NOTNEEDED.permutationfile ",
                  "PRINT_PERMUTED_DATA 1 ",
                  paste("PERMUTED_DATAFILE ",out1,"-aligned.txt ",sep=""),
                  "PRINT_EVERY_PERM 0 ",
                  paste("EVERY_PERMFILE ",out1,".every_permfile ",sep=""),
                  "PRINT_RANDOM_INPUTORDER 0 ",
                  paste("RANDOM_INPUTORDERFILE ",out1,".random_inputorderfile ",sep=""),
                  "OVERRIDE_WARNINGS 0 ",
                  "ORDER_BY_RUN 0 ")
      
      write(params, "paramfile")
      cat(paste("paramfile exported\n", sep=""))
      
      setwd(paste(currwd))
      cat("***********************\n")
    }else
    {
      cat(paste(prefix, k, " not exported. Repeats less than 2\n", sep=""))
      cat("***********************\n")
    }
  }
  cat("Run completed.\n")
  return(df)
}

# FUNCTION runsToDfStructure
#' Convert STRUCTURE run files to R dataframes.
#' @description Takes one or more STRUCTURE output files and converts each of them to separate dataframes.
#' @param files One or more STRUCTURE run files. Use \code{choose.files(multi=TRUE)} to select interactively.
#' @return If a single file is selected, a single dataframe is returned. If multiple files are selected, a list with multiple dataframes is returned.
#' @export
#'
runsToDfStructure <- function(files=NA)
{
  if (all(is.na(files))) stop("No input files")
  #number of files selected
  number <- length(files)
  #cat(paste("Number of files selected: ", number, "\n", sep=""))
  
  i=1
  dlist <- list(length=number)
  for (i in 1:length(files))
  {
    name <- basename(files[i])
    
    file1 <- readLines(as.character(files[i]), warn=FALSE)
    chk1 <- grep("STRUCTURE", toupper(file1[4]))
    if (length(chk1) == 0) stop("Input not suitable STRUCTURE file/Incorrect input format.")
    if (length(file1)<1) stop("Cannot read file")
    
    #find individuals and get number of individuals
    ind <- as.numeric(as.character(gsub("\\D", "", grep("\\d individuals", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    if (is.na(ind)) cat(paste("Number of individuals is NA in file: ", name, sep=""))
    
    #get value of k & error check
    k <- as.numeric(as.character(gsub("\\D", "", grep("\\d populations assumed", file1, perl=TRUE, ignore.case=TRUE, value=TRUE)[1])))
    if (is.na(k)) cat(paste("Value of K is NA in file: ", name, sep=""))
    
    cstart <- charmatch("Inferred ancestry of individuals", file1)
    cend <- charmatch("Estimated Allele Frequencies in each cluster", file1)
    cstart1 <- cstart+2
    cend1 <- cend-3
    file_a <- file1[cstart1:cend1]
    file_b <- gsub(":  ", "", substr(file_a, regexpr(":\\W+\\d\\.\\d+", file_a), nchar(file_a)-1))
    file_c <- as.vector(as.numeric(as.character(unlist(strsplit(file_b, " ")))))
    dframe <- as.data.frame(matrix(file_c, nrow=ind, byrow=TRUE),stringsAsFactors=FALSE)
    dframe <- as.data.frame(sapply(dframe, as.numeric))
    colnames(dframe) <- paste("Cluster", 1:k, sep="")
    dlist[[i]] <- dframe
    #names(dlist[[i]]) <- as.character(name)
  }
  if (number>1) {return(dlist)} else{return(dframe)}
}


# FUNCTION runsToDfTess
#' Convert TESS cluster files to R dataframe.
#' @description Takes one or more TESS cluster run files and converts each of them to separate dataframes.
#' @param files One or more TESS cluster run files. Use \code{choose.files(multi=TRUE)} to select interactively.
#' @return If a single file is selected, a single dataframe is returned. If multiple files are selected, a list with multiple dataframes is returned.
#' @details Use collectRunsTess() to collect TESS runs into one directory.
#' @export
#'
runsToDfTess <- function(files=NA)
{
  if (all(is.na(files))) stop("No input files")
  #number of files selected
  number <- length(files)
  #cat(paste("Number of files selected: ", number, "\n", sep=""))
  
  i=1
  dlist <- list(length=number)
  for (i in 1:length(files))
  {
    name <- gsub(".txt", "", basename(files[i]))
    
    file1 <- readLines(files[i], warn=FALSE)
    #read TESS files
    chk <- grep("CLUSTERING PROBABILITIES", toupper(file1[1]))
    if (length(chk) == 0) stop("Input not appropriate TESS file/Incorrect input format.")
    if (length(file1) < 1) stop("Cannot read file")
    
    #extract the cluster table part
    file1 <- file1[3:c(grep("Estimated Allele Frequencies", file1)-4)]
    file2 <- as.vector(unlist(strsplit(file1, "\t")))
    file3 <- as.data.frame(matrix(file2, nrow=length(file1), byrow=TRUE),stringsAsFactors=FALSE)
    dframe <- file3[, -c(1, ncol(file3)-1, ncol(file3))]
    dframe <- as.data.frame(sapply(dframe, as.numeric))
    colnames(dframe) <- paste("Cluster", 1:ncol(dframe), sep="")
    dlist[[i]] <- dframe
    #names(dlist[[i]]) <- as.character(name)
  }
  if (number>1) {return(dlist)} else{return(dframe)}
}


#FUNCTION getDim
#' Internal: Get dimensions for figures.
#' @description Generate height and width of figure based on number of individuals. This is an internal function that calculates figure dimensions for export.
#' @param ind Number of individuals.
#' @param units unit of dimension: "cm","mm","in".
#' @param height Height of each plot.
#' @param width Width of each plot.
#' @param res Resolution of the figure.
#' @param plotnum Number of plot in the figure.
#' @return a vector with height and width.
#'
getDim <- function(ind, units="cm", height=NA, width=NA, res=300, plotnum=1)
{
  #in cm
  if (is.na(height))
  {
    height <- ind*0.0028; 
    if (height < 2.5) height <- 2.5
    if (height > 4.6) height <- 4.6
    if (plotnum > 1) height  <-  height-((height*plotnum)/90)
    if (units == "in") height <- round(height*0.3937, 2)
    if (units == "mm") height <- height*10
    if (units == "px") height <- round((height*res)/2.54, 0)
  }
  height <- height*plotnum
  
  if (is.na(width))
  {
    width <- ind*0.020; 
    if (width < 5) width <- 5
    if (width > 30) width <- 30
    if (units == "in") width <- round(width*0.3937, 2)
    if (units == "mm") width <- width*10
    if (units == "px") width <- round((width*res)/2.54, 0)
  }
  
  return(c(height, width))
}

# FUNCTION getPlotParams
#' Internal: Generate parameters for plots with labels
#' @description Generates various parameters required for plotting with labels. Internal function.
#' @param poplab A character vector of labels same length as number of individuals.
#' @param plotnum Number of plots on the figure.
#' @param labpos The y position of the labels.
#' @param labsize Size of the labels.
#' @param labangle Angle/Rotation of labels. 0 is horizontal while 90 is vertical.
#' @param labjust Justification of labels. Defaults to 0.5 if labangle=0  or 1 if labangle between 20 and 135.
#' @param labcol Colour of labels. Defaults to "grey30".
#' @param pointsize Size of points on label marker line.
#' @param pointcol Colour of the points on the label marker line. Defaults to "grey30".
#' @param pointtype Type of points on the label marker line. Defaults to |.
#' @param linepos The y position of the label marker line and the points.
#' @param linethick Thickness of the label marker line.
#' @param linecol Colour of the label marker line. Defaults to "grey30".
#' @param fmar Figure margins in 'lines' unit. A vector of 4 numbers for top, right, bottom and left margins. ex. c(0.2,0.2,0.2,0).
#' @return A list with following plot parameters: poplab, plotnum, labpos, labsize, labangle, labjust, labcol, pointsize, pointcol, pointtype, linepos, linethick, linecol, fmar
#' 
getPlotParams <- function(poplab=NA, plotnum=1, labpos=NA, labsize=NA, labangle=NA, labjust=NA, labcol=NA,
                          pointsize=NA, pointcol=NA, pointtype=NA,
                          linepos=NA, linethick=NA, linecol=NA, fmar=NA)
{
  if (all(is.na(poplab))) stop("Labels are empty")
  #labangle <- NA
  #labjust <- NA
  #labpos <- NA
  #labsize <- NA
  #labcol <- NA
  #pointsize <- NA
  #pointcol <- NA
  #pointtype <- NA
  #linepos <- NA
  #linethick <- NA
  #linecol <- NA
  #fmar <- NA
  
  #basic values
  col3 <- "grey30"
  if (is.na(labcol)) labcol <- col3
  if (is.na(pointcol)) pointcol <- col3
  if (is.na(linecol)) linecol <- col3
  if (is.na(pointtype)) pointtype <- "|"
  
  #estimate ct based on number of labels/ind
  lp <- length(as.character(poplab))
  
  #calculate just and margins
  if (is.na(labangle)) labangle <- 0
  if (labangle == 0)
  {
    if (is.na(labjust)) labjust <- 0.5
    if (all(is.na(fmar))) fmar <- c(0.2, 0.2, 0.2, 0)
  }
  
  if (abs(labangle) > 20 & abs(labangle) < 135)
  {
    if (is.na(labjust)) labjust=1
    bmar <- round(max(nchar(as.character(poplab)))/8, 2)+round(lp/900, 3)
    if (all(is.na(fmar))) fmar <- c(0.2, 0.2, bmar, 0)
  }
  
  if (plotnum > 1) fmar[1] <- 0
  
  linepos1 <- linepos
  labpos1 <- labpos
  labsize1 <- labsize
  pointsize1 <- pointsize
  linethick1 <- linethick
  
  if (is.na(linepos)) linepos1 <- lp*-0.000035
  if (is.na(labpos)) labpos1 <- linepos1*2.4
  if (is.na(labsize)) labsize1 <- lp*0.00125
  if (is.na(pointsize)) pointsize1 <- lp*0.0016
  if (is.na(linethick)) linethick1 <- lp*0.0003
  
  if (is.na(linepos)) {if (linepos1 < -0.08) linepos1 <- -0.08}
  if (is.na(labpos)) {if (linepos1 < -0.192) labpos1 <- linepos1*2.4}
  if (is.na(labsize)) {if (labsize1 < 1.5) labsize1 <- 1.5}
  if (is.na(pointsize)) {if (pointsize1 < 1.5) pointsize1 <- 1.5}
  if (is.na(linethick)) {if (linethick1 < 0.3) linethick1 <- 0.3}
  
  if (is.na(linepos)) {if (linepos1 > -0.07) linepos1 <- -0.07}
  if (is.na(labpos)) {if (labpos1 > -0.168) labpos1 <- linepos1*2.4}
  if (is.na(labsize))  {if (labsize1 > 2.5) labsize1 <- 2.5}
  if (is.na(pointsize)) {if (pointsize1 > 3.2) pointsize1 <- 3.2}
  if (is.na(linethick)) {if (linethick1 > 0.6) linethick1 <- 0.6}
  
  dlist <- list(poplab=poplab, plotnum=plotnum, labpos=labpos1, labsize=labsize1, labangle=labangle, labjust=labjust, labcol=labcol, 
                pointsize=pointsize1, pointcol=pointcol, pointtype=pointtype, linepos=linepos1, linethick=linethick1, 
                linecol=linecol, fmar=fmar)
  return(dlist)
}

# FUNCTION plotRuns
#' Plot STRUCTURE, TESS or table files as barplots.
#' @description Plot one or more STRUCTURE/TESS output files or table files (aligned/combined/merged) files. The STRUCTURE and TESS files can be plotted individually or joined together. The table files will be plotted based on number of runs in each file.
#' @param files One or more STRUCTURE/TESS/table files
#' @param imgoutput Options are 'sep','join' or'tab'.If output="sep", STRUCTURE/TESS run files are plotted as separate image files. If output="join", STRUCTURE/TESS run files are joined into a single image. If output="tab", combined/aligned/merged files are plotted into separate or joined plots based on number of tables within each file.
#' @param poplab A character vector of population labels equal to length of individuals. Each pop name must repeat to the number of individuals present in each pop.
#' @param popcol A vector of colours (representing populations) for colouring clusters. If NA, colours are automatically generated. K 1 to 12 are custom unique colours while K>12 are coloured by function rich.color().
#' @param imgtype Image file type. Possible options are "png","jpeg" or "pdf". For pdf, height and width are in inches and res does not apply.
#' @param height Height of individual run plot. By default, automatically generated based on number of Individuals. Ranges between 2.5cm and 4.6cm.
#' @param width Width of individual run plot. By default, automatically generated based on number of individuals. Ranges between 5cm and 30cm.
#' @param dpi Image resolution. Defaults to 300.
#' @param units Units of height and width. Default set to "cm". If type is eps, units must be "in".
#' @param flabsize The size of the filename label. Defaults to 4.
#' @param flabcol The colour of the filename label. Defaults to "grey10".
#' @param flabbackcol The background colour of the filename label. Defaults to white
#' @param labpos The y position of the labels.
#' @param labsize Size of the labels.
#' @param labangle Angle/Rotation of labels. 0 is horizontal while 90 is vertical.
#' @param labjust Justification of labels. Defaults to 0.5 if labangle=0  or 1 if labangle between 20 and 135.
#' @param labcol Colour of labels. Defaults to "grey30".
#' @param pointsize Size of points on label marker line.
#' @param pointcol Colour of the points on the label marker line. Defaults to "grey30".
#' @param pointtype Type of points on the label marker line. Defaults to |.
#' @param linepos The y position of the label marker line and the points.
#' @param linethick Thickness of the label marker line.
#' @param linecol Colour of the label marker line. Defaults to "grey30".
#' @param fmar Figure margins in 'lines' unit. A vector of 4 numbers for top, right, bottom and left margins. ex. c(0.2,0.2,0.2,0).
#' @return Nothing is returned.
#' @details It is possible to set either height or width and leave other as default.
#' @export
#' 
plotRuns <- function(files=NULL, imgoutput="sep", poplab=NA, popcol=NA, imgtype="png", height=NA, width=NA, dpi=NA, units=NA, 
                     flabsize=NA, flabcol=NA, flabbackcol=NA, labpos=NA, labsize=NA, labangle=NA, labjust=NA, labcol=NA, 
                     pointsize=NA, pointcol=NA, pointtype=NA, linepos=NA, linethick=NA, linecol=NA, fmar=NA)
{ 
  if (is.na(dpi)) dpi=300
  if (is.na(units)) units="cm"
  if (is.na(flabsize)) flabsize=4
  if (is.na(flabcol)) flabcol="grey10"
  if (is.na(flabbackcol)) flabbackcol="white"
  
  if (!all(is.na(poplab)))
  {
    col3 <- "grey30"
    
    #lplab <- length(poplab)
    poplab1 <- factor(as.character(poplab), ordered=FALSE)
    labs <- as.character(unique(poplab1))
    tab <- as.data.frame(table(as.character(poplab1)))
    tab <- tab[c(rank(labs)), ]
    #length of each population and pop names
    lenvec <- tab$Freq
    #cumulative sum for positions
    pos <- c(1, cumsum(lenvec))
    #labels
    posl <- round((diff(pos)/2)+pos[1:length(pos)-1], 1)
    if (length(labs)!=length(posl)) stop("Label position and label length mismatch")
    
    lframe <- data.frame(pos=posl, lab=labs)
    lframe$temp <- factor(rep(1, nrow(lframe)))
    pos1 <- data.frame(pos=pos)
  }
  
  
  #if no files chosen, stop excecution, give error message
  if (length(files) == 0) stop("No files selected")
  #check imgoutput
  imgoutput <- tolower(imgoutput)
  if (imgoutput != "sep" && imgoutput != "join" && imgoutput != "tab") stop("Argument 'imgoutput' set incorrectly. Set as 'sep' if input are structure files and output are separate plots. Set as 'join' if input are structure files and output is one joined plot. Set as 'tab' if input is aligned/combined/merged files")
  #check image
  imgtype <- tolower(imgtype)
  if (imgtype!="png" && imgtype != "pdf" && imgtype != "jpeg") stop("Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg' or 'pdf'")
  #length of files
  flen <- length(files)
  #make labeller function for facets
  plotlabeller <- function(variable, value){return(facetnames[value])}
  
  #ss separate plots
  if (imgoutput == "sep")
  {
    facetnames <- list()
    for (i in 1:flen)
    {
      fname <- gsub(".txt", "", basename(files[i]))
      
      #read Structure files
      chk <- grep("CLUSTERING PROBABILITIES", toupper(readLines(files[i], warn=FALSE))[1])
      if (length(chk) != 0) df1 <- runsToDfTess(files=files[i])
      #read TESS files
      chk1 <- grep("STRUCTURE", toupper(readLines(files[i], warn=FALSE))[4])
      if (length(chk1) != 0) df1 <- runsToDfStructure(files=files[i])
      if (length(chk) == 0 & length(chk1) == 0) stop("Incorrect input file type")
      
      k <- ncol(df1)
      Ind <- nrow(df1)
      df1$Ind <- factor(1:nrow(df1))
      df1$Num <- factor(rep(i, nrow(df1)))
      #df1$Lab <- poplab
      #df1 <- melt(df1, id.var=c("Ind", "Num", "Lab"))
      df2 <- melt(df1, id.var=c("Ind", "Num"))
      facetnames <- as.list(paste(fname, "\n", "K=", k, sep=""))
      
      #get Colours
      coll <- popcol
      if (all(is.na(popcol))) coll <- getColours(k)
      if (length(coll)!=k) stop("Number of colours not equal to number of populations")
      
      #getDim
      #pnum <- 1
      #if (!all(is.na(poplab))) pnum <- 2
      height1=getDim(ind=Ind, height=height, width=width, res=dpi, units=units, plotnum=1)[1]
      width1=getDim(ind=Ind, height=height, width=width, res=dpi, units=units, plotnum=1)[2]
      
      if (all(is.na(poplab)))
      {
        #plot
        p <- ggplot(data=df2, aes(x=Ind, y=value, fill=variable))+
          geom_bar(width=1, space=0, stat="identity", position="stack")+
          scale_x_discrete(expand=c(0, 0))+
          scale_y_continuous(expand=c(0, 0))+
          scale_fill_manual(values=coll)+
          facet_grid(Num~., labeller=plotlabeller)+
          labs(x=NULL, y=NULL)+
          theme(legend.position="none", panel.grid=element_blank(), panel.background=element_blank(), 
                axis.ticks=element_blank(), axis.text=element_blank(), axis.line=element_blank(), 
                axis.title=element_blank(), strip.text=element_text(size=flabsize, colour=flabcol), 
                strip.background=element_rect(colour=flabbackcol, fill=flabbackcol), 
                plot.margin=unit(c(0.1, 0.1, 0, 0), "cm"))
        
        if (imgtype == "png") ggsave(paste(fname, ".png", sep=""), p, height=height1, width=width1, dpi=dpi, units=units)
        if (imgtype == "jpeg") ggsave(paste(fname, ".jpg", sep=""), p, height=height1, width=width1, dpi=dpi, units=units, quality=100)
        if (imgtype == "pdf") ggsave(paste(fname, ".pdf", sep=""), p, height=height1, width=width1, units=units)
        
        if (imgtype == "png") cat(paste(fname, ".png exported\n", sep=""))
        if (imgtype == "jpeg") cat(paste(fname, ".jpg exported\n", sep=""))
        if (imgtype == "pdf") cat(paste(fname, ".pdf exported\n", sep=""))
      }
      if (!all(is.na(poplab)))
      {
        if (nrow(df1)!=length(as.character(poplab))) stop("Length of labels do not match number of individuals in input file.")
        
        ppar <- getPlotParams(poplab=poplab, plotnum=1, labpos=labpos, labsize=labsize, labangle=labangle, 
                              labjust=labjust, labcol=labcol,pointsize=pointsize, pointcol=pointcol, 
                              pointtype=pointtype, linepos=linepos, linethick=linethick, linecol=linecol, 
                              fmar=fmar)
        
        labangle <- ppar$labangle
        labjust <- ppar$labjust
        labpos <- ppar$labpos
        labsize <- ppar$labsize
        labjust <- ppar$labjust
        labcol <- ppar$labcol
        pointsize <- ppar$pointsize
        pointcol <- ppar$pointcol
        pointtype <- ppar$pointtype
        linepos <- ppar$linepos
        linethick <- ppar$linethick
        linecol <- ppar$linecol
        fmar <- ppar$fmar
        
        p <- ggplot()+
          geom_bar(data=df2, aes(x=Ind, y=value, fill=variable), width=1, space=0, stat="identity", position="stack")+
          scale_x_discrete(expand=c(0, 0))+
          scale_y_continuous(expand=c(0, 0))+
          scale_fill_manual(values=coll)+
          facet_grid(Num~., labeller=plotlabeller)+
          labs(x=NULL, y=NULL)+
          geom_text(data=lframe, aes_string(x="pos", y=labpos), label=labs, angle=labangle, hjust=labjust, size=labsize, colour="grey30")+
          geom_line(data=pos1, aes_string(x="pos", y=linepos), colour=linecol, size=linethick)+
          geom_point(data=pos1, aes_string(x="pos", y=linepos), size=pointsize, colour=pointcol, shape=pointtype)+
          theme(legend.position="none", panel.grid=element_blank(), panel.background=element_blank(), 
                axis.ticks=element_blank(), axis.text=element_blank(), axis.line=element_blank(), 
                axis.title=element_blank(), strip.text=element_text(size=flabsize, colour=flabcol), 
                strip.background=element_rect(colour=flabbackcol, fill=flabbackcol), 
                plot.margin=unit(fmar, "lines"))
        
        p1 <- ggplot_gtable(ggplot_build(p))
        p1$layout$clip[p1$layout$name == "panel"] <- "off"
        
        
        if (imgtype == "png") png(paste(fname, ".png", sep=""), height=height1, width=width1, res=dpi, units=units)
        if (imgtype == "jpeg") jpeg(paste(fname, ".jpg", sep=""), height=height1, width=width1, res=dpi, units=units, quality=100)
        if (imgtype == "pdf") pdf(paste(fname, ".pdf", sep=""), height=height1, width=width1)
        
        grid.draw(p1)
        dev.off()
        
        if (imgtype == "png") cat(paste(fname, ".png exported\n", sep=""))
        if (imgtype == "jpeg") cat(paste(fname, ".jpg exported\n", sep=""))
        if (imgtype == "pdf") cat(paste(fname, ".pdf exported\n", sep=""))
      }
    }
  }
  
  #sj joined plots
  if (imgoutput == "join")
  {
    #checks
    if (flen < 2) stop("Joined plot not processed. Number of selected files less than 2")
    
    chk <- grep("CLUSTERING PROBABILITIES", toupper(readLines(files[1], warn=FALSE))[1])
    if (length(chk) !=0 ) tempdf <- tabulateRunsTess(files=files, quiet=TRUE)
    #read TESS files
    chk1 <- grep("STRUCTURE", toupper(readLines(files[1], warn=FALSE))[4])
    if (length(chk1) !=0 ) tempdf <- tabulateRunsStructure(files=files, quiet=TRUE)
    if (length(chk) == 0 & length(chk1) == 0) stop("Incorrect input file type")
    
    if (all(tempdf$ind[1] != tempdf$ind)) stop("Joined plot not processed. Number of individuals differ between selected runs")
    Ind <- tempdf$ind[1]
    
    #loop to process selected files
    plist <- list()
    facetnames <- list()
    kvec <- vector()
    for (i in 1:flen)
    {
      #read files
      chk <- grep("CLUSTERING PROBABILITIES", toupper(readLines(files[i], warn=FALSE))[1])
      if (length(chk) != 0) df1 <- runsToDfTess(files=files[i])
      #read TESS files
      chk1 <- grep("STRUCTURE", toupper(readLines(files[i], warn=FALSE))[4])
      if (length(chk1) != 0) df1 <- runsToDfStructure(files=files[i])
      if (length(chk) == 0 & length(chk1) == 0) stop("Incorrect input file type")
      
      k <- ncol(df1)
      df1$Ind <- factor(1:nrow(df1))
      df1$Num <- factor(rep(i, nrow(df1)))
      facetnames[[i]] <- paste(sub(".txt", "", basename(files[i])), "\n", "K=", k, sep="")
      kvec <- c(kvec, k)
      plist[[i]] <- df1
    }
    
    #combine list to one dataframe 
    df2 <- rbind.fill(plist)
    #melt
    df3 <- melt(df2, id.var=c("Ind", "Num"))
    
    #get Dim
    pnum <- flen
    #if (!all(is.na(poplab))) pnum <- flen+1
    height1 <- getDim(ind=Ind, height=height, width=width, res=dpi, units=units, plotnum=pnum)[1]
    width1 <- getDim(ind=Ind, height=height, width=width, res=dpi, units=units, plotnum=pnum)[2]
    #Get Col
    coll <- popcol
    if (all(is.na(popcol))) coll <- getColours(max(kvec))
    if (length(coll)!=max(kvec)) stop("Number of colours not equal to number of populations")
    
    
    #save plot
    dt <- gsub(":", "", as.character(format(Sys.time(), "%H:%M:%S")))
    
    if (all(is.na(poplab)))
    {
      #ggplot
      p <- ggplot(data=df3, aes(x=Ind, y=value, fill=variable))+
        geom_bar(width=1, space=0, stat="identity", position="stack")+
        scale_x_discrete(expand=c(0, 0))+
        scale_y_continuous(expand=c(0, 0))+
        scale_fill_manual(values=coll)+
        facet_grid(Num~., labeller=plotlabeller)+
        labs(x=NULL, y=NULL)+
        theme(legend.position="none", panel.grid=element_blank(), panel.background=element_blank(), 
              axis.ticks=element_blank(), axis.text=element_blank(), axis.line=element_blank(), 
              axis.title=element_blank(), strip.text=element_text(size=flabsize, colour=flabcol), 
              strip.background=element_rect(colour=flabbackcol, fill=flabbackcol), 
              plot.margin=unit(c(0.1, 0.1, 0, 0), "cm"))
      
      if (imgtype == "png") ggsave(paste("Joined", flen, "Files-", dt, ".png", sep=""), p, height=height1, width=width1, dpi=dpi, units=units)
      if (imgtype == "jpeg") ggsave(paste("Joined", flen, "Files-", dt, ".jpg", sep=""), p, height=height1, width=width1, dpi=dpi, units=units, quality=100)
      if (imgtype == "pdf") ggsave(paste("Joined", flen, "Files-", dt, ".pdf", sep=""), p, height=height1, width=width1, units=units)
      
      if (imgtype == "png") cat(paste("Joined", flen, "Files-", dt, ".png exported\n", sep=""))
      if (imgtype == "jpeg") cat(paste("Joined", flen, "Files-", dt, ".jpg exported\n", sep=""))
      if (imgtype == "pdf") cat(paste("Joined", flen, "Files-", dt, ".pdf exported\n", sep=""))
    }
    
    if (!all(is.na(poplab)))
    {
      #plot with labels
      if (Ind!=length(as.character(poplab))) stop("Length of labels do not match number of individuals in input file.")
      
      df3 <- subset(df2, df2$Num == 1)
      df3$Num <- df3$Num[drop=TRUE]
      df3.1 <- melt(df3, id.var=c("Ind", "Num"))
      df4 <- subset(df2, df2$Num!=1)
      df4$Num <- df4$Num[drop=TRUE]
      df4.1 <- melt(df4, id.var=c("Ind", "Num"))
      
      ppar <- getPlotParams(poplab=poplab, plotnum=1, labpos=labpos, labsize=labsize, labangle=labangle, 
                            labjust=labjust, labcol=labcol,pointsize=pointsize, pointcol=pointcol, 
                            pointtype=pointtype, linepos=linepos, linethick=linethick, linecol=linecol, 
                            fmar=fmar)
      
      labangle <- ppar$labangle
      labjust <- ppar$labjust
      labpos <- ppar$labpos
      labsize <- ppar$labsize
      labjust <- ppar$labjust
      labcol <- ppar$labcol
      pointsize <- ppar$pointsize
      pointcol <- ppar$pointcol
      pointtype <- ppar$pointtype
      linepos <- ppar$linepos
      linethick <- ppar$linethick
      linecol <- ppar$linecol
      fmar <- ppar$fmar
      
      p <- ggplot()+
        geom_bar(data=df3.1, aes(x=Ind, y=value, fill=variable), width=1, space=0, stat="identity", position="stack")+
        scale_x_discrete(expand=c(0, 0))+
        scale_y_continuous(expand=c(0, 0))+
        scale_fill_manual(values=coll)+
        facet_grid(Num~., labeller=plotlabeller)+
        labs(x=NULL, y=NULL)+
        geom_text(data=lframe, aes_string(x="pos", y=labpos), label=labs, angle=labangle, hjust=labjust, size=labsize, colour="grey30")+
        geom_line(data=pos1, aes_string(x="pos", y=linepos), colour=linecol, size=linethick)+
        geom_point(data=pos1, aes_string(x="pos", y=linepos), size=pointsize, colour=pointcol, shape=pointtype)+
        theme(legend.position="none", panel.grid=element_blank(), panel.background=element_blank(), 
              axis.ticks=element_blank(), axis.text=element_blank(), axis.line=element_blank(), 
              axis.title=element_blank(), strip.text=element_text(size=flabsize, colour=flabcol), 
              strip.background=element_rect(colour=flabbackcol, fill=flabbackcol), 
              plot.margin=unit(fmar, "lines"))
      
      gp1 <- ggplot_gtable(ggplot_build(p))
      gp1$layout$clip[gp1$layout$name == "panel"] <- "off"
      
      #fix facet names. take out first and change all others to start from 1
      facetnames <- as.list(unlist(facetnames)[-1])
      
      p2 <- ggplot()+
        geom_bar(data=df4.1, aes(x=Ind, y=value, fill=variable), width=1, space=0, stat="identity", position="stack")+
        scale_x_discrete(expand=c(0, 0))+
        scale_y_continuous(expand=c(0, 0))+
        scale_fill_manual(values=coll)+
        facet_grid(Num~., labeller=plotlabeller)+
        labs(x=NULL, y=NULL)+
        theme(legend.position="none", panel.grid=element_blank(), panel.background=element_blank(), 
              axis.ticks=element_blank(), axis.text=element_blank(), axis.line=element_blank(), 
              axis.title=element_blank(), strip.text=element_text(size=flabsize, colour=flabcol), 
              strip.background=element_rect(colour=flabbackcol, fill=flabbackcol), 
              plot.margin=unit(c(ppar$fmar[1], ppar$fmar[2], 0.1, ppar$fmar[4]), "lines"))
      
      gp2 <- ggplot_gtable(ggplot_build(p2))
      maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
      gp1$widths[2:3] <- maxWidth
      gp2$widths[2:3] <- maxWidth
      
      if (imgtype == "png") png(paste("Joined", flen, "Files-", dt, ".png", sep=""), height=height1, width=width1, res=dpi, units=units)
      if (imgtype == "jpeg") jpeg(paste("Joined", flen, "Files-", dt, ".jpg", sep=""), height=height1, width=width1, res=dpi, units=units, quality=100)
      if (imgtype == "pdf") pdf(paste("Joined", flen, "Files-", dt, ".pdf", sep=""), height=height1, width=width1)
      
      grid.arrange(gp2, gp1, heights=c((flen-1)/flen, (1/flen)))
      dev.off()
      
      if (imgtype == "png") cat(paste("Joined", flen, "Files-", dt, ".png exported\n", sep=""))
      if (imgtype == "jpeg") cat(paste("Joined", flen, "Files-", dt, ".jpg exported\n", sep=""))
      if (imgtype == "pdf") cat(paste("Joined", flen, "Files-", dt, ".pdf exported\n", sep=""))
    }
  }
  
  #ta input tables
  if (imgoutput == "tab")
  {
    i=1
    for (i in 1:flen)
    {
      fname <- gsub(".txt", "", basename(files[i]))
      df1 <- read.table(files[i])
      if (class(df1)!="data.frame") stop("Incorrect input file type")
      
      Ind <- as.numeric(as.character(length(levels(df1[, 1]))))
      tempb <- as.numeric(nrow(df1))
      tempc <- as.numeric(tempb/Ind)
      tempd <- ncol(df1)-2
      
      df2 <- data.frame(Num=factor(rep(1:tempc, 1, each=Ind)), Ind=factor(rep(1:Ind, tempc)), df1[, 2:(tempd+1)])
      rm(df1)
      df3 <- melt(df2, id.var=c("Ind", "Num"))
      facetnames <- as.list(rep(paste("K=", tempd, sep=""), tempc))
      
      #get Dim
      #pnum <- tempc
      #if (!all(is.na(poplab))) pnum <- tempc+1
      height1=getDim(ind=Ind, height=height, width=width, res=dpi, units=units, plotnum=tempc)[1]
      width1=getDim(ind=Ind, height=height, width=width, res=dpi, units=units, plotnum=tempc)[2]
      #Get col
      coll <- popcol
      if (all(is.na(popcol))) coll <- getColours(tempd)
      if (length(coll)!=tempd) stop("Number of colours not equal to number of populations")
      
      
      #save plot
      dt <- gsub(":", "", as.character(format(Sys.time(), "%H:%M:%S")))
      
      if (all(is.na(poplab)))
      {
        #ggplot
        p <- ggplot(data=df3, aes(x=Ind, y=value, fill=variable))+
          geom_bar(width=1, space=0, stat="identity", position="stack")+
          scale_x_discrete(expand=c(0, 0))+
          scale_y_continuous(expand=c(0, 0))+
          scale_fill_manual(values=coll)+
          facet_grid(Num~., labeller=plotlabeller)+
          labs(x=NULL, y=NULL)+
          theme(legend.position="none", panel.grid=element_blank(), panel.background=element_blank(), 
                axis.ticks=element_blank(), axis.text=element_blank(), axis.line=element_blank(), 
                axis.title=element_blank(), strip.text=element_text(size=flabsize, colour=flabcol), 
                strip.background=element_rect(colour=flabbackcol, fill=flabbackcol), 
                plot.margin=unit(c(0.1, 0.1, 0, 0), "cm"))
        
        if (imgtype == "png") ggsave(paste(fname, ".png", sep=""), p, height=height1, width=width1, dpi=dpi, units=units)
        if (imgtype == "jpeg") ggsave(paste(fname, ".jpg", sep=""), p, height=height1, width=width1, dpi=dpi, units=units, quality=100)
        if (imgtype == "pdf") ggsave(paste(fname, ".pdf", sep=""), p, height=height1, width=width1, units=units)
        if (imgtype == "png") cat(paste(fname, ".png exported\n", sep=""))
        if (imgtype == "jpeg") cat(paste(fname, ".jpg exported\n", sep=""))
        if (imgtype == "pdf") cat(paste(fname, ".pdf exported\n", sep=""))
      }
      
      if (!all(is.na(poplab)))
      {
        #plot with labels
        if (Ind!=length(as.character(poplab))) stop("Length of labels do not match number of individuals in input file.")
        df3 <- subset(df2, df2$Num == 1)
        df3$Num <- df3$Num[drop=TRUE]
        df3.1 <- melt(df3, id.var=c("Ind", "Num"))
        
        ppar <- getPlotParams(poplab=poplab, plotnum=tempc, labpos=labpos, labsize=labsize, labangle=labangle, 
                              labjust=labjust, labcol=labcol,pointsize=pointsize, pointcol=pointcol, 
                              pointtype=pointtype, linepos=linepos, linethick=linethick, linecol=linecol, 
                              fmar=fmar)
        
        labangle <- ppar$labangle
        labjust <- ppar$labjust
        labpos <- ppar$labpos
        labsize <- ppar$labsize
        labjust <- ppar$labjust
        labcol <- ppar$labcol
        pointsize <- ppar$pointsize
        pointcol <- ppar$pointcol
        pointtype <- ppar$pointtype
        linepos <- ppar$linepos
        linethick <- ppar$linethick
        linecol <- ppar$linecol
        fmar <- ppar$fmar
        
        p <- ggplot()+
          geom_bar(data=df3.1, aes(x=Ind, y=value, fill=variable), width=1, space=0, stat="identity", position="stack")+
          scale_x_discrete(expand=c(0, 0))+
          scale_y_continuous(expand=c(0, 0))+
          scale_fill_manual(values=coll)+
          facet_grid(Num~., labeller=plotlabeller)+
          labs(x=NULL, y=NULL)+
          geom_text(data=lframe, aes_string(x="pos", y=labpos), label=labs, angle=labangle, hjust=labjust, size=labsize, colour="grey30")+
          geom_line(data=pos1, aes_string(x="pos", y=linepos), colour=linecol, size=linethick)+
          geom_point(data=pos1, aes_string(x="pos", y=linepos), size=pointsize, colour=pointcol, shape=pointtype)+
          theme(legend.position="none", panel.grid=element_blank(), panel.background=element_blank(), 
                axis.ticks=element_blank(), axis.text=element_blank(), axis.line=element_blank(), 
                axis.title=element_blank(), strip.text=element_text(size=flabsize, colour=flabcol), 
                strip.background=element_rect(colour=flabbackcol, fill=flabbackcol), 
                plot.margin=unit(fmar, "lines"))
        
        gp1 <- ggplot_gtable(ggplot_build(p))
        gp1$layout$clip[gp1$layout$name == "panel"] <- "off"
        
        if (tempc>1)
        {
          df4 <- subset(df2, df2$Num!=1)
          df4$Num <- df4$Num[drop=TRUE]
          df4.1 <- melt(df4, id.var=c("Ind", "Num"))
          
          p2 <- ggplot()+
            geom_bar(data=df4.1, aes(x=Ind, y=value, fill=variable), width=1, space=0, stat="identity", position="stack")+
            scale_x_discrete(expand=c(0, 0))+
            scale_y_continuous(expand=c(0, 0))+
            scale_fill_manual(values=coll)+
            facet_grid(Num~., labeller=plotlabeller)+
            labs(x=NULL, y=NULL)+
            theme(legend.position="none", panel.grid=element_blank(), panel.background=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank(), axis.line=element_blank(), 
                  axis.title=element_blank(), strip.text=element_text(size=flabsize, colour=flabcol), 
                  strip.background=element_rect(colour=flabbackcol, fill=flabbackcol), 
                  plot.margin=unit(c(0.2, ppar$fmar[2], 0, ppar$fmar[4]), "lines"))
          
          gp2 <- ggplot_gtable(ggplot_build(p2))
          maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
          gp1$widths[2:3] <- maxWidth
          gp2$widths[2:3] <- maxWidth 
        }
        
        if (imgtype == "png") png(paste(fname, ".png", sep=""), height=height1, width=width1, res=dpi, units=units)
        if (imgtype == "jpeg") jpeg(paste(fname, ".jpg", sep=""), height=height1, width=width1, res=dpi, units=units, quality=100)
        if (imgtype == "pdf") pdf(paste(fname, ".pdf", sep=""), height=height1, width=width1)
        
        if (tempc == 1) grid.draw(gp1)
        if (tempc > 1) grid.arrange(gp2, gp1, heights=c(((tempc-1)/tempc)-0.08, (1/tempc)+0.08))
        #grid.arrange(gp2,gp1, heights=c(0.6,0.4))
        dev.off()
        
        if (imgtype == "png") cat(paste(fname, ".png exported\n", sep=""))
        if (imgtype == "jpeg") cat(paste(fname, ".jpg exported\n", sep=""))
        if (imgtype == "pdf") cat(paste(fname, ".pdf exported\n", sep=""))
      }
      
    } 
  }
}

# FUNCTION collectRunsTess
#' Collect TESS cluster run files from multiple folders
#' @description Collect TESS cluster run files from multiple folders to one folder and rename by run
#' @param runsdir The directory containing TESS runs in multiple directories. Use \code{choose.dir()} for interactively selecting the directory. If NA, or no directory is selected, the current working directory is used.
#' @param newdir The name of the new directory to be created with the collected runs. IF NA, the default name 'AllTESSRuns' is used. 
#' @param quiet Set to FALSE by default. Prints directories without TESS runs and number of runs copied and renamed. 
#' @details Within each TESS run folder, the function searches for filename ending with 'TR.txt' as the cluster file. This file is copied to the new folder and renamed as the name of the respective run directory. Therefore, do not manually rename original run files or directories.
#' @return Two integers are ruturned. The first denotes the number of TESS run files copied and renamed. The second number denotes number of directories without TESS run files.
#' @export
#' 
collectRunsTess <- function(runsdir=NA, newdir=NA, quiet=FALSE)
{
  quiet <- toupper(quiet)
  currwd <- getwd()
  if (is.na(newdir)) newdir <- "AllTESSRuns"
  if (is.na(runsdir)) runsdir <- getwd()
  dirs <- list.dirs(path=runsdir, full.names=TRUE, recursive=FALSE)
  dir.create(paste(runsdir, "/", newdir, sep=""))
  k <- 0
  l <- 0
  for (i in 1:length(dirs))
  {
    setwd(dirs[i])
    files <- list.files()
    sel <- grep("\\w+TR.txt", files)
    if (length(sel) == 0) 
    {
      if (quiet == FALSE | quiet == "F" | quiet == "FALSE") cat("No TESS cluster file found in directory: ", basename(dirs[i]), "\n", sep="")
      l=l+1
    }
    if (length(sel) != 0) 
    {
      file.copy(from=paste(dirs[i], "/", files[sel], sep=""), to=paste(runsdir, "/", newdir, sep="")) 
      file.rename(from=paste(runsdir, "/", newdir, "/", files[sel], sep=""), to=paste(runsdir, "/", newdir, "/", basename(dirs[i]), ".txt", sep=""))
      k=k+1  
    }
  }
  setwd(currwd)
  if (quiet == FALSE | quiet == "F" | quiet == "FALSE") cat(paste(k, " TESS cluster files copied and renamed.\n"))
  return(c(k, l))
}

# FUNCTION collectClumppOutput
#' Collect CLUMPP output files from multiple folders
#' @description Collect CLUMPP output files from multiple folders to one folder
#' @param prefix The prefix of the CLUMPP directories before the underscore. For ex. if the directories are STRUCTUREpop_K2, then prefix is STRUCTUREpop.
#' @param filetype the type of file to be copied. Options are 'aligned' to copy aligned files only, 'merged' to copy merged files only and 'both' to copy both files.
#' @param runsdir The directory containing CLUMPP output files in multiple directories. Use \code{choose.dir()} for interactively selecting the directory. If NA, the current working directory is used.
#' @param newdir The name of the new directory to be created with the collected runs. IF NA, the a directory name joining prefix and filetype is created. 
#' @param quiet Set to FALSE by default. Prints number of folders processed and number of files processed. 
#' @details Within each CLUMPP output folder, the function searches for filenames containing combination of prefix and filetype. This file is copied to the new folder. Therefore, do not manually rename CLUMPP output files or output directories.
#' @return Two integers are ruturned. The first denotes the number of directories processed. The second number denotes the number files copied.
#' @export
#' 
collectClumppOutput <- function(prefix="STRUCTUREpop", filetype="aligned", runsdir=NA, newdir=NA, quiet=FALSE)
{
  #check imgoutput
  if (tolower(filetype)!="aligned" && tolower(filetype)!="merged" && tolower(filetype)!="both") stop("Argument 'filetype' set incorrectly. Set as 'aligned', 'merged' or 'both'.")
  quiet <- toupper(quiet)
  
  currwd <- getwd()
  if (is.na(newdir)) newdir <- paste(prefix, "-", filetype, sep="")
  if (is.na(runsdir)) runsdir <- getwd()
  dirs <- list.dirs(path=runsdir, full.names=TRUE, recursive=FALSE)
  dirs1 <- dirs[grep(paste(prefix, "_", sep=""), dirs)]
  dir.create(paste(runsdir, "/", newdir, sep=""))
  k <- 0
  l <- 0
  i=1
  for (i in 1:length(dirs1))
  {
    setwd(dirs1[i])
    files <- list.files()
    sel1 <- grep("aligned", files)
    sel2 <- grep("merged", files)
    if (tolower(filetype) == "aligned") sel3 <- sel1
    if (tolower(filetype) == "merged") sel3 <- sel2
    if (tolower(filetype) == "both") sel3 <- c(sel1, sel2)
    if (length(sel3) == 0) 
    {
      cat("No suitable file found in directory: ", basename(dirs1[i]), "\n", sep="")
    }
    if (length(sel3) != 0) 
    {
      file.copy(from=paste(dirs1[i], "/", files[sel3], sep=""), to=paste(runsdir, "/", newdir, sep="")) 
      k=k+1
      l=l+length(sel3)
    }
  }
  setwd(currwd)
  if (quiet == FALSE | quiet == "F" | quiet == "FALSE") cat(paste("Directories processed: ", k, "\n Files copied: ", l, "\n"))
  return(c(k, l))
}

# FUNCTION plotMultiline
#' Plot STRUCTURE/TESS/table run files in multiline
#' @description Plot STRUCTURE/TESS run files as barplots with multiple lines
#' @param files One or more STRUCTURE output files. Use \code{choose.files(multi=TRUE)} for interactive selection.
#' @param spl samples per line. Defaults to 60.
#' @param lpp lines per page. Defaults to 11.
#' @param popcol A vector of colours for populations.
#' @param barwidth The width of the bars.
#' @param barspace The space between the bars.
#' @param indlabs By default, \code{indlabs=TRUE}, then individual labels 1, 2, 3.. are indicated below bars. To hide labels, set \code{indlabs=FALSE}
#' @param labsize The size of the labels.
#' @param labangle The angle of labels.
#' @param labvjust The vertical justification of the labels.
#' @param labhjust The horizontal justification of the labels.
#' @param imgtype Figure output format. Options are 'png', 'jpeg' or 'pdf'. If pdf, height and width must be in inches and res argument is ignored.
#' @param height Height of the full figure. If NA, height is set to 29.7cm (A4 height).
#' @param width Width of the full figure. If NA, width is set to 21cm (A4 width).
#' @param res Resolution of the figure. Default is 300.
#' @param units Units of dimension of the figure. Default is cm.
#' @details Figures are always created to A4 size. Any plotted row will span the width of the figure. Note that this function is slow and may take several minutes when plotting mutiple tables.
#' @export
#'
plotMultiline <- function(files=NA, spl=NA, lpp=NA, popcol=NA, barwidth=0.9, barspace=0.1, indlabs=TRUE, labsize=5, labangle=90, labvjust=0.5,labhjust=1, imgtype="png", height=NA, width=NA, res=NA, units=NA)
{
  #check image
  imgtype <- tolower(imgtype)
  indlabs <- toupper(indlabs)
  if (imgtype != "png" && imgtype != "pdf" && imgtype != "jpeg" ) stop("Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg' or 'pdf'.")
  
  #set NA values
  if (is.na (height)) {height <- 29.7; if (imgtype == "pdf") height <- round(height*0.3937,2)}
  if (is.na (width)) {width <- 21; if (imgtype == "pdf") width <- round(width*0.3937,2)}
  if (is.na (res)) res <- 300
  if (is.na (units)) units <- "cm"
  
  for (i in 1:length(files))
  {
    fname <- gsub(".txt", "", basename(files[i]))
    #read STRUCTURE file
    chk <- grep("CLUSTERING PROBABILITIES", toupper(readLines(files[i], warn=FALSE))[1])
    if (length(chk) != 0) df1 <- runsToDfTess(files=files[i])
    #read TESS file
    chk1 <- grep("STRUCTURE", toupper(readLines(files[i], warn=FALSE))[4])
    if (length(chk1) != 0) df1 <- runsToDfStructure(files=files[i])
    #read TAB files
    if (length(chk) == 0 & length(chk1) == 0) 
    {
      df1 <- read.table(files[i])
      if (class(df1) != "data.frame") stop("Incorrect input file type")
      nrow1 <- length(df1$V1)/length(levels(factor(as.character(df1$V1))))
      df1$tab <- rep(1:nrow1, each=length(levels(factor(as.character(df1$V1)))))
      df1 <- df1[, -c(1, length(df1)-1)]
      colnames(df1)[1:length(df1)-1] <- paste("Cluster", 1:(length(df1)-1), sep="")
      df1 <- split(df1[, -length(df1)], df1$tab)
    }
    
    #determine if df1 is list or dataframe
    if (as.character(class(df1)) == "data.frame") flen <- 1
    if (as.character(class(df1)) == "list") flen <- length(df1)
    
    for (j in 1:flen)
    {
      #move to dff
      if (as.character(class(df1)) == "data.frame") dff <- df1
      if (as.character(class(df1)) == "list") dff <- df1[[j]]
      
      #primary calculation of spl
      nr1 <- nrow(dff)

      #numrows <- floor(nr1/spl)
      #numextra <- nr1-(spl*numrows)
      #nr2 <- numrows
      #if (numextra > 0) nr2 <- nr2+1
      
      if (!is.na(spl))
      {
        if (spl > nr1) stop("Samples per line (spl) is greater than total number of samples")
        spl1<-spl
        numrows <- floor(nr1/spl1)
        numextra <- nr1-(spl1*numrows)
      }
      
      #optimise spl
      if (is.na(spl))
      {
        if (nr1 <= 60) {spl1 <- nr1} else {spl1 <- 60}
        
        #automatically optimise number of rows and spl
        numextra <- 0
        while(numextra < 0.70*spl1)
        {
          numrows <- floor(nr1/spl1)
          numextra <- nr1-(spl1*numrows)
          if (numextra < 0.70*spl1) spl1=spl1+1
          if (spl1 > nr1) {spl1 <- nr1; break;}
        }
      }
     
      nr2 <- numrows
      if (numextra > 0) nr2 <- nr2+1
      
      #get colours
      coll <- popcol
      if (is.na(popcol)) coll <- getColours(ncol(dff))
      
      dff$rows <- factor(c(rep(1:numrows, each=spl1), rep(nr2, each=numextra)))
      dff$ind <- as.factor(as.numeric(1:nr1))
      #dff$line <- as.factor(c(rep(1:spl1, numrows), 1:numextra))
      
      #split and plot rows
      dlist <- split(dff, dff$rows)
      plist <- list(length=nr2)
      #widthsvec <- vector(length=nr2)
      for (i in 1: nr2)
      {
        if (indlabs == TRUE)
        {
          df2 <- melt(dlist[[i]], id.var=c("ind", "rows"))
          plist[[i]] <- ggplot(data=df2, aes(x=ind, y=value, fill=variable))+
            geom_bar(width=barwidth, space=barspace, stat="identity", position="stack")+
            scale_x_discrete(expand=c(0, 0))+
            scale_y_continuous(expand=c(0, 0))+
            scale_fill_manual(values=coll)+
            labs(x=NULL, y=NULL)+
            theme(legend.position="none", panel.grid=element_blank(), panel.background=element_blank(), 
                  axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), 
                  axis.title=element_blank(), axis.text.x=element_text(size=labsize, angle=labangle, 
                  vjust=labvjust,hjust=labhjust), plot.margin=unit(c(0.1, 0.1, 0.1, 0), "cm"))
        }

        if (indlabs == FALSE)
        {
          df2 <- melt(dlist[[i]], id.var=c("ind", "rows"))
          plist[[i]] <- ggplot(data=df2, aes(x=ind, y=value, fill=variable))+
            geom_bar(width=barwidth, space=barspace, stat="identity", position="stack")+
            scale_x_discrete(expand=c(0, 0))+
            scale_y_continuous(expand=c(0, 0))+
            scale_fill_manual(values=coll)+
            labs(x=NULL, y=NULL)+
            theme(legend.position="none", panel.grid=element_blank(), panel.background=element_blank(), 
                  axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.line=element_blank(), 
                  axis.title=element_blank(), axis.text.x=element_blank(),
                  plot.margin=unit(c(0.1, 0.1, 0.1, 0), "cm"))
        }
        
        #calculate widths. not implemented.
        #widthsvec[i] <- nrow(dlist[[i]])/spl1
      }
      
      #lpp calculations
      if (!is.na(lpp)) lpp1 <- lpp
      if (is.na(lpp)) {lpp1 <- 11; if (lpp1 > nr2) lpp1 <- nr2}
      numpages <- ceiling(nr2/lpp1)
      #numpagesextra <- nr2-(lpp*numpages)
      #numpages1 <- numpages
      #if (numpagesextra > 0) numpages1 <- numpages1+1
      
      e=0
      r=1
      while (r <= numpages)
      {
        start1 <- e+1
        stop1 <- e+lpp1
        if (stop1 > length(plist)) stop1 <- length(plist)
        
        #widths <- widthsvec[start1:stop1]
        alist <- c(plist[start1:stop1], lpp1, 1)
        names(alist) <- c(as.character(start1:stop1), "nrow", "ncol")
        
        if (imgtype == "png") png(paste(fname, "-Multiline-", j, "-", r, ".png", sep=""), height=height, width=width, res=res, units=units)
        if (imgtype == "jpeg") jpeg(paste(fname, "-Multiline-", j, "-", r, ".jpg", sep=""), height=height, width=width, res=res, units=units, quality=100)
        if (imgtype == "pdf") pdf(paste(fname, "-Multiline-", j, "-", r, ".pdf", sep=""), height=height, width=width)
        
        do.call(grid.arrange, alist)
        #grid.arrange(arrangeGrob(plist[start1:stop1]))
        #grid.arrange(plist[[1]], plist[[2]], nrow=2, widths=c(0.4, 0.6))
        #do.call(fn1, plist[[1]])
        dev.off()
        
        if (imgtype == "png") cat(paste(fname, "-Multiline-", j, "-", r, ".png exported\n", sep=""))
        if (imgtype == "jpeg") cat(paste(fname, "-Multiline-", j, "-", r, ".jpg exported\n", sep=""))
        if (imgtype == "pdf") cat(paste(fname, "-Multiline-", j, "-", r, ".pdf exported\n", sep=""))
        
        e=stop1
        r=r+1
      }
      rm(nr1,nr2,numrows,numextra,numpages,start1,stop1,e,r,dlist,plist,df2,dff)
    }
  }
}

# New concepts
# Option to have custom labels in plotMultiline

cat("pophelper v1.0.0 loaded\n")
