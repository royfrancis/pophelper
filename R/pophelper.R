# getColours -------------------------------------------------------------------

#' @title Internal: Generate colours based on number of K
#' @description Internal: Generate colours based on number of K.
#' @param k A numeric indicating the number of colours required
#' @return Returns a character vector of k colours in hexadecimal format
#' @details Colours 1 to 12 are custom unique colours. Colours beyond 15 are 
#' generated from colour ramp \code{rich.colors()} from package \code{gplots}.
#' @noRd
#' @keywords internal
#' 
getColors <- getColours <- function(k)
{
  if(length(k) > 1) stop("getColours: Input has more than one value. Argument k must be a single numeric or integer.")
  if(!is.integer(k) && !is.numeric(k) ) stop("getColours: Input is not an integer. Argument k must be a single numeric or integer.")
  k <- as.integer(k)
  # standard colours
  col1 <- c("#2121D9","#9999FF","#DF0101","#04B404","#FFFB23","#FF9326","#A945FF","#0089B2","#B26314","#610B5E","#FE2E9A","#BFF217")
  # col1 <- c("#1D72F5","#DF0101","#77CE61", 
  #           "#FF9326","#A945FF","#0089B2",
  #           "#FDF060","#FFA6B2","#BFF217",
  #           "#60D5FD","#CC1577","#F2B950",
  #           "#7FB21D","#EC496F","#326397",
  #           "#B26314","#027368","#A4A4A4",
  #           "#610B5E")
  if(k <= 12) return(col1[1:k])
  if(k > 12) 
  {
    cr <- colorRampPalette(colors=c("#000040FF","#00004FFF","#000060FF","#000074FF","#000088FF","#00009DFF","#0000B2FF",
                                    "#0000C6FF","#000CD8FF","#0022E7FF","#0037F3FF","#004BFBFF","#005EFFFF","#0070FEFF",
                                    "#0081F8FF","#0091EEFF","#00A0E0FF","#00ADCFFF","#00BABCFF","#00C6A7FF","#01D092FF",
                                    "#02DA7EFF","#03E26AFF","#07E958FF","#0EF047FF","#1BF539FF","#31F92CFF","#54FC22FF",
                                    "#80FE1AFF","#ABFF13FF","#CEFF0EFF","#E4FE0AFF","#F1FB07FF","#F8F805FF","#FCF403FF",
                                    "#FDEE02FF","#FEE801FF","#FFE001FF","#FFD801FF","#FFCE00FF","#FFC300FF","#FFB800FF",
                                    "#FFAB00FF","#FF9D00FF","#FF8E00FF","#FF7E00FF","#FF6D00FF","#FF5B00FF","#FF4700FF",
                                    "#FF3300FF"),space="rgb")
    return(cr(k))
  }
}

# verifyGrplab --------------------------------------------------------------------

#' @title Verify a grplab dataframe
#' @description Verify if a grplab dataframe is formatted correctly.
#' @param grplab A dataframe with character fields.
#' @return Nothing.
#' @export
#' 
verifyGrplab <- function(grplab=NULL)
{
  if(is.null(grplab)) stop("verifyGrplab: Argument 'grplab' is empty.")
  
  # is it a dataframe?
  if(!is.data.frame(grplab)) stop("verifyGrplab: Argument 'grplab' is not a data.frame object.")

  # are there NAs in labels?
  if(any(sapply(grplab,is.na))) stop("verifyGrplab: Argument 'grplab' contains NAs.")

  # are all elements character datatype?
  if(!any(sapply(grplab,is.character))) stop("verifyGrplab: Argument 'grplab' contains one or more fields which are not character datatype.")
}

# unitConverter ----------------------------------------------------------------

#' @title Internal: Convert value between dimension units
#' @description Internal: Convert value between dimension units
#' @param value A numeric value or numeric vector to convert
#' @param fromunit A character indicating the current unit of the value. 
#' Options are "cm", "mm", "in" or "px".
#' @param tounit A character indicating the unit to change to. Options are 
#' "cm", "mm", "in" or "px".
#' @param dpi A numeric indicating the resolution for pixel conversion. This 
#' should be in PPI (pixels per inch).
#' @return Returns a numeric value or numeric vector in changed units.
#' @noRd
#' @keywords internal
#' 
unitConverter <- function(value=NA,fromunit=NA,tounit=NA,dpi=NA)
{
  # check
  if(all(is.na(value))) stop("unitConverter: Argument value is empty.")
  if(is.na(fromunit)) stop("unitConverter: Argument fromunit is empty.")
  if(is.na(tounit)) stop("unitConverter: Argument tounit is empty.")
  
  if(fromunit=="cm")
  {
    if(tounit=="cm") outvalue <- value
    if(tounit=="mm") outvalue <- round(value*10,2)
    if(tounit=="in") outvalue <- round(value*0.3937,2)
    if(tounit=="px")
    {
      if(is.na(dpi)) stop("unitConverter: Argument dpi is empty.")
      #convert dpi to 1 cm
      pxpercm <- dpi/2.54
      outvalue <- round(pxpercm*value,0)
    }
  }
  
  if(fromunit=="mm")
  {
    if(tounit=="mm") outvalue <- value
    if(tounit=="cm") outvalue <- round(value/10,2)
    if(tounit=="in") outvalue <- round(value*0.03937,2)
    if(tounit=="px")
    {
      if(is.na(dpi)) stop("unitConverter: Argument dpi is empty.")
      #convert dpi to 1 mm
      pxpermm <- dpi/25.4
      outvalue <- round(pxpermm*value,0)
    }
  }
  
  if(fromunit=="in")
  {
    if(tounit=="in") outvalue <- value
    if(tounit=="cm") outvalue <- round(value*2.54,2)
    if(tounit=="mm") outvalue <- round(value*0.254,2)
    if(tounit=="px")
    {
      if(is.na(dpi)) stop("unitConverter: Argument dpi is empty.")
      outvalue <- round(dpi*value,0)
    }
  }
  
  if(fromunit=="px")
  {
    if(tounit=="px") outvalue <- value
    if(is.na(dpi)) stop("unitConverter: Argument dpi is empty.")
    
    if(tounit=="cm")
    {
      pxpercm <- dpi/2.54
      outvalue <- value/pxpercm
    }
    
    if(tounit=="mm")
    {
      pxpermm <- dpi/25.4
      outvalue <- value/pxpermm
    }
    
    if(tounit=="in") outvalue <- value/dpi
    
  }
  
  return(outvalue)
}


# tabulateQ --------------------------------------------------------------------

#' @title Tabulate runs from a qlist
#' @description Takes a qlist of one of more numeric dataframes and creates a 
#' table with filenames, K and number of individuals.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param writetable A logical indicating if the output table must be exported 
#' as a tab-delimited text file in the working directory.
#' @param sorttable A logical indicating if output table is to be sorted. Sorts 
#' table by ind and K.
#' @param exportpath A path to where content must be exported. For example,
#' \code{"./dir/anotherdir/"}. Defaults to NULL which means current working
#' directory.
#' @return Returns a dataframe with filenames (if list is not named, then 
#' sample1, sample2 etc. is used), K and number of individuals of all runs 
#' sorted by ind and K (if \code{sorttable=TRUE}). The row numbers of the output 
#' table denotes the file number selected. This is helpful if a particular file 
#' from the table needs to be identified in the selection vector. If input files 
#' come from STRUCTURE runs, columns loci, burnin, reps, elpd, mvll and vll are 
#' also returned. In input files come from TESS3, columns loci, gif, rmse, 
#' crossentropy and ploidy are included as well.
#' @details The input must be a list of dataframes. If one dataframe is used, 
#' then it must be inside a list. If the list items are named, then the item 
#' name is used as filename, else sample1, sample2 etc. is used.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @seealso \code{\link{summariseQ}}
#' @examples 
#' 
#' # STRUCTURE files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' slist <- readQ(sfiles)
#' tabulateQ(qlist=slist)
#' 
#' # TESS files
#' tfiles <- list.files(path=system.file("files/tess",package="pophelper"),
#' full.names=TRUE)
#' tlist <- readQ(tfiles)
#' tabulateQ(qlist=tlist)
#' 
#' # ADMIXTURE files
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),
#' full.names=TRUE)
#' alist <- readQ(afiles)
#' tabulateQ(qlist=alist)
#' 
#  @import xlsx
#' @export
#'
tabulateQ <- function(qlist=NULL,writetable=FALSE,sorttable=TRUE,exportpath=NULL)
{
  
  # check input
  is.qlist(qlist)
  if(!is.logical(writetable)) stop("tabulateQ: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(sorttable)) stop("tabulateQ: Argument 'sorttable' not set correctly. Set as TRUE or FALSE.")
  
  # get filenames from selection
  filenames <- names(qlist)
  if(is.null(filenames)) filenames <- paste0("sample",1:length(qlist))
  #number of files selected
  flen <- length(filenames)
  
  # make dataframe container
  main <- data.frame(file=filenames,k=1:flen,ind=1:flen,stringsAsFactors=FALSE)
  
  # loop to make dataframe with filenames and other variables
  # initialise variables
  tq_k <- vector(length=flen,mode="numeric")
  tq_ind <- vector(length=flen,mode="numeric")
  tq_loci <- vector(length=flen,mode="numeric")
  tq_burnin <- vector(length=flen,mode="numeric")
  tq_reps <- vector(length=flen,mode="numeric")
  tq_elpd <- vector(length=flen,mode="numeric")
  tq_mvll <- vector(length=flen,mode="numeric")
  tq_vll <- vector(length=flen,mode="numeric")
  tq_gif <- vector(length=flen,mode="numeric")
  tq_rmse <- vector(length=flen,mode="numeric")
  tq_crossentropy <- vector(length=flen,mode="numeric")
  tq_ploidy <- vector(length=flen,mode="numeric")
  
  for (i in seq_along(qlist))
  {
    # read file & error check
    df1 <- qlist[[i]]
    if(!is.data.frame(df1)) stop(paste0("tabulateQ: List item ",i," is not a data.frame object."))
    if(!any(sapply(df1,is.numeric))) stop(paste0("tabulateQ: List item ",i," has non-numeric columns."))
    
    # get k
    tq_k[i] <- ncol(df1)
    # get ind
    tq_ind[i] <- nrow(df1)
    # loci
    tq_loci[i] <- ifelse(is.null(attr(df1,"loci")),NA,attr(df1,"loci"))
    # burnin
    tq_burnin[i] <- ifelse(is.null(attr(df1,"burnin")),NA,attr(df1,"burnin"))
    # reps
    tq_reps[i] <- ifelse(is.null(attr(df1,"reps")),NA,attr(df1,"reps"))
    # elpd
    tq_elpd[i] <- ifelse(is.null(attr(df1,"elpd")),NA,attr(df1,"elpd"))
    # mvll
    tq_mvll[i] <- ifelse(is.null(attr(df1,"mvll")),NA,attr(df1,"mvll"))
    # vll
    tq_vll[i] <- ifelse(is.null(attr(df1,"vll")),NA,attr(df1,"vll"))
    # gif
    tq_gif[i] <- ifelse(is.null(attr(df1,"gif")),NA,attr(df1,"gif"))
    # rmse
    tq_rmse[i] <- ifelse(is.null(attr(df1,"rmse")),NA,attr(df1,"rmse"))
    # crossentropy
    tq_crossentropy[i] <- ifelse(is.null(attr(df1,"crossentropy")),NA,attr(df1,"crossentropy"))
    # ploidy
    tq_ploidy[i] <- ifelse(is.null(attr(df1,"ploidy")),NA,attr(df1,"ploidy"))
  }
  
  # create dataframe
  main <- data.frame(file=filenames,k=tq_k,ind=tq_ind,stringsAsFactors=FALSE)
  if(all(!is.na(tq_loci))) main$loci <- tq_loci
  if(all(!is.na(tq_burnin))) main$burnin <- tq_burnin
  if(all(!is.na(tq_reps))) main$reps <- tq_reps
  if(all(!is.na(tq_elpd))) main$elpd <- tq_elpd
  if(all(!is.na(tq_mvll))) main$mvll <- tq_mvll
  if(all(!is.na(tq_vll))) main$vll <- tq_vll
  if(all(!is.na(tq_gif))) main$gif <- tq_gif
  if(all(!is.na(tq_rmse))) main$rmse <- tq_rmse
  if(all(!is.na(tq_crossentropy))) main$crossentropy <- tq_crossentropy
  if(all(!is.na(tq_ploidy))) main$ploidy <- tq_ploidy
  
  # sort table on K
  if(sorttable) main <- main[with(main,order(ind,k)),]
  
  # write table if opted
  if(writetable) write.table(main,paste0(exportpath,"tabulateQ.txt"),quote=FALSE,row.names=FALSE,sep="\t",dec=".")

  return(main)
}

# summariseQ ----------------------------------------------------------------

#' @title Summarise a tabulated dataframe
#' @description Creates a summary table from a tabulated dataframe of two or 
#' more runs with k, number of runs and individuals.
#' @param data A dataframe with tabulated runs. An output from \code{tabulateQ()}. 
#' Must have minimum 2 columns named k and ind.
#' @param writetable A logical indicating if the output table is to be exported 
#' as a tab-delimited text file in the working directory.
#' @param exportpath A path to where content must be exported. For example,
#' \code{"./dir/anotherdir/"}. Defaults to NULL which means current working
#' directory.
#' @return Returns a dataframe with all values of K sorted by K. The table has 
#' 3 columns namely value of K, number of runs for each K and number of 
#' individuals.
#' If the input file is derived from STRUCTURE runs, the table is sorted by loci 
#' as well. Other columns include elpdmean, elpdsd, elpdmin and elpdmax.
#' @details See the \href{http://royfrancis.github.io/pophelper/}{vignette} for 
#' more details.
#' @aliases summarizeQ
#' @seealso \code{\link{tabulateQ}}
#  @import xlsx
#' @examples 
#' 
#' # STRUCTURE files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' slist <- readQ(sfiles)
#' tr1 <- tabulateQ(slist)
#' summariseQ(tr1)
#' 
#' # ADMIXTURE files
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),
#' full.names=TRUE)
#' tr1 <- tabulateQ(readQ(afiles))
#' summariseQ(tr1)
#' 
#' @export
#' @aliases summarizeQ
#' 
summariseQ <- summarizeQ <- function(data=NULL,writetable=FALSE,exportpath=NULL)
{
  # does df data contain any data?
  if(is.null(data) || length(data)==0) stop("summariseQ: No input files.")
  if(!is.logical(writetable)) stop("summariseQ: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")

  # make sure dataframe
  if(class(data) != "data.frame") stop("summariseQ: Input is not a dataframe.")
  # convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  # is column k available?
  if(length(grep("k",colnames(data)))==0) stop("summariseQ: Column k not available.")
  # is column ind available?
  if(length(grep("ind",colnames(data)))==0) stop("summariseQ: Column ind not available.")
  
  # check
  #if(nrow(data) < 2) stop("summariseQ: At least 2 runs are required for this function.")
  
  if(all(c("k","ind","loci","elpd") %in% colnames(data)))
  {
    dframe1 <- stats::aggregate(elpd ~ loci + ind + k,data=data,length)
    colnames(dframe1)[4] <- "runs"
    dframe2 <- aggregate(elpd ~ loci + ind + k,data=data,FUN=function(x) c(elpdmean =mean(x,na.rm=TRUE),elpdsd=sd(x,na.rm=TRUE),elpdmin=min(x,na.rm=TRUE),elpdmax=max(x,na.rm=TRUE)))[,-c(1:3)]
    dframe1 <- cbind(dframe1,dframe2)
  }else{
    dframe1 <- stats::aggregate(file ~ ind + k,data=data[,c("file","k","ind")],length)
    colnames(dframe1)[3] <- "runs"
  }
  
  # write table if opted
  if(writetable) write.table(dframe1,paste0(exportpath,"summariseQ.txt"),quote=FALSE,row.names=FALSE,sep="\t",dec=".")
  
  return(dframe1)
}

# evannoMethodStructure --------------------------------------------------------

#' @title Perform the Evanno method for STRUCTURE runs.
#' @description The Evanno method for detecting the appropriate number of 
#' population clusters from STRUCTURE results. Creates table and figure with 
#' Evanno method derivatives. Refer to return for detailed list of columns. See 
#' details for Evanno method reference.
#' @param data A dataframe with summarised runs. An output from 
#' \code{summariseQ()} derived from STRUCTURE runs. Must have minimum 7 columns 
#' named elpdmean, elpdsd, k, runs, loci, elpdmax and elpdmin.
#' @param writetable A logical indicating if the output table is to be exported 
#' as a file in the working directory.
#' @param exportplot A logical indicating if the Evanno plots are to be exported 
#' as an image in the working directory. If Evanno method cannot be computed, a 
#' kPlot (elpd over k) is exported instead.
#' @param returndata A logical indicating if the data must be returned. A 
#' data.frame object is returned alone when \code{returnplot=FALSE}. When 
#' \code{returnplot=TRUE}, the data.frame object is returned in a list. See 'Value'.
#' @param returnplot A logical indicating if the plot must be returned. A 
#' gtable object is returned alone when \code{returndata=FALSE}. When 
#' \code{returndata=TRUE}, the gtable object is returned in a list. See 'Value'.
#' @param pointsize A numeric indicating size of points. Default for 
#' \code{basesize=6} is 1.8.
#' @param pointtype A character or number for the type of points. Defaults to 20. 
#' Same as pch in standard R.
#' @param pointcol A colour character for the colour of points. Defaults to 
#' "steelblue".
#' @param linesize A numeric indicating the thickness of the line. Default for 
#' \code{basesize=6} is 0.24.
#' @param linecol A colour character for the colour of line. Defaults to 
#' "steelblue".
#' @param ebwidth A numeric indicating size od width of error abrs. Defaults to 
#' 0.2.
#' @param ebcol A colour character for colour for errorbar. Defaults to "grey30".
#' @param textcol A colour character for all text elements on the plot. 
#' Defaults to "grey30".
#' @param xaxisbreaks A numeric vector indicating x-axis breaks. Automatically 
#' calculated by default.
#' @param xaxislabels A character vector indicating x-axis labels. Automatically 
#' calculated by default.
#' @param basesize A numeric indicating the base size of various plot elements 
#' such as pointsize, linesize etc. Increase basesize with larger figure 
#' dimensions. Defaults to 6. Manually specified arguments (eg: pointsize) 
#' override basesize.
#' @param gridsize A numeric indicating thickness of background grid. Default 
#' for \code{basesize=6} is 0.18.
#' @param imgtype A character indicating the type of exported image. Default 
#' set to 'png'. Other possible 
#' options are 'jpeg', 'tiff' or 'pdf'.
#' @param height A numeric denoting the height of exported image. Default units 
#' in 'cm'.
#' @param width A numeric denoting the width of exported image. Default units in 
#' 'cm'.
#' @param dpi A numeric denoting the resolution of exported image. Default set 
#' to 300. If \code{imgtype="pdf"}, dpi is fixed at 300.
#' @param units A character denoting the unit of measure of the export image. 
#' Default is 'cm'. Other options are 'px', 'in' or 'mm'. 
#' @param theme A character indicating ggplot theme to be used. Use like 
#' "theme_grey", "theme_bw" etc.
#' @param font A character indicating font family to be used in the plots. 
#' Uses default system fonts by default for jpeg, png and tiff. Uses 'Helvetica' 
#' as default for pdf. Use package \code{extrafonts} to import custom fonts. 
#' See vignette for examples.
#' @param na.rm Default set to FALSE. Does not remove NAs for plot and this 
#' generates warnings from \code{ggplot}. If set to TRUE, NAs are removed before 
#' plotting and warning messages from \code{ggplot} are avoided.
#' @param quiet A logical indicating if messages must be printed to console.
#' @param outputfilename A character indicating output file name. Defaults to 
#' 'evannoMethodStructure'.
#' @param exportpath A path to where content must be exported. For example,
#' \code{"./dir/anotherdir/"}. Defaults to NULL which means current working
#' directory.
#' @return When \code{returndata=TRUE} and \code{returnplot=FALSE}, a data.frame is 
#' returned.
#' When \code{returndata=FALSE} and \code{returnplot=TRUE}, a gtable plot object is 
#' returned.
#' When \code{returndata=TRUE} and \code{returnplot=TRUE}, a list with data.frame 
#' and gtable object is returned.
#' 
#' The data.frame contains Evanno results sorted by K. The table has 16 
#' columns namely Mean estimated ln probability of data, Standard deviation, 
#' Value of K, Number of runs for each K, Number of runs for each K, Number of 
#' individuals for each K, Number of loci for each K, Estimated ln probability 
#' of data plus standard deviation, Estimated ln probability of data minus 
#' standard deviation, First derivative, Max error of first derivative, Min 
#' error of first derivative, Second derivative, Max error of second derivative, 
#' Min error of second derivative and the Third derivative.
#' 
#' The gtable object is a result of \code{gridExtra::arrangeGrob()}. This is 
#' suitable for plotting in a report.
#' 
#' @details The Evanno method is based on the paper: Evanno, G., Regnaut, S., 
#' and Goudet, J. (2005). Detecting the number of clusters of individuals using 
#' the software STRUCTURE: a simulation study. Molecular ecology, 14(8), 
#' 2611-2620. The Evanno plot generated from this function can be recreated 
#' from the returned dataframe if furthur customisation is required.
#' 
#' See the vignette for more details.
#' 
#' @examples
#' 
#' \dontrun{
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' tr1 <- tabulateQ(readQ(sfiles))
#' sr1 <- summariseQ(tr1)
#' evannoMethodStructure(sr1)
#' evannoMethodStructure(data=sr1,exportplot=TRUE)
#' }
#' 
#  @import xlsx
#' @import grid
#' @import gridExtra
#' @export
#' 
evannoMethodStructure <- function(data=NULL,writetable=FALSE,exportplot=FALSE,returnplot=FALSE,returndata=TRUE,
                                  pointsize=NA,pointtype=20,pointcol="steelblue",linesize=NA,linecol="steelblue",
                                  ebwidth=0.2,ebcol="grey30",
                                  textcol="grey30",xaxisbreaks=waiver(),xaxislabels=waiver(),basesize=6,gridsize=NA,
                                  imgtype="png",height=NA,width=NA,dpi=300,units="cm",
                                  theme="theme_bw",font="",na.rm=TRUE,quiet=TRUE,outputfilename="evannoMethodStructure",exportpath=NULL)
{
  # does df data contain any data?
  if(is.null(data) || length(data)==0) stop("evannoMethodStructure: No input files.")
  if(class(data) != "data.frame") stop("evannoMethodStructure: Input is not a dataframe datatype.")
  if(!is.logical(writetable)) stop("evannoMethodStructure: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(exportplot)) stop("evannoMethodStructure: Argument 'exportplot' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(na.rm)) stop("evannoMethodStructure: Argument 'na.rm' not set correctly. Set as TRUE or FALSE.")
  imgtype <- tolower(imgtype)
  if(imgtype != "png" && imgtype != "pdf" && imgtype != "tiff" && imgtype != "jpeg") stop("evannoMethodStructure: Argument 'imgtype' set incorrectly. Options are 'png', 'jpeg', 'tiff' or 'pdf'.")

  # convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  cold <- colnames(data)
  
  # is column loci available?
  if(!"loci" %in% cold) stop("evannoMethodStructure: Column loci not available.")
  # is column ind available?
  if(!"ind" %in% cold) stop("evannoMethodStructure: Column ind not available.")
  # is column k available?
  if(!"k" %in% cold) stop("evannoMethodStructure: Column k not available.")
  # is column runs available?
  if(!"runs" %in% cold) stop("evannoMethodStructure: Column runs not available.")
  # is column elpdmean available?
  if(!"elpdmean" %in% cold) stop("evannoMethodStructure: Column elpdmean not available.")
  # is column elpdsd available?
  if(!"elpdsd" %in% cold) stop("evannoMethodStructure: Column elpdsd not available.")
  # is column minelpd available?
  if(!"elpdmin" %in% cold) stop("evannoMethodStructure: Column elpdmin not available.")
  # is column maxelpd available?
  if(!"elpdmax" %in% cold) stop("evannoMethodStructure: Column elpdmax not available.")
  
  
  err <- 0
  # are there atleast 3 values of K?
  if(length(data$k) < 3) {warning("Error: The Evanno method not computed. Requires at least 3 values of K.\n"); err <- 1}
  # do loci vary?
  if(!all(data$loci[1]==data$loci)) {warning("Error: The Evanno method not computed. Number of loci vary between runs.\n"); err <- 1}
  # do ind vary?
  if(!all(data$ind[1]==data$ind)) {warning("Error: The Evanno method not computed. Number of individuals vary between runs.\n"); err <- 1}
  # are k values sequential?
  is.sequential <- function(x) all(abs(diff(x))==1)
  if(!is.sequential(data$k)) {warning("Error: The Evanno method not computed. Requires increasing sequential values of K.\n"); err <- 1}
  # are repeats of any k<2?
  if(any(data$runs < 2)) warning("evannoMethodStructure: Results may not be meaningful if repeats (runs) for any value of K is less than 2.")
  
  base_size <- basesize
  height1 <- height
  width1 <- width
  if(imgtype=="pdf") dpi <- 300
  if(imgtype=="pdf" && font=="") font <- "Helvetica"
  if(is.na(pointsize)) pointsize <- base_size*0.3
  if(is.na(linesize)) linesize <- base_size*0.04
  if(is.na(gridsize)) gridsize <- base_size*0.03
  
  if(err==1)
  {
    if(exportplot || returnplot)
    {
      #create plots list
      plist <- vector("list",1)
      
      #settings for kPlot
      if(is.na(height)) {height1 <- 7}else{height1 <- height}
      if(is.na(width)) {width1 <- 7}else{width1<-width}
      if(is.na(basesize)) base_size <- round((5*height1)/7,1)
      
      if(imgtype=="pdf") height1 <- pophelper:::unitConverter(height1,units,"in",dpi)
      if(imgtype=="pdf") width1 <- pophelper:::unitConverter(width1,units,"in",dpi)
      
      #if(is.na(height) && imgtype=="pdf") height1 <- pophelper:::unitConverter(value=height1, fromunit="cm", tounit="in", dpi)
      #if(is.na(width) && imgtype =="pdf") width1 <- pophelper:::unitConverter(value=width1, fromunit="cm", tounit="in", res=res)
      #if(!is.na(height) && imgtype=="pdf" && units != "in") height1 <- pophelper:::unitConverter(value=height, fromunit=units, tounit="in", dpi)
      #if(!is.na(width) && imgtype =="pdf" && units != "in") width1 <- pophelper:::unitConverter(value=width, fromunit=units, tounit="in", dpi)
      
      plist[[1]] <- ggplot2::ggplot(data,aes(x=k,y=elpdmean))+
        geom_path(colour=linecol,size=linesize,na.rm=na.rm)+
        geom_point(colour=pointcol,fill=pointcol,size=pointsize,shape=pointtype,na.rm=na.rm)+
        geom_errorbar(aes(x=k,ymax=elpdmax,ymin=elpdmin,width=ebwidth),size=linesize,colour=ebcol,na.rm=na.rm)+
        scale_x_continuous(breaks=xaxisbreaks,labels=xaxislabels)+
        get(theme)(base_family=font)+
        labs(x=expression(paste(italic(K))),
             y=expression(paste("Mean L(",italic(K),") " %+-% " SD")))+
              theme(legend.position="none",
              axis.text.y=element_text(angle=90,hjust=0.5,size=base_size,colour=textcol),
              axis.text.x=element_text(size=base_size,colour=textcol),
              axis.title=element_text(size=base_size+1,colour=textcol,face="bold"),
              plot.title=element_text(size=base_size+3,hjust=0,colour=textcol),
              axis.ticks=element_blank(),
              panel.border=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_line(size=gridsize),
              plot.margin=grid::unit(c(0.2,0.2,0.2,0.2),"cm"))
      
      if(exportplot)
      {
        # check image imgtype
        if(imgtype=="pdf") pdf(paste0(exportpath,outputfilename,".pdf"),height=height1,width=width1,fonts=font)
        if(imgtype=="png") png(paste0(exportpath,outputfilename,".png"),height=height1,width=width1,res=dpi,units=units,family=font)
        if(imgtype=="jpeg") jpeg(paste0(exportpath,outputfilename,".jpg"),height=height1,width=width1,res=dpi,units=units,quality=100,family=font)
        if(imgtype=="tiff") tiff(paste0(exportpath,outputfilename,".tiff"),height=height1,width=width1,res=dpi,units=units,compression="lzw",family=font)
        
        print(plist[[1]])
        dev.off()
        
        if(imgtype=="tiff" && !quiet) cat(paste0(exportpath,outputfilename,".tiff exported.\n"))
        if(imgtype=="pdf" && !quiet) cat(paste0(exportpath,outputfilename,".pdf exported.\n"))
        if(imgtype=="png" && !quiet) cat(paste0(exportpath,outputfilename,".png exported.\n"))
        if(imgtype=="jpeg" && !quiet) cat(paste0(exportpath,outputfilename,".jpg exported.\n"))
      }

    }
    if(returnplot) return(plist[[1]])
    stop("evannoMethodStructure: Evanno method not computed.")
  }
  
  # convert dataframe to list
  datal <- as.list(data)
  
  # Loop to get first derivative of l(K) and its sd
  drv1 <- vector(length=nrow(data)-1,mode="numeric")
  drv1sd <- vector(length=nrow(data)-1,mode="numeric")
  i <- 1
  len1 <- length(datal$elpdmean)
  while (i < len1)
  {
    drv1[i] <- datal$elpdmean[i+1]-datal$elpdmean[i]
    drv1sd[i] <- abs(datal$elpdsd[i+1]-datal$elpdsd[i])
    i=i+1
  }
  
  # Loop to get second derivative of l(K) and its sd
  drv2 <- vector(length=nrow(data)-2,mode="numeric")
  drv2sd <- vector(length=nrow(data)-2,mode="numeric")
  i <- 1
  len1 <- length(drv1)
  while (i < len1)
  {
    drv2[i] <- abs(drv1[i+1]-drv1[i])
    drv2sd[i] <- abs(drv1sd[i+1]-drv1sd[i])
    i=i+1
  }
  
  # add NA to SD vector 1 and 2
  drv1sdf <- c(NA,drv1sd)
  drv2sdf <- c(NA,drv2sd,NA)
  
  datal$drv1 <- c(NA,drv1)
  datal$drv1max <- datal$drv1+drv1sdf
  datal$drv1min <- datal$drv1-drv1sdf
  datal$drv2 <- c(NA,drv2,NA)
  datal$drv2max <- datal$drv2+drv2sdf
  datal$drv2min <- datal$drv2-drv2sdf
  datal$drv3 <- abs(datal$drv2)/datal$elpdsd

  data <- data.frame(datal,stringsAsFactors=FALSE)
  rm(datal)
  colnames(data)[9:15] <- c("lnk1","lnk1max","lnk1min","lnk2","lnk2max","lnk2min","deltaK")
  
  # write table if opted
  if(writetable)
  {
    write.table(data,paste0(exportpath,outputfilename,".txt"),quote=FALSE,row.names=FALSE,sep="\t",dec=".")
    if(!quiet) cat(paste0(exportpath,outputfilename,".txt exported. \n"))
  }
  
  # show plot
  if(exportplot || returnplot)
  {
    if(is.na(height)) {height1 <- 8}else{height1 <- height}
    if(is.na(width)) {width1 <- 8}else{width1 <- width}
    if(is.na(basesize)) base_size <- round((5*height1)/7,1)
    
    if(imgtype=="pdf") height1 <- pophelper:::unitConverter(height1,units,"in",dpi)
    if(imgtype=="pdf") width1 <- pophelper:::unitConverter(width1,units,"in",dpi)
    
    #if(is.na(height) && imgtype=="pdf") height1 <- pophelper:::unitConverter(value=height1, fromunit="cm", tounit="in", res=dpi)
    #if(is.na(width) && imgtype =="pdf") width1 <- pophelper:::unitConverter(value=width1, fromunit="cm", tounit="in", res=dpi)
    #if(!is.na(height) && imgtype=="pdf" && units != "in") height1 <- pophelper:::unitConverter(value=height, fromunit=units, tounit="in", res=dpi)
    #if(!is.na(width) && imgtype =="pdf" && units != "in") width1 <- pophelper:::unitConverter(value=width, fromunit=units, tounit="in", res=dpi)
    
    #create plots list
    plist <- vector("list",4)
    
    # plot1
    plist[[1]] <- ggplot2::ggplot(data,aes(x=k,y=elpdmean))+
      geom_path(colour=linecol,size=linesize,na.rm=na.rm)+
      geom_point(colour=pointcol,fill=pointcol,size=pointsize,shape=pointtype,na.rm=na.rm)+
      geom_errorbar(aes(x=k,ymax=elpdmax,ymin=elpdmin,width=ebwidth),size=linesize,colour=ebcol,na.rm=na.rm)+
      scale_x_continuous(breaks=xaxisbreaks,labels=xaxislabels)+
      get(theme)(base_family=font)+
      labs(x=expression(paste(italic(K))),y=expression(paste("Mean L(",italic(K),") " %+-% " SD")),title="A")
    
    # plot 2
    plist[[2]] <- ggplot2::ggplot(data,aes(x=k,y=lnk1))+
      geom_path(colour=linecol,size=linesize,na.rm=na.rm)+
      geom_point(colour=pointcol,fill=pointcol,size=pointsize,shape=pointtype,na.rm=na.rm)+
      geom_errorbar(aes(x=k,ymax=lnk1max,ymin=lnk1min,width=ebwidth),
                    size=linesize,colour=ebcol,na.rm=na.rm)+
      scale_x_continuous(breaks=xaxisbreaks,labels=xaxislabels)+
      get(theme)(base_family=font)+
      labs(x=expression(paste(italic(K))),y=expression(paste("L'(",italic(K),") " %+-% " SD")),title="B")
    
    # plot 3
    plist[[3]] <- ggplot2::ggplot(data,aes(x=k,y=lnk2))+
      geom_path(colour=linecol,size=linesize,na.rm=na.rm)+
      geom_point(colour=pointcol,fill=pointcol,size=pointsize,shape=pointtype,na.rm=na.rm)+
      geom_errorbar(aes(x=k,ymax=lnk2max,ymin=lnk2min,width=0.2),
                    size=linesize,colour=ebcol,na.rm=na.rm)+
      scale_x_continuous(breaks=xaxisbreaks,labels=xaxislabels)+
      get(theme)(base_family=font)+
      labs(x=expression(paste(italic(K))),y=expression(paste("|L\"(",italic(K),")| " %+-% " SD")),title="C")
    
    # plot 4
    if(is.finite(sum(data$drv3,na.rm=TRUE)))
    {
      plist[[4]] <- ggplot2::ggplot(data,aes(x=k,y=deltaK))+
        geom_path(colour=linecol,size=linesize,na.rm=na.rm)+
        geom_point(colour=pointcol,fill=pointcol,size=pointsize,shape=pointtype,na.rm=na.rm)+
        scale_x_continuous(breaks=xaxisbreaks,labels=xaxislabels)+
        get(theme)(base_family=font)+
        labs(x=expression(paste(italic(K))),y=expression(paste(Delta,italic(K))),title="D")
    }
    
    plen <- length(plist)
    for (r in seq_along(plist))
    {
      plist[[r]] <- plist[[r]] + theme(legend.position="none",
                                       axis.text.y=element_text(angle=90,hjust=0.5,size=base_size-0.5,colour=textcol),
                                       axis.text.x=element_text(size=base_size-0.5,colour=textcol),
                                       axis.title=element_text(size=base_size+0.6,colour=textcol,face="bold"),
                                       plot.title=element_text(size=base_size+2.5,hjust=0,colour=textcol),
                                       panel.border=element_blank(),
                                       axis.ticks=element_blank(),
                                       panel.grid.minor=element_blank(),
                                       panel.grid.major=element_line(size=gridsize),
                                       plot.margin=grid::unit(c(0.2,0.2,0.2,0.2),"cm"))
    }
    
    # export image
    if(exportplot)
    {
      # check image imgtype  
      if(imgtype=="pdf") pdf(paste0(exportpath,outputfilename,".pdf"),height=height1,width=width1,fonts=font)
      if(imgtype =="png") png(paste0(exportpath,outputfilename,".png"),height=height1,width=width1,res=dpi,units=units,family=font)
      if(imgtype =="tiff") tiff(paste0(exportpath,outputfilename,".tiff"),height=height1,width=width1,res=dpi,units=units,compression="lzw",family=font)
      if(imgtype=="jpeg") jpeg(paste0(exportpath,outputfilename,".jpg"),height=height1,width=width1,res=dpi,units=units,quality=100,family=font)
      
      if(plen==3) gridExtra::grid.arrange(plist[[1]],plist[[2]],plist[[3]],ncol=2,nrow=2)
      if(plen==4) gridExtra::grid.arrange(plist[[1]],plist[[2]],plist[[3]],plist[[4]],ncol=2,nrow=2)
      dev.off()
      
      if(imgtype=="tiff" && !quiet) cat(paste0(exportpath,outputfilename,".tiff exported.\n"))
      if(imgtype=="pdf" && !quiet) cat(paste0(exportpath,outputfilename,".pdf exported.\n"))
      if(imgtype=="png" && !quiet) cat(paste0(exportpath,outputfilename,".png exported.\n"))
      if(imgtype=="jpeg" && !quiet) cat(paste0(exportpath,outputfilename,".jpg exported.\n"))
    }
    
    if(returnplot)
    {
      if(plen==3) p <- gridExtra::arrangeGrob(plist[[1]],plist[[2]],plist[[3]],ncol=2,nrow=2)
      if(plen==4) p <- gridExtra::arrangeGrob(plist[[1]],plist[[2]],plist[[3]],plist[[4]],ncol=2,nrow=2)
    }
  }
  
  # return
  if(returndata && !returnplot) return(data)
  if(!returndata && returnplot) return(p)
  if(returndata && returnplot) return(list(data=data,plot=p))
}

# clumppExport -----------------------------------------------------------------

#' @title Generate CLUMPP output from a qlist
#' @description Takes a qlist and combines several repeats for each K into a 
#' single file along with a parameter file suitable for input to CLUMPP. The two 
#' output files are organised into folders by K. The CLUMPP executable file is executed in each directory to generate aligned output files. CLUMPP reorders the clusters for each K.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param prefix A character prefix for folder names. By default, set to 'pop'.
#' @param parammode A numeric 1, 2 or 3 indicating the algorithm option for 
#' CLUMPP paramfile. Calculated automatically by default. Set this value to 3 if 
#' CLUMPP runs too long. See details.
#' @param paramrep A numeric indicating the number of repeats for CLUMPP 
#' paramfile. Calculated automatically by default. See details.
#' @param useexe DEPRECATED. CLUMPP executable is no longer included from v2.3.0. 
#' Download and use CLUMPP manually or use the function \code{\link{alignK}}.
#' @return The combined file and paramfile are written into respective folders 
#' named by K.
#' @details This function only generates the files needed to run CLUMPP. 
#' The CLUMPP executable can be downloaded and run for downstream steps. It can 
#' be obtained from \url{https://web.stanford.edu/group/rosenberglab/clumpp.html}.
#' Please remember to cite CLUMPP if this option is used.\cr
#' 
#' When multiple repeats are run for each K in runs, the order of 
#' clusters may be jumbled for each run. Therefore, when plotting multiple runs 
#' within each K, the colours cannot be assigned correctly. The software CLUMPP 
#' helps to overcome this issue by reordering the clusters correctly. This 
#' function \code{clumppExport()} takes multiple runs for each K and combines 
#' them into a single file and generates a parameter file for easy use with 
#' CLUMPP. Further details for CLUMPP can be found here: Jakobsson, M., and 
#' Rosenberg, N. A. (2007). CLUMPP: a cluster matching and permutation program 
#' for dealing with label switching and multimodality in analysis of population 
#' structure. Bioinformatics, 23(14), 1801-1806.\cr
#' \cr
#' \strong{parammode}\cr
#' The parammode (M) is the type of algorithm used. Option 1 is 'FullSearch' 
#' (takes the longest time), option 2 is 'Greedy' and option 3 is 'LargeKGreedy'
#' (fastest). If clumpp takes more than a few minutes, consider changing 
#' parammode to a higher number (ex. from 2 to 3), or open the exported 
#' paramfile and manually change GREEDY_OPTION to 3.\cr
#' \cr
#' The parammode and paramrep for CLUMPP paramfile is set based on this 
#' calculation.
#' T <- factorial(k)*((runs*(runs-1))/2)*k*ind, where k is number of 
#' populations, runs is number of runs for k and ind is number of individuals.
#' If T <= 100000000, then parammode is 2 and paramrep is 20, otherwise 
#' parammode is 3 and paramrep is set to 500.\cr
#' \cr
#' To find out more about parammode (algorithm type) and paramrep (repeats), 
#' refer to CLUMPP documentation.\cr
#' 
#' See the vignette for more details.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # generate input files for CLUMPP from STRUCTURE files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' clumppExport(readQ(sfiles))
#' 
#' # generate input files for CLUMPP from ADMIXTURE files
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),
#' full.names=TRUE)
#' clumppExport(readQ(afiles))
#' 
#' }
#' @seealso \code{\link{alignK}}
#' @export
#' 
clumppExport <- function(qlist=NULL,prefix=NA,parammode=NA,paramrep=NA,useexe=FALSE)
{
  # check input
  is.qlist(qlist)
  
  if(useexe) warning("CLUMPP executable is no longer included from v2.3.0. Download and use CLUMPP manually or use the function alignK().")
  
  if(is.na(prefix)) prefix <- "pop"
  prefix <- paste0(prefix,"_K")

  # get tabulated runs
  df1 <- pophelper::tabulateQ(qlist)
  df2 <- pophelper::summariseQ(df1)
  df1l <- as.list(df1)
  df2l <- as.list(df2)
  
  if(is.null(names(qlist))) names(qlist) <- paste0("sample",1:length(qlist))
  
  # k val duplicated
  if(any(duplicated(df2l$k))) stop("clumppExport: Repeating values of K found.")
  # do ind vary?
  if(!all(df2l$ind[1]==df2l$ind)) warning("clumppExport: Number of individuals vary between runs.\n")
  
  e <- 1
  p <- 1
  len1 <- length(df2l$k)
  while (e <= len1)
  {
    k <- df2l$k[e]
    ind <- df2l$ind[e]
    runs <- df2l$runs[e]
    
    ldata <- vector("list",length=runs)
    for (f in 1:runs)
    {
      sel <- which(names(qlist)==as.character(df1l$file[p]))
      dframe1 <- qlist[[sel]]
      
      # generate df
      dframe3 <- as.matrix(data.frame(V1=paste0(1:ind,":"),dframe1,last=as.character(rep(1,ind)),stringsAsFactors=FALSE))
      
      # add dataframes to list
      ldata[[f]] <- dframe3
      rm(dframe3)
      p=p+1
    }
    
    if(runs > 1 && k > 1)
    {
      currwd <- getwd()
      if(as.numeric(file.access(currwd,2))==-1) stop(paste0("clumppExport: Directory ",currwd," has no write permission."))
      
      dir.create(paste0(currwd,"/",prefix,k))
      setwd(paste0(currwd,"/",prefix,k))
      cat(paste0("Folder created: ",basename(getwd()),"\n"))  
      out <- paste0(prefix,k,"-combined.txt")
      
      ## file output block
      
      # make 2 line space
      spacer <- matrix(rep("  ",(k+2)*2),nrow=2)
      
      # write file
      write(t(format(ldata[[1]],nsmall=15)),paste(out),ncolumns=k+2)
      for (i in 2:length(ldata))
      {
        write(t(spacer),paste(out),ncolumns=k+2,append=TRUE)
        write(t(format(ldata[[i]],nsmall=15)),append=TRUE,paste(out),ncolumns=k+2)
      }
      cat(paste0(out),"exported.\n")
      
      ## paramfile section
      T1 <- factorial(k)*((length(ldata)*(length(ldata)-1))/2)*k*ind
      if(T1 <= 100000000)
      {
        if(is.na(parammode)) parammode <- 2
        if(is.na(paramrep)) paramrep <- 20
      }else{
        if(is.na(parammode)) parammode <- 3
        if(is.na(paramrep)) paramrep <- 500
      }
      out1 <- base::gsub(".txt","",out)
      params <- c("DATATYPE 1 ",
                  "INDFILE NOTNEEDED.indfile ",
                  paste0("POPFILE ",out," "),
                  paste0("OUTFILE ",out1,"-merged.txt "),
                  paste0("MISCFILE ",out1,"-miscfile.txt "),
                  paste0("K ",k," "),
                  paste0("C ",ind," "),
                  paste0("R ",length(ldata)," "),
                  paste0("M ",parammode," "),
                  "W 0 ",
                  "S 2 ",
                  "GREEDY_OPTION 2 ",
                  paste0("REPEATS ",paramrep," "),
                  "PERMUTATIONFILE NOTNEEDED.permutationfile ",
                  "PRINT_PERMUTED_DATA 1 ",
                  paste0("PERMUTED_DATAFILE ",out1,"-aligned.txt "),
                  "PRINT_EVERY_PERM 0 ",
                  paste0("EVERY_PERMFILE ",out1,".every_permfile "),
                  "PRINT_RANDOM_INPUTORDER 0 ",
                  paste0("RANDOM_INPUTORDERFILE ",out1,".random_inputorderfile "),
                  "OVERRIDE_WARNINGS 0 ",
                  "ORDER_BY_RUN 0 ")
      
      write(params,"paramfile")
      cat(paste0("paramfile exported.\n"))
      
      setwd(paste(currwd))
      cat("-----------------------\n")
    }else
    {
      if(k==1) message(paste0(prefix,k," not exported. K less than 2.\n"))
      if(runs < 2) message(paste0(prefix,k," not exported. Repeats less than 2.\n"))
      cat("-----------------------\n")
    }
    e <- e + 1
  }
  
  cat("Run completed.\n")
}

# collectRunsTess --------------------------------------------------------------

#' @title Collect TESS cluster run files from multiple folders
#' @description Collect TESS cluster run files from multiple folders to one 
#' folder and rename each run by folder name
#' @param runsdir A character path indicating the directory containing TESS 
#' runs in multiple directories. Use \code{choose.dir()} for interactively 
#' selecting the directory. If NA, or no directory is selected, the current 
#' working directory is used.
#' @param newdir A character indicating the name of the new directory to be 
#' created with the collected runs. IF NA, the default name 'AllTESSRuns' is 
#' used. 
#' @param quiet A logical indicating if a message is to be displayed for 
#' directories without TESS runs and number of runs copied and renamed. 
#' @details Within each TESS run folder, the function searches for filename 
#' ending with 'TR.txt' as the cluster file. This file is copied to the new 
#' folder and renamed as the name of the respective run directory. Therefore, 
#' DO NOT manually rename original run files or directories.
#' 
#' See the vignette for more details.
#' 
#' @return Two integers are ruturned. The first denotes the number of TESS run 
#' files copied and renamed. The second number denotes number of directories 
#' without TESS run files.
#' @examples 
#' \dontrun{
#' collectRunsTess("path")
#' }
#' @export
#' 
collectRunsTess <- function(runsdir=NA,newdir=NA,quiet=FALSE)
{
  if(!is.logical(quiet)) stop("collectRunsTess: Argument 'quiet' set incorrectly. Set as TRUE or FALSE.")
  currwd <- getwd()
  if(is.na(newdir)) newdir <- "AllTESSRuns"
  if(is.na(runsdir)) runsdir <- currwd
  dirs <- list.dirs(path=runsdir,full.names=TRUE,recursive=FALSE)
  dir.create(paste0(runsdir,"/",newdir))
  k <- 0
  l <- 0
  len1 <- length(dirs)
  for (i in seq_along(dirs))
  {
    setwd(dirs[i])
    files <- list.files()
    sel <- grep("\\w+TR.txt",files)
    if(length(sel)==0) 
    {
      if(!quiet) cat(paste0("No TESS cluster file found in directory: ",basename(dirs[i]),"\n"))
      l=l+1
    }
    if(length(sel) != 0) 
    {
      file.copy(from=paste0(dirs[i],"/",files[sel],sep=""),to=paste0(runsdir,"/",newdir)) 
      file.rename(from=paste0(runsdir,"/",newdir,"/",files[sel]),to=paste0(runsdir,"/",newdir,"/",basename(dirs[i]),".txt"))
      k=k+1  
    }
  }
  setwd(currwd)
  if(!quiet) cat(paste0(k," TESS cluster files copied and renamed.\n"))
  return(c(k,l))
}

# collectClumppOutput ----------------------------------------------------------

#' @title Collect CLUMPP output files from multiple folders
#' @description Collect CLUMPP output files from multiple folders to one folder
#' @param prefix A character indicating the prefix of the CLUMPP directories 
#' before the underscore. For ex. if the directories are pop_K2, then prefix 
#' is pop.
#' @param filetype A character indicating the type of file to be copied. 
#' Options are 'aligned' to copy aligned files only, 'merged' to copy merged 
#' files only and 'both' to copy both files.
#' @param runsdir A character denoting the directory containing CLUMPP output 
#' files in multiple directories. Use \code{choose.dir()} for interactively 
#' selecting the directory. If NA, the current working directory is used.
#' @param newdir A character indicating the name of the new directory to be 
#' created with the collected runs. IF NA, the a directory name joining prefix 
#' and filetype is created. 
#' @param quiet A logical indicating if a message is to be displayed showing 
#' the number of folders processed and number of files processed. 
#' @details Within each CLUMPP output folder, the function searches for 
#' filenames containing combination of prefix and filetype. This file is 
#' copied to the new folder. Therefore, do not manually rename CLUMPP output 
#' files or output directories.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more 
#' details.
#' 
#' @return Two integers are ruturned. The first denotes the number of 
#' directories processed. The second number denotes the number files copied.
#' @examples 
#' \dontrun{
#' collectClumppOutput(runsdir="path")
#' collectClumppOutput(prefix="pop",runsdir="path")
#' }
#' @export
#' 
collectClumppOutput <- function(prefix="pop",filetype="aligned",runsdir=NA,newdir=NA,quiet=FALSE)
{
  if(!is.character(prefix)) stop("Argument 'prefix' must be a character.")
  if(!is.character(filetype)) stop("Argument 'filetype' must be a character.")
  if(!is.logical(quiet)) stop("collectClumppOutput: Argument 'quiet' set incorrectly. Set as TRUE or FALSE.")
  
  # check imgoutput
  if(tolower(filetype)!="aligned" && tolower(filetype)!="merged" && tolower(filetype)!="both") stop("collectClumppOutput: Argument 'filetype' set incorrectly. Set as 'aligned', 'merged' or 'both'.")
  
  currwd <- getwd()
  if(is.na(newdir)) newdir <- paste0(prefix,"-",filetype)
  if(is.na(runsdir)) runsdir <- currwd
  dirs <- list.dirs(path=runsdir,full.names=TRUE,recursive=FALSE)
  dirs1 <- dirs[grep(paste0(prefix,"_"),dirs)]
  dir.create(paste0(runsdir,"/",newdir))
  
  k <- 0
  l <- 0
  i <- 1
  len1 <- length(dirs1)
  for (i in seq_along(dirs1))
  {
    setwd(dirs1[i])
    files <- list.files()
    sel1 <- grep("aligned",files)
    sel2 <- grep("merged",files)
    if(tolower(filetype)=="aligned") sel3 <- sel1
    if(tolower(filetype)=="merged") sel3 <- sel2
    if(tolower(filetype)=="both") sel3 <- c(sel1,sel2)
    if(length(sel3)==0) 
    {
      cat("No suitable file found in directory: ",basename(dirs1[i]),"\n",sep="")
    }
    if(length(sel3) != 0) 
    {
      file.copy(from=paste0(dirs1[i],"/",files[sel3]),to=paste0(runsdir,"/",newdir)) 
      k=k+1
      l=l+length(sel3)
    }
  }
  
  setwd(currwd)
  if(!quiet) cat(paste0("Directories processed: ",k,"\nFiles copied: ",l,"\n"))
  
  return(c(k,l))
}


# analyseQ ------------------------------------------------------------------

#' @title Analyse STRUCTURE, TESS, BAPS or BASIC text runs. Wrapper around 
#' several smaller functions.
#' @description A single function to analyse STRUCTURE, TESS, BAPS or BASIC 
#' text runs. Converts runs to a qlist, tabulates, summarises, runs 
#' evanno method (for STRUCTURE runs), aligns clusters and plots all runs.
#' @param files A character or character vector of one or more STRUCTURE, TESS, 
#' BAPS or BASIC run files. Use \code{choose.files(multi=TRUE)} to choose 
#' interactively.
#' @param evannomethod A logical indicating if evanno method should be 
#' performed. Applies only to STRUCTURE runs.
#' @param align A logical indicating if clusters must be aligned within and across K.
#' @param plotruns A logical indicating if selected files should be exported as 
#' barplots.
#' @param imgoutput A character indicating if files are plotted as separate 
#' image files ("sep") or joined into a single image ("join").
#' @param grplab A dataframe with one or more columns (group label sets), and 
#' rows equal to the number of individuals.
#' @param clustercol A character vector of colours for colouring clusters. If 
#' NA, colours are automatically generated. K 1 to 12 are custom unique colours 
#' while K>12 are coloured by function \code{rich.color()}.
#' @param writetable A logical T or F. Setting to TRUE writes the output table 
#' to the working directory.
#' @param sorttable A logical indicating if the output table must be sorted. 
#' Sorts table by loci, ind and K when available.
#' @details The function \code{analyseQ} is a wrapper around several other 
#' \code{pophelper} functions. All arguments for all sub-functions are 
#' not available. If more arguments/options are required, consider running the functions separately.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more 
#' details.
#' 
#' @return A qlist (list of data.frames) is returned.
#' @examples 
#' \dontrun{
#' # structure files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' analyseQ(sfiles)
#' }
#' @export
#' @aliases analyzeQ
#' 
analyseQ <- analyzeQ <- function(files=NULL,evannomethod=TRUE,align=TRUE,plotruns=TRUE,
                      imgoutput="sep",grplab=NA,clustercol=NA,writetable=TRUE,sorttable=TRUE)
{
  if(is.null(files) || (length(files)==0)) stop("analyseQ: No input files.")
  if(!is.character(files)) stop("analyseQ: Input is not character dataype.")
  if(!is.logical(evannomethod)) stop("analyseQ: Argument 'evannoMethod' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(align)) stop("analyseQ: Argument 'align' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(plotruns)) stop("analyseQ: Argument 'plotQ' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(writetable)) stop("analyseQ: Argument 'writetable' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(sorttable)) stop("analyseQ: Argument 'sorttable' set incorrectly. Set as TRUE or FALSE.")
  
  chk1 <- unique(pophelper:::checkQ(files=files,warn=FALSE)$type)
  if(length(chk1) > 1) stop("analyseQ: Mixed runs selected.")
  if("CLUMPP" %in% chk1) stop("analyseQ: CLUMPP files cannot be used in this function.")
  if("UNIDENTIFIED" %in% chk1) stop("analyseQ: Input file has incorrect format. Check if selected files are STRUCTURE, BAPS, TESS or BASIC runs.")
  if(imgoutput != "sep" && imgoutput != "join") stop("analyseQ: Argument 'imgoutput' set incorrectly. Set as 'sep' to export as separate plots. Set as 'join' to export as one joined plot.")
  if(!(chk1 %in% c("STRUCTURE","TESS","BAPS","BASIC"))) stop("analyseQ: Input files are not STRUCTURE, BAPS, TESS or BASIC run formats.")
  
  if(chk1=="STRUCTURE") rlist <- pophelper::readQ(files,filetype="structure",indlabfromfile=TRUE)
  if(chk1=="TESS") rlist <- pophelper::readQ(files,filetype="tess")
  if(chk1=="BAPS") rlist <- pophelper::readQ(files,filetype="baps")
  if(chk1=="BASIC") rlist <- pophelper::readQ(files,filetype="basic")
  
  df1 <- pophelper::tabulateQ(rlist,writetable=writetable,sorttable=sorttable)
  df2 <- pophelper::summariseQ(df1,writetable=writetable)
  
  if(chk1=="STRUCTURE")
  {
    if(evannomethod) pophelper::evannoMethodStructure(df2,writetable=writetable,exportplot=TRUE)
  }
    
  if(align) rlist <- pophelper::alignK(rlist)
  if(plotruns) pophelper::plotQ(rlist,imgoutput=imgoutput,grplab=grplab,clustercol=clustercol)
  return(rlist)
}

# distructExport ---------------------------------------------------------------

#' @title Generate files for DISTRUCT.
#' @description Create DISTRUCT input files from a qlist.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param grplabbottom A character vector of group labels to be plotted below 
#' the plot. The vector must be the same length as number of individuals. See 
#' details.
#' @param grplabtop An optional character vector of group labels to be plotted 
#' above the plot. The vector must be the same length as number of individuals. 
#' See details.
#' @param grpmean A logical indicating if individual values are to be plotted 
#' (F) or group means are to be plotted (T).
#' @param overwritedirs A logical indicating if existing directories must be 
#' overwritten (T) automatically or not (F).
#' @param printtitle A logical indicating if the filename must be printed as the
#'  title on the plot.
#' @param clustercol A character vector of colours equal to the number of 
#' clusters. Note these are not R colours. Use \code{\link{distructColours}} or refer 
#' to DISTRUCT manual for colours.  
#' With multiple files, number of colours must equal input file with highest 
#' number of clusters. 
#' @param grayscale A logical indicating if clusters must be shown in grayscale.
#' @param printcolorbrewer A logical indicating if the colours provided in 
#' \code{clustercol} are ColorBrewer colours. See details.
#' @param sepline A logical indicating if divider lines must be drawn between 
#' groups (T).
#' @param seplinewidth A numeric indicating width of sepline.
#' @param borderlinewidth A numeric indicating width of border around the plot.
#' @param indlinewidth A numeric indicating width of border around individual 
#' bars and ticks.
#' @param fontsize A numeric indicating font size of group labels.
#' @param topdist A numeric indicating distance of top labels from the top edge 
#' of the plot.
#' @param bottomdist A numeric indicating distance of bottom labels from the 
#' bottom edge of the plot. Usually a negative number.
#' @param figheight A numeric indicating height of the plot.
#' @param indwidth A numeric indicating width of each individual bar. The width 
#' of the plot depends on this value.
#' @param orientation An integer (0,1,2,3) indicating orientation of the plot. 
#' See details.
#' @param xorigin A numeric indicating lower left x-coordinate of the plot. See 
#' details.
#' @param yorigin A numeric indicating lower left y-coordinate of the plot. See 
#' details.
#' @param xscale A numeric indicating scaling for the x direction.
#' @param yscale A numeric indicating scaling for the y direction.
#' @param toplabangle A numeric between 0 and 180 indicating angle of top labels. 
#' @param bottomlabangle A numeric between 0 and 180 indicating angle of bottom 
#' labels. 
#' @param echodata A logical. Not really sure what this does.
#' @param printdata A logical indicating if head and tail of data must be shown 
#' in display on running DISTRUCT.
#' @param quiet A logical TRUE or FALSE. Set to TRUE by default to print verbose 
#' statements to screen.
#' @param useexe DEPRECATED. DISTRUCT executable is no longer included from v2.3.0.
#' Download and run DISTRUCT manually.
#' @return This function does not run DISTRUCT. It only generates the files 
#' necessary to run DISTRUCT. The DISTRUCT executable can be downloaded from 
#' here \url{https://web.stanford.edu/group/rosenberglab/distruct.html}. Please 
#' remember to cite DISTRUCT if this option is used.\cr
#' 
#' The function does not return anything. The function creates 
#' directories for each input file and populates it with files necessary to run 
#' DISTRUCT. The files are individual q-matrix file (xx-indq.txt), population 
#' q-matrix file (xx-popq.txt), a cluster colour file (xx-colours.txt) and 
#' drawparams file. If group labels were defined, then (xx-poplab-bottom.txt) or 
#' (xx-poplab-top.txt) are also exported. The DISTRUCT executable can be run in this 
#' directory to generate an xx.ps file.
#' @details
#' 
#' \cr
#' \strong{Orientation} \cr
#' 0 for horizontal orientation (default) \cr
#' 1 for vertical orientation \cr
#' 2 for reverse horizontal orientation \cr
#' 3 for reverse vertical orientation \cr
#' \cr
#' \strong{Origin} \cr
#' Default values of origin for a given orientation: \cr
#' orientation, xorigin, yorigin \cr
#' 0,72,288 \cr
#' 1,360,72 \cr
#' 2,540,504 \cr
#' 3,288,720 \cr
#' If plot exceeds canvas size, consider shifting \code{xorigin} to the left 
#' and/or decreasing \code{indwidth}.\cr
#' \cr
#' \strong{Colorbrewer colours} \cr
#' Colorbrewer colours are not automatically generated for now. Refer to 
#' DISTRUCT manual for colour names. \cr
#' Replace the colour names in xx-colours.txt output file with selected 
#' colorbrewer colours (ex: Accent_3_qual_1). \cr
#' \cr
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more 
#' details.
#' 
#  @import xlsx
#' @examples 
#' \dontrun{
#' # read some data
#' slist <- readQ(list.files(path=system.file("files/structure",
#' package="pophelper"),full.names=TRUE))
#' grps1 <- read.delim(system.file("files/structuregrplabels.txt",
#' package="pophelper"),header=FALSE, stringsAsFactor=FALSE)
#' grps2 <- read.delim(system.file("files/structuregrplabels2.txt",
#' package="pophelper"),header=FALSE, stringsAsFactor=FALSE)
#' 
#' # plot without labels
#' distructExport(slist[1])
#' 
#' # plot with bottom group label
#' distructExport(slist[1],grplabbottom=grps1$V1)
#' 
#' # plot with top group label
#' distructExport(slist[1],grplabtop=grps2$V1)
#' 
#' # plot group mean values
#' distructExport(slist[1],grplabbottom=grps1$V1,grpmean=TRUE)
#' 
#' }
#' @import tidyr
#' @export
#' 
distructExport <- function(qlist=NULL,grplabbottom=NA,grplabtop=NA,grpmean=FALSE,overwritedirs=FALSE,
                           printtitle=FALSE,clustercol=NA,grayscale=FALSE,printcolorbrewer=FALSE,
                           sepline=TRUE,seplinewidth=0.2,borderlinewidth=1.2,indlinewidth=0.2,
                           fontsize=6,topdist=5,bottomdist=-7,figheight=36,indwidth=1,
                           orientation=0,xorigin=NA,yorigin=NA,xscale=1,yscale=1,toplabangle=60,bottomlabangle=60,
                           echodata=TRUE,printdata=FALSE,quiet=FALSE,useexe=FALSE)
{
  # check input
  is.qlist(qlist)
  if(useexe) warning("DISTRUCT executable is no longer included from v2.3.0. Download and run DISTRUCT manually.\n")
  grplabbottom <- as.character(grplabbottom)
  grplabtop <- as.character(grplabtop)
  if((length(grplabbottom) > 1) && any(is.na(grplabbottom))) stop("distructExport: Missing values (NA) in grplabbottom.")
  if((length(grplabtop) > 1) && any(is.na(grplabtop))) stop("distructExport: Missing values (NA) in grplabtop.")
  clustercol <- as.character(clustercol)
  if((length(clustercol) > 1) && any(is.na(clustercol))) stop("distructExport: Missing values (NA) in clustercol.")
  if(all(!c(0,1,2,3) %in% orientation)) stop("distructExport: Argument 'orientation' must be a numeric value of 0, 1, 2 or 3.")
  if(!is.logical(grpmean)) stop("distructExport: Argument 'grpmean' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(overwritedirs)) stop("distructExport: Argument 'overwritedirs' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(printtitle)) stop("distructExport: Argument 'printtitle' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(sepline)) stop("distructExport: Argument 'sepline' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(grayscale)) stop("distructExport: Argument 'grayscale' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(printcolorbrewer)) stop("distructExport: Argument 'printcolorbrewer' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(echodata)) stop("distructExport: Argument 'echodata' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(printdata)) stop("distructExport: Argument 'printdata' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(quiet)) stop("distructExport: Argument 'quiet' not set correctly. Set as TRUE or FALSE.")

  # number of files selected
  flen <- length(qlist)
  if(!quiet) cat(paste0("Number of runs in the qlist: ",flen,"\n"))
  
  currwd <- getwd()
  if(as.numeric(file.access(currwd,2))==-1) stop(paste0("distructExport: Directory ",currwd," has no write permission."))
  
  for(i in seq_along(qlist))
  {
    # sample name
    fname <- names(qlist)[i]
    if(is.null(fname)) fname <- paste0("sample",i)
    if(!quiet) cat(paste0("Computing Distruct files for ",fname,"\n"))
    
    # check file
    df <- qlist[[i]]
    if(!is.data.frame(df)) stop(paste0("distructExport: List item ",fname," is not a data.frame object."))
    if(!any(sapply(df,is.numeric))) stop(paste0("distructExport: List item ",fname," has non-numeric columns."))
    tab_runs <- 1
    
    for(j in 1:tab_runs)
    {
      
      dirname <- paste0(fname,"-distruct")
      dop_k <- ncol(df)
      dop_n <- nrow(df)
      
      # top labels & bottom labels
      if((!any(is.na(grplabbottom))) && (!any(is.na(grplabtop))))
      {
        grplabbottom <- as.character(grplabbottom)
        grplabtop <- as.character(grplabtop)
        
        if(dop_n != length(grplabbottom)) {message(paste0("distructExport: Number of rows of data (",dop_n,") not equal to length of bottom group labels (",length(grplabbottom),").")); next;}
        if(dop_n != length(grplabtop)) {message(paste0("distructExport: Number of rows of data (",dop_n,") not equal to length of top group labels (",length(grplabtop),").")); next;}
        if(length(grplabbottom) != length(grplabtop)) {message(paste0("distructExport: Length of top group labels (",length(grplabtop),") not equal to length of bottom group labels (",length(grplabbottom),").")); next;}
        
        dop_bottomlabel <- TRUE
        dop_toplabel <- TRUE
        
        rlevalb <- rle(grplabbottom)
        facb <- factor(rep(1:length(rlevalb$values),rlevalb$lengths))
        rlevalt <- rle(grplabtop)
        fact <- factor(rep(1:length(rlevalt$values),rlevalt$lengths))
        
        if(length(levels(facb)) != length(levels(fact))) {message("distructExport: Number of levels of top labels are not equal to the number of levels of bottom labels."); next;}
        
        # indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 grp=sprintf("%06.0f",as.numeric(as.character(facb))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=FALSE),df)
        
        # grpq
        dfb <- df
        dfb$grp <- facb
        dfb1 <- tidyr::gather(dfb,"variable","value",-grp)
        dfb2 <- cbind(aggregate(value~grp+variable,data=dfb1,FUN=mean),aggregate(value~grp+variable,data=dfb1,FUN=length)[,3,drop=FALSE])
        colnames(dfb2) <- c("grp","variable","mean","len")
        dfb3 <- tidyr::spread(dfb2,"variable","mean")
        rm(dfb1,dfb2)
        dfb3$length <- dfb3$len
        dfb3 <- dfb3[,!(colnames(dfb3) %in% c("grp","len"))]
        pnames <- sprintf("%06.0f",1:nrow(dfb3))
        grpq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=FALSE),dfb3)
        
        # bottomlabels
        plb <- data.frame(sample=pnames,label=rlevalb$values,stringsAsFactors=FALSE)
        dop_m <- nrow(plb)
        
        # grpq
        dft <- df
        dft$grp <- facb
        dft1 <- tidyr::gather(dft,"variable","value",-grp)
        dft2 <- cbind(aggregate(value~grp+variable,data=dft1,FUN=mean),aggregate(value~grp+variable,data=dft1,FUN=length)[,3,drop=FALSE])
        colnames(dft2) <- c("grp","variable","mean","len")
        dft3 <- tidyr::spread(dft2,"variable","mean")
        rm(dft1,dft2)
        dft3$length <- dft3$len
        dft3 <- dft3[,!(colnames(dft3) %in% c("grp","len"))]
        pnamest <- sprintf("%06.0f",1:nrow(dft3))
        # grpq <- cbind(data.frame(sample=paste0(pnamesb,":"),stringsAsFactors=FALSE),dft3)
        
        # tolabels
        plt <- data.frame(sample=pnamest,label=rlevalt$values,stringsAsFactors=FALSE)
        #dop_m <- nrow(plb)
        fname_plb <- paste0(fname,"-grplab-bottom.txt")
        fname_plt <- paste0(fname,"-grplab-top.txt")
      }
      
      # bottom labels
      if((!any(is.na(grplabbottom))) && (any(is.na(grplabtop))))
      {
        grplabbottom <- as.character(grplabbottom)
        if(dop_n != length(grplabbottom)) {message(paste0("distructExport: Number of rows of data (",dop_n,") not equal to length of bottom group labels (",length(grplabbottom),").")); next;}
        
        dop_bottomlabel <- TRUE
        dop_toplabel <- FALSE
        rleval <- rle(grplabbottom)
        fac <- factor(rep(1:length(rleval$values),rleval$lengths))
        
        # indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 grp=sprintf("%06.0f",as.numeric(as.character(fac))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=FALSE),df)
        
        # grpq
        df$grp <- fac
        df1 <- tidyr::gather(df,"variable","value",-grp)
        df2 <- cbind(aggregate(value~grp+variable,data=df1,FUN=mean),aggregate(value~grp+variable,data=df1,FUN=length)[,3,drop=FALSE])
        colnames(df2) <- c("grp","variable","mean","len")
        df3 <- tidyr::spread(df2,"variable","mean")
        rm(df1,df2)
        df3$length <- df3$len
        df3 <- df3[,!(colnames(df3) %in% c("grp","len"))]
        pnames <- sprintf("%06.0f",1:nrow(df3))
        grpq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=FALSE),df3)
        
        # bottomlabels
        plb <- data.frame(sample=pnames,label=rleval$values,stringsAsFactors=FALSE)
        dop_m <- nrow(plb)
        
        fname_plb <- paste0(fname,"-grplab-bottom.txt")
        fname_plt <- "null"
      }
      
      # top labels
      if((!any(is.na(grplabtop))) && (any(is.na(grplabbottom))))
      {
        grplabtop<- as.character(grplabtop)
        if(dop_n != length(grplabtop)) {message(paste0("distructExport: Number of rows of data (",dop_n,") not equal to length of bottom group labels (",length(grplabtop),").")); next;}
        
        dop_toplabel <- TRUE
        dop_bottomlabel <- FALSE
        
        rleval <- rle(grplabtop)
        fac <- factor(rep(1:length(rleval$values),rleval$lengths))
        
        # indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 grp=sprintf("%06.0f",as.numeric(as.character(fac))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=FALSE),df)
        
        # grpq
        df$grp <- fac
        df1 <- tidyr::gather(df,"variable","value",-grp)
        df2 <- cbind(aggregate(value~grp+variable,data=df1,FUN=mean),aggregate(value~grp+variable,data=df1,FUN=length)[,3,drop=FALSE])
        colnames(df2) <- c("grp","variable","mean","len")
        df3 <- tidyr::spread(df2,"variable","mean")
        rm(df1,df2)
        df3$length <- df3$len
        df3 <- df3[,!(colnames(df3) %in% c("grp","len"))]
        pnames <- sprintf("%06.0f",1:nrow(df3))
        grpq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=FALSE),df3)
        
        # toplabels
        plt <- data.frame(sample=pnames,label=rleval$values,stringsAsFactors=FALSE)
        dop_m <- nrow(plt)
        
        fname_plb <- "null"
        fname_plt <- paste0(fname,"-grplab-top.txt")
      }
      
      # top and bottom labels absent
      if((any(is.na(grplabbottom))) && (any(is.na(grplabtop))))
      {
        
        # indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 grp=sprintf("%06.0f",as.numeric(as.character(rep(1,nrow(df))))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=FALSE),df)
        
        df1 <- tidyr::gather(df,"variable","value")
        df2 <- cbind(aggregate(value~variable,data=df1,FUN=mean),aggregate(value~variable,data=df1,FUN=length)[,2,drop=FALSE])
        colnames(df2) <- c("variable","mean","len")
        df3 <- tidyr::spread(df2,"variable","mean")
        
        rm(df1,df2)
        df3$length <- df3$len
        df3 <- df3[,!(colnames(df3) %in% c("len"))]
        pnames <- sprintf("%06.0f",1:nrow(df3))
        grpq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=FALSE),df3)
        
        dop_m <- 1
        dop_toplabel <- FALSE
        dop_bottomlabel <- FALSE
        fname_plb <- "null"
        fname_plt <- "null"
      }
      
      # colours
      if(any(is.na(clustercol))){
        colsdf <- data.frame(sample=1:dop_k,cols=pophelper:::distructColours()[1:dop_k],stringsAsFactors=FALSE)
      }else{
        if(length(clustercol) != length(pnames)) stop(paste0("distructExport: Length of colours (",length(clustercol),") not equal to length of groups (",length(pnames),"). Change number of colours in 'clustercol' or set 'clustercol=NA'."))
        #if(any(!clustercol %in% pophelper:::distructColours())) stop(paste0("distructExport: One or more colours provided (",clustercol[which(!clustercol %in% pophelper:::distructColours())],") is not a standard Distruct colour."))
        colsdf <- data.frame(sample=pnames,cols=clustercol,stringsAsFactors=FALSE)
      }
      
      # grayscale
      if(grayscale) 
      {
        fngray <- function(n) {
          x <- round(seq(0,1,by=1/n),nchar(n))
          x[round(seq(1,length(x),length.out=n))]
        }
        colsdf <- data.frame(sample=1:dop_k,cols=fngray(dop_k),stringsAsFactors=FALSE)
      }
      
      # xorigin
      if(is.na(xorigin))
      {
        if(orientation==0) xorigin <- 72
        else if(orientation==1) xorigin <- 360
        else if(orientation==2) xorigin <- 540
        else if(orientation==3) xorigin <- 288
      }
      
      # yorigin
      if(is.na(yorigin))
      {
        if(orientation==0) yorigin <- 288
        else if(orientation==1) yorigin <- 72
        else if(orientation==2) yorigin <- 504
        else if(orientation==3) yorigin <- 720
      }
      
      # output file names
      fname_grpq <- paste0(fname,"-grpq.txt")
      fname_indq <- paste0(fname,"-indq.txt")
      fname_col <- paste0(fname,"-colours.txt")
      fname_out <- paste0(fname,".ps")
      
      # params
      params <- c(
        "PARAMETERS FOR THE PROGRAM distruct.",
        "",
        "'(int)' means that this takes an integer value.",
        "'(B)' means that this variable is Boolean",
        "(1 for True, and 0 for False)",
        "'(str)' means that this is a string (but not enclosed in quotes)",
        "'(d)' means that this is a double (a real number).",
        "",
        "Data settings",
        "",
        paste0("#define INFILE_POPQ ",fname_grpq),
        paste0("#define INFILE_INDIVQ ",fname_indq),
        paste0("#define INFILE_LABEL_BELOW ",fname_plb),
        paste0("#define INFILE_LABEL_ATOP ",fname_plt),
        paste0("#define INFILE_CLUST_PERM ",fname_col),
        paste0("#define OUTFILE ",fname_out),
        "",
        "// number of clusters",
        paste0("#define K	",dop_k," "),
        "// number of pre-defined groups",
        paste0("#define NUMPOPS ",dop_m," "),
        "// number of individuals",
        paste0("#define NUMINDS ",dop_n," "),
        "",
        "Main usage options",
        "",
        "// (B) 1 if indiv q\'s are to be printed, 0 if only group q\'s",
        paste0("#define PRINT_INDIVS ",as.numeric(!grpmean)," "),
        "// (B) print labels above figure",
        paste0("#define PRINT_LABEL_ATOP ",as.numeric(dop_toplabel)," "),
        "// (B) print labels below figure",
        paste0("#define PRINT_LABEL_BELOW ",as.numeric(dop_bottomlabel)," "),
        "// (B) print lines to separate groups",
        paste0("#define PRINT_SEP ",as.numeric(sepline)," "),
        "",
        "Figure appearance",
        "",
        "// (d) size of font",
        paste0("#define FONTHEIGHT ",as.numeric(fontsize)," "),
        "// (d) distance above plot to place text",
        paste0("#define DIST_ABOVE ",as.numeric(topdist)," "),
        "// (d) distance below plot to place text",
        paste0("#define DIST_BELOW ",as.numeric(bottomdist)," "),
        "// (d) height of the figure",
        paste0("#define BOXHEIGHT ",as.numeric(figheight)," "),
        "// (d) width of an individual",
        paste0("#define INDIVWIDTH ",as.numeric(indwidth)," "),
        "",
        "Extra options",
        "",
        "// (int) 0 for horizontal orientation (default)",
        "// (int) 1 for vertical orientation",
        "//	(int) 2 for reverse horizontal orientation",
        "// (int) 3 for reverse vertical orientation",     
        paste0("#define ORIENTATION ",as.numeric(orientation)," "),
        "// (d) lower-left x-coordinate of figure",
        paste0("#define XORIGIN ",as.numeric(xorigin)," "),
        "// (d) lower-left y-coordinate of figure",
        paste0("#define YORIGIN ",as.numeric(yorigin)," "),
        "// (d) scale for x direction",
        paste0("#define XSCALE ",as.numeric(xscale)," "),
        "// (d) scale for y direction",
        paste0("#define YSCALE ",as.numeric(yscale)," "),
        "// (d) angle for labels atop figure (in [0,180])",
        paste0("#define ANGLE_LABEL_ATOP ",as.numeric(toplabangle)," "),
        "// (d) angle for labels below figure (in [0,180])",
        paste0("#define ANGLE_LABEL_BELOW ",as.numeric(bottomlabangle)," "),
        "// (d) width of 'pen' for rim of box",
        paste0("#define LINEWIDTH_RIM  ",as.numeric(borderlinewidth)," "),
        "// (d) width of 'pen' for separators between grps and for tics",
        paste0("#define LINEWIDTH_SEP ",as.numeric(seplinewidth)," "),
        "// (d) width of 'pen' used for individuals",
        paste0("#define LINEWIDTH_IND ",as.numeric(indlinewidth)," "),
        "// (B) use grayscale instead of colors",
        paste0("#define GRAYSCALE ",as.numeric(grayscale)," "),
        "// (B) print some of the data to the screen",
        paste0("#define ECHO_DATA ",as.numeric(printdata)," "),
        "// (B) print the data as a comment in the ps file",
        paste0("#define REPRINT_DATA ",as.numeric(printdata)," "),
        "// (B) print the name of INFILE_POPQ above the figure",
        "// this option is meant for use only with ORIENTATION=0",
        paste0("#define PRINT_INFILE_NAME ",as.numeric(printtitle)," "),
        "// (B) print ColorBrewer settings in the output file",
        paste0("#define PRINT_COLOR_BREWER ",as.numeric(printcolorbrewer)," "),
        "// this option adds 1689 lines and 104656 bytes to the output and is required if using ColorBrewer colors",
        "",
        "Command line options:",
        "",
        "-d drawparams",
        paste0("-K ",dop_k),
        paste0("-M ",dop_m),
        paste0("-N ",dop_n),
        paste0("-p ",fname_grpq),
        paste0("-i ",fname_indq),
        paste0("-a ",fname_plt),
        paste0("-b ",fname_plb),
        paste0("-c ",fname_col),
        paste0("-o ",fname_out))
      
      dir.create(dirname)
      if(!quiet) cat("-------------------------------\n")
      if(!quiet) cat(paste0("Directory ",dirname," created.\n"))
      setwd(paste0(currwd,"/",dirname))
      if(as.numeric(file.access(paste0(currwd,"/",dirname),2))==-1) stop(paste0("distructExport: Directory ",paste0(currwd,"/",dirname)," has no write permission."))
      
      write.table(grpq,fname_grpq,row.names=FALSE,col.names=FALSE,quote=FALSE,dec=".",sep="\t")
      if(!quiet) cat(paste0(fname_grpq," exported.\n"))
      write.table(indq,fname_indq,row.names=FALSE,col.names=FALSE,quote=FALSE,dec=".",sep="\t")
      if(!quiet) cat(paste0(fname_indq," exported.\n"))
      if(!any(is.na(grplabbottom))) 
      {
        write.table(plb,fname_plb,row.names=FALSE,col.names=FALSE,quote=FALSE,dec=".",sep="\t")
        if(!quiet) cat(paste0(fname_plb," exported.\n"))
      }
      if(!any(is.na(grplabtop))) 
      {
        write.table(plt,fname_plt,row.names=FALSE,col.names=FALSE,quote=FALSE,dec=".",sep="\t")
        if(!quiet) cat(paste0(fname_plt," exported.\n"))
      } 
      write.table(colsdf,fname_col,row.names=FALSE,col.names=FALSE,quote=FALSE,dec=".",sep="\t")
      if(!quiet) cat(paste0(fname_col," exported.\n"))
      writeLines(params,"drawparams")
      if(!quiet) cat(paste0("drawparams exported.\n"))
      
      setwd(currwd)
    }
  }
  if(!quiet) cat("===============================\n")
}

# distructColours --------------------------------------------------------------

#' @title Vector of 90 Distruct colours
#' @description Vector of 90 Distruct colours.
#' @return Returns a character vector of 90 colours recognised by Distruct.
#' @aliases distructColors
#' @examples 
#' distructColours();
#' distructColours()[1:5]; 
#' distructColours()[10:15];
#' @export
#' 
distructColours <- distructColors <- function()
{
  return(c("orange","blue","yellow","pink","green","purple","red","light_green","dark_blue",
           "light_purple","light_yellow","brown","light_blue","olive_green","peach","sea_green",
           "yellow_green","blue_purple","blue_green","gray","dark_green","light_gray","red2",
           "light_blue2","light_orange","dark_gray","light_pink","dark_brown","dark_orange",
           "dark_purple","white",paste0("color",32:60),paste0("color",101:130)))
}

# End --------------------------------------------------------------------------
