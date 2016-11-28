#pophelper v1.2.1
#05-Sep-2016

#check packages
pkgs <- c("akima","fields","grid","gridExtra","ggplot2","gtable","PBSmapping","spatstat","tidyr")
if(any(!pkgs %in% installed.packages()))
{
  warning(paste0("Package(s) '",paste0(pkgs[which(!pkgs %in% installed.packages())],collapse=", "),"' is not installed."))
}
rm(pkgs)

#compiler::enableJIT(3)

# utils::suppressForeignCheck(c("k", "elpdmean","geom_path","geom_point","geom_errorbar","elpdmax","elpdmin","theme_bw","labs","theme",
#                             "element_text","element_blank","lnk1","lnk1max","lnk1min","lnk2","lnk2max","lnk2min","deltaK","aes","ind",
#                             "value","variable","geom_bar","scale_x_discrete","scale_y_continuous","scale_fill_manual","labs","theme",
#                             "element_blank","element_line","element_text"))

#FUNCTION getColours
#' Internal: Get Colours
#' @description Internal: Generate colours based on number of K.
#' @param k A numeric indicating the number of colours required
#' @return Returns a character vector of k colours in hexadecimal format
#' @details Colours 1 to 12 are custom unique colours. Colours beyond 15 are generated from colour ramp \code{rich.colors()} from package \code{gplots}.
# @export
#' 
getColors <- getColours <- function(k)
{
  if (length(k) > 1) stop("getColours: Input has than one value. Argument k needs a single integer as input.")
  if (!is.numeric(k)) stop("getColours: Input is non-numeric. Argument k needs a single integer as input.")
  col1 <- c("#2121D9", "#9999FF", "#DF0101", "#04B404", "#FFFB23", "#FF9326", "#A945FF", "#0089B2", "#B26314", "#610B5E", "#FE2E9A", "#BFF217")
  if (k <= 12) return(col1[1:k])
  if (k > 12) 
    {
    cr <- colorRampPalette(colors=c("#000040FF", "#00004FFF", "#000060FF", "#000074FF", "#000088FF", "#00009DFF", "#0000B2FF",
    "#0000C6FF", "#000CD8FF", "#0022E7FF", "#0037F3FF", "#004BFBFF", "#005EFFFF", "#0070FEFF",
    "#0081F8FF", "#0091EEFF", "#00A0E0FF", "#00ADCFFF", "#00BABCFF", "#00C6A7FF", "#01D092FF",
    "#02DA7EFF", "#03E26AFF", "#07E958FF", "#0EF047FF", "#1BF539FF", "#31F92CFF", "#54FC22FF",
    "#80FE1AFF", "#ABFF13FF", "#CEFF0EFF", "#E4FE0AFF", "#F1FB07FF", "#F8F805FF", "#FCF403FF",
    "#FDEE02FF", "#FEE801FF", "#FFE001FF", "#FFD801FF", "#FFCE00FF", "#FFC300FF", "#FFB800FF",
    "#FFAB00FF", "#FF9D00FF", "#FF8E00FF", "#FF7E00FF", "#FF6D00FF", "#FF5B00FF", "#FF4700FF",
    "#FF3300FF"),space="rgb")
    return(cr(k))
    }
}

#-------------------------------------------------------------------------------

# FUNCTION checkRuns
#' Internal: Check if a selected run is STRUCTURE, TESS, MATRIX or TAB file.
#' @description Internal: Check if a selected run is STRUCTURE, TESS, MATRIX or TAB file.
#' @param files A character or character vector of one or more input text files
#' @param warn A logical indicating if a warning be displayed for file those are not STRUCTURE, TESS or MATRIX file.
#' @return A character or character vector indicating 'STRUCTURE', 'TESS', 'MATRIX' or 'TAB' for all selected files.
# @export
#' 
checkRuns <- function(files=NULL, warn=FALSE)
{
  if (is.null(files)) stop("checkRuns: Input is empty.")
  len1 <- length(files)
  
  
  checkvec <- rep("UNIDENTIFIED",length=len1)
  subtype <- rep(NA,length=len1)
  for(i in 1:len1)
  {
    chk <- FALSE
    read1 <- readLines(files[i], n=7, warn = FALSE)
    
    #read TESS file
    chk <- grepl("ESTIMATED CLUSTERING PROBABILITIES", toupper(read1)[1])
    if (chk)
    {
      checkvec[i] <- "TESS"
    }
    
    #read STRUCTURE file
    if (!chk)
    {
      chk <- grepl("STRUCTURE BY PRITCHARD", toupper(read1)[4])
      if (chk)
      {
        checkvec[i] <- "STRUCTURE"
      }
    }
    rm(read1)
    
    #read MATRIX/TAB files
    if (!chk)
    {
      seps <- c("","\t",",")
      subtypes <- c("SPACE","TAB","COMMA")
      k=1
      while(!chk)
      {
        if(class(try(suppressWarnings(read.table(files[i],header=FALSE,sep=seps[k],nrows=1,quote="",stringsAsFactors = FALSE))))!="try-error")
        {
          df <- read.table(files[i],header=FALSE,sep=seps[k],nrows=1,quote="",stringsAsFactors = FALSE)
          if(all(sapply(df, is.numeric))) {
            checkvec[i] <- "MATRIX"
            subtype[i] <- subtypes[k]
            chk <- TRUE
          }else{
            if((ncol(df) > 2) && (is.character(df[,1])))
            {
              checkvec[i] <- "TAB"
              chk <- TRUE
            }
          }
        }
        k=k+1
        if(k>3)
        {
          break
        }
      }
    }
    if((!chk) && warn) warning(paste0("checkRuns: ",files[i]," is not a STRUCTURE, TESS, MATRIX or TAB file."))
  }
  return(list(type=checkvec,subtype=subtype))
}


#-------------------------------------------------------------------------------

# FUNCTION unitConverter
#' Internal: Convert between dimension units
#' @description Internal: Convert value between dimension units
#' @param value A numeric value or numeric vector to convert
#' @param fromunit A character indicating the current unit of the value. Options are "cm", "mm", "in" or "px".
#' @param tounit A character indicating the unit to change to. Options are "cm", "mm", "in" or "px".
#' @param res A numeric indicating the resolution for pixel conversion. This should be in PPI (pixels per inch).
#' @return Returns a numeric value or numeric vector in changed units.
# @export
unitConverter <- function(value=NA, fromunit=NA, tounit=NA, res=NA)
{
  #check
  if (all(is.na(value))) stop("unitConverter: Argument value is empty.")
  if (is.na(fromunit)) stop("unitConverter: Argument fromunit is empty.")
  if (is.na(tounit)) stop("unitConverter: Argument tounit is empty.")
  
  if (fromunit=="cm")
  {
    if (tounit == "cm") outvalue <- value
    if (tounit == "mm") outvalue <- round(value*10,2)
    if (tounit == "in") outvalue <- round(value*0.3937,2)
    if (tounit == "px")
    {
      if (is.na(res)) stop("unitConverter: Argument res is empty.")
      #convert res to 1 cm
      pxpercm <- res/2.54
      outvalue <- round(pxpercm*value,0)
    }
  }
  
  if (fromunit=="mm")
  {
    if (tounit == "mm") outvalue <- value
    if (tounit == "cm") outvalue <- round(value/10,2)
    if (tounit == "in") outvalue <- round(value*0.03937,2)
    if (tounit == "px")
    {
      if (is.na(res)) stop("unitConverter: Argument res is empty.")
      #convert res to 1 mm
      pxpermm <- res/25.4
      outvalue <- round(pxpermm*value,0)
    }
  }
  
  if (fromunit=="in")
  {
    if (tounit == "in") outvalue <- value
    if (tounit == "cm") outvalue <- round(value*2.54,2)
    if (tounit == "mm") outvalue <- round(value*0.254,2)
    if (tounit == "px")
    {
      if (is.na(res)) stop("unitConverter: Argument res is empty.")
      outvalue <- round(res*value,0)
    }
  }
  
  #check this part
  if (fromunit=="px")
  {
    if (tounit == "px") outvalue <- value
    if (is.na(res)) stop("unitConverter: Argument res is empty.")
    
    if (tounit == "cm")
    {
      pxpercm <- res/2.54
      outvalue <- value/pxpercm
    }
    
    if (tounit == "mm")
    {
      pxpermm <- res/25.4
      outvalue <- value/pxpermm
    }
    
    if (tounit == "in") outvalue <- value/res
    
  }
  
  return(outvalue)
}

#-------------------------------------------------------------------------------

#FUNCTION tabulateRunsStructure
#' Tabulate STRUCTURE runs
#' @description Creates a table from STRUCTURE output files with various STRUCTURE parameters.
#' @param files A character or vector of STRUCTURE output files to be tabulated. On windows, use \code{choose.files(multi = TRUE)} for interactive selection.
#' @param writetable A logical TRUE or FALSE. Set to FALSE by default. Setting to TRUE writes the output table to the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @param sorttable A logical indicating if the output table needs to be sorted. Default set to TRUE. Sorts table by loci, ind and K.
#' @param quiet A logical TRUE or FALSE. Set to FALSE by default to print number of selected files. If set to TRUE, then number of selected files are not printed.
#' @return Returns a dataframe with all runs sorted by loci, ind and K (if sorttable = T). The table has 10 columns namely file name, value of K, number of individuals, number of loci, estimated ln probability of data, mean value of ln likelihood, variance of ln likelihood, mean value of alpha, number of burn-in and number of repeats. Missing values are given NA.
#' The row numbers of the output table denotes the file number selected. This is helpful if a particular file from the table needs to 
#' be identified in the selection vector.
#' @seealso \code{\link{tabulateRunsTess}}, \code{\link{tabulateRunsMatrix}}
#' @examples 
#' slist <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' tabulateRunsStructure(files=slist)
#  @import xlsx
#' @export
#' 
tabulateRunsStructure <- function(files = NULL, writetable = FALSE, sorttable = TRUE, quiet = FALSE)
{
  #if no files chosen, stop excecution, give error message
  if (length(files) == 0) stop("tabulateRunsStructure: No input files.")
  if(!is.logical(writetable)) stop("tabulateRunsStructure: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(sorttable)) stop("tabulateRunsStructure: Argument 'sorttable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(quiet)) stop("tabulateRunsStructure: Argument 'quiet' not set correctly. Set as TRUE or FALSE.")
  #check data format
  #if (exportdataformat != "excel" && exportdataformat != "txt") stop("tabulateRunsStructure: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")
  
  #get filenames from selection
  filenames <- basename(files)
  #number of files selected
  number <- length(filenames)
  if (!quiet) cat(paste0("Number of files selected: ", number, "\n"))
  
  #check file
  if (any(pophelper:::checkRuns(files)$type != "STRUCTURE")) stop("tabulateRunsStructure: Input contains one or more non-STRUCTURE files./Incorrect input format.")
  
  #loop to make dataframe with filenames and other variables
  
  ind <- vector(length = number, mode = "numeric")
  k <- vector(length = number, mode = "numeric")
  loci <- vector(length = number, mode = "numeric")
  burnin <- vector(length = number, mode = "numeric")
  reps <- vector(length = number, mode = "numeric")
  elpd <- vector(length = number, mode = "numeric")
  mvll <- vector(length = number, mode = "numeric")
  vll <- vector(length = number, mode = "numeric")
  #mva <- vector(length = number, mode = "numeric")
  
  i <- 1
  for (i in i:number)
  {
    #read STRUCTURE file & error check
    file1 <- readLines(files[i], warn = FALSE)
    
    #read files
    #chk1 <- grep("STRUCTURE", toupper(file1[4]))
    #if (length(chk1) == 0) stop("tabulateRunsStructure: Input not suitable STRUCTURE file/Incorrect input format.")
    
    #find individuals and get number of individuals
    ind[i] <- as.numeric(base::gsub("\\D", "", grep("\\d individuals", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)[1]))
    if (is.na(ind[i])) cat(paste0("Number of individuals is NA in file: ", filenames[i],"\n"))
    #get value of k & error check
    k[i] <- as.numeric(base::gsub("\\D", "", grep("\\d populations assumed", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)[1]))
    if (is.na(k[i])) cat(paste0("Value of K is NA in file: ", filenames[i],"\n"))
    #get number of loci & error check
    loci[i] <- as.numeric(base::gsub("\\D", "", grep("\\d loci", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)[1]))
    if (is.na(loci[i])) cat(paste0("Number of Loci is NA in file: ", filenames[i], "\n"))
    #get burn-in value & error check
    burnin[i] <- as.numeric(base::gsub("\\D", "", grep("\\d Burn-in period", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)[1]))
    if (is.na(burnin[i])) cat(paste0("Burn-in value is NA in file: ", filenames[i], "\n"))
    #get burn-in value & error check
    reps[i] <- as.numeric(base::gsub("\\D", "", grep("\\d Reps", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)[1]))
    if (is.na(reps[i])) cat(paste0("Reps value is NA in file: ", filenames[i], "\n"))
    #get est ln prob of data & error check
    elpd[i] <- as.numeric(base::gsub("=", "", base::gsub("Estimated Ln Prob of Data", "", grep("Estimated Ln Prob of Data", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)[1])))
    if (is.na(elpd[i])) cat(paste0("Estimated Ln Prob of Data is NA in file: ", filenames[i], "\n"))
    #get mn value of ln likelihood & error check
    mvll[i] <- as.numeric(base::gsub("=", "", base::gsub("Mean value of ln likelihood", "", grep("Mean value of ln likelihood", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)[1])))
    if (is.na(mvll[i])) cat(paste0("Mean value of ln likelihood is NA in file: ", filenames[i], "\n"))
    #get Variance of ln likelihood else NA
    vll[i] <- as.numeric(base::gsub("=", "", base::gsub("Variance of ln likelihood", "", grep("Variance of ln likelihood", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)[1])))
    if (is.na(vll[i])) cat(paste0("Variance of ln likelihood is NA in file: ", filenames[i], "\n"))
    #get Mean value of alpha
    # mvat <- grep("Mean value of alpha", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)
    # if (length(mvat) == 0) {cat(paste0("Mean value of alpha is NA in file: ", filenames[i], "\n")); mva[i] <- NA}
    # if (length(mvat) > 1)
    # {
    #   mva[i] <- mean(as.numeric(sub("= ","",substr(mvat,regexpr("=",mvat),regexpr("$",mvat)))))
    #   if (is.na(mva[i])) cat(paste0("Mean value of alpha is NA in file: ", filenames[i], "\n"))
    # }else{
    #   mva[i] <- as.numeric(sub("= ","",substr(mvat,regexpr("=",mvat),regexpr("$",mvat))))
    #   if (is.na(mva[i])) cat(paste0("Mean value of alpha is NA in file: ", filenames[i], "\n"))
    # }
  }
  
  #make dataframe container
  main <- data.frame(file = filenames, k = as.numeric(k), ind = as.numeric(ind),
                     loci = as.numeric(loci), elpd = as.numeric(elpd), 
                     mvll = as.numeric(mvll), vll = as.numeric(vll),
                     burnin = as.numeric(burnin), 
                     reps = as.numeric(reps),stringsAsFactors = FALSE)
  #mva = as.numeric(mva), 
  #sort table on loci, ind, K
  if(sorttable) main <- main[with(main, order(loci, ind, k)), ]
  
  #write table if opted
  if (writetable)
  {
    #if (exportdataformat == "txt")
    #{
      write.table(main, "tabulateRunsStructure.txt", quote = FALSE, row.names = FALSE, sep = "\t", dec = ".")
      cat("tabulateRunsStructure.txt exported.\n") 
    #}
    #if (exportdataformat == "excel")
    #{
      #xlsx::write.xlsx2(main,file = "tabulateRunsStructure.xlsx",sheetName = "tabulateRunsStructure",row.names = FALSE)
      #cat("tabulateRunsStructure.xlsx exported.\n")
    #}
  }
  return(main)
}

#-------------------------------------------------------------------------------

#FUNCTION tabulateRunsTess
#' Tabulate TESS runs
#' @description Creates a table from TESS output files with filenames, K and number of individuals.
#' @param files A character vector of TESS cluster files to be tabulated. On windows, use \code{choose.files(multi = TRUE)} for interactive selection. Use \code{collectRunsTess()} to collect TESS runs from multiple folders into one.
#' @param writetable A logical indicating if output table must be exported as a tab-delimited text file in the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @param sorttable A logical indicating if output table is to be sorted. Sorts table by ind and K.
#' @param quiet A logical indicating if a message showing the number of selected files is displayed. The message is supressed if set to T.
#' @return Returns a dataframe with filenames, K and number of individuals of all runs sorted by ind and K (if \code{sorttable = T}).
#' The row numbers of the output table denotes the file number selected. This is helpful if a particular file from the table needs to 
#' be identified in the selection vector.
#' @seealso \code{\link{tabulateRunsStructure}}, \code{\link{tabulateRunsMatrix}}
#' @examples 
#' tlist <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' tabulateRunsTess(files=tlist)
#  @import xlsx
#' @export
#'
tabulateRunsTess <- function(files = NULL, writetable = FALSE, sorttable = TRUE, quiet = FALSE)
{
  #if no files chosen, stop excecution, give error message
  if (is.null(files) | (length(files) == 0)) stop("tabulateRunsTess: No input files.")
  if(!is.logical(writetable)) stop("tabulateRunsTess: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(sorttable)) stop("tabulateRunsTess: Argument 'sorttable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(quiet)) stop("tabulateRunsTess: Argument 'quiet' not set correctly. Set as TRUE or FALSE.")
  #check data format
  #if (exportdataformat != "excel" && exportdataformat != "txt") stop("tabulateRunsTess: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")
  
  #check file
  if (any(pophelper:::checkRuns(files)$type != "TESS")) stop("tabulateRunsTess: Input contains one or more non-TESS files./Incorrect input format.")
  
  #get filenames from selection
  filenames <- basename(files)
  #number of files selected
  number <- length(filenames)
  if (!quiet) cat(paste0("Number of files selected: ", number, "\n"))
  #make dataframe container
  main <- data.frame(file = filenames, k = 1:number, ind = 1:number,stringsAsFactors = FALSE)
  
  #loop to make dataframe with filenames and other variables
  k <- vector(length = number, mode = "numeric")
  ind <- vector(length = number, mode = "numeric")
  i <- 1
  for (i in i:number)
  {
    #read file & error check
    df1 <- pophelper::runsToDfTess(files[i])
    #get k
    k[i] <- ncol(df1)
    #get ind
    ind[i] <- nrow(df1)
  }
  
  #create df
  main <- data.frame(file = filenames, k = k, ind = ind,stringsAsFactors = FALSE)
  
  #sort table on K
  if(sorttable) main <- main[with(main, order(ind, k)), ]
  
  #write table if opted
  if (writetable)
  {
    #if (exportdataformat == "txt")
    #{
      write.table(main, "tabulateRunsTess.txt", quote = FALSE, row.names = FALSE, sep = "\t", dec = ".")
      cat("tabulateRunsTess.txt exported.\n")
    #}
    #if (exportdataformat == "excel")
    #{
      #xlsx::write.xlsx2(main,file = "tabulateRunsTess.xlsx",sheetName = "tabulateRunsTess",row.names = FALSE)
      #cat("tabulateRunsTess.xlsx exported.\n")
    #}
    
  }
  
  return(main)
}

#-------------------------------------------------------------------------------

#FUNCTION tabulateRunsAdmixture
#' Tabulate Admixture runs (Deprecated)
#' @description DEPRECATED. USE tabulateRunsMatrix(). Creates a table from ADMIXTURE output files with filenames, K and number of individuals.
#' @param files A character vector of ADMIXTURE cluster files to be tabulated. On windows, use \code{choose.files(multi = TRUE)} for interactive selection.
#' @param writetable A logical indicating if output table must be exported as a tab-delimited text file in the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @param sorttable A logical indicating if output table is to be sorted. Sorts table by ind and K.
#' @param quiet A logical indicating if a message showing the number of selected files is displayed. The message is supressed if set to T.
#' @return Returns a dataframe with filenames, K and number of individuals of all runs sorted by ind and K (if \code{sorttable = T}).
#' The row numbers of the output table denotes the file number selected. This is helpful if a particular file from the table needs to 
#' be identified in the selection vector.
#' @seealso \code{\link{tabulateRunsStructure}}, \code{\link{tabulateRunsTess}}
# @import xlsx
#' @export
#'
tabulateRunsAdmixture <- function(files = NULL, writetable = FALSE, sorttable = TRUE, quiet = FALSE)
{
  warning("tabulateRunsAdmixture() is deprecated. Use tabulateRunsMatrix().")
  #if no files chosen, stop excecution, give error message
  if (is.null(files) | (length(files) == 0)) stop("tabulateRunsAdmixture: No input files.")
  if(!is.logical(writetable)) stop("tabulateRunsAdmixture: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(sorttable)) stop("tabulateRunsAdmixture: Argument 'sorttable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(quiet)) stop("tabulateRunsAdmixture: Argument 'quiet' not set correctly. Set as TRUE or FALSE.")
  #check data format
  #if (exportdataformat != "excel" && exportdataformat != "txt") stop("tabulateRunsAdmixture: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")
  
  #check file
  if (any(pophelper:::checkRuns(files)$type != "MATRIX")) stop("tabulateRunsAdmixture: Input contains one or more non-ADMIXTURE files./Incorrect input format.")
  
  #get filenames from selection
  filenames <- basename(files)
  #number of files selected
  number <- length(filenames)
  if (!quiet) cat(paste0("Number of files selected: ", number, "\n"))
  #make dataframe container
  main <- data.frame(file = filenames, k = 1:number, ind = 1:number,stringsAsFactors = FALSE)
  
  #loop to make dataframe with filenames and other variables
  k <- vector(length = number, mode = "numeric")
  ind <- vector(length = number, mode = "numeric")
  i <- 1
  for (i in i:number)
  {
    #read file & error check
    df1 <- pophelper::runsToDfMatrix(files[i])
    #get k
    k[i] <- ncol(df1)
    #get ind
    ind[i] <- nrow(df1)
  }
  
  #create df
  main <- data.frame(file = filenames, k = k, ind = ind,stringsAsFactors = FALSE)
  
  #sort table on K
  if(sorttable) main <- main[with(main, order(ind, k)), ]
  
  #write table if opted
  if (writetable)
  {
    write.table(main, "tabulateRunsAdmixture.txt", quote = FALSE, row.names = FALSE, sep = "\t", dec = ".")
    cat("tabulateRunsAdmixture.txt exported.\n")
  }
  
  return(main)
}

#-------------------------------------------------------------------------------

#FUNCTION tabulateRunsMatrix
#' Tabulate Matrix runs
#' @description Imports tabular data (Admixture, fastStructure etc) and creates a table with filenames, K and number of individuals.
#' @param files A character vector of MATRIX cluster files to be tabulated. See details. On windows, use \code{choose.files(multi = TRUE)} for interactive selection.
#' @param writetable A logical indicating if output table must be exported as a tab-delimited text file in the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @param sorttable A logical indicating if output table is to be sorted. Sorts table by ind and K.
#' @param quiet A logical indicating if a message showing the number of selected files is displayed. The message is supressed if set to T.
#' @return Returns a dataframe with filenames, K and number of individuals of all runs sorted by ind and K (if \code{sorttable = T}). 
#' The row numbers of the output table denotes the file number selected. This is helpful if a particular file from the table needs to 
#' be identified in the selection vector.
#' @details Expected input files are Admixture run files or fastStructure meanQ files. Input files can be any tab-delimited, space-delimited or comma-delimited tabular data without headers.
#' @seealso \code{\link{tabulateRunsStructure}}, \code{\link{tabulateRunsTess}}
#' alist <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' tabulateRunsMatrix(files=alist)
#  @import xlsx
#' @export
#'
tabulateRunsMatrix <- function(files = NULL, writetable = FALSE, sorttable = TRUE, quiet = FALSE)
{
  #if no files chosen, stop excecution, give error message
  if (is.null(files) | (length(files) == 0)) stop("tabulateRunsMatrix: No input files.")
  if(!is.logical(writetable)) stop("tabulateRunsMatrix: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(sorttable)) stop("tabulateRunsMatrix: Argument 'sorttable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(quiet)) stop("tabulateRunsMatrix: Argument 'quiet' not set correctly. Set as TRUE or FALSE.")
  #check data format
  #if (exportdataformat != "excel" && exportdataformat != "txt") stop("tabulateRunsMatrix: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")
  
  #check file
  if (any(pophelper:::checkRuns(files)$type != "MATRIX")) stop("tabulateRunsMatrix: Input contains one or more non-MATRIX files./Incorrect input format.")
  
  #get filenames from selection
  filenames <- basename(files)
  #number of files selected
  number <- length(filenames)
  if (!quiet) cat(paste0("Number of files selected: ", number, "\n"))
  #make dataframe container
  main <- data.frame(file = filenames, k = 1:number, ind = 1:number,stringsAsFactors = FALSE)
  
  #loop to make dataframe with filenames and other variables
  k <- vector(length = number, mode = "numeric")
  ind <- vector(length = number, mode = "numeric")
  i <- 1
  for (i in i:number)
  {
    #read file & error check
    df1 <- pophelper::runsToDfMatrix(files[i])
    #get k
    k[i] <- ncol(df1)
    #get ind
    ind[i] <- nrow(df1)
  }
  
  #create df
  main <- data.frame(file = filenames, k = k, ind = ind,stringsAsFactors = FALSE)
  
  #sort table on K
  if(sorttable) main <- main[with(main, order(ind, k)), ]
  
  #write table if opted
  if (writetable)
  {
    #if (exportdataformat == "txt")
    #{
    write.table(main, "tabulateRunsMatrix.txt", quote = FALSE, row.names = FALSE, sep = "\t", dec = ".")
    cat("tabulateRunsMatrix.txt exported.\n")
    #}
    #if (exportdataformat == "excel")
    #{
    #xlsx::write.xlsx2(main,file = "tabulateRunsMatrix.xlsx",sheetName = "tabulateRunsMatrix",row.names = FALSE)
    #cat("tabulateRunsMatrix.xlsx exported.\n")
    #}
  }
  
  return(main)
}

#-------------------------------------------------------------------------------

#FUNCTION summariseRunsStructure
#' Summarise STRUCTURE runs
#' @description Creates a summary table of several STRUCTURE runs with means and std deviation.
#' @param data A dataframe with tabulated runs. An output from 
#' \code{tabulateRunsStructure()}. Must have minimum 4 columns named k, ind, loci and elpd.
#' @param writetable A logical indicating if the output table is to be exported as a tab-delimited text file in the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @return Returns a dataframe with all values of K sorted by loci, ind and K. The table has 
#' 6 columns namely mean estimated ln probability of data, standard deviation, 
#' value of K, Number of runs for each K, number of individuals, number of loci, 
#' estimated ln probability of data plus standard deviation, estimated ln 
#' probability of data minus standard deviation.
#' @aliases summarizeRunsStructure
#' @seealso \code{\link{summariseRunsTess}}, \code{\link{summariseRunsMatrix}}
#' @examples 
#' slist <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' tr1 <- tabulateRunsStructure(files=slist)
#' summariseRunsStructure(tr1)
# @import xlsx
#' @export
#' 
summariseRunsStructure <- summarizeRunsStructure <- function(data = NULL, writetable = FALSE)
{
  #does df data contain any data
  if (is.null(data) | length(data) == 0) stop("summariseRunsStructure: No input files.")
  if(!is.logical(writetable)) stop("summariseRunsStructure: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  #check data format
  #if (exportdataformat != "excel" && exportdataformat != "txt") stop("summariseRunsStructure: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")
  
  #make sure dataframe
  if(class(data) != "data.frame") stop("summariseRunsStructure: Argument 'data' is not a dataframe.")
  #convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  #is column k available
  if (length(grep("k", colnames(data))) == 0) stop("summariseRunsStructure: Column k not available.")
  #is column ind available
  if (length(grep("ind", colnames(data))) == 0) stop("summariseRunsStructure: Column ind not available.")
  #is column loci available
  if (length(grep("loci", colnames(data))) == 0) stop("summariseRunsStructure: Column loci not available.")
  #is column elpd available
  if (length(grep("elpd", colnames(data))) == 0) stop("summariseRunsStructure: Column elpd not available.")
  #check
  if (nrow(data) < 2) stop("summariseRunsStructure: At least 2 runs are required for this function.")
  
  data1 <- stats::aggregate(elpd ~ loci + ind + k,data = data,length)
  colnames(data1)[4] <- "runs"
  data2 <- aggregate(elpd ~ loci + ind + k,data = data,FUN=function(x) c(elpdmean =mean(x,na.rm = T), elpdsd=sd(x,na.rm = T),elpdmin = min(x,na.rm = T),elpdmax = max(x,na.rm = T) ) )[,-c(1:3)]
  data1 <- cbind(data1,data2)
  
  #write table if opted
  if (writetable)
  {
    #if (exportdataformat == "txt")
    #{
      write.table(data1, "summariseRunsStructure.txt", quote = FALSE, row.names = FALSE, sep = "\t", dec = ".")
      cat("summariseRunsStructure.txt exported.\n")
    #}
    #if (exportdataformat == "excel")
    #{
      #xlsx::write.xlsx2(data1,file = "summariseRunsStructure.xlsx",sheetName = "summariseRunsStructure",row.names = FALSE)
      #cat("summariseRunsStructure.xlsx exported.\n")
    #}
  }
  
  return(data1)
}

#-------------------------------------------------------------------------------

#FUNCTION summariseRunsTess
#' Summarise TESS runs
#' @description Creates a summary table of several TESS runs with k, number of runs and individuals.
#' @param data A dataframe with tabulated runs. An output from \code{tabulateRunsTess()}. Must have minimum 2 columns named k and ind.
#' @param writetable A logical indicating if the output table is to be exported as a tab-delimited text file in the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @return Returns a dataframe with all values of K sorted by K. The table has 3 columns namely value of K, number of runs for each K and number of individuals.
#' @aliases summarizeRunsTess
#' @seealso \code{\link{summariseRunsStructure}}, \code{\link{summariseRunsMatrix}}
# @import xlsx
#' @examples 
#' tlist <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' tr1 <- tabulateRunsTess(files=tlist)
#' summariseRunsTess(tr1)
#' @export
#' 
summariseRunsTess <- summarizeRunsTess <- function(data = NULL, writetable = FALSE)
{
  #does df data contain any data
  if (is.null(data) | length(data) == 0) stop("summariseRunsTess: No input files.")
  if(!is.logical(writetable)) stop("summariseRunsTess: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  #check data format
  #if (exportdataformat != "excel" && exportdataformat != "txt") stop("summariseRunsTess: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")
  
  #make sure dataframe
  if(class(data) != "data.frame") stop("summariseRunsTess: Argument 'data' is not a dataframe.")
  #convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  #is column k available
  if (length(grep("k", colnames(data))) == 0) stop("summariseRunsTess: Column k not available.")
  #is column ind available
  if (length(grep("ind", colnames(data))) == 0) stop("summariseRunsTess: Column ind not available.")
  #is column loci available
  #check
  if (nrow(data) < 2) stop("summariseRunsTess: At least 2 runs are required for this function.")
  
  data1 <- stats::aggregate(. ~ ind + k,data = data, length)
  colnames(data1)[3] <- "runs"

  #write table if opted
  if (writetable)
  {
    #if (exportdataformat == "txt")
    #{
      write.table(data1, "summariseRunsTess.txt", quote = FALSE, row.names = FALSE, sep = "\t", dec = ".")
      cat("summariseRunsTess.txt exported.\n")
    #}
    #if (exportdataformat == "excel")
    #{
      #xlsx::write.xlsx2(data1,file = "summariseRunsTess.xlsx",sheetName = "summariseRunsTess",row.names = FALSE)
      #cat("summariseRunsTess.xlsx exported.\n")
    #}
  }
  
  return(data1)
}

#-------------------------------------------------------------------------------

#FUNCTION summariseRunsAdmixture
#' Summarise ADMIXTURE runs
#' @description DEPRECATED. USE summariseRunsMatrix(). Creates a summary table of two or more ADMIXTURE runs with k, number of runs and individuals.
#' @param data A dataframe with tabulated runs. An output from \code{tabulateRunsAdmixture()}. Must have minimum 2 columns named k and ind.
#' @param writetable A logical indicating if the output table is to be exported as a tab-delimited text file in the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @return Returns a dataframe with all values of K sorted by K. The table has 3 columns namely value of K, number of runs for each K and number of individuals.
#' @aliases summarizeRunsAdmixture
#' @seealso \code{\link{summariseRunsStructure}}, \code{\link{summariseRunsTess}}
# @import xlsx
#' @export
#' 
summariseRunsAdmixture <- summarizeRunsAdmixture <- function(data = NULL, writetable = FALSE)
{
  warning("summariseRunsAdmixture() is deprecated. Use summariseRunsMatrix()")
  #does df data contain any data
  if (is.null(data) | length(data) == 0) stop("summariseRunsAdmixture: No input files.")
  if(!is.logical(writetable)) stop("summariseRunsAdmixture: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")

  #make sure dataframe
  if(class(data) != "data.frame") stop("summariseRunsAdmixture: Argument 'data' is not a dataframe.")
  #convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  #is column k available
  if (length(grep("k", colnames(data))) == 0) stop("summariseRunsAdmixture: Column k not available.")
  #is column ind available
  if (length(grep("ind", colnames(data))) == 0) stop("summariseRunsAdmixture: Column ind not available.")
  #is column loci available
  #check
  if (nrow(data) < 2) stop("summariseRunsAdmixture: At least 2 runs are required for this function.")
  
  data1 <- stats::aggregate(. ~ ind + k,data = data, length)
  colnames(data1)[3] <- "runs"
  
  #write table if opted
  if (writetable)
  {
    write.table(data1, "summariseRunsAdmixture.txt", quote = FALSE, row.names = FALSE, sep = "\t", dec = ".")
    cat("summariseRunsAdmixture.txt exported.\n")
  }
  
  return(data1)
}

#-------------------------------------------------------------------------------

#FUNCTION summariseRunsMatrix
#' Summarise MATRIX runs
#' @description Creates a summary table of two or more MATRIX runs with k, number of runs and individuals.
#' @param data A dataframe with tabulated runs. An output from \code{tabulateRunsMatrix()}. Must have minimum 2 columns named k and ind.
#' @param writetable A logical indicating if the output table is to be exported as a tab-delimited text file in the working directory.
#  @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @return Returns a dataframe with all values of K sorted by K. The table has 3 columns namely value of K, number of runs for each K and number of individuals.
#' @aliases summarizeRunsMatrix
#' @seealso \code{\link{summariseRunsStructure}}, \code{\link{summariseRunsTess}}
#  @import xlsx
#' @examples 
#' alist <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' tr1 <- tabulateRunsMatrix(files=alist)
#' summariseRunsMatrix(tr1)
#' @export
#' 
summariseRunsMatrix <- summarizeRunsMatrix <- function(data = NULL, writetable = FALSE)
{
  #does df data contain any data
  if (is.null(data) | length(data) == 0) stop("summariseRunsMatrix: No input files.")
  if(!is.logical(writetable)) stop("summariseRunsMatrix: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  #check data format
  #if (exportdataformat != "excel" && exportdataformat != "txt") stop("summariseRunsMatrix: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")
  
  #make sure dataframe
  if(class(data) != "data.frame") stop("summariseRunsMatrix: Argument 'data' is not a dataframe.")
  #convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  #is column k available
  if (length(grep("k", colnames(data))) == 0) stop("summariseRunsMatrix: Column k not available.")
  #is column ind available
  if (length(grep("ind", colnames(data))) == 0) stop("summariseRunsMatrix: Column ind not available.")
  #is column loci available
  #check
  if (nrow(data) < 2) stop("summariseRunsMatrix: At least 2 runs are required for this function.")
  
  data1 <- stats::aggregate(. ~ ind + k,data = data, length)
  colnames(data1)[3] <- "runs"
  
  #write table if opted
  if (writetable)
  {
    #if (exportdataformat == "txt")
    #{
    write.table(data1, "summariseRunsMatrix.txt", quote = FALSE, row.names = FALSE, sep = "\t", dec = ".")
    cat("summariseRunsMatrix.txt exported.\n")
    #}
    #if (exportdataformat == "excel")
    #{
    #xlsx::write.xlsx2(data1,file = "summariseRunsMatrix.xlsx",sheetName = "summariseRunsMatrix",row.names = FALSE)
    #cat("summariseRunsMatrix.xlsx exported.\n")
    #}
  }
  
  return(data1)
}

#-------------------------------------------------------------------------------

#FUNCTION evannoMethodStructure
#' Perform the Evanno method for STRUCTURE runs.
#' @description The Evanno method for detecting the appropriate number of population clusters from STRUCTURE results. Creates table and figure with 
#' Evanno method derivatives. Refer to return for detailed list of columns. See details for Evanno method reference.
#' @param data A dataframe with summarised runs. An output from \code{summariseRunsStructure()}. Must have minimum 7 columns named elpdmean, elpdsd, k, runs, loci, elpdmax and elpdmin.
#' @param writetable A logical indicating if the output table is to be exported as a file in the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @param exportplot A logical indicating if the Evanno plots are to be exported as an image in the working directory. If Evanno method cannot be computed, a kPlot (elpd over k) is exported instead.
#' @param na.rm Default set to FALSE. Does not remove NAs for plot and this 
#' generates warnings from \code{ggplot}. If set to TRUE, NAs are removed before 
#' plotting and warning messages from \code{ggplot} are avoided.
#' @param imgtype A character indicating the type of exported image. Default set to png. Other possible 
#' options are jpeg or pdf.
#' @param basesize A numeric indicating the base size of various plot elements such as point size, line thickness etc. Increase basesize with larger figure dimensions. Defaults to 5.
#' @param height A numeric denoting the height of exported image. Default units in 'cm'. If imgtype is 
#' pdf, height must be in inches.
#' @param width A numeric denoting the width of exported image. Default units in 'cm'. If imgtype is 
#' pdf, height must be in inches.
#' @param res A numeric denoting the resolution of exported image. Default set to 200. If imgtype is 
#' pdf, this option does not apply.
#' @param units A character denoting the unit of measure of the export image. By default, units is set to 'cm' for png and jpeg and to 'in' if imgtype is 'pdf'. 
#' Other options include 'px', 'in' and 'mm' etc. 
#' @return Returns a dataframe with all values sorted by K. The table has 16 
#' columns namely Mean estimated ln probability of data, Standard deviation, 
#' Value of K, Number of runs for each K, Number of runs for each K, Number of 
#' individuals for each K, Number of loci for each K, Estimated ln probability 
#' of data plus standard deviation, Estimated ln probability of data minus 
#' standard deviation, First derivative, Max error of first derivative, Min 
#' error of first derivative, Second derivative, Max error of second derivative, 
#' Min error of second derivative and the Third derivative.
#' @details The Evanno method is based on the paper: Evanno, G., Regnaut, S., 
#' and Goudet, J. (2005). Detecting the number of clusters of individuals using 
#' the software STRUCTURE: a simulation study. Molecular ecology, 14(8), 
#' 2611-2620. The Evanno plot generated from this function can be recreated 
#' from the returned dataframe if furthur customisation is required.
#' @examples 
#' \dontrun{
#' slist <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' tr1 <- tabulateRunsStructure(files=slist)
#' sr1 <- summariseRunsStructure(tr1)
#' evannoMethodStructure(sr1)
#' evannoMethodStructure(data=sr1,exportplot=T)
#' }
#  @import xlsx
#' @import grid
#' @import gridExtra
#' @export
#' 
evannoMethodStructure <- function(data = NULL, writetable = FALSE, exportplot = FALSE, na.rm = TRUE, imgtype = "png", basesize=NA, height = NA, width = NA, res = NA, units = NA)
{
  #does df data contain any data
  if (is.null(data) | length(data) == 0) stop("evannoMethodStructure: No input files.")
  if(!is.logical(writetable)) stop("evannoMethodStructure: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(exportplot)) stop("evannoMethodStructure: Argument 'exportplot' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(na.rm)) stop("evannoMethodStructure: Argument 'na.rm' not set correctly. Set as TRUE or FALSE.")
  imgtype <- tolower(imgtype)
  if (imgtype != "png" && imgtype != "pdf" && imgtype != "jpeg") stop("evannoMethodStructure: Argument 'imgtype' set incorrectly. Options are 'png', 'jpeg' or 'pdf'.")
  height1 <- height
  width1 <- width
  if (is.na(units)) units <- "cm"
  if (is.na(res)) res <- 300

  #check data format
  #if (exportdataformat != "excel" && exportdataformat != "txt") stop("evannoMethodStructure: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")
  

  #make sure dataframe
  data <- as.data.frame(data,stringsAsFactors = FALSE)
  #convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  cold <- colnames(data)
  
  #is column loci available
  if (!"loci" %in% cold) stop("evannoMethodStructure: Column loci not available.")
  #is column ind available
  if (!"ind" %in% cold) stop("evannoMethodStructure: Column ind not available.")
  #is column k available
  if (!"k" %in% cold) stop("evannoMethodStructure: Column k not available.")
  #is column runs available
  if (!"runs" %in% cold) stop("evannoMethodStructure: Column runs not available.")
  #is column elpdmean available
  if (!"elpdmean" %in% cold) stop("evannoMethodStructure: Column elpdmean not available.")
  #is column elpdsd available
  if (!"elpdsd" %in% cold) stop("evannoMethodStructure: Column elpdsd not available.")
  #is column minelpd available
  if (!"elpdmin" %in% cold) stop("evannoMethodStructure: Column elpdmin not available.")
  #is column maxelpd available
  if (!"elpdmax" %in% cold) stop("evannoMethodStructure: Column elpdmax not available.")
  
  
  err <- 0
  #atleast 3 values of K
  if (length(data$k) < 3) {cat("Error: The Evanno method not computed. Requires at least 3 values of K.\n"); err <- 1}
  #do loci vary
  if (!all(data$loci[1] == data$loci)) {cat("Error: The Evanno method not computed. Number of loci vary between runs.\n"); err <- 1}
  #do ind vary
  if (!all(data$ind[1] == data$ind)) {cat("Error: The Evanno method not computed. Number of individuals vary between runs.\n"); err <- 1}
  #are k values sequential
  is.sequential <- function(x) all(abs(diff(x)) == 1)
  if (!is.sequential(data$k)) {cat("Error: The Evanno method not computed. Requires increasing sequential values of K.\n"); err <- 1}
  #repeats of k<2
  if (any(data$runs < 2)) warning("evannoMethodStructure: Results may not be meaningful if repeats (runs) for any value of K is less than 2.")
  
  base_size <- basesize
  plotcol <- "grey30"
  plotcol1 <- "steelblue"
  pointsh <- 20
  #linewd <- 0.20
  #pointsz <- 1.5
  #linewd <- base_size*0.04
  #pointsz <- base_size*0.3
  
  if (err == 1)
  {
    if (exportplot)
    {      
      #create plots list
      plist <- vector("list",1)
      
      #settings for kPlot
      if (is.na(height)) height1 <- 7
      if (is.na(width)) width1 <- 7
      if(is.na(basesize)) base_size <- round((5*height1)/7,1)
      
      if (is.na(height) && imgtype == "pdf") height1 <- pophelper:::unitConverter(value = height1, fromunit = "cm", tounit = "in", res = res)
      if (is.na(width) && imgtype =="pdf") width1 <- pophelper:::unitConverter(value = width1, fromunit = "cm", tounit = "in", res = res)
      if (!is.na(height) && imgtype == "pdf" && units != "in") height1 <- pophelper:::unitConverter(value = height, fromunit = units, tounit = "in", res = res)
      if (!is.na(width) && imgtype =="pdf" && units != "in") width1 <- pophelper:::unitConverter(value = width, fromunit = units, tounit = "in", res = res)
      
      plist[[1]] <- ggplot2::ggplot(data, aes(x = k, y = elpdmean))+
        geom_path(colour = plotcol1, size = base_size*0.04, na.rm = na.rm)+
        geom_point(colour = plotcol1,fill = plotcol1, size = base_size*0.3, shape = pointsh, na.rm = na.rm)+
        geom_errorbar(aes(x = k, ymax = elpdmax, ymin = elpdmin, width = 0.2), size = base_size*0.04, colour = plotcol, na.rm = na.rm)+
        theme_bw(base_size = base_size)+
        labs(x = expression(paste(italic(K))), 
             y = expression(paste("Mean L(", italic(K), ") " %+-% " SD")))+
        theme(legend.position = "none",
              axis.text.y = element_text(angle = 90, hjust = 0.5,size = base_size, colour = plotcol),
              axis.text.x = element_text(size = base_size, colour = plotcol),
              axis.title = element_text(size = base_size+1, colour = plotcol,face = "bold"),
              plot.title = element_text(size = base_size+3, hjust = 0, colour = plotcol),
              axis.ticks = element_blank(),
              panel.border = element_blank(),
              plot.margin = grid::unit(c(0.15,0.15,0.15,0.15),"cm"))
      
      #show plot
      print(plist[[1]])

        #check image imgtype
        if (imgtype == "pdf") pdf(file = "kPlot.pdf", height = height1, width = width1)
        if (imgtype == "png") png(filename = "kPlot.png", height = height1, width = width1, res = res, units = units, type = "cairo")
        if (imgtype == "jpeg") jpeg(filename = "kPlot.jpg", height = height1, width = width1, res = res, units = units, quality = 100)
        print(plist[[1]])
        dev.off()
        
        if (imgtype == "pdf") cat("kPlot.pdf exported.\n")
        if (imgtype == "png") cat("kPlot.png exported.\n")
        if (imgtype == "jpeg") cat("kPlot.jpg exported.\n")
    }
    stop("evannoMethodStructure: Evanno method not computed.")
  }
  
  #convert dataframe to list
  datal <- as.list(data)
  
  #Loop to get first derivative of l(K) and its sd
  drv1 <- vector(length = nrow(data)-1, mode = "numeric")
  drv1sd <- vector(length = nrow(data)-1, mode = "numeric")
  i <- 1
  len1 <- length(datal$elpdmean)
  while (i < len1)
  {
    drv1[i] <- datal$elpdmean[i+1]-datal$elpdmean[i]
    drv1sd[i] <- abs(datal$elpdsd[i+1]-datal$elpdsd[i])
    i = i+1
  }
  
  #Loop to get second derivative of l(K) and its sd
  drv2 <- vector(length = nrow(data)-2, mode = "numeric")
  drv2sd <- vector(length = nrow(data)-2, mode = "numeric")
  i <- 1
  len1 <- length(drv1)
  while (i < len1)
  {
    drv2[i] <- abs(drv1[i+1]-drv1[i])
    drv2sd[i] <- abs(drv1sd[i+1]-drv1sd[i])
    i = i+1
  }
  
  #add NA to SD vector 1 and 2
  drv1sdf <- c(NA, drv1sd)
  drv2sdf <- c(NA, drv2sd, NA)
  
  datal$drv1 <- c(NA, drv1)
  datal$drv1max <- datal$drv1+drv1sdf
  datal$drv1min <- datal$drv1-drv1sdf
  datal$drv2 <- c(NA, drv2, NA)
  datal$drv2max <- datal$drv2+drv2sdf
  datal$drv2min <- datal$drv2-drv2sdf
  datal$drv3 <- abs(datal$drv2)/datal$elpdsd
  #datal$BestK <- ""
  #bestpos <- (1:length(datal$drv3))[(datal$drv3) == max(datal$drv3, na.rm = TRUE)]
  #bestpos <- bestpos[!is.na(bestpos)]
  #datal$BestK[bestpos] <- "*"
  
  data <- data.frame(datal,stringsAsFactors = FALSE)
  rm(datal)
  colnames(data)[9:15] <- c("lnk1" ,"lnk1max" ,"lnk1min" , "lnk2", "lnk2max", 
                            "lnk2min","deltaK")
  
  #write table if opted
  if (writetable == TRUE | writetable == "T" | writetable == "TRUE")
  {
    #if(exportdataformat == "txt")
    #{
      write.table(data, "evannoMethodStructure.txt", quote = FALSE, row.names = FALSE, sep = "\t", dec = ".")
      cat("evannoMethodStructure.txt exported.\n")
    #}
    
    #if(exportdataformat == "excel")
    #{
      #xlsx::write.xlsx2(data,file = "evannoMethodStructure.xlsx", sheetName = "evannoMethodStructure", row.names = FALSE)
      #cat("evannoMethodStructure.xlsx exported.\n")
    #} 
  }
  
  #show plot
  if (exportplot)
  {
    if (is.na(height)) height1 <- 8
    if (is.na(width)) width1 <- 8
    if(is.na(basesize)) base_size <- round((5*height1)/7,1)
    
    if (is.na(height) && imgtype == "pdf") height1 <- pophelper:::unitConverter(value = height1, fromunit = "cm", tounit = "in", res = res)
    if (is.na(width) && imgtype =="pdf") width1 <- pophelper:::unitConverter(value = width1, fromunit = "cm", tounit = "in", res = res)
    if (!is.na(height) && imgtype == "pdf" && units != "in") height1 <- pophelper:::unitConverter(value = height, fromunit = units, tounit = "in", res = res)
    if (!is.na(width) && imgtype =="pdf" && units != "in") width1 <- pophelper:::unitConverter(value = width, fromunit = units, tounit = "in", res = res)
    
    #create plots list
    plist <- vector("list",4)
    
    #plot1
    plist[[1]] <- ggplot2::ggplot(data, aes(x = k, y = elpdmean))+
      geom_path(colour = plotcol1, size = base_size*0.04, na.rm = na.rm)+
      geom_point(colour = plotcol1,fill = plotcol1, size = base_size*0.3, shape = pointsh, na.rm = na.rm)+
      geom_errorbar(aes(x = k, ymax = elpdmax, ymin = elpdmin, width = 0.2), size = base_size*0.04, colour = plotcol, na.rm = na.rm)+
      theme_bw(base_size = base_size)+
      labs(x = expression(paste(italic(K))), y = expression(paste("Mean L(", italic(K), ") " %+-% " SD")),title = "A")
    
    #plot 2
    plist[[2]] <- ggplot2::ggplot(data, aes(x = k, y = lnk1))+
      geom_path(colour = plotcol1, size = base_size*0.04, na.rm = na.rm)+
      geom_point(colour = plotcol1, fill = plotcol1, size = base_size*0.3, shape = pointsh, na.rm = na.rm)+
      geom_errorbar(aes(x = k, ymax = lnk1max, ymin = lnk1min, width = 0.2), 
                    size = base_size*0.04, colour = plotcol, na.rm = na.rm)+
      theme_bw(base_size = base_size)+
      labs(x = expression(paste(italic(K))), y = expression(paste("L'(", italic(K), ") " %+-% " SD")), title = "B")
    
    #plot 3
    plist[[3]] <- ggplot2::ggplot(data, aes(x = k, y = lnk2))+
      geom_path(colour = plotcol1, size = base_size*0.04, na.rm = na.rm)+
      geom_point(colour = plotcol1, fill = plotcol1, size = base_size*0.3, shape = pointsh, na.rm = na.rm)+
      geom_errorbar(aes(x = k, ymax = lnk2max, ymin = lnk2min, width = 0.2), 
                    size = base_size*0.04, colour = plotcol, na.rm = na.rm)+
      theme_bw(base_size = base_size)+
      labs(x = expression(paste(italic(K))), y = expression(paste("|L\"(", italic(K), ")| " %+-% " SD")), title = "C")
    
    #plot 4
    if (is.finite(sum(data$drv3, na.rm = TRUE)))
    {
      plist[[4]] <- ggplot2::ggplot(data, aes(x = k, y = deltaK))+
        geom_path(colour = plotcol1, size = base_size*0.04, na.rm = na.rm)+
        geom_point(colour = plotcol1, fill = plotcol1, size = base_size*0.3, shape = pointsh, na.rm = na.rm)+
        theme_bw(base_size = base_size)+
        labs(x = expression(paste(italic(K))), y = expression(paste(Delta, italic(K))), title = "D")
    }
    
    plen <- length(plist)
    for (r in 1:plen)
    {
      plist[[r]] <- plist[[r]] + theme(legend.position = "none",
                                       axis.text.y = element_text(angle = 90, hjust = 0.5,size = base_size-0.5, colour = plotcol),
                                       axis.text.x = element_text(size = base_size-0.5, colour = plotcol),
                                       axis.title = element_text(size = base_size+0.6, colour = plotcol,face = "bold"),
                                       plot.title = element_text(size = base_size+2.5, hjust = 0, colour = plotcol),
                                       panel.border = element_blank(),
                                       axis.ticks = element_blank(),
                                       plot.margin = grid::unit(c(0.01,0.1,0.01,0.01),"cm"))
    }
       
    #export image

      #check image imgtype  
      if (imgtype == "pdf") pdf("evannoMethodStructure.pdf", height = height1, width = width1)
      if (imgtype  == "png") png("evannoMethodStructure.png", height = height1, width = width1, res = res, units = units, type = "cairo")
      if (imgtype == "jpeg") jpeg("evannoMethodStructure.jpg", height = height1, width = width1, res = res, units = units, quality = 100)
      
      if (plen == 3) gridExtra::grid.arrange(plist[[1]],plist[[2]],plist[[3]], ncol = 2, nrow = 2)
      if (plen == 4) gridExtra::grid.arrange(plist[[1]],plist[[2]],plist[[3]], plist[[4]], ncol = 2, nrow = 2)
      
      dev.off()
      if (imgtype == "pdf") cat("evannoMethodStructure.pdf exported.\n")
      if (imgtype == "png") cat("evannoMethodStructure.png exported.\n")
      if (imgtype == "jpeg") cat("evannoMethodStructure.jpg exported.\n")
  }
  
  #return table
  return(data)
}

#-------------------------------------------------------------------------------

#FUNCTION clumppExportStructure
#' Combine STRUCTURE runs and export files for use with software CLUMPP
#' @description Takes multiple STRUCTURE runs and combines several repeats for 
#' each K into a single file along with a parameter file. The two output files 
#' are organised into folders by K. The CLUMPP executable file can simply be copied to 
#' this folder and run to reorder the clusters for each K.
#' @param files A character vector of STRUCTURE output files to be tabulated. 
#' Use \code{choose.files(multi = TRUE)} for interactive selection.
#' @param prefix A character prefix for folder names. Set to 'STRUCTUREpop' by default.
#' @param parammode A numeric 1, 2 or 3 indicating algorithm option for CLUMPP paramfile. Calculated 
#' automatically by default. Set this value to 3 if CLUMPP 
#' runs too long. See details.
#' @param paramrep A numeric indicating the number of repeats for CLUMPP paramfile. Calculated 
#' automatically by default. See details.
#' @param useexe A logical indicating if CLUMPP executable must be run automatically based on system OS (experimental). See details.
#' @return The combined file and paramfile are written into respective folders 
#' named by population.
#' @details When multiple repeats are run for each K in STRUCTURE, the order of 
#' clusters may be jumbled for each run. Therefore, when plotting multiple runs 
#' within each K, the colours cannot be assigned correctly. The software CLUMPP 
#' helps to overcome this issue by reordering the clusters correctly. This 
#' function clumppExportStructure() takes multiple runs for each K and combines 
#' them into a single file and generates a parameter file for easy use with 
#' CLUMPP. Further details for CLUMPP can be found here: Jakobsson, M., and 
#' Rosenberg, N. A. (2007). CLUMPP: a cluster matching and permutation program 
#' for dealing with label switching and multimodality in analysis of population 
#' structure. Bioinformatics, 23(14), 1801-1806.\cr
#' \cr
#' The parammode (M) is the type of algorithm used. Option 1 is 'FullSearch' 
#' (takes the longest time), option 2 is 'Greedy' and option 3 is 'LargeKGreedy'
#' (fastest). If clumpp takes more than a few minutes, consider changing parammode
#' to a higher number (ex. from 2 to 3), or open the exported paramfile and manually
#' change GREEDY_OPTION to 3.\cr
#' \cr
#' The parammode and paramrep for CLUMPP paramfile is set based on this calculation.
#' T <- factorial(k)*((runs*(runs-1))/2)*k*ind, where k is number of 
#' populations, runs is number of runs for k and ind is number of individuals.
#' If T <= 100000000, then parammode is 2 and paramrep is 20, otherwise 
#' parammode is 3 and paramrep is set to 500.\cr
#' \cr
#' To find out more about parammode (algorithm type) and paramrep (repeats), 
#' refer to CLUMPP documentation.\cr
#' \cr
#' \strong{useexe}\cr
#' This option automatically runs the CLUMPP executable that is provided with this package. 
#' The CLUMPP executable was obtained from \url{https://web.stanford.edu/group/rosenberglab/clumpp.html}.
#' Remember to cite CLUMPP if this option is used.
#' 
#' @seealso \code{\link{clumppExportTess}}, \code{\link{clumppExportMatrix}}
#' @examples 
#' \dontrun{
#' slist <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' clumppExportStructure(slist)
#' #auto execute clumpp
#' clumppExportStructure(slist,useexe=TRUE)
#' }
#' @export
#' 
clumppExportStructure <- function(files = NULL, prefix = NA, parammode = NA, paramrep = NA, useexe=FALSE)
{
  if (is.null(files) | (length(files) == 0)) stop("clumppExportStructure: No input files.")
  if (is.na(prefix)) prefix <- "STRUCTUREpop"
  prefix <- paste0(prefix, "_K")
  if(!is.logical(useexe)) stop("clumppExportStructure: Argument 'useexe' set incorrectly. Set as TRUE or FALSE.")
  
  #check file
  if (any(pophelper:::checkRuns(files)$type != "STRUCTURE")) stop("clumppExportStructure: Input contains one or more non-STRUCTURE files/Incorrect input format.")
  
  #get tabulated runs
  df1 <- pophelper::tabulateRunsStructure(files = files)
  df2 <- pophelper::summariseRunsStructure(df1)
  df1l <- as.list(df1)
  df2l <- as.list(df2)
  
  #k val duplicated
  if (any(duplicated(df2l$k))) stop("clumppExportStructure: clumppExport not computed. Repeating values of K found.")
  #do ind vary
  if (!all(df2l$ind[1] == df2l$ind)) warning("clumppExportStructure: Number of individuals vary between runs.")
  #do loci vary
  if (!all(df2l$loci[1] == df2l$loci)) warning("clumppExportStructure: Number of loci vary between runs.")
  
  e <- 1
  p <- 1
  len1 <- length(df2l$k)
  while (e <= len1)
  {
    k <- df2l$k[e]
    ind <- df2l$ind[e]
    runs <- df2l$runs[e]
    
    ldata <- vector("list",length = runs)
    f <- 1
    for (f in 1:runs)
    {
      sel <- grep(as.character(df1l$file[p]), files,fixed = T)
      dframe1 <- pophelper::runsToDfStructure(files[sel])
      
      #generate df
      dframe3 <- as.matrix(data.frame(V1 = paste0(1:ind, ":"), dframe1, last = as.character(rep(1, ind)),stringsAsFactors = F))
      
      #add dataframes to list
      ldata[[f]] <- dframe3
      rm(dframe3)
      p = p+1
    }
    
    if (runs > 1 & k > 1)
    {
      currwd <- getwd()
      if(as.numeric(file.access(currwd,2)) == -1) stop(paste0("clumppExportStructure: Directory ",currwd," has no write permission."))
      
      dir.create(paste0(currwd, "/", prefix, k))
      setwd(paste0(currwd, "/", prefix, k))
      cat(paste0("Directory created: ", basename(getwd()), "\n"))  
      out <- paste0(prefix, k, "-combined.txt")
      
      #File Output block
      
      #make 2 line space
      spacer <- matrix(rep("  ", (k+2)*2),nrow = 2)
      
      #Write file
      write(t(format(ldata[[1]], nsmall = 15)), paste(out), ncolumns = k+2)
      i = 2
      for (i in 2:length(ldata))
      {
        write(t(spacer), paste(out), ncolumns = k+2, append = TRUE)
        write(t(format(ldata[[i]], nsmall = 15)), append = TRUE, paste(out), ncolumns = k+2)
      }
      cat(paste0(out), "exported.\n")
      
      #PARAMFILE section
      T1 <- factorial(k)*((length(ldata)*(length(ldata)-1))/2)*k*ind
      if (T1 <= 100000000)
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
                  paste0("REPEATS ", paramrep," "),
                  "PERMUTATIONFILE NOTNEEDED.permutationfile ",
                  "PRINT_PERMUTED_DATA 1 ",
                  paste0("PERMUTED_DATAFILE ",out1,"-aligned.txt "),
                  "PRINT_EVERY_PERM 0 ",
                  paste0("EVERY_PERMFILE ",out1,".every_permfile "),
                  "PRINT_RANDOM_INPUTORDER 0 ",
                  paste0("RANDOM_INPUTORDERFILE ",out1,".random_inputorderfile "),
                  "OVERRIDE_WARNINGS 0 ",
                  "ORDER_BY_RUN 0 ")
      
      write(params, "paramfile")
      cat(paste0("paramfile exported.\n"))
      
      if(useexe)
      {
        sysos <- pophelper:::getOS()
        if(sysos == "windows")
        {
          file.copy(system.file("bin/clumpp_windows_1.1.2b.exe",package="pophelper"),".")
          system("clumpp_windows_1.1.2b.exe")
          unlink("clumpp_windows_1.1.2b.exe",force=TRUE)
        }
        
        if(sysos == "mac")
        {
          file.copy(system.file("bin/clumpp_mac_1.1.2b",package="pophelper"),".")
          system("chmod 777 clumpp_mac_1.1.2b")
          system("./clumpp_mac_1.1.2b")
          unlink("clumpp_mac_1.1.2b",force=TRUE)
        }
        
        if(sysos == "unix32")
        {
          file.copy(system.file("bin/clumpp_linux_1.1.2b_32bit",package="pophelper"),".")
          system("chmod 777 clumpp_linux_1.1.2b_32bit")
          system("./clumpp_linux_1.1.2b_32bit")
          unlink("clumpp_linux_1.1.2b_32bit",force=TRUE)
        }
        
        if(sysos == "unix64")
        {
          file.copy(system.file("bin/clumpp_linux_1.1.2b_64bit",package="pophelper"),".")
          system("chmod 777 clumpp_linux_1.1.2b_64bit")
          system("./clumpp_linux_1.1.2b_64bit")
          unlink("clumpp_linux_1.1.2b_64bit",force=TRUE)
        }
        
        if(sysos == "unknown") warning("clumppExportStructure: CLUMPP executable not run because system cannot be identified as windows, mac or linux.")
      }
      
      setwd(paste(currwd))
      cat("-----------------------\n")
    }else
    {
      if (k == 1) message(paste0(prefix, k, " not exported. K less than 2.\n"))
      if (runs < 2) message(paste0(prefix, k, " not exported. Repeats less than 2.\n"))
      cat("-----------------------\n")
    }
    e <- e + 1
  }
  
  cat("Run completed.\n")
}

#-------------------------------------------------------------------------------

#FUNCTION clumppExportTess
#' Combine TESS runs and export files for use with software CLUMPP
#' @description Takes multiple TESS runs and combines several repeats for each 
#' K into a single file along with a parameter file. The two output files are 
#' organised into folders by K. The CLUMPP executable file can simply be copied to this 
#' folder and run to reorder the clusters for each K.
#' @param files A character vector of TESS cluster run files to be tabulated. 
#' Use \code{choose.files(multi = TRUE)} for interactive selection.
#' @param prefix A character prefix for folder names. By default, set to 'TESSpop'.
#' @param parammode A numeric 1, 2 or 3 indicating the algorithm option for CLUMPP paramfile. Calculated 
#' automatically by default. Set this value to 3 if CLUMPP runs too long. See details.
#' @param paramrep A numeric indicating the number of repeats for CLUMPP paramfile. Calculated 
#' automatically by default. See details.
#' @param useexe A logical indicating if CLUMPP executable must be run automatically based on system OS (experimental). See details.
#' @return The combined file and paramfile are written into respective folders 
#' named by population.
#' @details When multiple repeats are run for each K in TESS, the order of 
#' clusters may be jumbled for each run. Therefore, when plotting multiple runs 
#' within each K, the colours cannot be assigned correctly. The software CLUMPP 
#' helps to overcome this issue by reordering the clusters correctly. This 
#' function clumppExportTess() takes multiple runs for each K and combines 
#' them into a single file and generates a parameter file for easy use with 
#' CLUMPP. Further details for CLUMPP can be found here: Jakobsson, M., and 
#' Rosenberg, N. A. (2007). CLUMPP: a cluster matching and permutation program 
#' for dealing with label switching and multimodality in analysis of population 
#' structure. Bioinformatics, 23(14), 1801-1806.\cr
#' \cr
#' The parammode (M) is the type of algorithm used. Option 1 is 'FullSearch' 
#' (takes the longest time), option 2 is 'Greedy' and option 3 is 'LargeKGreedy'
#' (fastest). If clumpp takes more than a few minutes, consider changing parammode
#' to a higher number (ex. from 2 to 3), or open the exported paramfile and manually
#' change GREEDY_OPTION to 3.\cr
#' \cr
#' The parammode and paramrep for CLUMPP paramfile is set based on this calculation.
#' T <- factorial(k)*((runs*(runs-1))/2)*k*ind, where k is number of 
#' populations, runs is number of runs for k and ind is number of individuals.
#' If T <= 100000000, then parammode is 2 and paramrep is 20, otherwise 
#' parammode is 3 and paramrep is set to 500.\cr
#' \cr
#' To find out more about parammode (algorithm type) and paramrep (repeats), 
#' refer to CLUMPP documentation.\cr
#' 
#' \strong{useexe}\cr
#' This option automatically runs the CLUMPP executable that is provided with this package. 
#' The CLUMPP executable was obtained from \url{https://web.stanford.edu/group/rosenberglab/clumpp.html}.
#' Remember to cite CLUMPP if this option is used.
#' 
#' @seealso \code{\link{clumppExportStructure}}, \code{\link{clumppExportMatrix}}
#' @examples 
#' \dontrun{
#' tlist <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' clumppExportTess(tlist)
#' #auto execute clumpp
#' clumppExportTess(tlist,useexe=TRUE)
#' }
#' @export
#' 
clumppExportTess <- function(files = NULL, prefix = NA, parammode = NA, paramrep = NA, useexe=FALSE)
{
  if (is.null(files) | (length(files) == 0)) stop("clumppExportTess: No input files.")
  if (is.na(prefix)) prefix <- "TESSpop"
  prefix <- paste0(prefix, "_K")
  if(!is.logical(useexe)) stop("clumppExportTess: Argument 'useexe' set incorrectly. Set as TRUE or FALSE.")
  
  #check file
  if (any(pophelper:::checkRuns(files)$type != "TESS")) stop("clumppExportTess: Input contains one or more non-TESS files/Incorrect input format.")
  
  #get tabulated runs
  df1 <- pophelper::tabulateRunsTess(files = files)
  df2 <- pophelper::summariseRunsTess(df1)
  df1l <- as.list(df1)
  df2l <- as.list(df2)
  
  #k val duplicated
  if (any(duplicated(df2l$k))) stop("clumppExportTess: Repeating values of K found.")
  #do ind vary
  if (!all(df2l$ind[1] == df2l$ind)) warning("clumppExportTess: Number of individuals vary between runs.")
  
  e <- 1
  p <- 1
  len1 <- length(df2l$k)
  while (e <= len1)
  {
    k <- df2l$k[e]
    ind <- df2l$ind[e]
    runs <- df2l$runs[e]
    
    ldata <- vector("list",length = runs)
    f <- 1
    for (f in 1:runs)
    {
      sel <- grep(as.character(df1l$file[p]), files,fixed = T)
      dframe1 <- pophelper::runsToDfTess(files[sel])
      
      #generate df
      dframe3 <- as.matrix(data.frame(V1 = paste0(1:ind, ":"), 
                                      dframe1, last = as.character(rep(1, ind)),stringsAsFactors = FALSE))
      
      #add dataframes to list
      ldata[[f]] <- dframe3
      rm(dframe3)
      p = p+1
    }
    
    if (runs > 1 & k > 1)
    {
      currwd <- getwd()
      if(as.numeric(file.access(currwd,2)) == -1) stop(paste0("clumppExportTess: Directory ",currwd," has no write permission."))
      dir.create(paste0(currwd, "/", prefix, k))
      setwd(paste0(currwd, "/", prefix, k))
      cat(paste0("Folder created: ", basename(getwd()), "\n"))  
      out <- paste0(prefix, k, "-combined.txt")
      
      #File Output block
      
      #make 2 line space
      spacer <- matrix(rep("  ", (k+2)*2),nrow = 2)
      
      #Write file
      write(t(format(ldata[[1]], nsmall = 15)), paste(out), ncolumns = k+2)
      i = 2
      for (i in 2:length(ldata))
      {
        write(t(spacer), paste(out), ncolumns = k+2, append = TRUE)
        write(t(format(ldata[[i]], nsmall = 15)), append = TRUE, paste(out), ncolumns = k+2)
      }
      cat(paste0(out), "exported.\n")
      
      #PARAMFILE section
      T1 <- factorial(k)*((length(ldata)*(length(ldata)-1))/2)*k*ind
      if (T1 <= 100000000)
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
                  paste0("REPEATS ", paramrep," "),
                  "PERMUTATIONFILE NOTNEEDED.permutationfile ",
                  "PRINT_PERMUTED_DATA 1 ",
                  paste0("PERMUTED_DATAFILE ",out1,"-aligned.txt "),
                  "PRINT_EVERY_PERM 0 ",
                  paste0("EVERY_PERMFILE ",out1,".every_permfile "),
                  "PRINT_RANDOM_INPUTORDER 0 ",
                  paste0("RANDOM_INPUTORDERFILE ",out1,".random_inputorderfile "),
                  "OVERRIDE_WARNINGS 0 ",
                  "ORDER_BY_RUN 0 ")
      
      write(params, "paramfile")
      cat(paste0("paramfile exported.\n"))
      
      if(useexe)
      {
        sysos <- pophelper:::getOS()
        if(sysos == "windows")
        {
          file.copy(system.file("bin/clumpp_windows_1.1.2b.exe",package="pophelper"),".")
          system("clumpp_windows_1.1.2b.exe")
          unlink("clumpp_windows_1.1.2b.exe",force=TRUE)
        }
        
        if(sysos == "mac")
        {
          file.copy(system.file("bin/clumpp_mac_1.1.2b",package="pophelper"),".")
          system("chmod 777 clumpp_mac_1.1.2b")
          system("./clumpp_mac_1.1.2b")
          unlink("clumpp_mac_1.1.2b",force=TRUE)
        }
        
        if(sysos == "unix32")
        {
          file.copy(system.file("bin/clumpp_linux_1.1.2b_32bit",package="pophelper"),".")
          system("chmod 777 clumpp_linux_1.1.2b_32bit")
          system("./clumpp_linux_1.1.2b_32bit")
          unlink("clumpp_linux_1.1.2b_32bit",force=TRUE)
        }
        
        if(sysos == "unix64")
        {
          file.copy(system.file("bin/clumpp_linux_1.1.2b_64bit",package="pophelper"),".")
          system("chmod 777 clumpp_linux_1.1.2b_64bit")
          system("./clumpp_linux_1.1.2b_64bit")
          unlink("clumpp_linux_1.1.2b_64bit",force=TRUE)
        }
        
        if(sysos == "unknown") warning("clumppExportStructure: CLUMPP executable not run because system cannot be identified as windows, mac or linux.")
      }
      
      setwd(paste(currwd))
      cat("-----------------------\n")
    }else
    {
      if (k == 1) message(paste0(prefix, k, " not exported. K less than 2.\n"))
      if (runs < 2) message(paste0(prefix, k, " not exported. Repeats less than 2.\n"))
      cat("-----------------------\n")
    }
    e <- e + 1
  }
  
  cat("Run completed.\n")
}

#-------------------------------------------------------------------------------

#FUNCTION clumppExportAdmixture
#' Combine ADMIXTURE runs and export files for use with software CLUMPP
#' @description DEPRECATED. USE clumppExportMatrix(). Takes multiple ADMIXTURE runs and combines several repeats for each 
#' K into a single file along with a parameter file. The two output files are 
#' organised into folders by K. The CLUMPP executable file can simply be copied to this 
#' folder and run to reorder the clusters for each K.
#' @param files A character vector of ADMIXTURE cluster run files to be tabulated. 
#' Use \code{choose.files(multi = TRUE)} for interactive selection.
#' @param prefix A character prefix for folder names. By default, set to 'ADMIXTUREpop'.
#' @param parammode A numeric 1, 2 or 3 indicating the algorithm option for CLUMPP paramfile. Calculated 
#' automatically by default. Set this value to 3 if CLUMPP runs too long. See details.
#' @param paramrep A numeric indicating the number of repeats for CLUMPP paramfile. Calculated 
#' automatically by default. See details.
#' @param useexe A logical indicating if CLUMPP executable must be run automatically based on system OS (experimental).
#' @return The combined file and paramfile are written into respective folders 
#' named by population.
#' @details When multiple repeats are run for each K in ADMIXTURE, the order of 
#' clusters may be jumbled for each run. Therefore, when plotting multiple runs 
#' within each K, the colours cannot be assigned correctly. The software CLUMPP 
#' helps to overcome this issue by reordering the clusters correctly. This 
#' function clumppExportAdmixture() takes multiple runs for each K and combines 
#' them into a single file and generates a parameter file for easy use with 
#' CLUMPP. Further details for CLUMPP can be found here: Jakobsson, M., and 
#' Rosenberg, N. A. (2007). CLUMPP: a cluster matching and permutation program 
#' for dealing with label switching and multimodality in analysis of population 
#' structure. Bioinformatics, 23(14), 1801-1806.\cr
#' \cr
#' The parammode (M) is the type of algorithm used. Option 1 is 'FullSearch' 
#' (takes the longest time), option 2 is 'Greedy' and option 3 is 'LargeKGreedy'
#' (fastest). If clumpp takes more than a few minutes, consider changing parammode
#' to a higher number (ex. from 2 to 3), or open the exported paramfile and manually
#' change GREEDY_OPTION to 3.\cr
#' \cr
#' The parammode and paramrep for CLUMPP paramfile is set based on this calculation.
#' T <- factorial(k)*((runs*(runs-1))/2)*k*ind, where k is number of 
#' populations, runs is number of runs for k and ind is number of individuals.
#' If T <= 100000000, then parammode is 2 and paramrep is 20, otherwise 
#' parammode is 3 and paramrep is set to 500.\cr
#' \cr
#' To find out more about parammode (algorithm type) and paramrep (repeats), 
#' refer to CLUMPP documentation.\cr
#' \cr
#' \strong{useexe}\cr
#' This option automatically runs the CLUMPP executable that is provided with this package. 
#' The CLUMPP executable was obtained from \url{https://web.stanford.edu/group/rosenberglab/clumpp.html}.
#' Remember to cite CLUMPP if this option is used.
#' 
#' @seealso \code{\link{clumppExportStructure}}, \code{\link{clumppExportTess}}
#' @export
#' 
clumppExportAdmixture <- function(files = NULL, prefix = NA, parammode = NA, paramrep = NA, useexe=FALSE)
{
  warning("clumppExportAdmixture() is deprecated. Use clumppExportMatrix()")
  if (is.null(files) | (length(files) == 0)) stop("clumppExportAdmixture: No input files.")
  if (is.na(prefix)) prefix <- "ADMIXTUREpop"
  prefix <- paste0(prefix, "_K")
  if(!is.logical(useexe)) stop("clumppExportAdmixture: Argument 'useexe' set incorrectly. Set as TRUE or FALSE.")
  
  #check file
  if (any(pophelper:::checkRuns(files)$type != "MATRIX")) stop("clumppExportAdmixture: Input contains one or more non-ADMIXTURE files/Incorrect input format.")
  
  #get tabulated runs
  df1 <- pophelper::tabulateRunsMatrix(files = files)
  df2 <- pophelper::summariseRunsMatrix(df1)
  df1l <- as.list(df1)
  df2l <- as.list(df2)
  
  #k val duplicated
  if (any(duplicated(df2l$k))) stop("clumppExportAdmixture: Repeating values of K found.")
  #do ind vary
  if (!all(df2l$ind[1] == df2l$ind)) warning("clumppExportAdmixture: Number of individuals vary between runs.")
  
  e <- 1
  p <- 1
  len1 <- length(df2l$k)
  while (e <= len1)
  {
    k <- df2l$k[e]
    ind <- df2l$ind[e]
    runs <- df2l$runs[e]
    
    ldata <- vector("list",length = runs)
    f <- 1
    for (f in 1:runs)
    {
      sel <- grep(as.character(df1l$file[p]), files,fixed = T)
      dframe1 <- pophelper::runsToDfMatrix(files[sel])
      
      #generate df
      dframe3 <- as.matrix(data.frame(V1 = paste0(1:ind, ":"), 
                                      dframe1, last = as.character(rep(1, ind)),stringsAsFactors = FALSE))
      
      #add dataframes to list
      ldata[[f]] <- dframe3
      rm(dframe3)
      p = p+1
    }
    
    if (runs > 1 & k > 1)
    {
      currwd <- getwd()
      if(as.numeric(file.access(currwd,2)) == -1) stop(paste0("clumppExportAdmixture: Directory ",currwd," has no write permission."))
      
      dir.create(paste0(currwd, "/", prefix, k))
      setwd(paste0(currwd, "/", prefix, k))
      cat(paste0("Folder created: ", basename(getwd()), "\n"))  
      out <- paste0(prefix, k, "-combined.txt")
      
      #File Output block
      
      #make 2 line space
      spacer <- matrix(rep("  ", (k+2)*2),nrow = 2)
      
      #Write file
      write(t(format(ldata[[1]], nsmall = 15)), paste(out), ncolumns = k+2)
      i = 2
      for (i in 2:length(ldata))
      {
        write(t(spacer), paste(out), ncolumns = k+2, append = TRUE)
        write(t(format(ldata[[i]], nsmall = 15)), append = TRUE, paste(out), ncolumns = k+2)
      }
      cat(paste0(out), "exported.\n")
      
      #PARAMFILE section
      T1 <- factorial(k)*((length(ldata)*(length(ldata)-1))/2)*k*ind
      if (T1 <= 100000000)
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
                  paste0("REPEATS ", paramrep," "),
                  "PERMUTATIONFILE NOTNEEDED.permutationfile ",
                  "PRINT_PERMUTED_DATA 1 ",
                  paste0("PERMUTED_DATAFILE ",out1,"-aligned.txt "),
                  "PRINT_EVERY_PERM 0 ",
                  paste0("EVERY_PERMFILE ",out1,".every_permfile "),
                  "PRINT_RANDOM_INPUTORDER 0 ",
                  paste0("RANDOM_INPUTORDERFILE ",out1,".random_inputorderfile "),
                  "OVERRIDE_WARNINGS 0 ",
                  "ORDER_BY_RUN 0 ")
      
      write(params, "paramfile")
      cat(paste0("paramfile exported.\n"))
      
      if(useexe)
      {
        sysos <- pophelper:::getOS()
        if(sysos == "windows")
        {
          file.copy(system.file("bin/clumpp_windows_1.1.2b.exe",package="pophelper"),".")
          system("clumpp_windows_1.1.2b.exe")
          unlink("clumpp_windows_1.1.2b.exe",force=TRUE)
        }
        
        if(sysos == "mac")
        {
          file.copy(system.file("bin/clumpp_mac_1.1.2b",package="pophelper"),".")
          system("chmod 777 clumpp_mac_1.1.2b")
          system("./clumpp_mac_1.1.2b")
          unlink("clumpp_mac_1.1.2b",force=TRUE)
        }
        
        if(sysos == "unix32")
        {
          file.copy(system.file("bin/clumpp_linux_1.1.2b_32bit",package="pophelper"),".")
          system("chmod 777 clumpp_linux_1.1.2b_32bit")
          system("./clumpp_linux_1.1.2b_32bit")
          unlink("clumpp_linux_1.1.2b_32bit",force=TRUE)
        }
        
        if(sysos == "unix64")
        {
          file.copy(system.file("bin/clumpp_linux_1.1.2b_64bit",package="pophelper"),".")
          system("chmod 777 clumpp_linux_1.1.2b_64bit")
          system("./clumpp_linux_1.1.2b_64bit")
          unlink("clumpp_linux_1.1.2b_64bit",force=TRUE)
        }
        
        if(sysos == "unknown") warning("clumppExportStructure: CLUMPP executable not run because system cannot be identified as windows, mac or linux.")
      }
      
      setwd(paste(currwd))
      cat("-----------------------\n")
    }else
    {
      if (k == 1) message(paste0(prefix, k, " not exported. K less than 2.\n"))
      if (runs < 2) message(paste0(prefix, k, " not exported. Repeats less than 2.\n"))
      cat("-----------------------\n")
    }
    e <- e + 1
  }
  
  cat("Run completed.\n")
}

#-------------------------------------------------------------------------------

#FUNCTION clumppExportMatrix
#' Combine MATRIX runs and export files for use with software CLUMPP
#' @description Takes multiple MATRIX runs and combines several repeats for each 
#' K into a single file along with a parameter file. The two output files are 
#' organised into folders by K. The CLUMPP executable file can simply be copied to this 
#' folder and run to reorder the clusters for each K.
#' @param files A character vector of MATRIX cluster run files to be tabulated. 
#' Use \code{choose.files(multi = TRUE)} for interactive selection.
#' @param prefix A character prefix for folder names. By default, set to 'MATRIXpop'.
#' @param parammode A numeric 1, 2 or 3 indicating the algorithm option for CLUMPP paramfile. Calculated 
#' automatically by default. Set this value to 3 if CLUMPP runs too long. See details.
#' @param paramrep A numeric indicating the number of repeats for CLUMPP paramfile. Calculated 
#' automatically by default. See details.
#' @param useexe A logical indicating if CLUMPP executable must be run automatically based on system OS (experimental).
#' @return The combined file and paramfile are written into respective folders 
#' named by population.
#' @details When multiple repeats are run for each K in MATRIX runs, the order of 
#' clusters may be jumbled for each run. Therefore, when plotting multiple runs 
#' within each K, the colours cannot be assigned correctly. The software CLUMPP 
#' helps to overcome this issue by reordering the clusters correctly. This 
#' function clumppExportMatrix() takes multiple runs for each K and combines 
#' them into a single file and generates a parameter file for easy use with 
#' CLUMPP. Further details for CLUMPP can be found here: Jakobsson, M., and 
#' Rosenberg, N. A. (2007). CLUMPP: a cluster matching and permutation program 
#' for dealing with label switching and multimodality in analysis of population 
#' structure. Bioinformatics, 23(14), 1801-1806.\cr
#' \cr
#' The parammode (M) is the type of algorithm used. Option 1 is 'FullSearch' 
#' (takes the longest time), option 2 is 'Greedy' and option 3 is 'LargeKGreedy'
#' (fastest). If clumpp takes more than a few minutes, consider changing parammode
#' to a higher number (ex. from 2 to 3), or open the exported paramfile and manually
#' change GREEDY_OPTION to 3.\cr
#' \cr
#' The parammode and paramrep for CLUMPP paramfile is set based on this calculation.
#' T <- factorial(k)*((runs*(runs-1))/2)*k*ind, where k is number of 
#' populations, runs is number of runs for k and ind is number of individuals.
#' If T <= 100000000, then parammode is 2 and paramrep is 20, otherwise 
#' parammode is 3 and paramrep is set to 500.\cr
#' \cr
#' To find out more about parammode (algorithm type) and paramrep (repeats), 
#' refer to CLUMPP documentation.\cr
#' 
#' \strong{useexe}\cr
#' This option automatically runs the CLUMPP executable that is provided with this package. 
#' The CLUMPP executable was obtained from \url{https://web.stanford.edu/group/rosenberglab/clumpp.html}.
#' Remember to cite CLUMPP if this option is used.\cr
#' 
#' @seealso \code{\link{clumppExportStructure}}, \code{\link{clumppExportTess}}
#' @examples 
#' \dontrun{
#' alist <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' clumppExportMatrix(alist)
#' #auto execute clumpp
#' clumppExportMatrix(alist,useexe=TRUE)
#' }
#' @export
#' 
clumppExportMatrix <- function(files = NULL, prefix = NA, parammode = NA, paramrep = NA, useexe=FALSE)
{
  if (is.null(files) | (length(files) == 0)) stop("clumppExportMatrix: No input files.")
  if (is.na(prefix)) prefix <- "MATRIXpop"
  prefix <- paste0(prefix, "_K")
  if(!is.logical(useexe)) stop("clumppExportMatrix: Argument 'useexe' set incorrectly. Set as TRUE or FALSE.")
  
  #check file
  if (any(pophelper:::checkRuns(files)$type != "MATRIX")) stop("clumppExportMatrix: Input contains one or more non-MATRIX files/Incorrect input format.")
  
  #get tabulated runs
  df1 <- pophelper::tabulateRunsMatrix(files = files)
  df2 <- pophelper::summariseRunsMatrix(df1)
  df1l <- as.list(df1)
  df2l <- as.list(df2)
  
  #k val duplicated
  if (any(duplicated(df2l$k))) stop("clumppExportMatrix: Repeating values of K found.")
  #do ind vary
  if (!all(df2l$ind[1] == df2l$ind)) warning("clumppExportMatrix: Number of individuals vary between runs.")
  
  e <- 1
  p <- 1
  len1 <- length(df2l$k)
  while (e <= len1)
  {
    k <- df2l$k[e]
    ind <- df2l$ind[e]
    runs <- df2l$runs[e]
    
    ldata <- vector("list",length = runs)
    f <- 1
    for (f in 1:runs)
    {
      sel <- grep(as.character(df1l$file[p]), files,fixed = T)
      dframe1 <- pophelper::runsToDfMatrix(files[sel])
      
      #generate df
      dframe3 <- as.matrix(data.frame(V1 = paste0(1:ind, ":"), 
                                      dframe1, last = as.character(rep(1, ind)),stringsAsFactors = FALSE))
      
      #add dataframes to list
      ldata[[f]] <- dframe3
      rm(dframe3)
      p = p+1
    }
    
    if (runs > 1 & k > 1)
    {
      currwd <- getwd()
      if(as.numeric(file.access(currwd,2)) == -1) stop(paste0("clumppExportMatrix: Directory ",currwd," has no write permission."))
      
      dir.create(paste0(currwd, "/", prefix, k))
      setwd(paste0(currwd, "/", prefix, k))
      cat(paste0("Folder created: ", basename(getwd()), "\n"))  
      out <- paste0(prefix, k, "-combined.txt")
      
      #File Output block
      
      #make 2 line space
      spacer <- matrix(rep("  ", (k+2)*2),nrow = 2)
      
      #Write file
      write(t(format(ldata[[1]], nsmall = 15)), paste(out), ncolumns = k+2)
      i = 2
      for (i in 2:length(ldata))
      {
        write(t(spacer), paste(out), ncolumns = k+2, append = TRUE)
        write(t(format(ldata[[i]], nsmall = 15)), append = TRUE, paste(out), ncolumns = k+2)
      }
      cat(paste0(out), "exported.\n")
      
      #PARAMFILE section
      T1 <- factorial(k)*((length(ldata)*(length(ldata)-1))/2)*k*ind
      if (T1 <= 100000000)
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
                  paste0("REPEATS ", paramrep," "),
                  "PERMUTATIONFILE NOTNEEDED.permutationfile ",
                  "PRINT_PERMUTED_DATA 1 ",
                  paste0("PERMUTED_DATAFILE ",out1,"-aligned.txt "),
                  "PRINT_EVERY_PERM 0 ",
                  paste0("EVERY_PERMFILE ",out1,".every_permfile "),
                  "PRINT_RANDOM_INPUTORDER 0 ",
                  paste0("RANDOM_INPUTORDERFILE ",out1,".random_inputorderfile "),
                  "OVERRIDE_WARNINGS 0 ",
                  "ORDER_BY_RUN 0 ")
      
      write(params, "paramfile")
      cat(paste0("paramfile exported.\n"))
      
      if(useexe)
      {
        sysos <- pophelper:::getOS()
        if(sysos == "windows")
        {
          file.copy(system.file("files/executables/clumpp_windows_1.1.2b.exe",package="pophelper"),".")
          system("clumpp_windows_1.1.2b.exe")
          unlink("clumpp_windows_1.1.2b.exe",force=TRUE)
        }
        
        if(sysos == "mac")
        {
          file.copy(system.file("files/executables/clumpp_mac_1.1.2b",package="pophelper"),".")
          system("chmod 777 clumpp_mac_1.1.2b")
          system("./clumpp_mac_1.1.2b")
          unlink("clumpp_mac_1.1.2b",force=TRUE)
        }
        
        if(sysos == "unix32")
        {
          file.copy(system.file("bin/clumpp_linux_1.1.2b_32bit",package="pophelper"),".")
          system("chmod 777 clumpp_linux_1.1.2b_32bit")
          system("./clumpp_linux_1.1.2b_32bit")
          unlink("clumpp_linux_1.1.2b_32bit",force=TRUE)
        }
        
        if(sysos == "unix64")
        {
          file.copy(system.file("bin/clumpp_linux_1.1.2b_64bit",package="pophelper"),".")
          system("chmod 777 clumpp_linux_1.1.2b_64bit")
          system("./clumpp_linux_1.1.2b_64bit")
          unlink("clumpp_linux_1.1.2b_64bit",force=TRUE)
        }
        
        if(sysos == "unknown") warning("clumppExportStructure: CLUMPP executable not run because system cannot be identified as windows, mac or linux.")
      }
      
      setwd(paste(currwd))
      cat("-----------------------\n")
    }else
    {
      if (k == 1) message(paste0(prefix, k, " not exported. K less than 2.\n"))
      if (runs < 2) message(paste0(prefix, k, " not exported. Repeats less than 2.\n"))
      cat("-----------------------\n")
    }
    e <- e + 1
  }
  
  cat("Run completed.\n")
}

#-------------------------------------------------------------------------------

# FUNCTION runsToDfStructure
#' Convert STRUCTURE run files to R dataframes.
#' @description Takes one or more STRUCTURE output files and converts each of 
#' them to separate dataframes.
#' @param files A character or character vector of one or more STRUCTURE run files. Use \code{choose.files(multi=TRUE)} 
#' to select interactively.
#' @param indlabfromfile A logical indicating if individual labels must be read from input file and used as row names for resulting data frame. Spaces in labels may be replaced with _.
#' @return If a single file is selected, a single dataframe is returned. If 
#' multiple files are selected, a list with multiple dataframes is returned. 
#' If individual labels are present in the STRUCTURE file, they are added to the dataframe as row names.
#' @seealso \code{\link{runsToDfTess}}, \code{\link{runsToDfMatrix}}
#' @examples 
#' slist <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' #create a dataframe
#' runsToDfStructure(slist[1])
#' 
#' #use ind names from file
#' runsToDfStructure(slist[1],indlabfromfile=T)
#' 
#' #create a list of dataframes
#' runsToDfStructure(slist)
#' @export
#' 
runsToDfStructure <- function(files = NULL, indlabfromfile=FALSE)
{
  if (is.null(files) | (length(files) == 0)) stop("runsToDfStructure: No input files.")
  #number of files selected
  number <- length(files)
  #cat(paste("Number of files selected: ", number, "\n", sep = ""))
  
  #check file
  if (any(pophelper:::checkRuns(files)$type != "STRUCTURE")) stop("runsToDfStructure: Input contains one or more non-STRUCTURE files/Incorrect input format.")
  
  i <- 1
  dlist <- vector("list",length = number)
  len1 <- length(files)
  for (i in 1:len1)
  {
    name <- basename(files[i]) 
    file1 <- readLines(as.character(files[i]), warn = FALSE)
    
    #find individuals and get number of individuals
    ind <- as.numeric(as.character(base::gsub("\\D", "", grep("\\d individuals", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)[1])))
    if (is.na(ind)) cat(paste0("Number of individuals is NA in file: ", name))
    
    #get value of k & error check
    k <- as.numeric(as.character(base::gsub("\\D", "", grep("\\d populations assumed", file1, perl = TRUE, ignore.case = TRUE, value = TRUE)[1])))
    if (is.na(k)) cat(paste0("Value of K is NA in file: ", name))
    
    file1 <- file1[grep(".+\\(\\d+\\).+\\:.+",file1)]
    if(length(file1) == 0)
    {
      cstart <- base::charmatch("Inferred ancestry of individuals", file1)
      cend <- base::charmatch("Estimated Allele Frequencies in each", file1)
      file1 <- file1[(cstart+2):(cend-1)]
    }
    
    file_a <- file1[file1 != ""]
    rm(file1)
    
    #error check
    file_b <- read.delim(textConnection(file_a),header=F,sep="",stringsAsFactors = F)
    suppressWarnings(
      errorcheck <- try(
        file_b[,as.integer(grep(":",file_b[1,])+1):as.integer(max(grep("^[0-9]|[.]+$",file_b[1,]))),drop=F],
        silent=T)
    )
    rm(file_b)
    
    if(class(errorcheck) == "try-error")
    {
      #using manual substring
      file_a <- base::gsub("\\([0-9.,]+\\)","",file_a)
      file_b <- base::gsub(":  ", "", substr(file_a, base::regexpr(":\\W+\\d\\.\\d+", file_a), base::nchar(file_a)-1))
      file_b <- base::sub("\\s+$","",base::sub("^\\s+","",file_b))
      rm(file_a)
      file_c <- as.vector(as.numeric(as.character(unlist(base::strsplit(file_b, " ")))))
      rm(file_b)
      dframe <- as.data.frame(matrix(file_c, nrow = ind, byrow = TRUE),stringsAsFactors = FALSE)
    }else{
      #using textconnection
      file_b <- read.delim(textConnection(file_a),header=F,sep="",stringsAsFactors = F)
      dframe <- file_b[,as.integer(grep(":",file_b[1,])+1):as.integer(max(grep("^[0-9]|[.]+$",file_b[1,]))),drop=F]
    }

    dframe <- as.data.frame(sapply(dframe, as.numeric),stringsAsFactors = FALSE)
    colnames(dframe) <- paste0("Cluster", 1:k)
    row.names(dframe) <- 1:nrow(dframe)
    
    #labels
    if(indlabfromfile)
    {
      labeldf <- file_b[,(grep("[0-9]",file_b[1,])[1]+1):(grep("[(]",file_b[1,])[1]-1),drop=FALSE]
      
      if(ncol(labeldf) > 1) labeldf <- data.frame(V2=do.call(paste, c(labeldf, sep="_")),stringsAsFactors=FALSE)
      if(nrow(labeldf) == nrow(dframe))
      {
        if(any(duplicated(labeldf[,1]))) 
        {
          warning(paste0("runsToDfStructure: Individual names in file ",name," not used due to presence of duplicate names."))
        }else{
          row.names(dframe) <- as.character(labeldf[,1])
        }
      }else{
        warning(paste0("runsToDfStructure: Individual names in file ",name," not used due to incorrect length."))
      }
    }
    
    dlist[[i]] <- dframe
    #names(dlist[[i]]) <- as.character(name)
  }
  if (number>1) {return(dlist)} else{return(dframe)}
}

#-------------------------------------------------------------------------------

# FUNCTION runsToDfTess
#' Convert TESS cluster files to R dataframe.
#' @description Takes one or more TESS cluster run files and converts each of 
#' them to separate dataframes.
#' @param files A character or character vector of one or more TESS cluster run files. Use \code{choose.files(multi = TRUE)} 
#' to select interactively.
#' @return If a single file is selected, a single dataframe is returned. 
#' If multiple files are selected, a list with multiple dataframes is returned.
#' @details Use collectRunsTess() to collect TESS runs into one directory.
#' @seealso \code{\link{runsToDfStructure}}, \code{\link{runsToDfMatrix}}
#' @examples 
#' tlist <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' #create a dataframe
#' runsToDfTess(tlist[1])
#' 
#' #create a list of dataframes
#' runsToDfTess(tlist)
#' @export
#'
runsToDfTess <- function(files = NULL)
{
  if (is.null(files) | (length(files) == 0)) stop("runsToDfTess: No input files.")
  #number of files selected
  number <- length(files)
  #cat(paste("Number of files selected: ", number, "\n", sep = ""))
  
  #check file
  if (any(pophelper:::checkRuns(files)$type != "TESS")) stop("runsToDfTess: Input contains one or more non-TESS files/Incorrect input format.")
  
  i <- 1
  dlist <- vector("list",length = number)
  len1 <- length(files)
  for (i in 1:len1)
  {
    name <- base::gsub(".txt", "", basename(files[i]))
    
    file1 <- readLines(files[i], warn = FALSE)
    #read TESS files
    #chk <- grep("CLUSTERING PROBABILITIES", toupper(file1[1]))
    #if (length(chk) == 0) stop("runsToDfTess: Input not appropriate TESS file./Incorrect input format.")
    #if (length(file1) < 1) stop("runsToDfTess: Cannot read file.")
    
    #extract the cluster table part
    file1 <- file1[3:c(grep("Estimated Allele Frequencies", file1)-1)]
    file1 <- file1[file1 != ""]
    file2 <- as.vector(unlist(strsplit(file1, "\t")))
    file3 <- as.data.frame(matrix(file2, nrow = length(file1), byrow = TRUE),stringsAsFactors = FALSE)
    rm(file1, file2)
    dframe <- file3[, -c(1, ncol(file3)-1, ncol(file3))]
    rm(file3)
    dframe <- as.data.frame(sapply(dframe, as.numeric), stringsAsFactors = FALSE)
    colnames(dframe) <- paste0("Cluster", 1:ncol(dframe))
    dlist[[i]] <- dframe
    #names(dlist[[i]]) <- as.character(name)
  }
  if (number>1) {return(dlist)} else{return(dframe)}
}

#-------------------------------------------------------------------------------

# FUNCTION runsToDfAdmixture
#' Convert ADMIXTURE cluster files to R dataframe.
#' @description DEPRECATED. USE runsToDfMatrix(). Takes one or more ADMIXTURE cluster run files and converts each of 
#' them to separate dataframes.
#' @param files A character or character vector of one or more ADMIXTURE cluster run files. Use \code{choose.files(multi = TRUE)} 
#' to select interactively.
#' @return If a single file is selected, a single dataframe is returned. 
#' If multiple files are selected, a list with multiple dataframes is returned.
#' @seealso \code{\link{runsToDfStructure}}, \code{\link{runsToDfTess}}
#' @export
#'
runsToDfAdmixture <- function(files = NULL)
{
  warning("runsToDfAdmixture() is deprecated. Use runsToDfMatrix()")
  if (is.null(files) | (length(files) == 0)) stop("runsToDfAdmixture: No input files.")
  #number of files selected
  number <- length(files)
  #cat(paste("Number of files selected: ", number, "\n", sep = ""))
  
  #check file
  chk <- pophelper:::checkRuns(files)
  if (any(chk$type != "MATRIX")) stop("runsToDfAdmixture: Input contains one or more non-ADMIXTURE files/Incorrect input format.")
  if (any(chk$subtype != "SPACE")) stop("runsToDfAdmixture: Input contains one or more non-ADMIXTURE files/Incorrect input format.")
  
  i <- 1
  dlist <- vector("list",length = number)
  len1 <- length(files)
  for (i in 1:len1)
  {
    name <- base::gsub(".txt", "", basename(files[i]))
    
    # Test space-delim, tab-delim or comma-delim files here
    if(chk$subtype[i] == "SPACE") dframe <- read.delim(files[i], header=F, sep="", dec=".", stringsAsFactors = FALSE)
    if(chk$subtype[i] == "TAB") dframe <- read.delim(files[i], header=F, sep="\t", dec=".", stringsAsFactors = FALSE)
    if(chk$subtype[i] == "COMMA") dframe <- read.delim(files[i], header=F, sep=",", dec=".", stringsAsFactors = FALSE)
    
    if(!all(sapply(dframe, is.numeric))) stop("runsToDfAdmixture: One or more columns are not numeric.")
    colnames(dframe) <- paste0("Cluster", 1:ncol(dframe))
    dlist[[i]] <- dframe
  }
  if (number>1) {return(dlist)} else{return(dframe)}
}

#-------------------------------------------------------------------------------

# FUNCTION runsToDfMatrix
#' Convert MATRIX cluster files to R dataframe.
#' @description Takes one or more MATRIX cluster run files (tabular files, Admixture, fastStructure etc) and converts each of 
#' them to separate dataframes.
#' @param files A character or character vector of one or more ADMIXTURE cluster run files. Use \code{choose.files(multi = TRUE)} 
#' to select interactively.
#' @return If a single file is selected, a single dataframe is returned. 
#' If multiple files are selected, a list with multiple dataframes is returned.
#' @details Input files are expected to be Admixture run files or fastStructure meanQ files. 
#' Input files can also be any tab-delimited, space-delimited or comma-delimited tabular data without header.
#' @seealso \code{\link{runsToDfStructure}}, \code{\link{runsToDfTess}}
#' @examples 
#' alist <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' #create a dataframe
#' runsToDfMatrix(alist[1])
#' 
#' #create a list of dataframes
#' runsToDfMatrix(alist)
#' @export
#'
runsToDfMatrix <- function(files = NULL)
{
  if (is.null(files) | (length(files) == 0)) stop("runsToDfMatrix: No input files.")
  #number of files selected
  number <- length(files)
  #cat(paste("Number of files selected: ", number, "\n", sep = ""))
  
  #check file
  chk <- pophelper:::checkRuns(files)
  if (any(chk$type != "MATRIX")) stop("runsToDfMatrix: Input contains one or more non-MATRIX files/Incorrect input format.")
  if (any(is.na(chk$subtype))) stop("runsToDfMatrix: Input contains one or more non-MATRIX files/Incorrect input format.")
  
  i <- 1
  dlist <- vector("list",length = number)
  len1 <- length(files)
  for (i in 1:len1)
  {
    name <- base::gsub(".txt", "", basename(files[i]))
    
    # Test space-delim, tab-delim or comma-delim files here
    if(chk$subtype[i] == "SPACE") dframe <- read.delim(files[i], header=F, sep="", dec=".", stringsAsFactors = FALSE)
    if(chk$subtype[i] == "TAB") dframe <- read.delim(files[i], header=F, sep="\t", dec=".", stringsAsFactors = FALSE)
    if(chk$subtype[i] == "COMMA") dframe <- read.delim(files[i], header=F, sep=",", dec=".", stringsAsFactors = FALSE)
    
    if(!all(sapply(dframe, is.numeric))) stop("runsToDfMatrix: One or more columns are not numeric.")
    colnames(dframe) <- paste0("Cluster", 1:ncol(dframe))
    dlist[[i]] <- dframe
    #names(dlist[[i]]) <- as.character(name)
  }
  if (number>1) {return(dlist)} else{return(dframe)}
}

#-------------------------------------------------------------------------------

# FUNCTION collectRunsTess
#' Collect TESS cluster run files from multiple folders
#' @description Collect TESS cluster run files from multiple folders to one folder and rename by run
#' @param runsdir A character indicating the directory containing TESS runs in multiple directories. Use \code{choose.dir()} for interactively selecting the directory. If NA, or no directory is selected, the current working directory is used.
#' @param newdir A character indicating the name of the new directory to be created with the collected runs. IF NA, the default name 'AllTESSRuns' is used. 
#' @param quiet A logical indicating if a message is to be displayed for directories without TESS runs and number of runs copied and renamed. 
#' @details Within each TESS run folder, the function searches for filename ending with 'TR.txt' as the cluster file. This file is copied to the new folder and renamed as the name of the respective run directory. Therefore, DO NOT manually rename original run files or directories.
#' @return Two integers are ruturned. The first denotes the number of TESS run files copied and renamed. The second number denotes number of directories without TESS run files.
#' @examples 
#' \dontrun{
#' collectRunsTess("path")
#' }
#' @export
#' 
collectRunsTess <- function(runsdir = NA, newdir = NA, quiet = FALSE)
{
  if(!is.logical(quiet)) stop("collectRunsTess: Argument 'quiet' set incorrectly. Set as TRUE or FALSE.")
  currwd <- getwd()
  if (is.na(newdir)) newdir <- "AllTESSRuns"
  if (is.na(runsdir)) runsdir <- currwd
  dirs <- list.dirs(path = runsdir, full.names = TRUE, recursive = FALSE)
  dir.create(paste0(runsdir, "/", newdir))
  k <- 0
  l <- 0
  len1 <- length(dirs)
  for (i in 1:len1)
  {
    setwd(dirs[i])
    files <- list.files()
    sel <- grep("\\w+TR.txt", files)
    if (length(sel) == 0) 
    {
      if (!quiet) cat(paste0("No TESS cluster file found in directory: ", basename(dirs[i]), "\n"))
      l = l+1
    }
    if (length(sel) != 0) 
    {
      file.copy(from = paste0(dirs[i], "/", files[sel], sep = ""), to = paste0(runsdir, "/", newdir)) 
      file.rename(from = paste0(runsdir, "/", newdir, "/", files[sel]), to = paste0(runsdir, "/", newdir, "/", basename(dirs[i]), ".txt"))
      k = k+1  
    }
  }
  setwd(currwd)
  if (!quiet) cat(paste0(k, " TESS cluster files copied and renamed.\n"))
  return(c(k, l))
}

#-------------------------------------------------------------------------------

# FUNCTION collectClumppOutput
#' Collect CLUMPP output files from multiple folders
#' @description Collect CLUMPP output files from multiple folders to one folder
#' @param prefix A character indicating the prefix of the CLUMPP directories before the underscore. For ex. if the directories are STRUCTUREpop_K2, then prefix is STRUCTUREpop.
#' @param filetype A character indicating the type of file to be copied. Options are 'aligned' to copy aligned files only, 'merged' to copy merged files only and 'both' to copy both files.
#' @param runsdir A character denoting the directory containing CLUMPP output files in multiple directories. Use \code{choose.dir()} for interactively selecting the directory. If NA, the current working directory is used.
#' @param newdir A character indicating the name of the new directory to be created with the collected runs. IF NA, the a directory name joining prefix and filetype is created. 
#' @param quiet A logical indicating if a message is to be displayed showing the number of folders processed and number of files processed. 
#' @details Within each CLUMPP output folder, the function searches for filenames containing combination of prefix and filetype. This file is copied to the new folder. Therefore, do not manually rename CLUMPP output files or output directories.
#' @return Two integers are ruturned. The first denotes the number of directories processed. The second number denotes the number files copied.
#' @examples 
#' \dontrun{
#' collectClumppOutput(runsdir="path")
#' collectClumppOutput(prefix="TESSpop",runsdir="path")
#' collectClumppOutput(prefix="MATRIXpop",runsdir="path")
#' }
#' @export
#' 
collectClumppOutput <- function(prefix = "STRUCTUREpop", filetype = "aligned", runsdir = NA, newdir = NA, quiet = FALSE)
{
  if(!is.logical(quiet)) stop("collectClumppOutput: Argument 'quiet' set incorrectly. Set as TRUE or FALSE.")
  #check imgoutput
  if (tolower(filetype)!="aligned" && tolower(filetype)!="merged" && tolower(filetype)!="both") stop("collectClumppOutput: Argument 'filetype' set incorrectly. Set as 'aligned', 'merged' or 'both'.")
  
  currwd <- getwd()
  if (is.na(newdir)) newdir <- paste0(prefix, "-", filetype)
  if (is.na(runsdir)) runsdir <- currwd
  dirs <- list.dirs(path = runsdir, full.names = TRUE, recursive = FALSE)
  dirs1 <- dirs[grep(paste0(prefix, "_"), dirs)]
  dir.create(paste0(runsdir, "/", newdir))
  k <- 0
  l <- 0
  i <- 1
  len1 <- length(dirs1)
  for (i in 1:len1)
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
      cat("No suitable file found in directory: ", basename(dirs1[i]), "\n", sep = "")
    }
    if (length(sel3) != 0) 
    {
      file.copy(from = paste0(dirs1[i], "/", files[sel3]), to = paste0(runsdir, "/", newdir)) 
      k = k+1
      l = l+length(sel3)
    }
  }
  setwd(currwd)
  if (!quiet) cat(paste0("Directories processed: ", k, "\nFiles copied: ", l, "\n"))
  return(c(k, l))
}

#-------------------------------------------------------------------------------

#FUNCTION getDim
#' Internal: Get dimensions for figures.
#' @description Internal: Generate height and width of figure based on number of individuals.
#' @param ind A numeric indicating the number of individuals.
#' @param units A character indicating the unit of dimension: "cm","mm","in".
#' @param height A numeric indicating the height of each plot.
#' @param width A numeric indicating the width of each plot.
#' @param res A numeric indicating the resolution of the figure.
#' @param imgtype A character denoting image format. "png", "jpeg" or "pdf".
#' @param labpanelheight A numeric denoting the height of the label panel.
#' @param plotnum A numeric indicating the number of plots in the figure.
#' @return a vector with height and width.
# @export
#'
getDim <- function(ind=NA, units = NA, height = NA, width = NA, res = NA, imgtype=NA, labpanelheight=NA, plotnum = NA)
{
  if (is.na(units)) units <- "cm"
  if (is.na(units) && imgtype=="pdf") units <- "in"
  if (is.na(res)) res <- 300
  if (is.na(plotnum)) plotnum <- 1

  #height
  if (is.na(height))
  {
    if (plotnum == 1) height <- 2
    if (plotnum > 1) height <- 1.2
    if (imgtype=="pdf") height <- pophelper:::unitConverter(value=height, fromunit="cm", tounit="in", res=res)
  }else{
    if (units=="mm" && imgtype != "pdf") height <- pophelper:::unitConverter(value=height, fromunit="mm", tounit="cm", res=res)
    if (units=="px" && imgtype != "pdf") height <- pophelper:::unitConverter(value=height, fromunit="px", tounit="cm", res=res)
    if (units=="in" && imgtype != "pdf") height <- pophelper:::unitConverter(value=height, fromunit="in", tounit="cm", res=res)
    if (units=="cm" && imgtype == "pdf") height <- pophelper:::unitConverter(value=height, fromunit="cm", tounit="in", res=res)
    if (units=="mm" && imgtype == "pdf") height <- pophelper:::unitConverter(value=height, fromunit="mm", tounit="in", res=res)
    if (units=="px" && imgtype == "pdf") height <- pophelper:::unitConverter(value=height, fromunit="px", tounit="in", res=res)
  }
  height <- height*plotnum
  
  #width
  if (is.na(width))
  {
    if (is.na(ind)) stop("getDim: Argument ind is empty.")
    width <- ind*0.020 
    if (width < 5) width <- 5
    if (width > 30) width <- 30
    if (imgtype=="pdf") width <- pophelper:::unitConverter(value=width, fromunit="cm", tounit="in", res=res)
  }else{
    if (units=="mm" && imgtype != "pdf") width <- pophelper:::unitConverter(value=width, fromunit="mm", tounit="cm", res=res)
    if (units=="px" && imgtype != "pdf") width <- pophelper:::unitConverter(value=width, fromunit="px", tounit="cm", res=res)
    if (units=="in" && imgtype != "pdf") width <- pophelper:::unitConverter(value=width, fromunit="in", tounit="cm", res=res)
    if (units=="cm" && imgtype == "pdf") width <- pophelper:::unitConverter(value=width, fromunit="cm", tounit="in", res=res)
    if (units=="mm" && imgtype == "pdf") width <- pophelper:::unitConverter(value=width, fromunit="mm", tounit="in", res=res)
    if (units=="px" && imgtype == "pdf") width <- pophelper:::unitConverter(value=width, fromunit="px", tounit="in", res=res)
  }
  
  #labpanelheight
  if (is.na(labpanelheight)) 
  {
    #ggplot version
    ggv <- as.numeric(gsub("\\.","",packageDescription("ggplot2", fields = "Version")))
    if(ggv < 200) labpanelheight <- 0.4
    if(ggv == 200 || ggv > 200) labpanelheight <- 0.5
    if (imgtype=="pdf") labpanelheight <- pophelper:::unitConverter(value=labpanelheight, fromunit="cm", tounit="in", res=res)
  }else{
    if (units=="mm" && imgtype != "pdf") labpanelheight <- pophelper:::unitConverter(value=labpanelheight, fromunit="mm", tounit="cm", res=res)
    if (units=="in" && imgtype != "pdf") labpanelheight <- pophelper:::unitConverter(value=labpanelheight, fromunit="in", tounit="cm", res=res)
    if (units=="px" && imgtype != "pdf") labpanelheight <- pophelper:::unitConverter(value=labpanelheight, fromunit="px", tounit="cm", res=res)
    if (units=="mm" && imgtype == "pdf") labpanelheight <- pophelper:::unitConverter(value=labpanelheight, fromunit="mm", tounit="in", res=res)
    if (units=="cm" && imgtype == "pdf") labpanelheight <- pophelper:::unitConverter(value=labpanelheight, fromunit="cm", tounit="in", res=res)
    if (units=="px" && imgtype == "pdf") labpanelheight <- pophelper:::unitConverter(value=labpanelheight, fromunit="px", tounit="in", res=res)
  }
  
  if (imgtype!="pdf") units1 <- "cm"
  if (imgtype=="pdf") units1 <- "in"
  
  lst <- list(height=round(height,2), width=round(width,2), labpanelheight=round(labpanelheight,2),units=units1)
  return(lst)
}

#-------------------------------------------------------------------------------

# FUNCTION getPlotParams
#' Internal: Generate parameters for plots with labels
#' @description Internal: Generates various parameters required for plotting with labels.
#' @param poplab A character vector of labels same length as number of individuals.
#' @param plotnum A numeric indicating the number of plots on the figure.
#' @param labsize A numeric indicating the size of the labels.
#' @param labangle A numeric indicating the angle/rotation of labels. 0 is horizontal while 90 is vertical.
#' @param labjust A numeric indicating the justification of labels. Defaults to 0.5 if labangle = 0  or 1 if 
#' labangle between 20 and 135.
#' @param pointsize  A numeric indicating the size of points on label marker line.
#' @param linethick A numeric indicating the thickness of the label marker line.
#' @return A list with following plot parameters: poplab, plotnum, labsize, 
#' labangle, labjust, pointsize, linethick.
# @export
#' 
getPlotParams <- function(poplab = NA, plotnum = 1, labsize = NA, labangle = NA, labjust = NA,pointsize = NA, linethick = NA)
{
  if (all(is.na(poplab))) stop("getPlotParams: Labels are empty.")
  
  #estimate ct based on number of labels/ind
  lp <- length(as.character(poplab))
  
  #calculate labangle, just and margins
  if (is.na(labangle)) labangle <- 0
  if (labangle == 0)
  {
    if (is.na(labjust)) labjust <- 0.5
  }
  
  if (abs(labangle) > 20 & abs(labangle) < 135)
  {
    if (is.na(labjust)) labjust <- 1
    #bmar <- round(max(nchar(as.character(poplab)))/8, 2)+round(lp/900, 3)
    #if (all(is.na(fmar))) fmar <- c(0.2, 0.2, bmar, 0)
  }
  
  
  #linepos1 <- linepos
  #labpos1 <- labpos
  labsize1 <- labsize
  pointsize1 <- pointsize
  linethick1 <- linethick
  
  #if (is.na(linepos)) linepos1 <- lp*-0.000035
  #if (is.na(linepos)) linepos1 <- 1
  #if (is.na(labpos)) labpos1 <- linepos1*2.4
  #if (is.na(labpos)) labpos1 <- 0.2
  if (is.na(labsize)) labsize1 <- lp*0.00125
  if (is.na(pointsize)) pointsize1 <- lp*0.0016
  if (is.na(linethick)) linethick1 <- lp*0.0003
  
  #if (is.na(linepos)) {if (linepos1 < -0.08) linepos1 <- -0.08}
  #if (is.na(labpos)) {if (linepos1 < -0.192) labpos1 <- linepos1*2.4}
  if (is.na(labsize)) {if (labsize1 < 1.5) labsize1 <- 1.5}
  if (is.na(pointsize)) {if (pointsize1 < 1.2) pointsize1 <- 1.2}
  if (is.na(linethick)) {if (linethick1 < 0.3) linethick1 <- 0.3}
  
  #if (is.na(linepos)) {if (linepos1 > -0.07) linepos1 <- -0.07}
  #if (is.na(labpos)) {if (labpos1 > -0.168) labpos1 <- linepos1*2.4}
  if (is.na(labsize))  {if (labsize1 > 2.5) labsize1 <- 2.5}
  if (is.na(pointsize)) {if (pointsize1 > 3.2) pointsize1 <- 3.2}
  if (is.na(linethick)) {if (linethick1 > 0.6) linethick1 <- 0.6}
  
  dlist <- list(poplab = poplab, plotnum = plotnum, labsize = labsize1, labangle = labangle, labjust = labjust, 
                pointsize = pointsize1, linethick = linethick1)
  return(dlist)
}

#-------------------------------------------------------------------------------

# FUNCTION popLabels
#' Internal: Handles pop subset/order
#' @description Internal: Takes a cluster matrix along with pop labels and pop order. If pop order is different,
#' cluster matrix is reordered by pops without sorting individuals. Also creates labelpos and markerpos dfs.
#' @param df A q-matrix dataframe
#' @param poplab A character vector of population labels
#' @param subsetpops A character or character vector of pop names to subset/reorder
#' @param popmean A logical indicating if q-matrix must be converted from individual to population mean.
#' @param labpos A numeric indicating y-axis position of labels
#' @param linepos A numeric indicating y-axis position of label line
#' @return Returns a list with subsetted/reordered q-matrix and a character vector 
#' or subsetted/reordered pop label vector. If labpos and linepos is not NA, then they are included in the list.
# @export
popLabels <- function(df=NULL,poplab=NA,subsetpops=NA,popmean=FALSE,labpos=NA,linepos=NA)
{
  if (is.null(df)) stop("popLabels: Argument 'df' is empty.")
  if (any(is.na(poplab))) stop("popLabels: Argument 'poplab' is empty.")
  if (length(poplab) != nrow(df)) stop(paste0("popLabels: Length of pop labels (",length(poplab),") not equal to number of individuals (",nrow(df),")."))
  if (any(is.na(subsetpops))) subsetpops <- "None"
  if (is.na(labpos)) labpos <- 0
  if (is.na(linepos)) linepos <- 1
  
  poplab = as.character(poplab)
  if(popmean) df <- as.data.frame(sapply(df,ave,poplab),stringsAsFactors=F)

  #in case of subsetpops
  if(!("None" %in% subsetpops))
  {
    subsetpops = as.character(subsetpops)
    #rle pops
    rlepop <- rle(poplab)
    
    #checks
    if(length(rlepop$values) != length(unique(rlepop$values))) stop("popLabels: Duplicated contiguous block of pop labels. Not possible to subset or reorder pops.")
    if(!all(subsetpops %in% rlepop$values)) stop("popLabels: Subsetted pop not present in pop labels.")
    
    #compute positions and labels of subset pops
    popvec = vector()
    posvec = vector()
    for(i in 1:length(subsetpops))
    {
      selpop = subsetpops[i]
      pos = which(poplab==selpop)
      posvec = c(posvec,pos)
      popvec = c(popvec,rep(subsetpops[i],length(pos)))
    }
    df <- df[posvec,]
    
    #create labelpos and markerpos
    if((!is.null(labpos)) && (!is.null(linepos)))
    {
      rlepop <- rle(popvec)
      labelpos <- data.frame(label=rlepop$values,freq=rlepop$lengths,stringsAsFactors = F)
      markerpos <- data.frame(markerxpos=c(0, cumsum(labelpos$freq)),stringsAsFactors=F)
      labelpos$labxpos <- round((diff(markerpos$markerxpos)/2)+markerpos$markerxpos[1:length(markerpos$markerxpos)-1], 1)
      labelpos$labypos <- rep(labpos, nrow(labelpos))
      markerpos$temp <- factor(rep(1, nrow(markerpos)))
      markerpos$markerypos <- rep(linepos, nrow(markerpos))
    }
  }
  
  if("None" %in% subsetpops)
  {
    #create labelpos and markerpos
    if((!is.null(labpos)) && (!is.null(linepos)))
    {
      rlepop <- rle(poplab)
      labelpos <- data.frame(label=rlepop$values,freq=rlepop$lengths,stringsAsFactors = F)
      markerpos <- data.frame(markerxpos=c(0, cumsum(labelpos$freq)),stringsAsFactors=F)
      labelpos$labxpos <- round((diff(markerpos$markerxpos)/2)+markerpos$markerxpos[1:length(markerpos$markerxpos)-1], 1)
      labelpos$labypos <- rep(labpos, nrow(labelpos))
      markerpos$temp <- factor(rep(1, nrow(markerpos)))
      markerpos$markerypos <- rep(linepos, nrow(markerpos))
    }
    
    df <- df
    popvec <- poplab
  }
  
  #adjust divider position
  markerpos$divxpos <- markerpos$markerxpos+0.5
  
  if((!is.null(labpos)) && (!is.null(linepos)))
  {
    return(list(df=df,poplab=popvec,labelpos=labelpos,markerpos=markerpos))
  }
  
  if((!is.null(labpos)) && (!is.null(linepos)))
  {
    return(list(df=df,poplab=popvec)) 
  }
}

#-------------------------------------------------------------------------------

# FUNCTION plotRuns
#' Plot STRUCTURE, TESS, MATRIX or TAB files as barplots.
#' @description Plot one or more STRUCTURE, TESS, MATRIX run files or TAB (aligned/combined/merged) files. The STRUCTURE, TESS and MATRIX files can be plotted individually or joined together. The TAB files will be plotted based on number of runs in each file.
#' @param files A character vector of one or more STRUCTURE/TESS/table files
#' @param imgoutput A character with options: 'sep','join' or'tab'.If set to "sep", STRUCTURE/TESS/MATRIX run files are plotted as separate image files. If set to "join", STRUCTURE/TESS/MATRIX run files are joined into a single image. If set to "tab", combined/aligned/merged files are plotted into separate or joined plots based on number of tables within each file.
#' @param poplab A character vector of population labels equal to length of individuals. Each pop name must repeat to the number of individuals present in each pop.
#' @param popcol A vector of colours (representing populations) for colouring clusters. If NA, colours are automatically generated. K=1 to 12 are custom unique colours while K>12 are coloured by function \code{gplots::rich.colors()}.
#' @param sortind A character indicating how individuals are sorted. Default is NA. Other options are 'all' or any one cluster (eg. 'Cluster1'). See details.
#' @param subsetpops A character or character vector with population names to subset or reorder populations. Only applicable when \code{poplab} is in use. Default is NA. See details.
#' @param popmean A logical indicating if q-matrix must be converted from individual values to population mean values. Applicable only when \code{poplab} is in use.
#' @param na.rm A logical indicating if NAs are removed from data, else \code{ggplot} prints warning messages for NAs. If set to TRUE, NAs are removed before plotting and \code{ggplot} NA warning is suppressed.
#' @param imgtype A character indicating image file type. Possible options are "png","jpeg" or "pdf". For pdf, height and width are in inches and res does not apply.
#' @param height A numeric indicating the height of individual run plot. By default, automatically generated based on number of Individuals. Ranges between 2.5cm and 4.6cm.
#' @param width A numeric indicating the width of individual run plot. By default, automatically generated based on number of individuals. Ranges between 5cm and 30cm.
#' @param dpi A numeric indicating the image resolution in pixels per inch (PPI). Defaults to 300.
#' @param units A numeric indicating the units of height and width. Default set to "cm".
#' @param panelspacer A numeric indicating the space at the bottom of one or more barplot panels.
#' @param flab A logical indicating if strip panels on right side must be shown. Strip panels display file name and K value. Defaults to TRUE.
#' @param flabsize A numeric indicating the size of the filename label. Defaults to 4. Applicable only if \code{flab=T}.
#' @param flabcol A colour character indicating the colour of the filename label. Defaults to "grey10". Applicable only if \code{flab=T}.
#' @param flabbackcol A colour character denoting the background colour of the filename label. Defaults to white. Applicable only if \code{flab=T}.
#' @param labspacer A numeric indicating the space between the plots and the label area. Applicable only with population labels.
#' @param labpanelheight A numeric indicating the height of the label area in cm. Default is 0.4 cm. If units are different, cm will be be converted automatically. Applicable only with population labels.
#' @param labpos A numeric indicating the y position of the labels. Applicable only with population labels.
#' @param labsize A numeric indicating the size of the labels. 
#' @param labangle A numeric indicating the angle/rotation of labels. 0 is horizontal while 90 is vertical.
#' @param labjust A numeric indicating the justification of labels. Defaults to 0.5 if labangle = 0  or 1 if labangle between 20 and 135.
#' @param labcol A colour character for the colour of labels. Defaults to "grey30".
#' @param pointsize A numeric indicating the size of points on label marker line.
#' @param pointcol A colour character for the colour of points on the label marker line. Defaults to "grey30".
#' @param pointbgcol A colour character for the background of marker point for certain pointtypes.
#' @param pointtype A character or number for the type of points on the label marker line. Defaults to |. Same as pch in standard R.
#' @param linepos A numeric indicating the y position of the label marker line and the points.
#' @param linethick A numeric indicating the thickness of the label marker line.
#' @param linecol A colour character for the label marker line. Defaults to "grey30".
#' @param linetype A numeric indicating the type of line for marker line. Same as lty in standard R. Default value is 1.
#' @param div A logical indicating if divider lines between population clusters must be drawn. Applicable only with population labels.
#' @param divcol A character or hexadecimal colour denoting the colour of the divider line. Default is white.
#' @param divtype A numeric indicating the type of line for the divider line. Same as lty in standard R. Default value is 1.
#' @param divthick A numeric indicating the thickness of the divider line. Default is 0.25
#' @return Nothing is returned.
#' @details It is possible to set either height or width and leave other as default.\cr
#' \strong{sortind}\cr
#' This argument takes one character as input.  Default NA means individuals are plotted in the same order as input. Individuals can be ordered by one cluster. For ex. \code{sortind="Cluster1"} or \code{sortind="Cluster2"}.
#' To order by all clusters as the 'Sort by Q' option in STRUCTURE software, use \code{sortind="all"}. When using \code{sortind} with \code{subsetpops}, individuals
#' are sorted within the population groups.\cr
#' \cr
#' \strong{subsetpops}\cr
#' This argument takes one or more characters as input. Use only populations labels used in the \code{poplab} vector. For ex. In case of two pops in order 'Pop A' and 'Pop B',
#' use \code{subsetpops=c("Pop B","Pop A")} to change order of populations. Use \code{subsetpops="Pop B"} to subset only Pop B.
#' @seealso \code{\link{plotMultiline}}
#' @examples 
#' slist <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' 
#' #plot one separate figure
#' plotRuns(files=slist[1])
#' 
#' #plot two separate figures
#' plotRuns(files=slist[1:2])
#' 
#' #plot a joined figure with 3 plots
#' plotRuns(files=slist[1:2],imgoutput="join")
#' 
#' #sort individuals
#' plotRuns(files=slist[c(1,3)],sortind="all")
#' plotRuns(files=slist[c(1,3)],sortind="all",imgoutput="join")
#' 
#' pops <- read.delim(system.file("files/structurepoplabels.txt", 
#' package="pophelper"), header=F,stringsAsFactors=F)
#' 
#' #plot with labels
#' plotRuns(files=slist[1],poplab=pops$V1)
#' plotRuns(files=slist[1:2],poplab=pops$V1,imgoutput="join")
#' 
#' #sort within population groups
#' plotRuns(files=slist[1:2],poplab=pops$V1,imgoutput="join",sortind="Cluster1")
#' 
#' @import gtable
#' @import grid
#' @import gridExtra
#' @import tidyr
#' @export
#' 
plotRuns <- function(files = NULL, imgoutput = "sep", poplab = NA, popcol = NA, sortind=NA, subsetpops=NA, popmean = FALSE, na.rm = TRUE, imgtype = "png", height = NA, width = NA, dpi = NA, units = NA,
                     panelspacer=NA, flab=TRUE, flabsize = NA, flabcol = NA, flabbackcol = NA,
                     labspacer=NA,labpanelheight=NA,labpos = NA, labsize = NA, labangle = NA, labjust = NA, labcol = NA,
                     pointsize = NA, pointcol = NA, pointbgcol = NA, pointtype = NA, linepos = NA, linethick = NA, linecol = NA, linetype = NA,
                     div=TRUE, divcol = NA, divtype = NA, divthick = NA)
{ 
  #if no files chosen, stop excecution, give error message
  if (is.null(files) | (length(files) == 0)) stop("plotRuns: No input files.")
  if (any(files == "")) stop("plotRuns: Input has no filename.")
  poplab <- as.character(poplab)
  if(length(poplab)>1 && any(is.na(poplab))) stop("plotRuns: Missing values (NA) in poplab.")
  #check imgoutput
  imgoutput <- tolower(imgoutput)
  if (imgoutput != "sep" && imgoutput != "join" && imgoutput != "tab") stop("plotRuns: Argument 'imgoutput' set incorrectly. Set as 'sep' to export as separate plots. Set as 'join' to export as one joined plot. Set as 'tab' if input is aligned/combined/merged files.")
  #check image
  imgtype <- tolower(imgtype)
  if (imgtype!="png" && imgtype != "pdf" && imgtype != "jpeg") stop("plotRuns: Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg' or 'pdf'.")
  if (all(!is.na(poplab)) && !is.na(labpos)) {if(labpos > 1 | labpos < 0) stop("plotRuns: Argument 'labpos' is set incorrectly. Set a numeric value between 0 and 1. To further increase space, adjust argument 'labpanelheight'.")}
  if (all(!is.na(poplab)) && !is.na(linepos)) {if(linepos > 1 | linepos < 0) stop("plotRuns: Argument 'linepos' is set incorrectly. Set a numeric value between 0 and 1. To further increase space, adjust argument 'labpanelheight'.")}
  if (all(!is.na(sortind)))
  {
    if(length(sortind) > 1) stop("plotRuns: Argument 'sortind' must be of length 1. Use 'all' or a cluster like 'Cluster1'.")
    if(sortind != "all" && !grepl("Cluster[0-9+]",sortind)) stop("plotRuns: Argument 'sortind' must be set to 'all' or a cluster like 'Cluster1'.")
  }
  if(any(is.na(poplab)) && all(!is.na(subsetpops))) stop("plotRuns: Argument 'subsetpops' can only be used when argument 'poplab' is in use.")
  if(all(!is.na(poplab)) && all(!is.na(subsetpops))) {if(!any(subsetpops %in% poplab)) stop("plotRuns: Elements in 'subsetpops' must be an element in 'poplab'.")}
  if(!is.logical(popmean)) stop("plotRuns: Argument 'popmean' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(na.rm)) stop("plotRuns: Argument 'na.rm' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(flab)) stop("plotRuns: Argument 'flab' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(div)) stop("plotRuns: Argument 'div' set incorrectly. Set as TRUE or FALSE.")
  
  #ggplot version
  ggv <- as.numeric(gsub("\\.","",packageDescription("ggplot2", fields = "Version")))
  
  col3 <- "grey30"
  if (is.na(dpi)) dpi <- 300
  if (is.na(units)) units <- "cm"
  if (is.na(flabsize)) flabsize <- 4
  if (is.na(flabcol)) flabcol <- "grey10"
  if (is.na(flabbackcol)) flabbackcol <- "white"
  if (is.na(labpos))
  {
  if(ggv < 200) labpos <- 0
  if(ggv == 200 || ggv > 200) labpos <- 0.25
  }
  if (is.na(labcol)) labcol <- col3
  if (is.na(pointcol)) pointcol <- col3
  if (is.na(pointbgcol)) pointbgcol <- col3
  if (is.na(pointtype)) pointtype <- "|"
  if (is.na(linepos))
  {
    if(ggv < 200) linepos <- 1
    if(ggv == 200 || ggv > 200) linepos <- 0.75
  }
  if (is.na(linecol)) linecol <- col3
  if (is.na(linetype)) linetype <- 1
  if (is.na(panelspacer)) panelspacer <- 0.05
  if (is.na(labspacer)) labspacer <- 0
  if (is.na(divcol)) divcol <- "white"
  if (is.na(divtype)) divtype <- "21"
  if (is.na(divthick)) divthick <- 0.25
  
  #length of files
  flen <- length(files)
  #make labeller function for facets
  if(ggv < 200) plotlabeller <- function(variable, value){return(facetnames[value])}
  
  #ss separate plots
  if (imgoutput == "sep")
  {
    #if(ggv < 200) facetnames <- list()
    for (i in 1:flen)
    {
      fname <- base::gsub(".txt", "", basename(files[i]))
      fname <- base::gsub(".csv", "", fname)
      
      #check files
      chk <- pophelper:::checkRuns(files[i])$type
      if (chk == "STRUCTURE") df1 <- runsToDfStructure(files = files[i])
      if (chk == "TESS") df1 <- runsToDfTess(files = files[i])
      #if (chk == "ADMIXTURE") df1 <- runsToDfAdmixture(files = files[i])
      if (chk == "MATRIX") df1 <- runsToDfMatrix(files = files[i])
      if (chk == "TAB") stop("plotRuns: If using table files (combined/aligned/merged), set argument imgoutput = 'tab'.")
      if (chk == "UNIDENTIFIED") stop("plotRuns: Unidentified input format.")
      
      #ordering pops
      if(all(!is.na(poplab)))
      {
          #ordering pops
          lablist <- pophelper:::popLabels(df = df1, poplab = poplab, subsetpops = subsetpops,
                                            popmean=popmean, labpos = labpos, linepos = linepos)
          df1 <- lablist$df
          poplab <- lablist$poplab
          markerpos <- lablist$markerpos
          labelpos <- lablist$labelpos
          poplabnames <- rle(poplab)$values
          rm(lablist)
      }
      
      #sorting individuals
      if(!is.na(sortind))
      {
        if(sortind == "all")
        {
          maxval <- apply(df1,1,max)
          matchval <- vector(length=nrow(df1))
          for(j in 1:nrow(df1)) matchval[j] <- match(maxval[j],df1[j,])
          dftemp <- df1
          dftemp$maxval <- maxval
          dftemp$matchval <- matchval
          
          if(any(is.na(poplab))) df1 <- df1[with(dftemp, order(matchval,-maxval)), ,drop=FALSE]
          if(all(!is.na(poplab)))
          {
            dftemp$pop <- poplab
            #sort within population
            # for(k in 1:length(poplabnames))
            # {
            #   dftemp1 <- subset(dftemp,dftemp$pop==poplabnames[k])
            #   dftemp1$pop <- NULL
            #   dftemplist[[k]] <- dftemp1[with(dftemp1, order(matchval,-maxval)), ,drop=FALSE]
            # }

            tovec <- cumsum(rle(dftemp$pop)$lengths)
            fromvec <- (tovec - rle(dftemp$pop)$lengths)+1
            dftemplist <- vector("list",length=length(poplabnames))
            for(k in 1:length(tovec))
            {
              dftemp1 <- dftemp[fromvec[k]:tovec[k],]
              dftemp1$pop <- NULL
              dftemplist[[k]] <- dftemp1[with(dftemp1, order(matchval,-maxval)), ,drop=FALSE]
            }
            
            df1 <- do.call("rbind",dftemplist)
            df1$maxval <- NULL
            df1$matchval <- NULL
          }
          
          }else{
          if(!(sortind %in% colnames(df1))) stop("plotRuns: Ordering cluster not found in file header.")
            if(any(is.na(poplab))) df1 <- df1[order(df1[[sortind]]), ,drop=FALSE]
            if(all(!is.na(poplab)))
            {
              clnum <- which(sortind == colnames(df1))
              dftemp <- df1
              dftemp$pop <- poplab
              #sort within population
              # for(k in 1:length(poplabnames))
              # {
              #   dftemp1 <- subset(dftemp,dftemp$pop == poplabnames[k])
              #   dftemp1$pop <- NULL
              #   dftemplist[[k]] <- dftemp1[order(dftemp1[[sortind]]), ,drop=FALSE]
              # }
              
              tovec <- cumsum(rle(dftemp$pop)$lengths)
              fromvec <- (tovec - rle(dftemp$pop)$lengths)+1
              dftemplist <- vector("list",length=length(poplabnames))
              for(k in 1:length(tovec))
              {
                dftemp1 <- dftemp[fromvec[k]:tovec[k],]
                dftemp1$pop <- NULL
                dftemplist[[k]] <- dftemp1[order(dftemp1[[sortind]]), ,drop=FALSE]
              }
              
              df1 <- do.call("rbind",dftemplist)
            }
        }
      }
      
      k <- ncol(df1)
      Ind <- nrow(df1)
      df1$Ind <- factor(1:nrow(df1))
      df1$Num <- factor(rep(i, nrow(df1)))
      #df1$Lab <- poplab
      #df2 <- reshape::melt.data.frame(df1, id.var = c("Ind", "Num"))
      df2 <- tidyr::gather(df1,"variable","value",-c(Ind,Num))
      df2 <- df2[rev(1:nrow(df2)),] 
      if(ggv < 200) facetnames <- as.list(paste0(fname, "\n", "K=", k))
      if(ggv == 200 || ggv > 200)
      {
        facetnames <- paste0(fname, "\n", "K=", k)
        names(facetnames) <- levels(df1$Num)
      }

      #get Colours
      coll <- popcol
      if (any(is.na(popcol))) coll <- getColours(k)
      if (length(coll) < k) stop(paste0("plotRuns: Number of colours (",length(coll),") less than number of clusters (",k,")."))
      
      #getDim
      dimtemp <- pophelper:::getDim(ind = Ind, height = height, width = width, res = dpi, units = units, imgtype=imgtype, labpanelheight=labpanelheight, plotnum = 1)
      height1 <- as.numeric(dimtemp[1])
      width1 <- as.numeric(dimtemp[2])
      labpanelheight1 <- as.numeric(dimtemp[3])
      units1 <- as.character(dimtemp[4])
      
      if (any(is.na(poplab)))
      {
        #plot
        p <- ggplot2::ggplot(data = df2, aes(x = Ind, y = value, fill = variable))+
          geom_bar(width = 1, stat = "identity", position = "fill", na.rm = na.rm)+
          scale_x_discrete(expand = c(0, 0))+
          scale_y_continuous(expand = c(0, 0))+
          scale_fill_manual(values = coll)
        
        if(ggv == 200 || ggv > 200) p <- p + facet_grid(Num~., labeller = labeller(Num = facetnames))
        if(ggv < 200) p <- p + facet_grid(Num~., labeller = plotlabeller)
          
          p <- p + labs(x = NULL, y = NULL)+
          theme(legend.position = "none", panel.grid = element_blank(), panel.background = element_blank(), 
                axis.ticks = element_blank(), axis.text = element_blank(), axis.line = element_blank(), 
                axis.title = element_blank(), strip.text = element_text(size = flabsize, colour = flabcol), 
                strip.background = element_rect(colour = flabbackcol, fill = flabbackcol), 
                plot.margin = grid::unit(c(0.1, 0, 0, 0), "lines"),
                panel.margin=grid::unit(panelspacer,"lines"))
        
        #remove strip panels on right
        if(!flab) p <- p + theme(strip.text=element_blank())
        
        if (imgtype == "png") png(paste0(fname, ".png"), height = height1, width = width1, res = dpi, units = units1, type = "cairo")
        if (imgtype == "jpeg") jpeg(paste0(fname, ".jpg"), height = height1, width = width1, res = dpi, units = units1, quality = 100)
        if (imgtype == "pdf") pdf(paste0(fname, ".pdf"), height = height1, width = width1)
        print(p)
        dev.off()
        if (imgtype == "png") cat(paste0(fname, ".png exported.\n"))
        if (imgtype == "jpeg") cat(paste0(fname, ".jpg exported.\n"))
        if (imgtype == "pdf") cat(paste0(fname, ".pdf exported.\n"))
      }
      if (all(!is.na(poplab)))
      {
        if (Ind!=length(as.character(poplab))) stop(paste0("plotRuns: Length of labels (", length(as.character(poplab)),") do not match number of individuals in input file (",Ind,")."))
        
        ppar <- pophelper:::getPlotParams(poplab = poplab, plotnum = 1, labsize = labsize, labangle = labangle, 
                              labjust = labjust,pointsize = pointsize, linethick = linethick)
        
        labangle <- ppar$labangle
        labjust <- ppar$labjust
        labsize <- ppar$labsize
        labjust <- ppar$labjust
        pointsize <- ppar$pointsize
        linethick <- ppar$linethick
        
        #add labpos to lframe df
        #lframe$labpos <- as.numeric(rep(labpos,nrow(lframe)))
        #pos1$linepos <- as.numeric(rep(linepos,nrow(pos1)))
        
        p1 <- ggplot2::ggplot(data = df2, aes(x = Ind, y = value, fill = variable))+
          geom_bar(width = 1, stat = "identity", position = "fill", na.rm = na.rm)+
          scale_x_discrete(expand = c(0, 0))+
          scale_y_continuous(expand = c(0, 0))+
          scale_fill_manual(values = coll)
          
          if(ggv == 200 || ggv > 200) p1 <- p1 + facet_grid(Num~., labeller = labeller(Num = facetnames))
          if(ggv < 200) p1 <- p1 + facet_grid(Num~., labeller = plotlabeller)
          
          p1 <- p1 + labs(x = NULL, y = NULL)+
          theme(legend.position = "none", panel.grid = element_blank(), panel.background = element_blank(), 
                axis.ticks = element_blank(), axis.text = element_blank(), axis.line = element_blank(), 
                axis.title = element_blank(), strip.text = element_text(size = flabsize, colour = flabcol), 
                strip.background = element_rect(colour = flabbackcol, fill = flabbackcol), 
                plot.margin = grid::unit(c(0.1, 0, -0.3, 0), "lines"),
                panel.margin=grid::unit(panelspacer,"lines"))
        
        #add pop divider lines only if 2 pops or more
        if(div)
        {
          if(nrow(markerpos) > 2) p1 <- p1+geom_vline(xintercept = markerpos$divxpos[-c(1,length(markerpos$divxpos))],colour=divcol,linetype=divtype, size=divthick)
        }
        
        #remove strip panels on right
        if(!flab) p1 <- p1+theme(strip.text=element_blank())
        
        #create bottom plot with labels
        p2 <- ggplot2::ggplot()+
          geom_blank(data = labelpos, aes(x = labxpos, y = labypos))+
          geom_text(data = labelpos, aes(x = labxpos, y = labypos), label = labelpos$label, angle = labangle, hjust = labjust, size = labsize, colour = labcol)+
          geom_line(data = markerpos, aes(x = markerxpos, y = markerypos), colour = linecol, size = linethick, linetype = linetype)+
          geom_point(data = markerpos, aes(x = markerxpos, y = markerypos), size = pointsize, colour = pointcol, shape = pointtype, fill = pointbgcol)+
          scale_x_continuous(expand = c(0, 0))+
          scale_y_continuous(limits=c(0,1))+
          labs(x = NULL, y = NULL)+
          #facet_grid(temp~., labeller = plotlabeller)+
          theme(legend.position = "none", panel.grid = element_blank(), 
                panel.background = element_rect(fill="white"), plot.background=element_rect(fill="white"),
                axis.ticks = element_blank(), axis.text = element_blank(), 
                axis.title = element_blank(), strip.text = element_text(size = flabsize, colour = flabcol), 
                strip.background = element_rect(colour = flabbackcol, fill = flabbackcol),
                axis.line = element_blank(), panel.border=element_blank(),
                plot.margin = grid::unit(c(labspacer,0,0,0), "lines"), panel.margin=grid::unit(0,"lines"))
        
        #gtable conversion
        gp1 <- ggplot_gtable(ggplot_build(p1))
        #turn off clipping for panel
        gp1$layout$clip[gp1$layout$name == "panel"] <- "off"
        
        #gtable conversion
        gp2 <- ggplot_gtable(ggplot_build(p2))
        #turn off clipping for panel
        gp2$layout$clip[gp2$layout$name == "panel"] <- "off"
        #change width of bottom plot to that of top plot
        gp2 <- gtable::gtable_add_cols(gp2, gp1$widths[5])
        
        #calculate size of panels
        height2 <- height1 - labpanelheight1
        #if (imgtype == "pdf") height2 <- height1-(pophelper:::unitConverter(value=labpanelheight,fromunit="cm",tounit="in",res=res))
        
        if (imgtype == "png") png(paste0(fname, ".png"), height = height1, width = width1, res = dpi, units = units1, type = "cairo")
        if (imgtype == "jpeg") jpeg(paste0(fname, ".jpg"), height = height1, width = width1, res = dpi, units = units1, quality = 100)
        if (imgtype == "pdf") pdf(paste0(fname, ".pdf"), height = height1, width = width1)
        
        gridExtra::grid.arrange(gp1, gp2, heights = grid::unit(c(height2,labpanelheight1),units1))
        dev.off()
        
        if (imgtype == "png") cat(paste0(fname, ".png exported.\n"))
        if (imgtype == "jpeg") cat(paste0(fname, ".jpg exported.\n"))
        if (imgtype == "pdf") cat(paste0(fname, ".pdf exported.\n"))
      }
    }
  }
  
  #sj joined plots
  if (imgoutput == "join")
  {
    #checks
    if (flen < 2) stop("plotRuns: Joined plot not processed. Number of selected files less than 2.")
    
    chk <- "UNIDENTIFIED"
    chk <- unique(pophelper:::checkRuns(files)$type)
    if (length(chk) > 1) stop("plotRuns: Input contains mixed formats.") 
    if (chk == "UNIDENTIFIED") stop("plotRuns: Unidentified input format.")
    if (chk == "TAB") stop("plotRuns: If using table files (combined/aligned/merged), set argument imgoutput = 'tab'.")
    if (chk == "STRUCTURE") tempdf <- tabulateRunsStructure(files = files)
    if (chk == "TESS") tempdf <- tabulateRunsTess(files = files)
    #if (chk == "ADMIXTURE") tempdf <- tabulateRunsAdmixture(files = files)
    if (chk == "MATRIX") tempdf <- tabulateRunsMatrix(files = files)
    
    #checks if num of individuals differ between runs
    if (all(tempdf$ind[1] != tempdf$ind)) stop("plotRuns: Joined plot not processed. Number of individuals differ between selected runs.")
    Ind <- tempdf$ind[1]
    rm(tempdf)
    
    #loop to process selected files
    plist <- vector("list",length=flen)
    if(ggv == 200 || ggv > 200) facetnames <- vector(length=flen)
    if(ggv < 200) facetnames <- vector("list",length=flen)
    kvec <- vector(length=flen)
    for (i in 1:flen)
    {
      fname <- base::gsub(".txt", "", basename(files[i]))
      fname <- base::gsub(".csv", "", fname)
      
      #check files
      chk <- pophelper:::checkRuns(files[i])$type
      if (chk == "STRUCTURE") df1 <- runsToDfStructure(files = files[i])
      if (chk == "TESS") df1 <- runsToDfTess(files = files[i])
      #if (chk == "ADMIXTURE") df1 <- runsToDfAdmixture(files = files[i])
      if (chk == "MATRIX") df1 <- runsToDfMatrix(files = files[i])
      if (chk == "TAB") stop("plotRuns: If using table files (combined/aligned/merged), set argument imgoutput = 'tab'.")
      if (chk == "UNIDENTIFIED") stop("plotRuns: Unidentified input format.")
      
      
      #ordering pops
      if(all(!is.na(poplab)))
      {
        #ordering pops
        lablist <- pophelper:::popLabels(df = df1, poplab = poplab, subsetpops = subsetpops,
                                          popmean=popmean,labpos = labpos, linepos = linepos)
        df1 <- lablist$df
        poplab <- lablist$poplab
        markerpos <- lablist$markerpos
        labelpos <- lablist$labelpos
        poplabnames <- rle(poplab)$values
        rm(lablist)
      }
      
      #sorting individuals
      if(all(!is.na(sortind)))
      {
        if(sortind == "all")
        {
          maxval <- apply(df1,1,max)
          matchval <- vector(length=nrow(df1))
          for(j in 1:nrow(df1)) matchval[j] <- match(maxval[j],df1[j,])
          dftemp <- df1
          dftemp$maxval <- maxval
          dftemp$matchval <- matchval
          
          if(any(is.na(poplab))) df1 <- df1[with(dftemp, order(matchval,-maxval)), ,drop=FALSE]
          if(all(!is.na(poplab)))
          {
            dftemp$pop <- poplab
            
            #sort within population
            tovec <- cumsum(rle(dftemp$pop)$lengths)
            fromvec <- (tovec - rle(dftemp$pop)$lengths)+1
            dftemplist <- vector("list",length=length(poplabnames))
            for(k in 1:length(tovec))
            {
              dftemp1 <- dftemp[fromvec[k]:tovec[k],]
              dftemp1$pop <- NULL
              dftemplist[[k]] <- dftemp1[with(dftemp1, order(matchval,-maxval)), ,drop=FALSE]
            }
            
            df1 <- do.call("rbind",dftemplist)
            df1$maxval <- NULL
            df1$matchval <- NULL
          }
          
        }else{
          if(!(sortind %in% colnames(df1))) stop("plotRuns: Ordering cluster not found in file header.")
          if(any(is.na(poplab))) df1 <- df1[order(df1[[sortind]]), ,drop=FALSE]
          if(all(!is.na(poplab)))
          {
            clnum <- which(sortind == colnames(df1))
            dftemp <- df1
            dftemp$pop <- poplab
            
            #sort within population
            tovec <- cumsum(rle(dftemp$pop)$lengths)
            fromvec <- (tovec - rle(dftemp$pop)$lengths)+1
            dftemplist <- vector("list",length=length(poplabnames))
            for(k in 1:length(tovec))
            {
              dftemp1 <- dftemp[fromvec[k]:tovec[k],]
              dftemp1$pop <- NULL
              dftemplist[[k]] <- dftemp1[order(dftemp1[[sortind]]), ,drop=FALSE]
            }
            
            df1 <- do.call("rbind",dftemplist)
          }
        }
      }
      
      k <- ncol(df1)
      df1$Ind <- factor(1:nrow(df1))
      df1$Num <- factor(rep(i, nrow(df1)))
      if(ggv < 200) facetnames[[i]] <- paste0(fname, "\n", "K=", k, sep="")
      if(ggv == 200 || ggv > 200) facetnames[[i]] <- paste0(fname, "\n", "K=", k)
      kvec[[i]] <- k
      #df2 <- reshape::melt(df1, id.var = c("Ind", "Num"))
      df2 <- tidyr::gather(df1,"variable","value",-c(Ind,Num))
      plist[[i]] <- df2[rev(1:nrow(df2)),]
      rm(df2)
    }

    Ind <- nrow(df1)
    #combine list to one dataframe 
    df3 <- do.call("rbind",plist)
    if(ggv == 200 || ggv > 200) names(facetnames) <- levels(factor(as.character(df3$Num)))
    
    #get Dim
    dimtemp <- pophelper:::getDim(ind = Ind, height = height, width = width, res = dpi, units = units, imgtype=imgtype, labpanelheight=labpanelheight, plotnum = flen)
    height1 <- as.numeric(dimtemp[1])
    width1 <- as.numeric(dimtemp[2])
    labpanelheight1 <- as.numeric(dimtemp[3])
    units1 <- as.character(dimtemp[4])
    
    #Get Col
    coll <- popcol
    if (any(is.na(popcol))) coll <- getColours(max(kvec))
    if (length(coll) < max(kvec)) stop(paste0("plotRuns: Number of colours (",length(coll),") is less than the number of clusters (",max(kvec),")."))
    
    #save plot
    dt <- as.character(format(Sys.time(), "%Y%m%d%H%M%S"))
    
    if (any(is.na(poplab)))
    {
      
      #ggplot
      p <- ggplot2::ggplot(data = df3, aes(x = Ind, y = value, fill = variable))+
        geom_bar(width = 1, stat = "identity", position = "fill", na.rm = na.rm)+
        scale_x_discrete(expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0))+
        scale_fill_manual(values = coll)
        
        if(ggv == 200 || ggv > 200) p <- p + facet_grid(Num~., labeller = labeller(Num = facetnames))
        if(ggv < 200) p <- p + facet_grid(Num~., labeller = plotlabeller)
        
        p <- p + labs(x = NULL, y = NULL)+
        theme(legend.position = "none", panel.grid = element_blank(), panel.background = element_blank(), 
              axis.ticks = element_blank(), axis.text = element_blank(), axis.line = element_blank(), 
              axis.title = element_blank(), strip.text = element_text(size = flabsize, colour = flabcol), 
              strip.background = element_rect(colour = flabbackcol, fill = flabbackcol), 
              plot.margin = grid::unit(c(0.1, 0, 0, 0), "lines"),
              panel.margin=grid::unit(panelspacer,"lines"))
      
      #remove strip panels on right
      if(!flab) p <- p + theme(strip.text=element_blank())
      
      if (imgtype == "png") png(paste0("Joined", flen, "Files-", dt, ".png"), height = height1, width = width1, res = dpi, units = units1, type = "cairo")
      if (imgtype == "jpeg") jpeg(paste0("Joined", flen, "Files-", dt, ".jpg"), height = height1, width = width1, res = dpi, units = units1, quality = 100)
      if (imgtype == "pdf") pdf(paste0("Joined", flen, "Files-", dt, ".pdf"), height = height1, width = width1)
      print(p)
      dev.off()
      if (imgtype == "png") cat(paste0("Joined", flen, "Files-", dt, ".png exported.\n"))
      if (imgtype == "jpeg") cat(paste0("Joined", flen, "Files-", dt, ".jpg exported.\n"))
      if (imgtype == "pdf") cat(paste0("Joined", flen, "Files-", dt, ".pdf exported.\n"))
    }
    
    if (all(!is.na(poplab)))
    {
      #plot with labels
      if (Ind!=length(as.character(poplab))) stop(paste0("Length of labels (", length(as.character(poplab)),") do not match number of individuals in input file (",Ind,")."))
      
      
      ppar <- pophelper:::getPlotParams(poplab = poplab, plotnum = flen, labsize = labsize, labangle = labangle,labjust = labjust,
                            pointsize = pointsize, linethick = linethick)
      
      labangle <- ppar$labangle
      labjust <- ppar$labjust
      labsize <- ppar$labsize
      labjust <- ppar$labjust
      pointsize <- ppar$pointsize
      linethick <- ppar$linethick
      
      #add labpos to lframe df
      #lframe$labpos <- as.numeric(rep(labpos,nrow(lframe)))
      #pos1$linepos <- as.numeric(rep(linepos,nrow(pos1)))
      
      #create top plot with multiple barplots
      p1 <- ggplot2::ggplot(data = df3, aes(x = Ind, y = value, fill = variable))+
        geom_bar(width = 1, stat = "identity", position = "fill", na.rm = na.rm)+
        scale_x_discrete(expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0))+
        scale_fill_manual(values = coll)
      
      if(ggv == 200 || ggv > 200) p1 <- p1 + facet_grid(Num~., labeller = labeller(Num = facetnames))
      if(ggv < 200) p1 <- p1 + facet_grid(Num~., labeller = plotlabeller)
      
        p1 <- p1 + labs(x = NULL, y = NULL)+
        theme(legend.position = "none", panel.grid = element_blank(), 
              panel.background = element_rect(fill="white"), plot.background=element_rect(fill="white"),
              axis.ticks = element_blank(), axis.text = element_blank(),
              axis.line = element_blank(), panel.border=element_blank(),
              axis.title = element_blank(), strip.text = element_text(size = flabsize, colour = flabcol), 
              strip.background = element_rect(colour = flabbackcol, fill = flabbackcol),
              plot.margin = grid::unit(c(0.1, 0, -0.3, 0), "lines"),
              panel.margin=grid::unit(panelspacer,"lines"))
      
      #add pop divider lines only if 2 pops or more
      if(div)
      {
        if(nrow(markerpos) > 2) p1 <- p1+geom_vline(xintercept = markerpos$divxpos[-c(1,length(markerpos$divxpos))],colour=divcol,linetype=divtype, size=divthick)
      }
      
      #remove strip panels on right
      if(!flab) p1 <- p1+theme(strip.text=element_blank())
      
      #create bottom plot with labels
      p2 <- ggplot2::ggplot()+
        geom_blank(data = labelpos, aes(x = labxpos, y = labypos))+
        geom_text(data = labelpos, aes(x = labxpos, y = labypos), label = labelpos$label, angle = labangle, hjust = labjust, size = labsize, colour = labcol)+
        geom_line(data = markerpos, aes(x = markerxpos, y = markerypos), colour = linecol, size = linethick, linetype = linetype)+
        geom_point(data = markerpos, aes(x = markerxpos, y = markerypos), size = pointsize, colour = pointcol, shape = pointtype, fill = pointbgcol)+
        scale_x_continuous(expand = c(0,0))+
        scale_y_continuous(limits = c(0,1))+
        labs(x = NULL, y = NULL)+
        #facet_grid(temp~., labeller = plotlabeller)+
        theme(legend.position = "none", panel.grid = element_blank(), 
              panel.background = element_rect(fill="white"), plot.background=element_rect(fill="white"),
              axis.ticks = element_blank(), axis.text = element_blank(), 
              axis.title = element_blank(), strip.text = element_text(size = flabsize, colour = flabcol), 
              strip.background = element_rect(colour = flabbackcol, fill = flabbackcol),
              axis.line = element_blank(), panel.border=element_blank(),
              plot.margin = grid::unit(c(labspacer,0,0,0), "lines"), panel.margin=grid::unit(0,"lines"))
      
      #gtable conversion
      gp1 <- ggplot_gtable(ggplot_build(p1))
      #turn off clipping for panel
      gp1$layout$clip[gp1$layout$name == "panel"] <- "off"
      
      #gtable conversion
      gp2 <- ggplot_gtable(ggplot_build(p2))
      #turn off clipping for panel
      gp2$layout$clip[gp2$layout$name == "panel"] <- "off"
      #change width of bottom plot to that of top plot
      gp2 <- gtable::gtable_add_cols(gp2, gp1$widths[5]) 
      
      #calculate size of panels
      height2 <- height1-labpanelheight1
      height3 <- height2/flen
      if (imgtype == "png") png(paste0("Joined", flen, "Files-", dt, ".png"), height = height1, width = width1, res = dpi, units = units1, type = "cairo")
      if (imgtype == "jpeg") jpeg(paste0("Joined", flen, "Files-", dt, ".jpg"), height = height1, width = width1, res = dpi, units = units1, quality = 100)
      if (imgtype == "pdf") pdf(paste0("Joined", flen, "Files-", dt, ".pdf"), height = height1, width = width1)
      
      gridExtra::grid.arrange(gp1, gp2, heights = grid::unit(c(height2,labpanelheight1),units1))
      dev.off()
      
      if (imgtype == "png") cat(paste0("Joined", flen, "Files-", dt, ".png exported.\n"))
      if (imgtype == "jpeg") cat(paste0("Joined", flen, "Files-", dt, ".jpg exported.\n"))
      if (imgtype == "pdf") cat(paste0("Joined", flen, "Files-", dt, ".pdf exported.\n"))
    }
  }
  
  #ta input tables
  if (imgoutput == "tab")
  {
    i=1
    for (i in 1:flen)
    {
      chk <- pophelper:::checkRuns(files[i])$type
      if (chk == "STRUCTURE") stop("plotRuns: Incorrect input format. For STRUCTURE files, use imgoutput='sep' or imgoutput='join'.")
      if (chk == "TESS") stop("plotRuns: Incorrect input format. For TESS files, use imgoutput='sep' or imgoutput='join'.")
      #if (chk == "ADMIXTURE") stop("plotRuns: Incorrect input format. For ADMIXTURE files, use imgoutput='sep' or imgoutput='join'.")
      if (chk == "MATRIX") stop("plotRuns: Incorrect input format. For MATRIX files, use imgoutput='sep' or imgoutput='join'.")
      if (chk != "TAB") stop("plotRuns: Incorrect input format. Not a TABLE input file.")
      if (chk == "UNIDENTIFIED") stop("plotRuns: Unidentified input format.")
      
      fname <- base::gsub(".txt", "", basename(files[i]))
      fname <- base::gsub(".csv", "", fname)
      df1 <- read.table(files[i],header = F, sep = "", dec = ".", quote = "",stringsAsFactors=FALSE)
      if (class(df1)!="data.frame") stop("plotRuns: Input is not a dataframe. Incorrect input file type.")
      
      df1[,1] <- factor(df1[ ,1])
      indlev <- levels(df1[,1])
      Ind <- as.numeric(as.character(length(indlev)))
      tempb <- as.numeric(nrow(df1))
      numruns <- as.numeric(tempb/Ind)
      numk <- ncol(df1) - 2
      
      #if poplab
      if(all(!is.na(poplab)))
      {
        df1.1 <- df1
        df1.1$run <- rep(1:numruns, each=Ind)
        #ordering pops
        runlist <- vector("list",length=numruns)
        for(r in 1:numruns)
        {
          s1 <- subset(df1.1, df1.1$run==r)
          s1$run <- NULL
          lablist <- pophelper:::popLabels(df = s1, poplab = poplab, subsetpops = subsetpops,
                                           popmean=popmean, labpos = labpos, linepos = linepos)
          runlist[[r]] <- lablist$df
        }
        
        df1 <- do.call("rbind",runlist)
        Ind <- nrow(lablist$df)
        poplab <- lablist$poplab
        markerpos <- lablist$markerpos
        labelpos <- lablist$labelpos
        poplabnames <- rle(poplab)$values
        rm(lablist,runlist,s1,df1.1,r)
      }
      
      df1 <- data.frame(Num = factor(rep(1:numruns, 1, each = Ind)), 
                        Ind = factor(rep(1:Ind, numruns)), 
                        df1[, 2:(numk+1)],stringsAsFactors=FALSE)
      colnames(df1)[3:ncol(df1)] <- paste0("Cluster",1:(ncol(df1)-2))
      
      #ordering individuals
      if(all(!is.na(sortind)))
      {
        runlist <- vector("list",length=numruns)
        for(r in 1:numruns)
        {
          s1 <- subset(df1, df1$Num==r)
          s1$Num <- NULL
          s1$Ind <- NULL
          if(sortind == "all")
          {
            maxval <- apply(s1,1,max)
            matchval <- vector(length=nrow(s1))
            for(j in 1:nrow(s1)) matchval[j] <- match(maxval[j],s1[j,])
            dftemp <- s1
            dftemp$maxval <- maxval
            dftemp$matchval <- matchval
            
            if(any(is.na(poplab))) s1 <- s1[with(dftemp, order(matchval,-maxval)), ,drop=FALSE]
            #sort within population
            if(all(!is.na(poplab)))
            {
              dftemp$pop <- poplab
              
              #sort within pop
              tovec <- cumsum(rle(dftemp$pop)$lengths)
              fromvec <- (tovec - rle(dftemp$pop)$lengths)+1
              dftemplist <- vector("list",length=length(poplabnames))
              for(k in 1:length(tovec))
              {
                dftemp1 <- dftemp[fromvec[k]:tovec[k],]
                dftemp1$pop <- NULL
                dftemplist[[k]] <- dftemp1[with(dftemp1, order(matchval,-maxval)), ,drop=FALSE]
              }
              
              s1 <- do.call("rbind",dftemplist)
              s1$maxval <- NULL
              s1$matchval <- NULL
            }

          }else{
            if(!(sortind %in% colnames(s1))) stop("plotRuns: Ordering cluster not found in file header.")
            if(any(is.na(poplab))) s1 <- df1[order(s1[[sortind]]), ,drop=FALSE]
            if(all(!is.na(poplab)))
            {
              clnum <- which(sortind == colnames(s1))
              dftemp <- s1
              dftemp$pop <- poplab
              
              #sort within population
              tovec <- cumsum(rle(dftemp$pop)$lengths)
              fromvec <- (tovec - rle(dftemp$pop)$lengths)+1
              dftemplist <- vector("list",length=length(poplabnames))
              for(k in 1:length(tovec))
              {
                dftemp1 <- dftemp[fromvec[k]:tovec[k],]
                dftemp1$pop <- NULL
                dftemplist[[k]] <- dftemp1[order(dftemp1[[sortind]]), ,drop=FALSE]
              }
              
              s1 <- do.call("rbind",dftemplist)
            }
          }
          runlist[[r]] <- s1
        }
        df2 <- do.call("rbind",runlist)
        df2$Num <- df1$Num
        df2$Ind <- df1$Ind
      }else{
        df2 <- df1
      }
      
      df3 <- tidyr::gather(df2,"variable","value",-c(Ind,Num))
      
      #labeller fn for facets
      if(ggv < 200)
      {
        plotlabeller <- function(variable, value){return(facetnames[value])}
        facetnames <- as.list(rep(paste0("K=", numk, sep = ""), numruns))
      }
      if(ggv == 200 || ggv > 200)
      {
        facetnames <- rep(paste0("K=", numk, sep = ""), numruns)
        names(facetnames) <- levels(factor(as.character(df2$Num)))
      }
      
      #get Dim
      dimtemp <- pophelper:::getDim(ind = Ind, height = height, width = width, res = dpi, units = units, imgtype=imgtype, labpanelheight=labpanelheight, plotnum = numruns)
      height1 <- as.numeric(dimtemp[1])
      width1 <- as.numeric(dimtemp[2])
      labpanelheight1 <- as.numeric(dimtemp[3])
      units1 <- as.character(dimtemp[4])
      
      #Get col
      coll <- popcol
      if (any(is.na(popcol))) coll <- pophelper:::getColours(numk)
      if (length(coll) < max(numk)) stop(paste0("Number of colours (",length(coll),") is less than the number of clusters (",max(kvec),")."))
      
      #save plot
      dt <- base::gsub(":", "", as.character(format(Sys.time(), "%H:%M:%S")))
      
      if (any(is.na(poplab)))
      {
        #ggplot
        p <- ggplot2::ggplot(data = df3, aes(x = Ind, y = value, fill = variable))+
          geom_bar(width = 1, stat = "identity", position = "fill", na.rm = na.rm)+
          scale_x_discrete(expand = c(0, 0))+
          scale_y_continuous(expand = c(0, 0))+
          scale_fill_manual(values = coll)
          
          if(ggv == 200 || ggv > 200) p <- p + facet_grid(Num~., labeller = labeller(Num = facetnames))
          if(ggv < 200) p <- p + facet_grid(Num~., labeller = plotlabeller)

          p <- p + labs(x = NULL, y = NULL)+
          theme(legend.position = "none", panel.grid = element_blank(), panel.background = element_blank(), 
                axis.ticks = element_blank(), axis.text = element_blank(), axis.line = element_blank(), 
                axis.title = element_blank(), strip.text = element_text(size = flabsize, colour = flabcol), 
                strip.background = element_rect(colour = flabbackcol, fill = flabbackcol), 
                plot.margin = grid::unit(c(0.1, 0, 0, 0), "lines"),
                panel.margin=grid::unit(panelspacer,"lines"))
        
        #remove strip panels on right
        if(!flab) p <- p + theme(strip.text=element_blank())
        
        if (imgtype == "png") png(paste0(fname, ".png"), height = height1, width = width1, res = dpi, units = units1, type = "cairo")
        if (imgtype == "jpeg") jpeg(paste0(fname, ".jpg"), height = height1, width = width1, res = dpi, units = units1, quality = 100)
        if (imgtype == "pdf") pdf(paste0(fname, ".pdf"), height = height1, width = width1)
        print(p)
        dev.off()
        if (imgtype == "png") cat(paste0(fname, ".png exported.\n"))
        if (imgtype == "jpeg") cat(paste0(fname, ".jpg exported.\n"))
        if (imgtype == "pdf") cat(paste0(fname, ".pdf exported.\n"))
      }
      
      if (!all(is.na(poplab)))
      {
        #plot with labels
        if (Ind!=length(as.character(poplab))) stop(paste0("Length of labels (", length(as.character(poplab)),") do not match number of individuals in input file (",Ind,")."))
        
        ppar <- pophelper:::getPlotParams(poplab = poplab, plotnum = numruns, labsize = labsize, labangle = labangle, labjust = labjust, pointsize = pointsize, linethick = linethick)
        
        labangle <- ppar$labangle
        labjust <- ppar$labjust
        labsize <- ppar$labsize
        labjust <- ppar$labjust
        pointsize <- ppar$pointsize
        linethick <- ppar$linethick
        
        #add labpos to lframe df
        #lframe$labpos <- as.numeric(rep(labpos,nrow(lframe)))
        #pos1$linepos <- as.numeric(rep(linepos,nrow(pos1)))
        
        #create top plot with multiple barplots
        p1 <- ggplot2::ggplot(data = df3, aes(x = Ind, y = value, fill = variable))+
          geom_bar(width = 1, stat = "identity", position = "fill", na.rm = na.rm)+
          scale_x_discrete(expand = c(0, 0))+
          scale_y_continuous(expand = c(0, 0))+
          scale_fill_manual(values = coll)
          
          if(ggv == 200 || ggv > 200) p1 <- p1 + facet_grid(Num~., labeller = labeller(Num = facetnames))
          if(ggv < 200) p1 <- p1 + facet_grid(Num~., labeller = plotlabeller)
        
          p1 <- p1 + labs(x = NULL, y = NULL)+
          theme(legend.position = "none", panel.grid = element_blank(), 
                panel.background = element_rect(fill="white"), plot.background=element_rect(fill="white"),
                axis.ticks = element_blank(), axis.text = element_blank(),
                axis.line = element_blank(), panel.border=element_blank(),
                axis.title = element_blank(), strip.text = element_text(size = flabsize, colour = flabcol), 
                strip.background = element_rect(colour = flabbackcol, fill = flabbackcol),
                plot.margin = grid::unit(c(0.1, 0, -0.3, 0), "lines"),
                panel.margin=grid::unit(panelspacer,"lines"))
        
        
        #add pop divider lines only if 2 pops or more
        if(div)
        {
          if(nrow(markerpos) > 2) p1 <- p1 + geom_vline(xintercept = markerpos$divxpos[-c(1,length(markerpos$divxpos))],colour=divcol,linetype=divtype, size=divthick)
        }
        
        #remove strip panels on right
        if(!flab) p1 <- p1 + theme(strip.text=element_blank())
        
        #create bottom plot with labels
        p2 <- ggplot2::ggplot()+
          geom_blank(data = labelpos, aes(x = labxpos, y = labypos))+
          geom_text(data = labelpos, aes(x = labxpos, y = labypos), label = labelpos$label, angle = labangle, hjust = labjust, size = labsize, colour = labcol)+
          geom_line(data = markerpos, aes(x = markerxpos, y = markerypos), colour = linecol, size = linethick, linetype = linetype)+
          geom_point(data = markerpos, aes(x = markerxpos, y = markerypos), size = pointsize, colour = pointcol, shape = pointtype, fill = pointbgcol)+
          scale_x_continuous(expand = c(0, 0))+
          scale_y_continuous(limits=c(0,1))+
          labs(x = NULL, y = NULL)+
          #facet_grid(temp~., labeller = plotlabeller)+
          theme(legend.position = "none", panel.grid = element_blank(), 
                panel.background = element_rect(fill="white"), plot.background=element_rect(fill="white"),
                axis.ticks = element_blank(), axis.text = element_blank(), 
                axis.title = element_blank(), strip.text = element_text(size = flabsize, colour = flabcol), 
                strip.background = element_rect(colour = flabbackcol, fill = flabbackcol),
                axis.line = element_blank(), panel.border=element_blank(),
                plot.margin = grid::unit(c(labspacer,0,0,0), "lines"), panel.margin=grid::unit(0,"lines"))
        
        #gtable conversion
        gp1 <- ggplot_gtable(ggplot_build(p1))
        #turn off clipping for panel
        gp1$layout$clip[gp1$layout$name == "panel"] <- "off"
        
        #gtable conversion
        gp2 <- ggplot_gtable(ggplot_build(p2))
        #turn off clipping for panel
        gp2$layout$clip[gp2$layout$name == "panel"] <- "off"
        #change width of bottom plot to that of top plot
        gp2 <- gtable::gtable_add_cols(gp2, gp1$widths[5]) 
        
        #calculate size of panels
        height2 <- height1-labpanelheight1
        height3 <- height2/flen
        
        if (imgtype == "png") png(paste0(fname, ".png"), height = height1, width = width1, res = dpi, units = units1, type = "cairo")
        if (imgtype == "jpeg") jpeg(paste0(fname, ".jpg"), height = height1, width = width1, res = dpi, units = units1, quality = 100)
        if (imgtype == "pdf") pdf(paste0(fname, ".pdf"), height = height1, width = width1)
        
        #if (numruns == 1) grid::grid.draw(gp1)
        #if (numruns > 1) gridExtra::grid.arrange(gp2, gp1, heights = c(((numruns-1)/numruns)-0.08, (1/numruns)+0.08))
        gridExtra::grid.arrange(gp1, gp2, heights = grid::unit(c(height2,labpanelheight1),units1))
        dev.off()
        
        if (imgtype == "png") cat(paste0(fname, ".png exported.\n"))
        if (imgtype == "jpeg") cat(paste0(fname, ".jpg exported.\n"))
        if (imgtype == "pdf") cat(paste0(fname, ".pdf exported.\n"))
      }
      
    } 
  }
}

#-------------------------------------------------------------------------------

# FUNCTION plotMultiline
#' Plot STRUCTURE, TESS, MATRIX run files and TAB files in multiline
#' @description Plot STRUCTURE, TESS, MATRIX run files and TAB files as barplots with multiple lines
#' @param files A character vector of filenames or paths. One or more STRUCTURE, TESS, MATRIX, combined, aligned or merged files. Use \code{choose.files(multi = TRUE)} for interactive selection.
#' @param spl An integer indicating samples per line. Defaults to 60.
#' @param lpp An integer indicating lines per page. Defaults to 11.
#' @param popcol A character vector of colours for clusters.
#' @param indlab A character vector of individual labels equal to length of individuals.
#' @param indlabfromfile A logical indicating if individual labels must be read from input file and used as labels.
#' @param sortind A character indicating how individuals are sorted. Default is NA. Other options are 'all' or any one cluster (eg. 'Cluster1').
#' @param sortlabels Deprecated. Not is use.
#' @param na.rm Default set to FALSE. NAs are not removed from data, therefore \code{ggplot} prints warning messages for NAs. If set to TRUE, NAs are removed before plotting and \code{ggplot} NA warning is suppressed.
#' @param barwidth A numeric indicating the width of the bars.
#' @param barspace This argument is deprecated. A numeric indicating the space between the bars.
#' @param ticks A logical indicating if ticks on axis should be displayed or not.
#' @param yaxislabs A logical indicating if y-axis labels should be displayed or not.
#' @param indlabs A logical indicating if individual labels 1, 2, 3.. are displayed below bars. To hide labels, set \code{indlabs = FALSE}.
#' @param labsize A numeric denoting size of the labels.
#' @param labangle A numeric denoting the angle of the labels.
#' @param labvjust A numeric denoting vertical justification of the labels.
#' @param labhjust A numeric denoting the horizontal justification of the labels.
#' @param imgtype A character denoting figure output format. Options are 'png', 'jpeg' or 'pdf'. If pdf, height and width must be in inches and res argument is ignored.
#' @param height A numeric denoting height of the full figure. If NA, height is set to 29.7cm (A4 height).
#' @param width A numeric denoting width of the full figure. If NA, width is set to 21cm (A4 width).
#' @param res A numeric denoting resolution of the figure. Default is 300.
#' @param units A character denoting the units of dimension of the figure. Default is "cm". Other options can be "in", "mm" or "px".
#' @details Figures are always created to A4 size. Any plotted row will span the width of the figure. Note that this function is slow and may take several minutes when plotting mutiple tables.
#' @examples 
#' \dontrun{
#' slist <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' 
#' #basic
#' plotMultiline(sfiles[1])
#' 
#' #adjust spl and lpp
#' plotMultiline(slist[1],spl=75,lpp=10)
#' 
#' #sort individuals
#' plotMultiline(slist[1],sortind="all")
#' plotMultiline(slist[1],sortind="Cluster1")
#' 
#' #include labels from file
#' plotMultiline(slist[1],indlabfromfile=T)
#' 
#' #external ind labels
#' inds <- read.delim(system.file("files/structureindlabels.txt",package="pophelper"),header=FALSE)
#' plotMultiline(slist[1],indlab=inds$V1)
#' }
#' @import tidyr
#' @import gridExtra
#' @export
#'
plotMultiline <- function(files = NULL, spl = NA, lpp = NA, popcol = NA, indlab=NA, indlabfromfile=FALSE, sortind=NA, sortlabels=FALSE, na.rm = FALSE, 
                          barwidth = 0.9, barspace = NA, ticks = FALSE, yaxislabs = FALSE, indlabs = TRUE, 
                          labsize = 5, labangle = 90, labvjust = 0.5,labhjust = 1, imgtype = "png", height = NA, 
                          width = NA, res = NA, units = NA)
{
  if(is.null(files) | (length(files) == 0)) stop("plotMultiline: No input files.")
  #check
  indlab <- as.character(indlab)
  if(length(indlab) > 1 && any(is.na(indlab))) stop("plotRunsMultiline: Missing values (NA) in argument 'indlab'.")
  if(!is.logical(na.rm)) stop("plotMultiline: Argument 'na.rm' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(ticks)) stop("plotMultiline: Argument 'ticks' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(yaxislabs)) stop("plotMultiline: Argument 'yaxislabs' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(indlabs)) stop("plotMultiline: Argument 'indlabs' set incorrectly. Set as TRUE or FALSE.")
  if(!is.na(barspace)) warning("plotMultiline: Argument 'barspace' is deprecated.")
  if (all(!is.na(sortind)))
  {
    if(length(sortind) > 1) stop("plotMultiline: Argument 'sortind' must be of length 1. Use 'all' or a cluster like 'Cluster1'.")
    if(sortind != "all" && !grepl("Cluster[0-9+]",sortind)) stop("plotMultiline: Argument 'sortind' must be set to 'all' or a cluster like 'Cluster1'.")
  }
  if(sortlabels) warning("plotMultiline: Argument 'sortlabels' is deprecated. Labels are always sorted to reflect original order.")
  if (imgtype != "png" && imgtype != "pdf" && imgtype != "jpeg" ) stop("plotMultiline: Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg' or 'pdf'.")
  
  #set NA values
  if (is.na (height)) 
  {
    height <- 29.7 
    if (imgtype == "pdf") height <- round(height*0.3937,2)
  }
  if (is.na (width)) 
  {
    width <- 21 
    if (imgtype == "pdf") width <- round(width*0.3937,2)
  }
  
  if (is.na (res)) res <- 300
  if (is.na (units)) units <- "cm"
  
  len1 <- length(files)
  for (i in 1:len1)
  {
    fname <- base::gsub(".txt", "", basename(files[i]))
    fname <- base::gsub(".csv", "", fname)
   
    chk <- pophelper:::checkRuns(files[i], warn=FALSE)$type
    if (chk == "STRUCTURE") df1 <- runsToDfStructure(files = files[i], indlabfromfile=indlabfromfile)
    if (chk == "TESS") df1 <- runsToDfTess(files = files[i])
    #if (chk == "ADMIXTURE") df1 <- runsToDfAdmixture(files = files[i])
    if (chk == "MATRIX") df1 <- runsToDfMatrix(files = files[i])
    
    #read TAB files
    if (chk == "TAB") 
    {
      df1 <- read.table(files[i],header = F, sep = "", dec = ".", quote = "",stringsAsFactors=F)
      if (class(df1) != "data.frame") stop("plotMultiline: Input is not a dataframe. Incorrect input file type.")
      df1$V1 <- factor(df1$V1)
      Ind <- as.numeric(as.character(length(levels(df1[, 1]))))
      numruns <- as.numeric(as.numeric(nrow(df1))/Ind)
      df1 <- df1[, -c(1, ncol(df1))]
      colnames(df1)[1:length(df1)] <- paste0("Cluster", 1:(length(df1)))
      numk <- ncol(df1)
      df1$run <- rep(1:numruns,each=Ind)
      df1 <- split(df1[, -length(df1)], df1$run)
      
    }
    
    if (chk == "UNIDENTIFIED") stop("plotMultiline: Input file type is unidentified.")

    #determine if df1 is list or dataframe
    if (as.character(class(df1)) == "data.frame") flen <- 1
    if (as.character(class(df1)) == "list") flen <- length(df1)
    
    for (j in 1:flen)
    {
      #move to dff
      if (as.character(class(df1)) == "data.frame") dff <- df1
      if (as.character(class(df1)) == "list") dff <- df1[[j]]
      
      #add rownames
      if(!any(is.na(indlab)))
      {
        if((nrow(dff))!=length(indlab)) stop(paste0("plotRunsMultiline: Length of indlab (", length(indlab),") do not match number of individuals in input file (",nrow(dff),")."))
        if(any(duplicated(indlab))) stop("plotMultiline: Duplicate labels in indlab.")
        row.names(dff) <- indlab
      }
      if(any(is.na(indlab)) && (!indlabfromfile)) row.names(dff) <- 1:nrow(dff) 
      
      #ordering individuals
      if(all(!is.na(sortind)))
      {
        if(sortind == "all")
        {
          maxval <- apply(dff,1,max)
          matchval <- vector(length=nrow(dff))
          for(k in 1:nrow(dff)) matchval[k] <- match(maxval[k],dff[k,])
          dftemp <- dff
          dftemp$maxval <- maxval
          dftemp$matchval <- matchval
          dff <- dff[with(dftemp, order(matchval,-maxval)), ,drop=FALSE]
        }else{
          if(!(sortind %in% colnames(dff))) stop("plotMultiline: Ordering cluster not found in file header.")
          dff <- dff[order(dff[[sortind]]), ,drop=FALSE]
        }
      }
      
      #primary calculation of spl
      nr1 <- nrow(dff)
      
      #numrows <- floor(nr1/spl)
      #numextra <- nr1-(spl*numrows)
      #nr2 <- numrows
      #if (numextra > 0) nr2 <- nr2+1
      
      if (!is.na(spl))
      {
        if (spl > nr1) stop("plotMultiline: Samples per line (spl) is greater than total number of samples.")
        spl1 <- spl
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
          if (numextra < 0.70*spl1) spl1 = spl1+1
          if (spl1 > nr1) 
          {
            spl1 <- nr1 
            break
          }
        }
      }
      
      nr2 <- numrows
      if (numextra > 0) nr2 <- nr2+1
      
      #get colours
      coll <- popcol
      if (any(is.na(popcol))) coll <- pophelper:::getColours(ncol(dff))
      if(length(coll) < ncol(dff)) stop(paste0("plotMultiline: Number of colours (",length(coll),") is less than the number of clusters (",ncol(dff),")."))

      #sorting labels
      # if(is.na(sortind)){
      #   if(!indlabfromfile) dff$ind <- as.factor(as.numeric(1:nr1))
      #   if(indlabfromfile) dff$ind <- factor(rownames(dff),levels=rownames(dff))
      #   if()
      # }else{
      #   if(!indlabfromfile)
      #   {
      #     if(!sortlabels) dff$ind <- as.factor(as.numeric(1:nr1))
      #     if(sortlabels) dff$ind <- factor(as.numeric(rownames(dff)),levels=rownames(dff))
      #   }
      #   if(indlabfromfile) dff$ind <- factor(rownames(dff),levels=rownames(dff))
      # }
      
      dff$ind <- factor(rownames(dff),levels=rownames(dff))
      dff$rows <- factor(c(rep(1:numrows, each = spl1), rep(nr2, each = numextra)))
      
      #dff$line <- as.factor(c(rep(1:spl1, numrows), 1:numextra))
      
      #split and plot rows
      dlist <- split(dff[,-ncol(dff)], dff$rows)
      plist <- vector("list",length = nr2)
      #widthsvec <- vector(length = nr2)
      for (i in 1: nr2)
      {
        #df2 <- reshape::melt(dlist[[i]], id.var = c("ind"))
        df2 <- tidyr::gather(dlist[[i]],"variable","value",-ind)
        df2 <- df2[rev(1:nrow(df2)),]
        plist[[i]] <- ggplot2::ggplot(data = df2, aes(x = ind, y = value, fill = variable))+
          geom_bar(width = barwidth, stat = "identity", position = "fill", na.rm = na.rm)+
          scale_x_discrete(expand = c(0, 0))+
          scale_y_continuous(expand = c(0, 0),limits=c(0,1))+
          scale_fill_manual(values = coll)+
          labs(x = NULL, y = NULL)+
          theme(legend.position = "none", panel.grid = element_blank(), panel.background = element_blank(), 
                axis.ticks = element_line(size = 0.25), axis.text.y = element_text(size = labsize), axis.line = element_blank(), 
                axis.title = element_blank(), axis.text.x = element_text(size = labsize, angle = labangle, 
                              vjust = labvjust, hjust = labhjust), plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0), "cm"))
        
        if (!yaxislabs) plist[[i]] <- plist[[i]] + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
        if (!indlabs) plist[[i]] <- plist[[i]] + theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())
        if (!ticks) plist[[i]] <- plist[[i]] + theme(axis.ticks = element_blank())
        #calculate widths. not implemented.
        #widthsvec[i] <- nrow(dlist[[i]])/spl1
      }
      
      #lpp calculations
      if (!is.na(lpp)) lpp1 <- lpp
      if (is.na(lpp)) 
      {
        lpp1 <- 11
        if (lpp1 > nr2) lpp1 <- nr2
      }
      numpages <- ceiling(nr2/lpp1)
      #numpagesextra <- nr2-(lpp*numpages)
      #numpages1 <- numpages
      #if (numpagesextra > 0) numpages1 <- numpages1+1
      
      e <- 0
      r <- 1
      while (r <= numpages)
      {
        start1 <- e + 1
        stop1 <- e + lpp1
        if (stop1 > length(plist)) stop1 <- length(plist)
        
        #widths <- widthsvec[start1:stop1]
        alist <- c(plist[start1:stop1], lpp1, 1)
        names(alist) <- c(as.character(start1:stop1), "nrow", "ncol")
        
        if (imgtype == "png") png(paste0(fname, "-Multiline-", j, "-", r, ".png"), height = height, width = width, res = res, units = units, type = "cairo")
        if (imgtype == "jpeg") jpeg(paste0(fname, "-Multiline-", j, "-", r, ".jpg"), height = height, width = width, res = res, units = units, quality = 100)
        if (imgtype == "pdf") pdf(paste0(fname, "-Multiline-", j, "-", r, ".pdf"), height = height, width = width)
        
        do.call(gridExtra::grid.arrange, alist)
        #grid.arrange(arrangeGrob(plist[start1:stop1]))
        #grid.arrange(plist[[1]], plist[[2]], nrow = 2, widths = c(0.4, 0.6))
        #do.call(fn1, plist[[1]])
        dev.off()
        
        if (imgtype == "png") cat(paste0(fname, "-Multiline-", j, "-", r, ".png exported.\n"))
        if (imgtype == "jpeg") cat(paste0(fname, "-Multiline-", j, "-", r, ".jpg exported.\n"))
        if (imgtype == "pdf") cat(paste0(fname, "-Multiline-", j, "-", r, ".pdf exported.\n"))
        
        e <- stop1
        r = r+1
      }
      rm(nr1,nr2,numrows,numextra,numpages,start1,stop1,e,r,dlist,plist,df2,dff)
    }
  }
}

#-------------------------------------------------------------------------------

#FUNCTION analyseRuns
#' Analyse STRUCTURE, TESS or MATRIX runs. Wrapper around several smaller functions.
#' @description A single function to analyse STRUCTURE, TESS or MATRIX runs. Performs tabulate, summarise, evanno method, clumpp export, plots runs and converts runs to an R object.
#' @param files A character or character vector of one or more STRUCTURE, TESS or MATRIX run files
#' @param evannoMethod A logical indicating if evanno method should be performed. Applies only to STRUCTURE runs.
#' @param clumppExport A logical indicating if files must be exported for clumpp.
#' @param plotRuns A logical indicating if selected files should be exported as barplots.
#' @param runsToDf A logical indicating if selected files should be converted and returned as an R object (dataframe or a list).
#' @param imgoutput A character indicating if files are plotted as separate image files ("sep") or joined into a single image ("join").
#' @param poplab A character vector of population labels equal to length of individuals. Each pop name must repeat to the number of individuals present in each pop.
#' @param popcol A character vector of colours (representing populations) for colouring clusters. If NA, colours are automatically generated. K 1 to 12 are custom unique colours while K>12 are coloured by function rich.color().
#' @param writetable A logical T or F. Setting to TRUE writes the output table to the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @param sorttable A logical indicating if the output table must be sorted. Sorts table by loci, ind and K when available.
#' @param quiet A logical indicating if messages must be printed
#' @details The function \code{analyseRuns} is a wrapper around several other \code{pophelper} functions. All arguments for all these other functions are not available. If more arguments/options are required, consider running the functions separately.
#' @return If a single file is selected, a single dataframe is returned. If 
#' multiple files are selected, a list with multiple dataframes is returned.
#' @examples 
#' \dontrun{
#' #structure files
#' slist <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' analyseRuns(slist)
#' 
#' #tess files
#' tlist <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' analyseRuns(tlist)
#' 
#' #admixture files
#' alist <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' analyseRuns(alist)
#' }
#' @export
#' 
analyseRuns <- function(files = NULL, evannoMethod = TRUE, clumppExport = TRUE, plotRuns = TRUE, runsToDf = TRUE, 
                        imgoutput = "sep", poplab = NA, popcol=NA, writetable = TRUE, sorttable = TRUE, quiet = FALSE)
{
  if(is.null(files) | (length(files) == 0)) stop("analyseRuns: No input files.")
  if(!is.logical(evannoMethod)) stop("analyseRuns: Argument 'evannoMethod' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(clumppExport)) stop("analyseRuns: Argument 'clumppExport' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(plotRuns)) stop("analyseRuns: Argument 'plotRuns' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(runsToDf)) stop("analyseRuns: Argument 'runsToDf' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(writetable)) stop("analyseRuns: Argument 'writetable' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(sorttable)) stop("analyseRuns: Argument 'sorttable' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(quiet)) stop("analyseRuns: Argument 'quiet' set incorrectly. Set as TRUE or FALSE.")
  
  chk1 <- unique(pophelper:::checkRuns(files=files, warn=FALSE)$type)
  if (length(chk1) > 1) stop("analyseRuns: Mixed runs selected.")
  if ("TAB" %in% chk1) stop("analyseRuns: TAB files cannot be used in this function.")
  if ("UNIDENTIFIED" %in% chk1) stop("analyseRuns: Input file has incorrect format. Check if selected files are STRUCTURE, TESS or MATRIX runs.")
  if (imgoutput != "sep" && imgoutput != "join") stop("analyseRuns: Argument 'imgoutput' set incorrectly. Set as 'sep' to export as separate plots. Set as 'join' to export as one joined plot.")
  
  
  if(chk1 == "STRUCTURE")
  {
    df1 <- tabulateRunsStructure(files,writetable = writetable, sorttable = sorttable, quiet = quiet)
    df2 <- summariseRunsStructure(df1, writetable = writetable)
    if (evannoMethod == TRUE) evannoMethodStructure(df2, writetable = writetable, exportplot = TRUE)
    if (clumppExport == TRUE) clumppExportStructure(files)
    if (plotRuns == TRUE) plotRuns(files, imgoutput = imgoutput, poplab = poplab, popcol = popcol)
    if (runsToDf == TRUE) return(runsToDfStructure(files))
  }
  
  if(chk1 == "TESS")
  {
    df1 <- tabulateRunsTess(files,writetable = writetable, sorttable = sorttable, quiet = quiet)
    df2 <- summariseRunsTess(df1, writetable = writetable)
    if (clumppExport == TRUE) clumppExportTess(files)
    if (plotRuns == TRUE) plotRuns(files, imgoutput = imgoutput, poplab = poplab, popcol = popcol)
    if (runsToDf == TRUE) return(runsToDfTess(files))
  }
  
  if(chk1 == "MATRIX")
  {
    df1 <- tabulateRunsMatrix(files,writetable = writetable, sorttable = sorttable, quiet = quiet)
    df2 <- summariseRunsMatrix(df1, writetable = writetable)
    if (clumppExport == TRUE) clumppExportMatrix(files)
    if (plotRuns == TRUE) plotRuns(files, imgoutput = imgoutput, poplab = poplab, popcol = popcol)
    if (runsToDf == TRUE) return(runsToDfMatrix(files))
  }
}

#-------------------------------------------------------------------------------

#FUNCTION determineRowsAndCols
#' Internal: Determine rows and columns for arbitrary number of plots
#' @description Internal: Determine rows and columns for figures from arbitrary number of plots
#' @param numplots A numeric indicating the number of plots available for plot
#' @return A 2 value vector wih first value denoting row and second value as column.
# @export
#' 
determineRowsAndCols <- function(numplots = NA)
{
  if(is.na(numplots)) stop("determineRowsAndCols: No input for number of plots.")
  if (numplots == 1) return(c(1,1))
  if (numplots == 2) return(c(1,2))
  if (numplots == 3) return(c(1,3))
  if (numplots == 4) return(c(2,2))
  if (numplots == 5) return(c(2,3))
  if (numplots == 6) return(c(2,3))
  if (numplots == 7) return(c(2,4))
  if (numplots == 8) return(c(2,4))
  if (numplots == 9) return(c(3,3))
  if (numplots == 10) return(c(2,5))
  if (numplots == 11) return(c(3,4))
  if (numplots == 12) return(c(3,4))
  if (numplots == 13) return(c(4,4))
  if (numplots == 14) return(c(4,4))
  if (numplots == 15) return(c(5,3))
  if (numplots == 16) return(c(4,4))
  if (numplots == 17) return(c(5,4))
  if (numplots == 18) return(c(6,3))
  if (numplots == 19) return(c(4,5))
  if (numplots == 20) return(c(4,5))
  if (numplots>20) stop("determineRowsAndCols: Number of clusters > 20. Specify number of rows and columns for figures manually using the option nrow and ncol arguments.")
}

#-------------------------------------------------------------------------------

#FUNCTION llToUtm
#' Internal: Find UTM zone from a latitude and longitude
#' @description Internal: Find UTM zone from a given latitude and longitude
#' @param lat The latitude in decimals. Southern hemisphere must be negative.
#' @param long The longitude in decimals
#' @details For a given latitude and longitude, the UTM zone must be determined 
#' prior to UTM coordinate conversion. 
#' @return A list of two values are returned namely UTMZone and Hemisphere.
# @export
#' 
llToUtmzone <- function(lat,long)
{
  lat <- as.numeric(lat)
  if(is.na(lat)) stop("llToUtmzone: Non-numeric input to latitude.")
  long <- as.numeric(long)
  if(is.na(long)) stop("llToUtmzone: Non-numeric input to longitude.")
  
  #basic UTM Zone calculation
  utm = floor((long + 180)/6) + 1
  if( lat >= 56.0 && lat < 64.0 && long >= 3.0 && long < 12.0 ) utm = 32
  
  #Special zones for Svalbard
  if( lat >= 72.0 && lat < 84.0 )
  {
    if ( long >= 0.0  && long <  9.0 ) utm = 31
    if ( long >= 9.0  && long < 21.0 ) utm = 33
    if ( long >= 21.0 && long < 33.0 ) utm = 35
    if ( long >= 33.0 && long < 42.0 ) utm = 37
  }
  
  #Hemisphere calculation
  if(lat>0) hem<-"N"
  if(lat<0) hem<-"S"
  
  return(list(UTMZone = utm,Hemisphere = hem))
}

#-------------------------------------------------------------------------------

#FUNCTION Interpolate STRUCTURE, TESS or MATRIX runs spatially
#' Interpolate STRUCTURE, TESS or MATRIX runs spatially
#' @description Interpolate clusters from STRUCTURE, TESS or MATRIX runs spatially using coordinates.
#' @param datafile One STRUCTURE, TESS or MATRIX output file. Input is either a character 
#' or a dataframe. If character, then a path pointing to location of the datafile. Can use 
#' \code{choose.files()}. If a dataframe, then an output from \code{runsToDfStructure()},
#' \code{runsToDfTess()} or \code{runsToDfMatrix()}.
#' @param coordsfile A character or a dataframe. If character, then a path pointing 
#' to location of the coordsfile. It must be a tab-delimited text file with x and y 
#' coordinates of the samples. The number of rows must be equal to the number of 
#' samples in datafile. The coordsfiles must have no header and 2 columns in the 
#' order: x (latitude) and then y (longitude). Coordinates must be in standard 
#' longitude latitude (LL) decimals.
#' @param method A character indicating the method employed for interpolation. Options are "bilinear",
#' "bicubic", "krig" (Kriging), "idw" (Inverse distance weighting) or "nn" (nearest 
#' neighbour). See Details for more information.
#' @param duplicate A character indicating how to deal with duplicate spatial locations. This is only 
#' applicable to 'bilinear' and 'bicubic' methods. Options are "error" (error message
#' if duplicate points), "strip" (remove all duplicate points), "mean" (mean of 
#' duplicate points), "median" (median of duplicate points).
#' @param idwpower A numeric indicating the power of inverse distance weighting if method is set to "idw".
#' @param clusters A numeric or numeric vector indicating the clusters to plot. If NA, all clusters are plotted. For ex.
#' If set to 2, cluster 2 is plotted. If set to 2:4, clusters 2, 3 and 4 are plotted.
#' If set to c(1,4,5), clusters 1, 4 and 5 are plotted.
#' @param gridsize A numeric indicating the size of the image grid on which interpolation is to be carried
#' out. Set to 60 by default. Higher values produces less pixelated grids, but more
#' computationally intensive.
#' @param imgoutput A character. To plot each cluster as a seperate figure, set to "sep". To plot 
#' multiple clusters in a single figure, set to "join". If number of clusters is less
#' than 2, then automatically set to "sep".
#' @param colours A vector of colours. R colour names or hexadecimal values. Set to
#' 9 value 'Blues' palette from RColorBrewer by default.
#' @param nrow A numeric indicating the number of rows of plots in a joined plot. Determined automatically
#' if number of plots <20 and \code{nrow = NA}.
#' @param ncol A numeric indicating the number of columns of plots in a joined plot. Determined automatically
#' if number of plots <20 and \code{ncol = NA}.
#' @param exportplot A logical. If set to FALSE, no image is exported.
#' @param imgtype A character indicating the export format for figures. Options are "png", "jpeg" or "pdf". 
#' If pdf, height and width must be in inches and res argument is ignored (set to 300).
#' @param height A numeric indicating the height of export figure in cm unless units are changed.
#' @param width A numeric indicating the width of export figure in cm unless units are changed.
#' @param units A character indicating units of measurement for figure dimensions. Set to 'cm' by default.
#' @param res A numeric indicating the pixel resolution of the export image. Set to 200 by default.
#' @param showaxis A logical. If TRUE, then axis text, axis ticks and plot border are plotted.
#' @param addpoints A logical. If TRUE, then sample coordinates are overplotted on interpolated grid.
#' @param pointcol Colour of sample points. An R colour or hexadecimal value.
#' @param pointtype The shape/pch of sample points. A numeric or a character.
#' @param pointsize The size of sample points. A number usually 0.4,0.8,1,3 etc.
#' @param legend A logical. If TRUE, the legend for the colours is plotted.
#' @param legendpos A character of 2-value numeric vector indicating the position of the legend. If "right","left","top" or "bottom", then,
#' legend is plotted outside the plot area. To plot inside plot area use a 2 vale vector.
#' If a vector like c(1,1), first value denotes x-axis from 0 to 1 and second value 
#' denotes y-axis from 0 to 1. For ex. to plot in bottom left corner, use c(0,0).
#' @param legendjust The x and y axis justification of the legend. A 2 value vector.
#' @param legendsize A numeric indicating the size of the legend in cm. Usually values like 0.5,0.7,1.2 etc.
#' The legendsize does not control the text in the legend.
#' @param legendtextsize A numeric indicating the size of the text in the legend.
#' @param dataout A logical. If set to TRUE, a list of one or more \code{ggplot} gtable elements are returned.
#' This output can be modified using \code{ggplot} themes() for more figure control if required.
#' @return If \code{dataout = TRUE}, a list of one or more \code{ggplot} gtable output is returned for more theme 
#' control if required.
#' @details The "bilinear", "bicubic", "idw" and "nn" are essentially direct interpolation
#' between spatial points. The "krig" option is predictive rather than direct interpolation.
#' The kriging function is same function provided by the TESS authors in their R script.
#' The function uses great circle distances \code{rdist.earth()} from \code{fields} package to
#' determine theta. Therefore coordinates must be in LL and not UTM.\cr
#' \cr
#' For more details of methods, see R package \code{akima} function \code{interp} for "bilinear" 
#' and "bicubic" methods, see R package \code{fields} function \code{Krig} for "krig" method, see
#' R package \code{spatstat} function \code{idw} for "idw" and function \code{nnmark} for "nn" method. 
#' The model for "krig" is automatically determined and may produce warning messages if
#' the GCV algorithm does not converge optimally. This shouldn't be an issue for exploratory
#' analyses. All methods require full coordinate data. No missing data allowed in coordsfile.\cr
#' @examples 
#' \dontrun{
#' #tess files
#' tlist <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' cd1 <- system.file("files/coords75.txt",package="pophelper")
#' plotRunsInterpolate(datafile=tlist[2],coordsfile=cd1)
#' }
#' @import akima
#' @import fields
#' @import gridExtra
#' @import spatstat
#' @export
#' 
plotRunsInterpolate <- function(datafile = NULL, coordsfile = NULL,method = "krig", 
                               duplicate = "mean",idwpower = 2,clusters = NA,gridsize = 60,
                               imgoutput = "join",colours = NA,nrow = NA,ncol = NA,exportplot = TRUE,
                               imgtype = "png",height = NA, width = NA, units = "cm",res = 200,
                               showaxis = FALSE,addpoints = TRUE,pointcol = "grey10",pointtype = "+",
                               pointsize = 4,legend = TRUE,legendpos = c(1,1),legendjust = c(1,1),
                               legendsize = NA,legendtextsize = NA,dataout = FALSE)
{
  #basic checks
  if (is.null(datafile) | length(datafile) == 0) stop("plotRunsInterpolate: No content in datafile.")
  if (is.null(coordsfile) | length(coordsfile) == 0) stop("plotRunsInterpolate: No content in coordsfile.")
  method <- tolower(method)
  if (method != "bilinear" && method != "bicubic" && method != "krig" && method != "idw" && method != "nn") stop("plotRunsInterpolate: Argument 'method' set incorrectly. Set as 'bilinear', bicubic', 'krig', 'idw' or 'nn'.")
  imgoutput <- tolower(imgoutput)
  if (imgoutput != "sep" && imgoutput != "join") stop("plotRunsInterpolate: Argument 'imgoutput' set incorrectly. Set as 'sep' to plot each cluster seperately. Set as 'join' to plot multiple clusters in one figure.")
  imgtype <- tolower(imgtype)
  if (imgtype!="png" && imgtype != "pdf" && imgtype != "jpeg") stop("plotRunsInterpolate: Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg' or 'pdf'.")
  duplicate <- tolower(duplicate)
  if (duplicate != "error" && duplicate != "strip" && duplicate != "mean" && duplicate != "median") stop("plotRunsInterpolate: Argument 'duplicate' not set correctly. Set as 'error','strip','mean' or 'median'.")
  if(!is.logical(exportplot)) stop("plotRunsInterpolate: Argument 'exportplot' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(showaxis)) stop("plotRunsInterpolate: Argument 'showaxis' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(addpoints)) stop("plotRunsInterpolate: Argument 'addpoints' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(legend)) stop("plotRunsInterpolate: Argument 'legend' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(dataout)) stop("plotRunsInterpolate: Argument 'dataout' set incorrectly. Set as TRUE or FALSE.")
  
  #declare colours
  if(all(is.na(colours))) colours <- rev(c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6","#4292C6", "#2171B5", "#08519C", "#08306B"))
  
  #READ DATA FILES AND CHECK
  if (is.data.frame(datafile)) 
  {
    df1 <- datafile
    fname <- format(Sys.Date(), format = "%Y%m%d")
  }
  
  if (is.character(datafile))
  {
    if(length(datafile) > 1) datafile <- datafile[1]
    #get file name
    fname <- gsub(".txt", "", basename(datafile))
    fname <- gsub(".txt", "", fname)
    
    chk <- pophelper:::checkRuns(datafile)$type
    #read files
    if (chk == "STRUCTURE") df1 <- pophelper::runsToDfStructure(files = datafile)
    if (chk == "TESS") df1 <- pophelper::runsToDfTess(files = datafile)
    #if (chk == "ADMIXTURE") df1 <- pophelper::runsToDfAdmixture(files = datafile)
    if (chk == "MATRIX") df1 <- pophelper::runsToDfMatrix(files = datafile)
    if (chk == "TAB" | chk == "UNIDENTIFIED") stop("plotRunsInterpolate: Incorrect input file type.")
  }
  
  #data check
  class1 <- lapply(df1,class)
  if (!all(unlist(class1) == "numeric")) warning("plotRunsInterpolate: Non numeric content in datafile.")
  
  #READ COORDS AND CHECK
  if (is.character(coordsfile)) coords <- read.delim(coordsfile, header = F)[,1:2]
  if (is.data.frame(coordsfile)) coords <- coordsfile
  #coords check
  class2 <- lapply(coords,class)
  if (!all(unlist(class2) == "numeric")) warning("plotRunsInterpolate: Non numeric content in coordsfile.")
  if (any(is.na(coords))) stop("plotRunsInterpolate: Missing data detected in coordsfile. Methods cannot handle missing coordinate data.")
  colnames(coords) <- c("X","Y")
  
  #data coords length check
  if (nrow(df1) != nrow(coords)) stop("plotRunsInterpolate: Number of rows of datafile not equal to number of rows of coordsfile.")
  
  #determine clusters to plot
  if (all(is.na(clusters))) flen <- 1:length(colnames(df1))
  if (length(clusters) == 1) {if(is.numeric(clusters)) flen <- clusters:clusters}
  if (length(clusters) > 1) flen <- clusters
  if (!is.numeric(clusters) && !is.na(clusters)) stop("plotRunsInterpolate: Argument clusters in non-numeric.")
  
  #determine if atleast 2 plots are available for joined option
  if (length(flen) < 2 && imgoutput == "join")
  {
    imgoutput <- "sep"
    message("Less than 2 plots available for joined. Argument imgoutput changed to 'sep'.")
  }
  
  #get dimensions for sep figures
  height1 <- height
  width1 <- width
  #determine aspect ratio from coordinates
  #this needs work. default is rubbish.
  figaspect <- round((max(coords$X,na.rm = T)-min(coords$X,na.rm = T))/(max(coords$Y,na.rm = T)-min(coords$Y,na.rm = T)),2)
  if (figaspect > 1)
  {
    if (is.na(height)) height1 <- 16
    if (is.na(width)) width1 <- round(height1*abs(figaspect),2)
  }else
  {
    if (is.na(width)) width1 <- 16
    if (is.na(height)) height1 <- round(width1*abs(figaspect),2)
  }
  if (imgtype == "pdf" && any(!is.na(height) | !is.na(width))) warning("plotRunsInterpolate: Height and width will be taken as inches if argument imgtype is set to pdf.")
  if (imgtype == "pdf" && all(is.na(height) && is.na(width))) 
  {
    height1 <- round(height1*0.394,2)
    width1 <- round(width1*0.394,2)
    units <- "in"
  }
  
  plist <- vector("list", length(flen))
  datalist <- vector("list", length(flen))
  #start loop
  i <- 1
  while(i <= length(flen))
  {
    j <- flen[i]
    plottitle <- paste0("Cluster ",sub("Cluster","",colnames(df1)[j]))
    
    #bicubic and bilinear methods
    if(method == "bilinear" | method == "bicubic")
    {
      X <- coords$X
      Y <- coords$Y
      interpX <- seq(min(X,na.rm = T), max(X,na.rm = T), le = gridsize)
      interpY <- seq(min(Y,na.rm = T), max(Y,na.rm = T), le = gridsize) 
      if (method == "bilinear") tempimg <- akima::interp(X, Y, df1[,j], xo = interpX, yo = interpY, duplicate = duplicate,linear = TRUE)
      if (method == "bicubic") tempimg <- akima::interp(X, Y, df1[,j], xo = interpX, yo = interpY, duplicate = duplicate,linear = FALSE)
      rm(X,Y,interpX,interpY)
    }
    
    #kriging method
    if (method == "krig")
    {
      sc <- mean(fields::rdist.earth(data.frame(Y = coords$Y,X = coords$X, stringsAsFactors = FALSE)),miles = FALSE)
      fit <- fields::Krig(x = coords,Y = df1[,j], theta = sc, m = 1, Distance = "rdist.earth",na.rm = TRUE)
      #fit <- fields::Krig(x = coords,Y = df1[,j], m = 1,na.rm = TRUE)
      tempimg <- fields::predictSurface(fit,nx = gridsize,ny = gridsize)
      #surface(tempimg)
      rm(sc,fit)
    }
    
    #inverse distance weighting
    if (method == "idw")
    {
      pp1 <- spatstat::as.ppp(coords, c(min(coords$X,na.rm = T),max(coords$X,na.rm = T),min(coords$Y,na.rm = T),max(coords$Y,na.rm = T)))
      pp1$marks <- as.vector(df1[,j])
      pp1$markformat <- "vector"
      pp2 <- spatstat::idw(pp1,power = idwpower,at = "pixels",dimyx = c(gridsize,gridsize))
      tempimg <- list(x = pp2$xcol,y = pp2$yrow,z = pp2$v)
      rm(pp1,pp2)
    }
    
    #nearest neighbour
    if (method == "nn")
    {
      pp1 <- spatstat::as.ppp(coords, c(min(coords$X,na.rm = T),max(coords$X,na.rm = T),min(coords$Y,na.rm = T),max(coords$Y,na.rm = T)))
      pp1$marks <- as.vector(df1[,j])
      pp1$markformat <- "vector"
      nn1 <- spatstat::nnmark(X = pp1,k = 1,at = "pixels",dimyx = c(gridsize,gridsize))
      tempimg <- list(x = nn1$xcol,y = nn1$yrow,z = nn1$v)
      rm(nn1)
    }
    
    if(all(is.na(tempimg$z))) stop("plotRunsInterpolate: NA in output. Method does not work.")
    
    #expand grid over gridsize
    tempimg1 <- expand.grid(x = tempimg$x, y = tempimg$y)
    tempimg1$z <- as.vector(tempimg$z)
    attr(tempimg1,"out.attrs") <- NULL
    rm(tempimg)
    
    #store to a list
    datalist[[i]] <- tempimg1
    #name list item
    names(datalist)[i] <- plottitle
    tempimg1$plot <- rep(plottitle,length(tempimg1$z))
    
    #plot
    p <- ggplot2::ggplot(tempimg1)+
      geom_tile(aes(x = x,y = y,fill = z))+
      scale_x_continuous(expand = c(0, 0))+
      scale_y_continuous(expand = c(0, 0))+
      scale_fill_gradientn(colours = rev(colours),na.value = "#FFFFFF00")+
      theme_bw()+labs(x = NULL, y = NULL, title = plottitle)+
      theme(legend.title = element_blank(),plot.title = element_text(colour = "grey40",hjust = 0),
            axis.text = element_text(colour = "grey30"),axis.ticks = element_line(colour = "grey30"),
            legend.justification = legendjust, legend.position = legendpos)
    
    #edit plot conditionally
    if (!showaxis) p <- p + theme(axis.text = element_blank(),axis.ticks = element_blank(), panel.border = element_blank())
    if (addpoints) p <- p + geom_point(data = coords,aes(x = X,y = Y),size = pointsize,shape = pointtype,fill = pointcol,colour = pointcol)
    if (!legend) p <- p + theme(legend.position = "none")
    if (!is.na(legendsize)) p <- p + theme(legend.key.size = grid::unit(legendsize, "cm"))
    if (!is.na(legendtextsize)) p <- p + theme(legend.text = element_text(size = legendtextsize))
    
    plist[[i]] <- p
    if (exportplot && imgoutput == "sep")
    {
      if (imgtype == "png") png(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".png"),height = height1,width = width1,units = units,res = res,type = "cairo")
      if (imgtype == "jpeg") jpeg(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".jpg"),height = height1,width = width1,units = units,res = res, quality = 100)
      if (imgtype == "pdf") pdf(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".pdf"),height = height1,width = width1)
      print(p)
      dev.off()
      if (imgtype == "png") cat(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".png exported.\n"))
      if (imgtype == "jpeg") cat(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".jpg exported.\n"))
      if (imgtype == "pdf") cat(paste0(fname,"-Interpolation-",method,"-",colnames(df1)[i],".pdf exported.\n"))
    }
    i = i+1
  }
  
  if (exportplot && imgoutput == "join")
  {
    #determine rows and cols
    if (is.na(nrow)) nrow <- pophelper:::determineRowsAndCols(length(flen))[1]
    if (is.na(ncol)) ncol <- pophelper:::determineRowsAndCols(length(flen))[2]
    #determine height and width
    height2 <- height
    width2 <- width
    if (is.na(height)) height2 <- (height1*nrow)/1.5
    if (is.na(width)) width2 <- (width1*ncol)/1.5
    
    alist <- c(plist, nrow, ncol)
    names(alist) <- c(as.character(flen), "nrow", "ncol")
    
    if (imgtype == "png") png(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.png"), height = height2, width = width2, res = res, units = units, type = "cairo")
    if (imgtype == "jpeg") jpeg(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.jpg"), height = height2, width = width2, res = res, units = units, quality = 100)
    if (imgtype == "pdf") pdf(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.pdf"), height = height2, width = width2)
    
    do.call(gridExtra::grid.arrange, alist)
    dev.off()
    
    if (imgtype == "png") cat(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.png exported.\n"))
    if (imgtype == "jpeg") cat(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.jpg exported.\n"))
    if (imgtype == "pdf") cat(paste0(fname, "-Interpolation-",method,"-", length(flen), "Clusters.pdf exported.\n"))
    
  }
  if (dataout) return(plist)
}

#-------------------------------------------------------------------------------

#FUNCTION ellipseCI
#' Internal: ellipseCI
#' @description Internal: Calculate ellipse for bivariate quantile
#' @param x A numeric vector of x coordinates
#' @param y A numeric vector of y coordinates
#' @param conf A numeric indicating confidence interval
#' @param np A numeric indicaitng the number of points
#' @details  Obtained from Claude J (2008) Morphometrics with R, Springer
#' @return A dataframe with x and y coordinates of the ellipse. Number of 
#' rows is equal to argument np.
# @export
#' 
ellipseCI <- function(x,y,conf = 0.95,np = 100)
{
  if(!is.numeric(x)) stop("ellipseCI: Argument 'x' is not numeric.")
  if(!is.numeric(y)) stop("ellipseCI: Argument 'y' is not numeric.")
  if(!is.numeric(conf)) stop("ellipseCI: Argument 'conf' is not numeric.")
  if(!is.numeric(np)) stop("ellipseCI: Argument 'np' is not numeric.")
  
  centroid <- apply(cbind(x,y),2,mean)
  ang <- seq(0,2*pi,length = np)
  z <- cbind(cos(ang),sin(ang))
  radiuscoef <- qnorm((1-conf)/2, lower.tail = F)
  vcvxy <- var(cbind(x,y))
  r <- cor(x,y)
  M1 <- matrix(c(1,1,-1,1),2,2)
  M2 <- matrix(c(var(x), var(y)),2,2)
  M3 <- matrix(c(1+r, 1-r),2,2, byrow = T)
  ellpar <- M1*sqrt(M2*M3/2)
  t1 <- t(centroid + radiuscoef * ellpar %*% t(z))
  t1 <- as.data.frame(t1,stringsAsFactors = FALSE)
  colnames(t1) <- c("x","y")
  return(t1)
}

#-------------------------------------------------------------------------------

#FUNCTION plotRunsSpatial
#' plotRunsSpatial
#' @description Plot STRUCTURE, TESS or MATRIX runs spatially and colour individuals by max assignment cluster.
#' @param datafile One STRUCTURE, TESS or MATRIX output file. Input is either a character 
#' or a dataframe. If character, then a path pointing to location of the datafile. Can use 
#' \code{choose.files()}. If a dataframe, then an output from \code{runsToDfStructure()}, 
#' \code{runsToDfTess()} or \code{runsToDfMatrix}.
#' @param coordsfile A character or a dataframe. If character, then a path pointing 
#' to location of the coordsfile. It must be a tab-delimited text file with x and y 
#' coordinates of the samples. The number of rows must be equal to the number of 
#' samples in datafile. The coordsfiles must have no header and 2 columns in the 
#' order: x (latitude) and then y (longitude). Coordinates must be in standard 
#' longitude latitude (LL) decimals.
#' @param popcol A vector of colours for the clusters. R colour names or hexadecimal 
#' values. If NA, colours are automatically generated. K 1 to 12 are custom unique 
#' colours while K>12 are coloured by function \code{rich.color()}.
#' @param exportplot If set to FALSE, no image is exported.
#' @param imgtype A character indicating the export format for figures. Options are "png", "jpeg" or "pdf". 
#' If pdf, height and width must be in inches and res argument is ignored (set to 300).
#' @param height A numeric indicating the height of export figure. Default in cm unless units are changed. If \code{imgtype}
#' is pdf, then height must be in inches.
#' @param width A numeric indicating the the width of export figure. Default in cm unless units are changed. If \code{imgtype}
#' is pdf, then height must be in inches.
#' @param units A character indicating the units of measurement for figure dimensions. "cm", "mm" or "in".
#' @param res A numeric indicating the pixel resolution of the export image. Set to 200 by default.
#' @param showaxis A logical indicating if the axis text, axis ticks and plot border are plotted.
#' @param pointcol The colour character for sample points. An R colour or hexadecimal value.
#' @param pointtype The shape/pch of sample points. A numeric or a character. By default,
#' "+" is used for all points. If NA, then each cluster is plotted using a different shape.
#' If a numeric or character of length one is used, then it is used for all points. 
#' If a vector is used, then it must be equal to number clusters.
#' @param pointsize A numeric indicating the size of sample points. A number usually 0.4,0.8,1,3 etc.
#' @param pointtransp A numeric between 0 and 1 indicating the transparency of points.
#' @param chull A logical indicating if the convex hull is computed for each cluster. The outer
#' points of each cluster are connected by lines. If less than 3 points are available
#' in a cluster, then convex hull is not computed and a warning is shown.
#' @param chulltransp A numeric between 0 and 1 indicating the transparency of the convex hull.
#' @param chullsize A numeric indicating the thickness of the convex hull border.
#' @param chulltype A numeric indicating the line type of the convex hull border. Option pch in standard R.
#' @param ellipse A logical indicating if an ellipse around the clusters. Set to F to supress ellipse.
#' @param ellconf A numeric indicating the confidence interval of the ellipse. Defaults to 0.95.
#' @param ellsize A numeric indicating the thickness of the ellipse line.
#' @param elltype A numeric indicating the linetype for the ellipse. Option lty in standard R.
#' @param ellpoints A numeric indicating the number of points on the ellipse.
#' @param legend A logical indicating if the legend for the colours is plotted.
#' @param legendlabels A vector of labels for the legend denoting clusters. Defaults to cluster numbers.
#' @param legendpos A character or 2-value numeric vector indicating the position of the legend. If "right","left","top" or "bottom", then,
#' legend is plotted outside the plot area. To plot inside plot area use a 2 vale vector.
#' If a vector like c(1,1), first value denotes x-axis from 0 to 1 and second value 
#' denotes y-axis from 0 to 1. For ex. to plot in bottom left corner, use c(0,0).
#' @param legendjust The x and y axis justification of the legend. A 2-value vector.
#' @param legendsize A numeric indicating the size of the legend in cm. Usually values like 0.5,0.7,1.2 etc.
#' The legendsize does not control the text in the legend.
#' @param legendtextsize A numeric indicating the size of the text in the legend.
#' @param plottitle A character for the title of the plot. NULL by default.
#' @param filename A character name for the export figure. Automatically computed if NA.
#' @param setutm A logical. If TRUE, then LL coordinates are converted to UTM coordinates. The midpoint
#' of the longitude within the dataset is used to determine the UTM zone.
#' @param dataout A logical. If set to TRUE, a list of one or more \code{ggplot} gtable elements are returned.
#' This output can be modified using \code{ggplot} themes() for more figure control if required.
#' @return If \code{dataout = T}, a list of one or more \code{ggplot} gtable output is returned for more theme 
#' control if required.
#' @details The coordinates must always be provided as standard longitude-latitude (LL) decimal
#' format.
#' @examples
#' \dontrun{
#' #structure file 
#' s1 <- system.file("files/Structure239_4",package="pophelper")
#' cd2 <- system.file("files/coords239.txt",package="pophelper")
#' plotRunsSpatial(datafile=s1,coordsfile=cd2)
#' 
#' #set height
#' plotRunsSpatial(datafile=s1,coordsfile=cd2,height=12)
#' }
#' @import PBSmapping
#' @export
#' 
plotRunsSpatial <- function(datafile = NULL, coordsfile = NULL,popcol = NA,
                            exportplot = TRUE,imgtype = "png",height = NA, width = NA, 
                            units = "cm",res = 200,showaxis = FALSE,pointcol = "grey10",
                            pointtype = "+",pointsize = 4,pointtransp = 0.9,chull = FALSE,
                            chulltransp = 0.02,chullsize = 0.4,chulltype = 1,ellipse = TRUE,
                            ellconf = 0.95,ellsize = 0.4,elltype = 1,ellpoints = 100,legend = TRUE,
                            legendlabels = NA,legendpos = c(1,1),legendjust = c(1,1),legendsize = NA,
                            legendtextsize = NA,plottitle = NULL,filename = NA,setutm = FALSE,
                            dataout = FALSE)
  
{
  #basic checks
  if (is.null(datafile) | length(datafile) == 0) stop("plotRunsSpatial: No content in datafile.")
  if (is.null(coordsfile) | length(coordsfile) == 0) stop("plotRunsSpatial: No content in coordsfile.")
  if(!is.numeric(res)) stop("plotRunsSpatial: Argument 'res' set incorrectly. Use a numeric.")
  if(!is.character(pointcol)) stop("plotRunsSpatial: Argument 'pointcol' set incorrectly. Use a character.")
  if(!is.numeric(pointsize)) stop("plotRunsSpatial: Argument 'pointsize' set incorrectly. Use a numeric.")
  if(!is.numeric(pointtransp)) stop("plotRunsSpatial: Argument 'pointtransp' set incorrectly. Use a numeric.")
  if(!is.numeric(chulltransp)) stop("plotRunsSpatial: Argument 'chulltransp' set incorrectly. Use a numeric.")
  if(!is.numeric(chullsize)) stop("plotRunsSpatial: Argument 'chullsize' set incorrectly. Use a numeric.")
  if(!is.numeric(chulltype)) stop("plotRunsSpatial: Argument 'chulltype' set incorrectly. Use a numeric.")
  if(!is.numeric(ellconf)) stop("plotRunsSpatial: Argument 'ellconf' set incorrectly. Use a numeric.")
  if(!is.numeric(ellsize)) stop("plotRunsSpatial: Argument 'ellsize' set incorrectly. Use a numeric.")
  if(!is.numeric(ellpoints)) stop("plotRunsSpatial: Argument 'ellpoints' set incorrectly. Use a numeric.")
  if(!is.logical(exportplot)) stop("plotRunsSpatial: Argument 'exportplot' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(showaxis)) stop("plotRunsSpatial: Argument 'showaxis' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(chull)) stop("plotRunsSpatial: Argument 'chull' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(ellipse)) stop("plotRunsSpatial: Argument 'ellipse' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(legend)) stop("plotRunsSpatial: Argument 'legend' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(setutm)) stop("plotRunsSpatial: Argument 'setutm' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(dataout)) stop("plotRunsSpatial: Argument 'dataout' set incorrectly. Set as TRUE or FALSE.")
  imgtype <- tolower(imgtype)
  if (imgtype!="png" && imgtype != "pdf" && imgtype != "jpeg") stop("plotRunsSpatial: Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg' or 'pdf'.")
  
  #READ DATA FILES AND CHECK
  if (is.data.frame(datafile))
  {
    df1 <- datafile
    fname <- format(Sys.Date(), format = "%Y%m%d")
  }
  if (is.character(datafile))
  {
    if(length(datafile) > 1) datafile <- datafile[1]
    #get file name
    fname <- gsub(".txt", "", basename(datafile))
    fname <- gsub(".txt", "", fname)
    
    chk <- pophelper:::checkRuns(datafile)$type
    #read files
    if (chk == "STRUCTURE") df1 <- pophelper::runsToDfStructure(files = datafile)
    if (chk == "TESS") df1 <- pophelper::runsToDfTess(files = datafile)
    #if (chk == "ADMIXTURE") df1 <- pophelper::runsToDfAdmixture(files = datafile)
    if (chk == "MATRIX") df1 <- pophelper::runsToDfMatrix(files = datafile)
    if (chk == "TAB" | chk == "UNIDENTIFIED") stop("plotRunsSpatial: Incorrect input file type.")
  }
  
  #data check
  class1 <- lapply(df1,class)
  if (all(unlist(class1) == "numeric") != "TRUE") warning("plotRunsSpatial: Non numeric content in datafile.")
  
  #READ COORDS AND CHECK
  if (is.character(coordsfile)) coords <- read.delim(coordsfile,header = F)[,1:2]
  if (is.data.frame(coordsfile)) coords <- coordsfile
  #coords check
  class2 <- lapply(coords,class)
  if (!all(unlist(class2) == "numeric")) warning("plotRunsSpatial: Non numeric content in coordsfile.")
  if (any(is.na(coords))) stop("plotRunsSpatial: Missing data detected in coordsfile. Cannot handle missing coordinate data.")
  colnames(coords)<-c("X","Y")
  
  #data coords length check
  if (nrow(df1) != nrow(coords)) stop("plotRunsSpatial: Number of rows of datafile not equal to number of rows of coordsfile.")
  
  #copy to new variable
  if (setutm)
  {
    utmval <- pophelper:::llToUtmzone(mean(coords$X,na.rm = T),mean(coords$Y,na.rm = T))
    attr(coords,"projection") <- "LL"
    attr(coords,"zone") <- utmval$UTMZone
    df2 <- PBSmapping::convUL(coords,km = T)
  }else{
    df2 <- coords 
  }
  
  #find max value cluster
  fun_maxprob <- function(x) match(max(x),x)
  df2$Clusters <- factor(as.numeric(apply(df1,1,fun_maxprob)))
  clev <- levels(factor(as.character(df2$Clusters)))
  len <- length(clev)
  
  #Convex hull calculation
  if (chull)
  {
    slist <- vector("list",length = len)
    i <- 1
    while(i <= len)
    {
      j <- as.numeric(clev[i])
      s1 <- as.data.frame(subset(df2,df2$Clusters == j,drop = T),stringsAsFactors = FALSE)
      #compute chull only if >2 coordinates are present
      if (nrow(s1) > 2) {slist[[i]] <- s1[chull(s1$X,s1$Y),]}else{warning(paste0("plotRunsSpatial: Less than 3 coordinates in cluster ",j,". Convex hull not computed."))}
      i = i+1
    }
    s2 <- do.call("rbind",slist)
    s2$X <- as.numeric(s2$X)
    s2$Y <- as.numeric(s2$Y)
    s2$Clusters <- factor(as.numeric(as.character(s2$Clusters)))
    #levels of chull clusters
    llev <- levels(s2$Clusters)
  }
  
  #ellipse calculation
  if (ellipse)
  {
    slist <- vector("list",length = len)
    i <- 1
    while(i <= len)
    {
      j <- as.numeric(clev[i])
      s1 <- as.data.frame(subset(df2,df2$Clusters == j,drop = T),stringsAsFactors = FALSE)
      #compute ellipse only if >2 coordinates are present
      if (nrow(s1) > 2) 
      {
        el1 <- pophelper:::ellipseCI(x = s1$X, y = s1$Y, conf = ellconf, np = ellpoints)
        el1$group <- rep(i,ellpoints)
        slist[[i]] <- el1
      }else{warning(paste0("plotRunsSpatial: Less than 3 coordinates in cluster ",j,". Ellipse not computed."))}
      i = i+1
    }
    s3 <- do.call("rbind",slist)
    s3$x <- as.numeric(s3$x)
    s3$y <- as.numeric(s3$y)
    s3$group <- factor(as.numeric(as.character(s3$group)))
    #levels of chull clusters
    elev <- levels(s3$group)
  }
  
  #legend details
  #if (is.na(legendheader)) legendheader <- "Clusters"
  if (all(is.na(legendlabels))) legendlabels <- clev
  if (length(legendlabels) != length(clev)) stop(paste0("plotRunsSpatial: Number of provided legendlabels (",length(legendlabels),") is not equal to number of clusters (",length(levels(df2$Clusters)),")."))
  llp <- legendlabels
  #chull legend
  if (chull) llc <- llp[match(llev,clev)]
  #ellipse legend
  if (ellipse) lle <- llp[match(elev,clev)]
  
  #get colours
  popcol1 <- popcol
  if (all(is.na(popcol))) popcol1 <- pophelper:::getColours(len)
  if(length(popcol1) < length(levels(factor(as.character(df2$Clusters))))) stop("plotRunsSpatial: Number of colours less than number of clusters.")
  #chull colours
  if (chull) popcol2 <- popcol1[match(llev,clev)]
  if (ellipse) popcol3 <- popcol1[match(elev,clev)]
  
  #get dimensions for sep figures
  height1 <- height
  width1 <- width
  if (length(height) > 1) stop("plotRunsSpatial: Height must be a single numeric and not a vector.")
  if (length(width) > 1) stop("plotRunsSpatial: Width must be a single numeric and not a vector.")
  
  #determine aspect ratio from coordinates
  figaspect <- round((max(coords$X,na.rm = T)-min(coords$X,na.rm = T))/(max(coords$Y,na.rm = T)-min(coords$Y,na.rm = T)),2)
  if (figaspect > 1)
  {
    if (is.na(height)) height1 <- 12
    if (is.na(width)) width1 <- round(height1*abs(figaspect),2)
  }else
  {
    if (is.na(width)) width1 <- 12
    if (is.na(height)) height1 <- round(width1*abs(figaspect),2)
  }
  if (imgtype == "pdf" && any(!is.na(height) | !is.na(width))) warning("plotRunsSpatial: Height and width will be taken as inches if argument imgtype is set to pdf.")
  if (imgtype == "pdf" && all(is.na(height) && is.na(width))) 
  {
    height1 <- round(height1*0.394,2)
    width1 <- round(width1*0.394,2)
    units <- "in"
  }
  
  #plotting
  p <- ggplot2::ggplot()
  #pointtype adjustment
  if(all(is.na(pointtype))) p <- p + geom_point(data = df2,aes(x = X,y = Y,colour = Clusters,shape = Clusters),size = pointsize,fill = pointcol,alpha = pointtransp)
  if(!all(is.na(pointtype)) && length(pointtype) == 1) p <- p + geom_point(data = df2,aes(x = X,y = Y,colour = Clusters),size = pointsize,shape = pointtype,fill = pointcol,alpha = pointtransp)
  if(!all(is.na(pointtype)) && length(pointtype) > 1) p <- p + geom_point(data = df2,aes(x = X,y = Y,colour = Clusters,shape = Clusters),size = pointsize,fill = pointcol,alpha = pointtransp) + scale_shape_manual(values = pointtype)
  
  p <-  p + scale_colour_manual(values = popcol1,labels = llp)+
    theme_bw()+labs(x = NULL, y = NULL, title = plottitle)+
    theme(plot.title = element_text(colour = "grey40",hjust = 0),axis.text = element_text(colour = "grey30"),
          axis.ticks = element_line(colour = "grey30"),legend.justification = legendjust,legend.position = legendpos,
          legend.text = element_text(colour = "grey30"),legend.title = element_blank())
  
  #show convex hulls if true
  if (chull) p <- p + geom_polygon(data = s2,aes(x = X,y = Y,group = Clusters,colour = Clusters,fill = Clusters),alpha = chulltransp,linetype = chulltype,size = chullsize)+
    scale_fill_manual(values = popcol2,labels = llc)
  if (ellipse) p <- p + geom_path(data = s3, aes(x = x, y = y,colour = group),size = ellsize,linetype = elltype)
  #hide axis if false
  if (!showaxis) p <- p + theme(axis.text = element_blank(),axis.ticks = element_blank())
  #hide legend if true
  if (!legend) p <- p + theme(legend.position = "none")
  #adjust legend size if not NA
  if (!is.na(legendsize)) p <- p + theme(legend.key.size = grid::unit(legendsize, "cm"))
  #adjust legend text size if not NA
  if (!is.na(legendtextsize)) p <- p + theme(legend.text = element_text(size = legendtextsize))
  
  if (exportplot)
  {
    fname1 <- paste0(fname,"-Spatial")
    if (!is.na(filename)) fname1 <- filename
    
    #cat(paste("Height: ",height1,"\n"))
    #cat(paste("Width: ",width1,"\n"))
    #cat(paste("Res: ",res,"\n"))
    #cat(paste("Units: ",units,"\n"))
    
    if (imgtype == "png") png(paste0(fname1,".png"), height = height1, width = width1, res = res, units = units,type = "cairo")
    if (imgtype == "jpeg") jpeg(paste0(fname1,".jpg"), height = height1, width = width1, res = res, units = units, quality = 100)
    if (imgtype == "pdf") pdf(paste0(fname1,".pdf"), height = height1, width = width1)
    print(p)
    dev.off()
    if (imgtype == "png") cat(paste0(fname1,".png exported.\n"))
    if (imgtype == "jpeg") cat(paste0(fname1,".jpg exported.\n"))
    if (imgtype == "pdf") cat(paste0(fname1,".pdf exported.\n"))
  }
  if (dataout) return(p)
}

#-------------------------------------------------------------------------------

#FUNCTION distructExport
#' Generate input files for DISTRUCT.
#' @description Create DISTRUCT input files from STRUCTURE/TESS/MATRIX runs or TAB files.
#' @param files A character/path or vector of files. Input can be STRUCTURE, TESS, MATRIX run files or TAB files. On windows, use \code{choose.files(multi = TRUE)} for interactive selection.
#' @param poplabbottom A character vector of population labels to be plotted below the plot. The vector must be the same length as number of individuals. See details.
#' @param poplabtop An optional character vector of population labels to be plotted above the plot. The vector must be the same length as number of individuals. See details.
#' @param popmean A logical indicating if individual values are to be plotted (F) or population means are to be plotted (T).
#' @param overwritedirs A logical indicating if existing directories must be overwritten (T) automatically or not (F).
#' @param printtitle A logical indicating if the filename must be printed as the title on the plot.
#' @param popcol A character vector of colours equal to the number of clusters. Note these are not R colours. Use \code{distructColours()} or refer to DISTRUCT manual for colours.  
#' With multiple files, number of colours must equal input file with highest number of clusters. 
#' @param grayscale A logical indicating if clusters must be shown in grayscale.
#' @param printcolorbrewer A logical indicating if the colours provided in \code{popcol} are ColorBrewer colours. See details.
#' @param sepline A logical indicating if divider lines must be drawn between populations (T).
#' @param seplinewidth A numeric indicating width of sepline.
#' @param borderlinewidth A numeric indicating width of border around the plot.
#' @param indlinewidth A numeric indicating width of border around individual bars and ticks.
#' @param fontsize A numeric indicating font size of population labels.
#' @param topdist A numeric indicating distance of top labels from the top edge of the plot.
#' @param bottomdist A numeric indicating distance of bottom labels from the bottom edge of the plot. Usually a negative number.
#' @param figheight A numeric indicating height of the plot.
#' @param indwidth A numeric indicating width of each individual bar. The width of the plot depends on this value.
#' @param orientation An integer (0,1,2,3) indicating orientation of the plot. See details.
#' @param xorigin A numeric indicating lower left x-coordinate of the plot. See details.
#' @param yorigin A numeric indicating lower left y-coordinate of the plot. See details.
#' @param xscale A numeric indicating scaling for the x direction.
#' @param yscale A numeric indicating scaling for the y direction.
#' @param toplabangle A numeric between 0 and 180 indicating angle of top labels. 
#' @param bottomlabangle A numeric between 0 and 180 indicating angle of bottom labels. 
#' @param echodata A logical. Not really sure what this does.
#' @param printdata A logical indicating if head and tail of data must be shown in display on running DISTRUCT.
#' @param quiet A logical TRUE or FALSE. Set to TRUE by default to print verbose statements to screen.
#' @param useexe A logical indicating if DISTRUCT executable is automatically run based on system OS (experimental).
#' @return The function does not return anything. The function creates directories for each input file and 
#' populates it with files necessary to run DISTRUCT. The files are individual q-matrix file (xx-indq.txt),
#' population q-matrix file (xx-popq.txt), a cluster colour file (xx-colours.txt) and drawparams file. If 
#' population labels were defined, then (xx-poplab-bottom.txt) or (xx-poplab-top.txt) are also exported. The
#' DISTRUCT executable is run in this directory to generate an xx.ps file.
#' @details
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
#' If plot exceeds canvas size, consider shifting \code{xorigin} to the left and/or decreasing \code{indwidth}.\cr
#' \cr
#' \strong{Colorbrewer colours} \cr
#' Colorbrewer colours are not automatically generated for now. Refer to DISTRUCT manual for colour names. \cr
#' Replace the colour names in xx-colours.txt output file with selected colorbrewer colours (ex: Accent_3_qual_1). \cr
#' \cr
#' \strong{useexe}\cr
#' This option automatically runs the DISTRUCT executable that is provided with this package. 
#' Note that the executable is 64bit LSB requiring a 64bit system. The DISTRUCT executable was obtained from 
#' here \url{https://web.stanford.edu/group/rosenberglab/distruct.html}. Remember to cite DISTRUCT if this option is used.\cr
#  @import xlsx
#' @examples 
#' \dontrun{
#' #read some data
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' pops <- read.delim(system.file("files/structurepoplabels.txt",package="pophelper"),header=FALSE)
#' 
#' #plot without labels
#' distructExport(sfiles)
#' 
#' #plot with bottom label
#' distructExport(files,poplabbottom=pops$V1)
#' 
#' #plot with top label
#' distructExport(files,poplabtop=pops$V1)
#' 
#' #plot population mean values
#' distructExport(files,poplabbottom=pops$V1,popmean=T)
#' }
#' @import tidyr
#' @export
#' 
distructExport <- function(files=NULL, poplabbottom=NA, poplabtop=NA, popmean=FALSE, overwritedirs=FALSE, 
                           printtitle=FALSE, popcol=NA, grayscale=FALSE, printcolorbrewer=FALSE,
                           sepline=T, seplinewidth=0.2, borderlinewidth=1.2, indlinewidth=0.2,
                           fontsize=6, topdist=5, bottomdist=-7,figheight=36,indwidth=1,
                           orientation=0, xorigin=NA, yorigin=NA, xscale=1, yscale=1, toplabangle=60, bottomlabangle=60,
                           echodata=TRUE, printdata=FALSE, quiet=FALSE, useexe=FALSE)
{
  #if no files chosen, stop excecution, give error message
  if (is.null(files) || (length(files) == 0)) stop("distructExport: No input files.")
  poplabbottom <- as.character(poplabbottom)
  poplabtop <- as.character(poplabtop)
  if((length(poplabbottom) > 1) && any(is.na(poplabbottom))) stop("distructExport: Missing values (NA) in poplabbottom.")
  if((length(poplabtop) > 1) && any(is.na(poplabtop))) stop("distructExport: Missing values (NA) in poplabtop.")
  popcol <- as.character(popcol)
  if((length(popcol) > 1) && any(is.na(popcol))) stop("distructExport: Missing values (NA) in popcol.")
  if(all(!c(0,1,2,3) %in% orientation)) stop("distructExport: Argument 'orientation' must be a numeric value of 0, 1, 2 or 3.")
  if(!is.logical(popmean)) stop("distructExport: Argument 'popmean' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(overwritedirs)) stop("distructExport: Argument 'overwritedirs' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(printtitle)) stop("distructExport: Argument 'printtitle' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(sepline)) stop("distructExport: Argument 'sepline' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(grayscale)) stop("distructExport: Argument 'grayscale' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(printcolorbrewer)) stop("distructExport: Argument 'printcolorbrewer' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(echodata)) stop("distructExport: Argument 'echodata' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(printdata)) stop("distructExport: Argument 'printdata' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(quiet)) stop("distructExport: Argument 'quiet' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(useexe)) stop("distructExport: Argument 'useexe' set incorrectly. Set as TRUE or FALSE.")

  #get filenames from selection
  filenames <- basename(files)
  filenames <- sub(".txt","",filenames)
  filenames <- sub(".csv","",filenames)
  filenames <- sub(".meanQ","",filenames)
  
  #number of files selected
  number <- length(filenames)
  if (!quiet) cat(paste0("Number of files selected: ", number, "\n"))
  
  currwd <- getwd()
  if(as.numeric(file.access(currwd,2)) == -1) stop(paste0("distructExport: Directory ",currwd," has no write permission."))
  
  for(i in 1:number)
  {
    if(!quiet) cat(paste0("Computing Distruct files for ",filenames[i],"\n"))

    #check file
    chk <- pophelper:::checkRuns(files[i])$type
    #read files
    if (chk == "STRUCTURE") df <- round(pophelper::runsToDfStructure(files = files[i]),3)
    if (chk == "TESS") df <- round(pophelper::runsToDfTess(files = files[i]),3)
    if (chk == "MATRIX") df <- round(pophelper::runsToDfMatrix(files = files[i]),3)
    if (chk == "TAB") dft <- read.table(files[i],header=F, sep="", dec=".", quote="",stringsAsFactors=FALSE)
    if (chk == "UNIDENTIFIED") {message("distructExport: Incorrect input file type."); next;}
    
    if(chk == "TAB")
    {
      dft[ ,1] <- factor(dft[ ,1])
      tab_n <- as.numeric(length(levels(dft[ ,1])))
      tab_runs <- as.numeric(nrow(dft)/tab_n)
      tab_k <- ncol(dft)-2
      dft$run <- rep(1:tab_runs, each=tab_n)
    }else{
      tab_runs <- 1
    }
    
    for(j in 1:tab_runs)
    {
      if(chk == "TAB")
      {
        df <- subset(dft,run==j)
        df <- df[,2:(ncol(df)-2)]
        colnames(df) <- paste0("Cluster",c(1:ncol(df)))
        dirname <- paste0(filenames[i],"-",j,"-distruct")
        if(!overwritedirs) {if(exists(paste0(currwd,"/",dirname))) stop(paste0("Directory ",dirname," already exists."))}
      }else{
        dirname <- paste0(filenames[i],"-distruct")
        if(!overwritedirs) {if(exists(paste0(currwd,"/",dirname))) stop(paste0("Directory ",dirname," already exists."))}
      }
      
      dop_k <- ncol(df)
      dop_n <- nrow(df)
      
      #top labels & bottom labels
      if((!any(is.na(poplabbottom))) && (!any(is.na(poplabtop))))
      {
        poplabbottom <- as.character(poplabbottom)
        poplabtop <- as.character(poplabtop)
        
        if(dop_n != length(poplabbottom)) {message(paste0("distructExport: Number of rows of data (",dop_n,") not equal to length of bottom population labels (",length(poplabbottom),").")); next;}
        if(dop_n != length(poplabtop)) {message(paste0("distructExport: Number of rows of data (",dop_n,") not equal to length of top population labels (",length(poplabtop),").")); next;}
        if(length(poplabbottom) != length(poplabtop)) {message(paste0("distructExport: Length of top population labels (",length(poplabtop),") not equal to length of bottom population labels (",length(poplabbottom),").")); next;}
        
        dop_bottomlabel <- TRUE
        dop_toplabel <- TRUE
        
        rlevalb <- rle(poplabbottom)
        facb <- factor(rep(1:length(rlevalb$values),rlevalb$lengths))
        rlevalt <- rle(poplabtop)
        fact <- factor(rep(1:length(rlevalt$values),rlevalt$lengths))
        
        if(length(levels(facb)) != length(levels(fact))) {message("distructExport: Number of levels of top labels are not equal to the number of levels of bottom labels."); next;}
        
        #indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 pop=sprintf("%06.0f",as.numeric(as.character(facb))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=F),df)
        
        #popq
        dfb <- df
        dfb$pop <- facb
        #dfb2 <- plyr::ddply(dfb1,.(pop,variable),mean=mean(value),len=length(value),summarise)
        dfb1 <- tidyr::gather(dfb,"variable","value",-pop)
        dfb2 <- cbind(aggregate(value~pop+variable, data=dfb1, FUN=mean),aggregate(value~pop+variable, data=dfb1, FUN=length)[,3,drop=F])
        colnames(dfb2) <- c("pop","variable","mean","len")
        dfb3 <- tidyr::spread(dfb2,"variable","mean")
        #dfb3 <- reshape::cast(data=dfb2, pop+len~variable, value="mean")
        rm(dfb1,dfb2)
        dfb3$length <- dfb3$len
        dfb3 <- dfb3[, !(colnames(dfb3) %in% c("pop","len"))]
        pnames <- sprintf("%06.0f",1:nrow(dfb3))
        popq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=F),dfb3)
        
        #bottomlabels
        plb <- data.frame(sample=pnames,label=rlevalb$values,stringsAsFactors=F)
        dop_m <- nrow(plb)
        
        #popq
        dft <- df
        dft$pop <- facb
        dft1 <- tidyr::gather(dft,"variable","value",-pop)
        dft2 <- cbind(aggregate(value~pop+variable, data=dft1, FUN=mean),aggregate(value~pop+variable, data=dft1, FUN=length)[,3,drop=F])
        colnames(dft2) <- c("pop","variable","mean","len")
        dft3 <- tidyr::spread(dft2,"variable","mean")
        rm(dft1,dft2)
        dft3$length <- dft3$len
        dft3 <- dft3[, !(colnames(dft3) %in% c("pop","len"))]
        pnamest <- sprintf("%06.0f",1:nrow(dft3))
        #popq <- cbind(data.frame(sample=paste0(pnamesb,":"),stringsAsFactors=F),dft3)
        
        #tolabels
        plt <- data.frame(sample=pnamest,label=rlevalt$values,stringsAsFactors=F)
        #dop_m <- nrow(plb)
        fname_plb <- paste0(filenames[i],"-poplab-bottom.txt")
        fname_plt <- paste0(filenames[i],"-poplab-top.txt")
      }
      
      #bottom labels
      if((!any(is.na(poplabbottom))) && (any(is.na(poplabtop))))
      {
        poplabbottom <- as.character(poplabbottom)
        if(dop_n != length(poplabbottom)) {message(paste0("distructExport: Number of rows of data (",dop_n,") not equal to length of bottom population labels (",length(poplabbottom),").")); next;}
        
        dop_bottomlabel <- TRUE
        dop_toplabel <- FALSE
        rleval <- rle(poplabbottom)
        fac <- factor(rep(1:length(rleval$values),rleval$lengths))
        
        #indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 pop=sprintf("%06.0f",as.numeric(as.character(fac))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=F),df)
        
        #popq
        df$pop <- fac
        df1 <- tidyr::gather(df,"variable","value",-pop)
        df2 <- cbind(aggregate(value~pop+variable, data=df1, FUN=mean),aggregate(value~pop+variable, data=df1, FUN=length)[,3,drop=F])
        colnames(df2) <- c("pop","variable","mean","len")
        df3 <- tidyr::spread(df2,"variable","mean")
        rm(df1,df2)
        df3$length <- df3$len
        df3 <- df3[, !(colnames(df3) %in% c("pop","len"))]
        pnames <- sprintf("%06.0f",1:nrow(df3))
        popq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=F),df3)
        
        #bottomlabels
        plb <- data.frame(sample=pnames,label=rleval$values,stringsAsFactors=F)
        dop_m <- nrow(plb)
        
        fname_plb <- paste0(filenames[i],"-poplab-bottom.txt")
        fname_plt <- "null"
      }
      
      #top labels
      if((!any(is.na(poplabtop))) && (any(is.na(poplabbottom))))
      {
        poplabtop<- as.character(poplabtop)
        if(dop_n != length(poplabtop)) {message(paste0("distructExport: Number of rows of data (",dop_n,") not equal to length of bottom population labels (",length(poplabtop),").")); next;}
        
        dop_toplabel <- TRUE
        dop_bottomlabel <- FALSE
        
        rleval <- rle(poplabtop)
        fac <- factor(rep(1:length(rleval$values),rleval$lengths))
        
        #indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 pop=sprintf("%06.0f",as.numeric(as.character(fac))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=F),df)
        
        #popq
        df$pop <- fac
        df1 <- tidyr::gather(df,"variable","value",-pop)
        df2 <- cbind(aggregate(value~pop+variable, data=df1, FUN=mean),aggregate(value~pop+variable, data=df1, FUN=length)[,3,drop=F])
        colnames(df2) <- c("pop","variable","mean","len")
        df3 <- tidyr::spread(df2,"variable","mean")
        rm(df1,df2)
        df3$length <- df3$len
        df3 <- df3[, !(colnames(df3) %in% c("pop","len"))]
        pnames <- sprintf("%06.0f",1:nrow(df3))
        popq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=F),df3)
        
        #toplabels
        plt <- data.frame(sample=pnames,label=rleval$values,stringsAsFactors=F)
        dop_m <- nrow(plt)
        
        fname_plb <- "null"
        fname_plt <- paste0(filenames[i],"-poplab-top.txt")
      }
      
      #top and bottom labels absent
      if((any(is.na(poplabbottom))) && (any(is.na(poplabtop))))
      {
        
        #indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 pop=sprintf("%06.0f",as.numeric(as.character(rep(1,nrow(df))))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=F),df)
        
        #popq
        #df1 <- reshape::melt(df)
        #df2 <- plyr::ddply(df1,.(variable),mean=mean(value),len=length(value),summarise)
        #df3 <- reshape::cast(data=df2, len~variable, value="mean")
        
        df1 <- tidyr::gather(df,"variable","value")
        df2 <- cbind(aggregate(value~variable, data=df1, FUN=mean),aggregate(value~variable, data=df1, FUN=length)[,2,drop=F])
        colnames(df2) <- c("variable","mean","len")
        df3 <- tidyr::spread(df2,"variable","mean")
        
        rm(df1,df2)
        df3$length <- df3$len
        df3 <- df3[, !(colnames(df3) %in% c("len"))]
        pnames <- sprintf("%06.0f",1:nrow(df3))
        popq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=F),df3)
        
        dop_m <- 1
        dop_toplabel <- FALSE
        dop_bottomlabel <- FALSE
        fname_plb <- "null"
        fname_plt <- "null"
      }
      
      #colours
      if(any(is.na(popcol))){
        colsdf <- data.frame(sample=1:dop_k,cols=pophelper:::distructColours()[1:dop_k],stringsAsFactors=F)
      }else{
        if(length(popcol) != length(pnames)) stop(paste0("distructExport: Length of colours (",length(popcol),") not equal to length of populations (",length(pnames),"). Change number of colours in 'popcol' or set 'popcol=NA'."))
        #if(any(!popcol %in% pophelper:::distructColours())) stop(paste0("distructExport: One or more colours provided (",popcol[which(!popcol %in% pophelper:::distructColours())],") is not a standard Distruct colour."))
        colsdf <- data.frame(sample=pnames,cols=popcol,stringsAsFactors=F)
      }
      
      #grayscale
      if(grayscale) 
      {
        fngray <- function(n) {
          x <- round(seq(0,1,by=1/n),nchar(n))
          x[round(seq(1,length(x),length.out=n))]
        }
        colsdf <- data.frame(sample=1:dop_k,cols=fngray(dop_k),stringsAsFactors=F)
      }
      
      #xorigin
      if(is.na(xorigin))
      {
        if (orientation == 0) xorigin <- 72
        else if (orientation == 1) xorigin <- 360
        else if (orientation == 2) xorigin <- 540
        else if (orientation == 3) xorigin <- 288
      }
      
      #yorigin
      if(is.na(yorigin))
      {
        if (orientation == 0) yorigin <- 288
        else if (orientation == 1) yorigin <- 72
        else if (orientation == 2) yorigin <- 504
        else if (orientation == 3) yorigin <- 720
      }
      
      #output file names
      fname_popq <- paste0(filenames[i],"-popq.txt")
      fname_indq <- paste0(filenames[i],"-indq.txt")
      fname_col <- paste0(filenames[i],"-colours.txt")
      fname_out <- paste0(filenames[i],".ps")
      
      #params
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
        paste0("#define INFILE_POPQ ",fname_popq),
        paste0("#define INFILE_INDIVQ ",fname_indq),
        paste0("#define INFILE_LABEL_BELOW ",fname_plb),
        paste0("#define INFILE_LABEL_ATOP ",fname_plt),
        paste0("#define INFILE_CLUST_PERM ",fname_col),
        paste0("#define OUTFILE ",fname_out),
        "",
        "// number of clusters",
        paste0("#define K	",dop_k," "),
        "// number of pre-defined populations",
        paste0("#define NUMPOPS ",dop_m," "),
        "// number of individuals",
        paste0("#define NUMINDS ",dop_n," "),
        "",
        "Main usage options",
        "",
        "// (B) 1 if indiv q\'s are to be printed, 0 if only population q\'s",
        paste0("#define PRINT_INDIVS ",as.numeric(!popmean)," "),
        "// (B) print labels above figure",
        paste0("#define PRINT_LABEL_ATOP ",as.numeric(dop_toplabel)," "),
        "// (B) print labels below figure",
        paste0("#define PRINT_LABEL_BELOW ",as.numeric(dop_bottomlabel)," "),
        "// (B) print lines to separate populations",
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
        "// (d) width of 'pen' for separators between pops and for tics",
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
        paste0("-p ",fname_popq),
        paste0("-i ",fname_indq),
        paste0("-a ",fname_plt),
        paste0("-b ",fname_plb),
        paste0("-c ",fname_col),
        paste0("-o ",fname_out))
      
      dir.create(dirname)
      if(!quiet) cat("-------------------------------\n")
      if(!quiet) cat(paste0("Directory ",dirname," created.\n"))
      setwd(paste0(currwd,"/",dirname))
      if(as.numeric(file.access(paste0(currwd,"/",dirname),2)) == -1) stop(paste0("distructExport: Directory ",paste0(currwd,"/",dirname)," has no write permission."))

      write.table(popq,fname_popq,row.names=F,col.names=F,quote=F,dec=".",sep="\t")
      if(!quiet) cat(paste0(fname_popq," exported.\n"))
      write.table(indq,fname_indq,row.names=F,col.names=F,quote=F,dec=".",sep="\t")
      if(!quiet) cat(paste0(fname_indq," exported.\n"))
      if(!any(is.na(poplabbottom))) 
      {
        write.table(plb,fname_plb,row.names=F,col.names=F,quote=F,dec=".",sep="\t")
        if(!quiet) cat(paste0(fname_plb," exported.\n"))
      }
      if(!any(is.na(poplabtop))) 
      {
        write.table(plt,fname_plt,row.names=F,col.names=F,quote=F,dec=".",sep="\t")
        if(!quiet) cat(paste0(fname_plt," exported.\n"))
      } 
      write.table(colsdf,fname_col,row.names=F,col.names=F,quote=F,dec=".",sep="\t")
      if(!quiet) cat(paste0(fname_col," exported.\n"))
      writeLines(params,"drawparams")
      if(!quiet) cat(paste0("drawparams exported.\n"))
      
      if(useexe)
      {
        #sysos <- tolower(as.character(Sys.info()['sysname']))
        sysos <- pophelper:::getOS()
        if(sysos == "windows")
        {
          file.copy(system.file("bin/distruct_windows_1.1.exe",package="pophelper"),".")
          system("distruct_windows_1.1.exe")
          unlink("distruct_windows_1.1.exe",force=TRUE)
        }
        
        if(sysos == "mac")
        {
          file.copy(system.file("bin/distruct_macosx_1.1_2013",package="pophelper"),".")
          system("chmod 777 distruct_macosx_1.1_2013")
          system("./distruct_macosx_1.1_2013")
          unlink("distruct_macosx_1.1_2013",force=TRUE)
        }
        
        if(sysos == "unix64")
        {
          file.copy(system.file("bin/distruct_linux_1.1_64bit",package="pophelper"),".")
          system("chmod 777 distruct_linux_1.1_64bit")
          system("./distruct_linux_1.1_64bit")
          unlink("distruct_linux_1.1_64bit",force=TRUE)
        }
        if(sysos == "unix32") warning("distructExport: DISTRUCT executable not run because system was identified as 32 bit while the executable is 64 bit.")
        if(sysos == "unknown") warning("distructExport: DISTRUCT executable not run because system cannot be identified as windows, mac or linux.")
      }
      
      setwd(currwd)
    }
  }
  if(!quiet) cat("===============================\n")
}

#-------------------------------------------------------------------------------

#FUNCTION distructColours
#' Internal: Vector of 90 Distruct colours
#' @description Internal: Vector of 90 Distruct colours.
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

#-------------------------------------------------------------------------------

#FUNCTION getOS
#' Internal: Find current OS
#' @description Find current OS
#' @return Returns a character in lowercase with OS name windows, mac or linux.
#' 
getOS <- function() {
  if (tolower(.Platform$OS.type) == "windows") { 
    return("windows")
  } else if (tolower(Sys.info()["sysname"]) == "darwin") {
    return("mac")
  } else if (tolower(.Platform$OS.type) == "unix") { 
    if(grepl("x86_64",as.character(sessionInfo()["platform"]))) {return("unix64")}else{return("unix32")}
  } else {
    return("unknown")
  }
}

#-------------------------------------------------------------------------------

#ON LOAD
.onLoad <- function(...) {
    packageStartupMessage("pophelper v1.2.1 ready.")
}

#-------------------------------------------------------------------------------

#FURTHER WORK
# Interactive barplot
# Multiple lines of labels in plotRuns()
