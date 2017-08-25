# Begin ------------------------------------------------------------------------

# pophelper v2.2.2
# Functions
# 25-Aug-2017

#check packages
pkgs <- c("Cairo","grid","gridExtra","ggplot2","gtable","tidyr")
if(any(!pkgs %in% installed.packages()))
{
  warning(paste0("Package(s) '",paste0(pkgs[which(!pkgs %in% installed.packages())],collapse=", "),"' is not installed."))
}
rm(pkgs)

#compiler::enableJIT(3)

# getColours -------------------------------------------------------------------

#' @title Internal: Generate colours based on number of K
#' @description Internal: Generate colours based on number of K.
#' @param k A numeric indicating the number of colours required
#' @return Returns a character vector of k colours in hexadecimal format
#' @details Colours 1 to 12 are custom unique colours. Colours beyond 15 are generated from colour ramp \code{rich.colors()} from package \code{gplots}.
# @export
#' 
getColors <- getColours <- function(k)
{
  if(length(k) > 1) stop("getColours: Input has than one value. Argument k must be a single numeric or integer.")
  if(!is.integer(k) && !is.numeric(k) ) stop("getColours: Input is not an integer. Argument k must be a single numeric or integer.")
  k <- as.integer(k)
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

# checkQ --------------------------------------------------------------------

#' @title Internal: Check input filetype.
#' @description Internal: Check input filetype.
#' @param files A character or character vector of one or more input text files or a list of dataframes.
#' @param warn A logical indicating if a warning must be displayed for file those are not STRUCTURE, TESS or BASIC file.
#' @return A character or character vector indicating input type 'STRUCTURE', 'TESS', 'BASIC', 'CLUMPP', 'list', 'data.frame', 'UNIDENTIFIED' for all selected files.
# @export
#' 
checkQ <- function(files=NULL,warn=FALSE)
{
  if(is.null(files)) stop("checkQ: Input is empty.")
  if(class(files) != "list" && class(files) != "character") stop("checkQ: Input is not a character or list datatype.")
  
  len1 <- length(files)
  
  checkvec <- rep("UNIDENTIFIED",length=len1)
  subtype <- rep(NA,length=len1)
  for(i in 1:len1)
  {
    chk <- FALSE
    
    if(class(files)=="list")
    {
      if(class(files[[i]])=="data.frame") 
      {
        chk <- TRUE
        checkvec[i] <- "data.frame"
      }
    }
    
    if(!chk)
    {
      
      read1 <- readLines(files[i],n=7,warn=FALSE)
      
      #read TESS file
      chk <- grepl("ESTIMATED CLUSTERING PROBABILITIES",toupper(read1)[1])
      if(chk)
      {
        checkvec[i] <- "TESS"
      }
      
      #read STRUCTURE file
      if(!chk)
      {
        chk <- grepl("STRUCTURE BY PRITCHARD",toupper(read1)[4])
        if(chk)
        {
          checkvec[i] <- "STRUCTURE"
        }
      }
      rm(read1)
      
      #read BASIC files
      if(!chk)
      {
        seps <- c("","\t",",")
        subtypes <- c("SPACE","TAB","COMMA")
        k=1
        while(!chk)
        {
          if(class(try(suppressWarnings(read.table(files[i],header=FALSE,sep=seps[k],nrows=1,quote="",stringsAsFactors=FALSE))))!="try-error")
          {
            df <- read.table(files[i],header=FALSE,sep=seps[k],nrows=1,quote="",stringsAsFactors=FALSE)
            if(all(sapply(df,is.numeric))) {
              checkvec[i] <- "BASIC"
              subtype[i] <- subtypes[k]
              chk <- TRUE
            }else{
              if((ncol(df) > 2) && (is.character(df[,1])))
              {
                checkvec[i] <- "CLUMPP"
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
    }
    if((!chk) && warn) warning(paste0("checkQ: ",files[i]," is not a STRUCTURE, TESS, BASIC or CLUMPP file."))
  }
  return(list(type=checkvec,subtype=subtype))
}

# is.qlist --------------------------------------------------------------------

#' @title Verify if a qlist is formatted correctly.
#' @description Verify if a qlist is formatted correctly.
#' @param qlist A qlist object.
#' @return Nothing.
#' @export
#' 
is.qlist <- function(qlist=NULL)
{
  if(is.null(qlist)) stop("is.qlist: Input is empty.")
  
  #is it a list
  if(!is.list(qlist)) stop("is.qlist: Input is not a list object.")
  
  #does it have dataframes
  if(!all(sapply(qlist,is.data.frame))) stop("is.qlist: One or more list elements are not data.frame datatype.")

  #any NA
  if(any(is.na(qlist))) stop("is.qlist: One or more list elements are NA.")
    
  #are dataframe lists named
  if(is.null(names(qlist))) stop("is.qlist: List elements are not named.")
  
  #are dataframe lists named
  if(any(is.na(names(qlist)))) stop("is.qlist: One or more list element name is NA.")
  
  #are all dataframe lists named
  if(any(nchar(names(qlist))==0)) stop("is.qlist: One or more list elements are not named.")

  #duplicated list names
  if(any(duplicated(names(qlist)))) stop("is.qlist: One or more list element names are duplicated.")

  #are dataframes all numeric
  if(!all(unlist(lapply(qlist,sapply,is.numeric)))) stop("is.qlist: One or more qlist dataframes have non-numeric columns.")

  #duplicated rownames
  if(any(unlist(lapply(lapply(qlist,rownames),duplicated)))) stop("is.qlist: One or more qlist dataframes have duplicated row names.")
  
  #duplicated column names
  if(any(unlist(lapply(lapply(qlist,colnames),duplicated)))) stop("is.qlist: One or more qlist dataframes have duplicated column names.")
  
  #do column names have format Cluster
  if(!all(grepl("Cluster[0-9]+$",unlist(lapply(qlist,colnames))))) warning("is.qlist: One or more qlist dataframes have column names that do not conform to format 'ClusterNumber' like 'Cluster1', 'Cluster12' etc.")
}

# verifyGrplab --------------------------------------------------------------------

#' @title Verify if a grplab dataframe is formatted correctly.
#' @description Verify if a grplab dataframe is formatted correctly.
#' @param grplab A dataframe with character fields.
#' @return Nothing.
#' @export
#' 
verifyGrplab <- function(grplab=NULL)
{
  if(is.null(grplab)) stop("verifyGrplab: Argument 'grplab' is empty.")
  
  #is it a dataframe?
  if(!is.data.frame(grplab)) stop("verifyGrplab: Argument 'grplab' is not a data.frame object.")

  #NAs in labels?
  if(any(sapply(grplab,is.na))) stop("verifyGrplab: Argument 'grplab' contains NAs.")

  #are all elements character datatype?
  if(!any(sapply(grplab,is.character))) stop("verifyGrplab: Argument 'grplab' contains one or more fields which are not character datatype.")
}

# unitConverter ----------------------------------------------------------------

#' @title Internal: Convert value between dimension units
#' @description Internal: Convert value between dimension units
#' @param value A numeric value or numeric vector to convert
#' @param fromunit A character indicating the current unit of the value. Options are "cm", "mm", "in" or "px".
#' @param tounit A character indicating the unit to change to. Options are "cm", "mm", "in" or "px".
#' @param dpi A numeric indicating the resolution for pixel conversion. This should be in PPI (pixels per inch).
#' @return Returns a numeric value or numeric vector in changed units.
# @export
unitConverter <- function(value=NA,fromunit=NA,tounit=NA,dpi=NA)
{
  #check
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
  
  #check this part
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

# readQ ------------------------------------------------------------------------

#' @title Convert run files (q-matrices) to qlist.
#' @description Takes one or more STRUCTURE, TESS, numeric delimited run files or 
#' CLUMPP format files and converts them to a qlist (list of dataframes).
#' @param files A character or character vector of one or more files.
#' @param filetype A character indicating input filetype. Options are 'auto','structure','tess',
#' 'basic' or 'clumpp'. See details.
#' @param indlabfromfile A logical indicating if individual labels must be read 
#' from input file and used as row names for resulting dataframe. Spaces in 
#' labels may be replaced with _. Currently only applicable to STRUCTURE runs files.
#' @return A list of lists with dataframes is returned. List items are named by input filenames.
#' File extensions such as '.txt','.csv','.tsv' and '.meanQ' are removed from filename.
#' In case filenames are missing or not available, lists are named sample1, sample2 etc.
#' For STRUCTURE runs, if individual labels are present in the run file and \code{indlabfromfile=T}, 
#' they are added to the dataframe as row names. Structure metadata including loci, 
#' burnin, reps, elpd, mvll, and vll is added as attributes to each dataframe.
#' For CLUMPP files, multiple runs within one file are suffixed by -1, -2 etc.
#' @details
#' STRUCTURE and TESS run files have unique layout and format (See vignette). BASIC files can be Admixture run files, 
#' fastStructure meanQ files or any tab-delimited, space-delimited or comma-delimited tabular data 
#' without a header. CLUMPP files can be COMBINED, ALIGNED or 
#' MERGED files. COMBINED files are generated from \code{clumppExport}. ALIGNED and 
#' MERGED files are generated by CLUMPP.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @examples 
#' 
#' # STRUCTURE files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' # create a qlist of all runs
#' slist <- readQ(sfiles)
#' slist <- readQ(sfiles,filetype="structure")
#' 
#' # use ind names from file
#' readQ(sfiles[1],indlabfromfile=T)
#' 
#' # access the first run
#' readQ(sfiles)[[1]]
#' 
#' # access names of runs
#' names(slist)
#' 
#' # TESS files
#' tfiles <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' # create a qlist
#' readQ(tfiles)
#' 
#' # BASIC files
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' # create a qlist
#' readQ(afiles)
#' 
#' # CLUMPP files
#' tabs1 <- system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper")
#' tabs2 <- system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper")
#' tabs3 <- system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper")
#' 
#' # create a qlist
#' readQ(tabs1)
#' readQ(tabs2)
#' readQ(tabs3)
#' 
#' # manually create qlist
#' df1 <- data.frame(Cluster1=c(0.2,0.4,0.6,0.2),Cluster2=c(0.8,0.6,0.4,0.8))
#' df2 <- data.frame(Cluster1=c(0.3,0.1,0.5,0.6),Cluster2=c(0.7,0.9,0.5,0.4))
#' 
#' # one-element qlist
#' q1 <- list("sample1"=df1)
#' str(q1)
#' 
#' # two-element qlist
#' q2 <- list("sample1"=df1,"sample2"=df2)
#' str(q2)
#' 
#' @export
#' 
readQ <- function(files=NULL,filetype="auto",indlabfromfile=FALSE)
{
  if(is.null(files) | (length(files)==0)) stop("readQ: No input files.")
  if(!is.character(files)) stop("readQ: Argument 'files' is not a character datatype.")
  flen <- length(files)
  
  dlist <- vector("list")
  len1 <- length(files)
  for (i in 1:len1)
  {
    #check file
    if(filetype=="auto") 
    {
      chk <- tolower(pophelper:::checkQ(files[i])$type)
      
      if(chk=="structure") df1 <- pophelper:::readQStructure(files[i],indlabfromfile=indlabfromfile)
      if(chk=="tess") df1 <- pophelper:::readQTess(files[i])
      if(chk=="basic") df1 <- pophelper:::readQBasic(files[i])
      if(chk=="clumpp") df1 <- pophelper:::readQClumpp(files[i])
      if(chk %in% c("structure","tess","basic","clumpp")) 
      {
        dlist <- append(dlist,df1)
      }else{
        warning(paste0("readQ: Input file ",files[i]," was not identified as a STRUCTURE, TESS, BASIC or CLUMPP filetype. Specify 'filetype' manually or check input."))
      }
    }else{
      if(filetype=="structure") df1 <- pophelper:::readQStructure(files[i],indlabfromfile=indlabfromfile)
      if(filetype=="tess") df1 <- pophelper:::readQTess(files[i])
      if(filetype=="basic") df1 <- pophelper:::readQBasic(files[i])
      if(filetype=="clumpp") df1 <- pophelper:::readQClumpp(files[i])
      dlist <- append(dlist,df1)
    }
  }
  return(dlist)
}

# readQStructure ------------------------------------------------------------

#' @title Convert STRUCTURE run files to qlist.
#' @description Takes one or more STRUCTURE run files and converts them to a list of dataframes.
#' @param files A character or character vector of one or more STRUCTURE run files. Use \code{choose.files(multi=TRUE)} 
#' to select interactively.
#' @param indlabfromfile A logical indicating if individual labels must be read from input file and used as row names for resulting dataframe. Spaces in labels may be replaced with _.
#' @return A list of lists with dataframes is returned. If individual labels are 
#' present in the STRUCTURE file, they are added to the dataframe as row names. Structure
#' metadata including loci, burnin, reps, elpd, mvll, and vll is added as attributes 
#' to each dataframe. List items are named by input filenames.
#' @examples 
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' #create a qlist of all runs
#' slist <- readQStructure(sfiles)
#' 
#' #use ind names from file
#' readQStructure(sfiles[1],indlabfromfile=T)
#' 
#' #access the first run
#' readQStructure(sfiles)[[1]]
#' 
#' #access names of runs
#' names(slist)
#' #@export
#' 
readQStructure <- function(files=NULL,indlabfromfile=FALSE)
{
  if(is.null(files) | (length(files)==0)) stop("readQStructure: No input files.")
  #number of files selected
  flen <- length(files)
  
  #check file
  if(any(pophelper:::checkQ(files)$type != "STRUCTURE")) warning("readQStructure: Input may contain incorrect input format.")
  
  i <- 1
  dlist <- vector("list",length=flen)
  len1 <- length(files)
  for (i in 1:len1)
  {
    fname <- basename(files[i]) 
    file1 <- readLines(as.character(files[i]),warn=FALSE)
    
    #find individuals and get number of individuals
    ind <- as.numeric(as.character(base::gsub("\\D","",grep("\\d individuals",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1])))
    if(is.na(ind)) cat(paste0("Number of individuals is NA in file: ",fname,"\n"))
    
    #get value of k & error check
    k <- as.numeric(as.character(base::gsub("\\D","",grep("\\d populations assumed",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1])))
    if(is.na(k)) cat(paste0("Value of K is NA in file: ",fname,"\n"))
    
    #get number of loci & error check
    loci <- as.numeric(base::gsub("\\D","",grep("\\d loci",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1]))
    if(is.na(loci)) cat(paste0("Number of Loci is NA in file: ",files[i],"\n"))
    
    #get burn-in value & error check
    burnin <- as.numeric(base::gsub("\\D","",grep("\\d Burn-in period",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1]))
    if(is.na(burnin)) cat(paste0("Burn-in value is NA in file: ",files[i],"\n"))
    
    #get burn-in value & error check
    reps <- as.numeric(base::gsub("\\D","",grep("\\d Reps",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1]))
    if(is.na(reps)) cat(paste0("Reps value is NA in file: ",files[i],"\n"))
    
    #get est ln prob of data & error check
    elpd <- as.numeric(base::gsub("=","",base::gsub("Estimated Ln Prob of Data","",grep("Estimated Ln Prob of Data",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1])))
    if(is.na(elpd)) cat(paste0("Estimated Ln Prob of Data is NA in file: ",files[i],"\n"))
    
    #get mn value of ln likelihood & error check
    mvll <- as.numeric(base::gsub("=","",base::gsub("Mean value of ln likelihood","",grep("Mean value of ln likelihood",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1])))
    if(is.na(mvll)) cat(paste0("Mean value of ln likelihood is NA in file: ",files[i],"\n"))
    
    #get Variance of ln likelihood else NA
    vll <- as.numeric(base::gsub("=","",base::gsub("Variance of ln likelihood","",grep("Variance of ln likelihood",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1])))
    if(is.na(vll)) cat(paste0("Variance of ln likelihood is NA in file: ",files[i],"\n"))
    
    file1 <- file1[grep(".+\\(\\d+\\).+\\:.+",file1)]
    if(length(file1)==0)
    {
      cstart <- base::charmatch("Inferred ancestry of individuals",file1)
      cend <- base::charmatch("Estimated Allele Frequencies in each",file1)
      file1 <- file1[(cstart+2):(cend-1)]
    }
    
    file_a <- file1[file1 != ""]
    rm(file1)
    
    #error check
    file_b <- read.delim(textConnection(file_a),header=F,sep="",stringsAsFactors=F)
    suppressWarnings(
      errorcheck <- try(
        file_b[,as.integer(grep(":",file_b[1,])+1):as.integer(max(grep("^[0-9]|[.]+$",file_b[1,]))),drop=F],
        silent=T)
    )
    rm(file_b)
    
    if(class(errorcheck)=="try-error")
    {
      #using manual substring
      file_a <- base::gsub("\\([0-9.,]+\\)","",file_a)
      file_b <- base::gsub(":  ","",substr(file_a,base::regexpr(":\\W+\\d\\.\\d+",file_a),base::nchar(file_a)-1))
      file_b <- base::sub("\\s+$","",base::sub("^\\s+","",file_b))
      rm(file_a)
      file_c <- as.vector(as.numeric(as.character(unlist(base::strsplit(file_b," ")))))
      rm(file_b)
      dframe <- as.data.frame(matrix(file_c,nrow=ind,byrow=TRUE),stringsAsFactors=FALSE)
    }else{
      #using textconnection
      file_b <- read.delim(textConnection(file_a),header=F,sep="",stringsAsFactors=F)
      dframe <- file_b[,as.integer(grep(":",file_b[1,])+1):as.integer(max(grep("^[0-9]|[.]+$",file_b[1,]))),drop=F]
    }
    
    dframe <- as.data.frame(sapply(dframe,as.numeric),stringsAsFactors=FALSE)
    colnames(dframe) <- paste0("Cluster",1:ncol(dframe))
    row.names(dframe) <- 1:nrow(dframe)
    #row.names(dframe) <- sprintf(paste0("%",paste0(rep(0,nchar(nrow(dframe))),collapse=""),nchar(nrow(dframe)),"d"),1:nrow(dframe))
    
    #labels
    if(indlabfromfile)
    {
      labeldf <- file_b[,(grep("[0-9]",file_b[1,])[1]+1):(grep("[(]",file_b[1,])[1]-1),drop=FALSE]
      
      if(ncol(labeldf) > 1) labeldf <- data.frame(V2=do.call(paste,c(labeldf,sep="_")),stringsAsFactors=FALSE)
      if(nrow(labeldf)==nrow(dframe))
      {
        if(any(duplicated(labeldf[,1]))) 
        {
          warning(paste0("readQStructure: Individual names in file ",fname," not used due to presence of duplicate names."))
        }else{
          row.names(dframe) <- as.character(labeldf[,1])
        }
      }else{
        warning(paste0("readQStructure: Individual names in file ",fname," not used due to incorrect length."))
      }
    }
    
    attr(dframe,"ind") <- nrow(dframe)
    attr(dframe,"k") <- ncol(dframe)
    attr(dframe,"loci") <- loci
    attr(dframe,"burnin") <- burnin
    attr(dframe,"reps") <- reps
    attr(dframe,"elpd") <- elpd
    attr(dframe,"mvll") <- mvll
    attr(dframe,"vll") <- vll
    
    dlist[[i]] <- dframe
    #names(dlist[[i]]) <- as.character(name)
  }
  
  fnames <- sub(".txt","",basename(files))
  names(dlist) <- fnames
  return(dlist)
}

# readQTess -----------------------------------------------------------------

#' @title Convert TESS cluster files to qlist.
#' @description Takes one or more TESS cluster run files and converts them to a 
#' list of dataframes.
#' @param files A character or character vector of one or more TESS cluster run files. Use \code{choose.files(multi=TRUE)} 
#' to select interactively.
#' @return A list of lists with dataframes is returned. List items are named by input filename.
#' @details Use collectRunsTess() to collect TESS runs into one directory.
#' @examples 
#' tfiles <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' #create a qlist
#' readQTess(tfiles)
#' #@export
#'
readQTess <- function(files=NULL)
{
  if(is.null(files) | (length(files)==0)) stop("readQTess: No input files.")
  #number of files selected
  flen <- length(files)
  
  #check file
  if(any(pophelper:::checkQ(files)$type != "TESS")) warning("readQTess: Input may contain incorrect input format.")
  
  i <- 1
  dlist <- vector("list",length=flen)
  len1 <- length(files)
  for (i in 1:len1)
  {
    name <- base::gsub(".txt","",basename(files[i]))
    
    file1 <- readLines(files[i],warn=FALSE)
    
    #extract the cluster table part
    file1 <- file1[3:c(grep("Estimated Allele Frequencies",file1)-1)]
    file1 <- file1[file1 != ""]
    file2 <- as.vector(unlist(strsplit(file1,"\t")))
    file3 <- as.data.frame(matrix(file2,nrow=length(file1),byrow=TRUE),stringsAsFactors=FALSE)
    rm(file1,file2)
    dframe <- file3[,-c(1,ncol(file3)-1,ncol(file3))]
    rm(file3)
    dframe <- as.data.frame(sapply(dframe,as.numeric),stringsAsFactors=FALSE)
    colnames(dframe) <- paste0("Cluster",1:ncol(dframe))
    
    attr(dframe,"ind") <- nrow(dframe)
    attr(dframe,"k") <- ncol(dframe)
    
    dlist[[i]] <- dframe
  }
  
  fnames <- sub(".txt","",basename(files))
  names(dlist) <- fnames
  return(dlist)
}

# readQBasic ---------------------------------------------------------------

#' @title Convert delimited text files to qlist.
#' @description Takes one or more delimited numeric text files and converts each of 
#' them to separate dataframes.
#' @param files A character or character vector of one or more delimited text files. Use \code{choose.files(multi=TRUE)} 
#' to select interactively.
#' @return A list of lists with dataframes is returned. List items are named by input filename.
#' @details Input files can be Admixture run files, fastStructure meanQ files. 
#' or any tab-delimited, space-delimited or comma-delimited tabular data without header.
#' @examples 
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' #create a qlist
#' readQBasic(afiles)
#' #@export
#'
readQBasic <- function(files=NULL)
{
  if(is.null(files) | (length(files)==0)) stop("readQBasic: No input files.")
  #number of files selected
  flen <- length(files)
  
  #check file
  chk <- pophelper:::checkQ(files)
  if(any(chk$type != "BASIC")) stop("readQBasic: Incorrect input format.")
  if(any(is.na(chk$subtype))) stop("readQBasic: Incorrect input format.")
  
  i <- 1
  dlist <- vector("list",length=flen)
  len1 <- length(files)
  for (i in 1:len1)
  {
    # Test space-delim, tab-delim or comma-delim files here
    if(chk$subtype[i]=="SPACE") dframe <- read.delim(files[i],header=F,sep="",dec=".",stringsAsFactors=FALSE)
    if(chk$subtype[i]=="TAB") dframe <- read.delim(files[i],header=F,sep="\t",dec=".",stringsAsFactors=FALSE)
    if(chk$subtype[i]=="COMMA") dframe <- read.delim(files[i],header=F,sep=",",dec=".",stringsAsFactors=FALSE)
    
    if(!all(sapply(dframe,is.numeric))) stop("readQBasic: One or more columns are not numeric.")
    colnames(dframe) <- paste0("Cluster",1:ncol(dframe))
    
    attr(dframe,"ind") <- nrow(dframe)
    attr(dframe,"k") <- ncol(dframe)
    
    dlist[[i]] <- dframe
  }
  
  fnames <- sub(".txt","",basename(files))
  fnames <- sub(".tsv","",basename(files))
  fnames <- sub(".csv","",basename(files))
  fnames <- sub(".meanQ","",basename(files))
  
  names(dlist) <- fnames
  return(dlist)
}

# readQClumpp ---------------------------------------------------------------

#' @title Convert CLUMPP format numeric text files to qlist.
#' @description Takes one or more CLUMPP format numeric text files and converts
#' them to a list of dataframes.
#' @param files A character or character vector of one or more COMBINED, ALIGNED or 
#' MERGED files. COMBINED files are generated from \code{clumppExport}. ALIGNED and 
#' MERGED files are generated by CLUMPP. Use \code{choose.files(multi=TRUE)} to 
#' select interactively.
#' @return A list of lists with dataframes is returned. Each list item is named by 
#' input filename. Multiple runs within one file are suffixed by -1, -2 etc.
#' @examples 
#' tabs1 <- system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper")
#' tabs2 <- system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper")
#' tabs3 <- system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper")
#' 
#' #create a qlist
#' readQClumpp(tabs1)
#' readQClumpp(tabs2)
#' readQClumpp(tabs3)
#' 
#' #@export
#'
readQClumpp <- function(files=NULL)
{
  if(is.null(files) | (length(files)==0)) stop("readQClumpp: No input files.")
  #number of files selected
  flen <- length(files)
  
  #check file
  chk <- pophelper:::checkQ(files)
  if(any(chk$type != "CLUMPP")) stop("readQClumpp: Incorrect input format.")
  
  i <- 1
  k <- 1
  dlist <- vector("list")
  snames <- vector()
  len1 <- length(files)
  for (i in 1:len1)
  {
    fname <- base::gsub(".txt","",basename(files[i]))
    
    df1 <- read.table(files[i],header=F,sep="",dec=".",quote="",stringsAsFactors=FALSE)
    if(class(df1)!="data.frame") stop("readQClumpp: Read error. Check input format.")
    
    df1[,1] <- factor(df1[ ,1])
    indlev <- levels(df1[,1])
    Ind <- as.numeric(as.character(length(indlev)))
    tempb <- as.numeric(nrow(df1))
    numruns <- as.numeric(tempb/Ind)
    numk <- ncol(df1) - 2
    
    df2 <- data.frame(Num=factor(rep(1:numruns,1,each=Ind)),
                      Ind=factor(rep(1:Ind,numruns)),
                      df1[,2:(numk+1)],stringsAsFactors=FALSE)
    colnames(df2)[3:ncol(df2)] <- paste0("Cluster",1:(ncol(df2)-2))
    
    for(j in 1:numruns)
    {
      dframe <- subset(df2,df2$Num==j)
      dframe$Num <- NULL
      dframe$Ind <- NULL
      snames <- c(snames,paste0(fname,"-",j))
      
      if(!all(sapply(dframe,is.numeric))) stop("readQClumpp: One or more columns are not numeric.")
      
      attr(dframe,"ind") <- nrow(dframe)
      attr(dframe,"k") <- ncol(dframe)
      
      dlist[[k]] <- dframe
      k <- k+1
    }
  }
  
  snames <- sub(".txt","",snames)
  snames <- sub(".tsv","",snames)
  snames <- sub(".csv","",snames)
  snames <- sub(".meanQ","",snames)
  
  names(dlist) <- snames
  return(dlist)
}

# tabulateQ --------------------------------------------------------------------

#' @title Tabulate runs from a qlist
#' @description Takes a qlist of one of more numeric dataframes and creates a table with filenames, K and number of individuals.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param writetable A logical indicating if the output table must be exported as a tab-delimited text file in the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @param sorttable A logical indicating if output table is to be sorted. Sorts table by ind and K.
#' @return Returns a dataframe with filenames (if list is not named, then sample1, sample2 etc. is used), K and number of individuals of all runs sorted by ind and K (if \code{sorttable=T}). 
#' The row numbers of the output table denotes the file number selected. This is helpful if a particular file from the table needs to 
#' be identified in the selection vector. If input files were derived from STRUCTURE runs, columns loci, burnin, reps, elpd, mvll and vll are also returned.
#' @details The input must be a list of dataframes. If one dataframe is used, then it must be inside a list. If the list items are named, then the item name is used as filename, else sample1, sample2 etc. is used.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @seealso \code{\link{summariseQ}}
#' @examples 
#' 
#' # STRUCTURE files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' slist <- readQ(sfiles)
#' tabulateQ(qlist=slist)
#' 
#' # TESS files
#' tfiles <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' tlist <- readQ(tfiles)
#' tabulateQ(qlist=tlist)
#' 
#' # ADMIXTURE files
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' alist <- readQ(afiles)
#' tabulateQ(qlist=alist)
#' 
#  @import xlsx
#' @export
#'
tabulateQ <- function(qlist=NULL,writetable=FALSE,sorttable=TRUE)
{
  
  #check input
  is.qlist(qlist)
  if(!is.logical(writetable)) stop("tabulateQ: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(sorttable)) stop("tabulateQ: Argument 'sorttable' not set correctly. Set as TRUE or FALSE.")
  
  #check data format
  #if(exportdataformat != "excel" && exportdataformat != "txt") stop("tabulateQ: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")

  #get filenames from selection
  filenames <- names(qlist)
  if(is.null(filenames)) filenames <- paste0("sample",1:length(qlist))
  #number of files selected
  flen <- length(filenames)
  
  #make dataframe container
  main <- data.frame(file=filenames,k=1:flen,ind=1:flen,stringsAsFactors=FALSE)
  
  #loop to make dataframe with filenames and other variables
  k <- vector(length=flen,mode="numeric")
  ind <- vector(length=flen,mode="numeric")
  loci <- vector(length=flen,mode="numeric")
  burnin <- vector(length=flen,mode="numeric")
  reps <- vector(length=flen,mode="numeric")
  elpd <- vector(length=flen,mode="numeric")
  mvll <- vector(length=flen,mode="numeric")
  vll <- vector(length=flen,mode="numeric")
  
  i <- 1
  for (i in i:flen)
  {
    #read file & error check
    df1 <- qlist[[i]]
    if(!is.data.frame(df1)) stop(paste0("tabulateQ: List item ",i," is not a data.frame object."))
    if(!any(sapply(df1,is.numeric))) stop(paste0("tabulateQ: List item ",i," has non-numeric columns."))
    
    #get k
    k[i] <- ncol(df1)
    #get ind
    ind[i] <- nrow(df1)
    #loci
    loci[i] <- ifelse(is.null(attr(df1,"loci")),NA,attr(df1,"loci"))
    #burnin
    burnin[i] <- ifelse(is.null(attr(df1,"burnin")),NA,attr(df1,"burnin"))
    #reps
    reps[i] <- ifelse(is.null(attr(df1,"reps")),NA,attr(df1,"reps"))
    #elpd
    elpd[i] <- ifelse(is.null(attr(df1,"elpd")),NA,attr(df1,"elpd"))
    #mvll
    mvll[i] <- ifelse(is.null(attr(df1,"mvll")),NA,attr(df1,"mvll"))
    #vll
    vll[i] <- ifelse(is.null(attr(df1,"vll")),NA,attr(df1,"vll"))
  }
  
  #create df
  main <- data.frame(file=filenames,k=k,ind=ind,stringsAsFactors=FALSE)
  if(all(!is.na(loci))) main$loci <- loci
  if(all(!is.na(burnin))) main$burnin <- burnin
  if(all(!is.na(reps))) main$reps <- reps
  if(all(!is.na(elpd))) main$elpd <- elpd
  if(all(!is.na(mvll))) main$mvll <- mvll
  if(all(!is.na(vll))) main$vll <- vll
  
  #sort table on K
  if(sorttable) main <- main[with(main,order(ind,k)),]
  
  #write table if opted
  if(writetable)
  {
    #if(exportdataformat=="txt")
    #{
    write.table(main,"tabulateQ.txt",quote=FALSE,row.names=FALSE,sep="\t",dec=".")
    cat("tabulateQ.txt exported.\n")
    #}
    #if(exportdataformat=="excel")
    #{
    #xlsx::write.xlsx2(main,file="tabulateQ.xlsx",sheetName="tabulateQ",row.names=FALSE)
    #cat("tabulateQ.xlsx exported.\n")
    #}
  }
  
  return(main)
}

# summariseQ ----------------------------------------------------------------

#' @title Summarise a tabulated dataframe
#' @description Creates a summary table from a tabulated dataframe of two or more runs with k, number of runs and individuals.
#' @param data A dataframe with tabulated runs. An output from \code{tabulateQ()}. Must have minimum 2 columns named k and ind.
#' @param writetable A logical indicating if the output table is to be exported as a tab-delimited text file in the working directory.
#  @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @return Returns a dataframe with all values of K sorted by K. The table has 
#' 3 columns namely value of K, number of runs for each K and number of individuals.
#' If the input file is derived from STRUCTURE runs, the table is sorted by loci as well.
#' Other columns include elpdmean, elpdsd, elpdmin and elpdmax.
#' @details See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' @aliases summarizeQ
#' @seealso \code{\link{tabulateQ}}
#  @import xlsx
#' @examples 
#' 
#' #STRUCTURE files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' slist <- readQ(sfiles)
#' tr1 <- tabulateQ(slist)
#' summariseQ(tr1)
#' 
#' #ADMIXTURE files
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' tr1 <- tabulateQ(readQ(afiles))
#' summariseQ(tr1)
#' 
#' @export
#' 
summariseQ <- summarizeQ <- function(data=NULL,writetable=FALSE)
{
  #does df data contain any data
  if(is.null(data) | length(data)==0) stop("summariseQ: No input files.")
  if(!is.logical(writetable)) stop("summariseQ: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  #check data format
  #if(exportdataformat != "excel" && exportdataformat != "txt") stop("summariseQ: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")
  
  #make sure dataframe
  if(class(data) != "data.frame") stop("summariseQ: Input is not a dataframe.")
  #convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  #is column k available
  if(length(grep("k",colnames(data)))==0) stop("summariseQ: Column k not available.")
  #is column ind available
  if(length(grep("ind",colnames(data)))==0) stop("summariseQ: Column ind not available.")
  
  #check
  if(nrow(data) < 2) stop("summariseQ: At least 2 runs are required for this function.")
  
  if(all(c("k","ind","loci","elpd") %in% colnames(data)))
  {
    dframe1 <- stats::aggregate(elpd ~ loci + ind + k,data=data,length)
    colnames(dframe1)[4] <- "runs"
    dframe2 <- aggregate(elpd ~ loci + ind + k,data=data,FUN=function(x) c(elpdmean =mean(x,na.rm=T),elpdsd=sd(x,na.rm=T),elpdmin=min(x,na.rm=T),elpdmax=max(x,na.rm=T)))[,-c(1:3)]
    dframe1 <- cbind(dframe1,dframe2)
  }else{
    dframe1 <- stats::aggregate(file ~ ind + k,data=data[,c("file","k","ind")],length)
    colnames(dframe1)[3] <- "runs"
  }
  
  #write table if opted
  if(writetable)
  {
    #if(exportdataformat=="txt")
    #{
    write.table(dframe1,"summariseQ.txt",quote=FALSE,row.names=FALSE,sep="\t",dec=".")
    cat("summariseQ.txt exported.\n")
    #}
    #if(exportdataformat=="excel")
    #{
    #xlsx::write.xlsx2(data1,file="summariseQ.xlsx",sheetName="summariseQ",row.names=FALSE)
    #cat("summariseQ.xlsx exported.\n")
    #}
  }
  
  return(dframe1)
}

# evannoMethodStructure --------------------------------------------------------

#' @title Perform the Evanno method for STRUCTURE runs.
#' @description The Evanno method for detecting the appropriate number of population clusters from STRUCTURE results. Creates table and figure with 
#' Evanno method derivatives. Refer to return for detailed list of columns. See details for Evanno method reference.
#' @param data A dataframe with summarised runs. An output from \code{summariseQ()} derived from STRUCTURE runs. Must have minimum 7 columns named elpdmean, elpdsd, k, runs, loci, elpdmax and elpdmin.
#' @param writetable A logical indicating if the output table is to be exported as a file in the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @param exportplot A logical indicating if the Evanno plots are to be exported as an image in the working directory. If Evanno method cannot be computed, a kPlot (elpd over k) is exported instead.
#' @param pointsize A numeric indicating size of points. Default for \code{basesize=6} is 1.8.
#' @param pointype A character or number for the type of points. Defaults to 20. Same as pch in standard R.
#' @param pointcol A colour character for the colour of points. Defaults to "steelblue".
#' @param linesize A numeric indicating the thickness of the line. Default for \code{basesize=6} is 0.24.
#' @param linecol A colour character for the colour of line. Defaults to "steelblue".
#' @param ebwidth A numeric indicating size od width of error abrs. Defaults to 0.2.
#' @param ebcol A colour character for colour for errorbar. Defaults to "grey30".
#' @param textcol A colour character for all text elements on the plot. Defaults to "grey30".
#' @param basesize A numeric indicating the base size of various plot elements such as pointsize, linesize etc. Increase basesize with larger figure dimensions. Defaults to 6. Manually specified arguments (eg: pointsize) override basesize.
#' @param gridsize A numeric indicating thickness of background grid. Default for \code{basesize=6} is 0.18.
#' @param imgtype A character indicating the type of exported image. Default set to 'png'. Other possible 
#' options are 'jpeg', 'tiff' or 'pdf'.
#' @param height A numeric denoting the height of exported image. Default units in 'cm'.
#' @param width A numeric denoting the width of exported image. Default units in 'cm'.
#' @param dpi A numeric denoting the resolution of exported image. Default set to 300. If \code{imgtype="pdf"}, dpi is fixed at 300.
#' @param units A character denoting the unit of measure of the export image. Default is 'cm'. Other options are 'px', 'in' or 'mm'. 
#' @param theme A character indicating ggplot theme to be used. Use like "theme_grey", "theme_bw" etc.
#' @param font A character indicating font family to be used in the plots. Uses default system fonts by default for jpeg, png and tiff. Uses 'Helvetica' as default for pdf. Use package \code{extrafonts} to import custom fonts. See vignette for examples.
#' @param na.rm Default set to FALSE. Does not remove NAs for plot and this 
#' generates warnings from \code{ggplot}. If set to TRUE, NAs are removed before 
#' plotting and warning messages from \code{ggplot} are avoided.
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
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @examples
#' 
#' \dontrun{
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' tr1 <- tabulateQ(readQ(sfiles))
#' sr1 <- summariseQ(tr1)
#' evannoMethodStructure(sr1)
#' evannoMethodStructure(data=sr1,exportplot=T)
#' }
#' 
#  @import xlsx
#' @import grid
#' @import gridExtra
#' @export
#' 
evannoMethodStructure <- function(data=NULL,writetable=FALSE,exportplot=FALSE,
                                  pointsize=NA,pointtype=20,pointcol="steelblue",linesize=NA,linecol="steelblue",
                                  ebwidth=0.2,ebcol="grey30",
                                  textcol="grey30",basesize=6,gridsize=NA,
                                  imgtype="png",height=NA,width=NA,dpi=300,units="cm",
                                  theme="theme_bw",font="",na.rm=TRUE)
{
  #does df data contain any data
  if(is.null(data) | length(data)==0) stop("evannoMethodStructure: No input files.")
  if(class(data) != "data.frame") stop("evannoMethodStructure: Input is not a dataframe datatype.")
  if(!is.logical(writetable)) stop("evannoMethodStructure: Argument 'writetable' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(exportplot)) stop("evannoMethodStructure: Argument 'exportplot' not set correctly. Set as TRUE or FALSE.")
  if(!is.logical(na.rm)) stop("evannoMethodStructure: Argument 'na.rm' not set correctly. Set as TRUE or FALSE.")
  imgtype <- tolower(imgtype)
  if(imgtype != "png" && imgtype != "pdf" && imgtype != "tiff" && imgtype != "jpeg") stop("evannoMethodStructure: Argument 'imgtype' set incorrectly. Options are 'png', 'jpeg', 'tiff' or 'pdf'.")

  #check data format
  #if(exportdataformat != "excel" && exportdataformat != "txt") stop("evannoMethodStructure: Argument 'exportdataformat' set incorrectly. Set as 'excel' or 'txt'.")
  
  #convert column names to lowercase
  colnames(data) <- tolower(colnames(data))
  cold <- colnames(data)
  
  #is column loci available
  if(!"loci" %in% cold) stop("evannoMethodStructure: Column loci not available.")
  #is column ind available
  if(!"ind" %in% cold) stop("evannoMethodStructure: Column ind not available.")
  #is column k available
  if(!"k" %in% cold) stop("evannoMethodStructure: Column k not available.")
  #is column runs available
  if(!"runs" %in% cold) stop("evannoMethodStructure: Column runs not available.")
  #is column elpdmean available
  if(!"elpdmean" %in% cold) stop("evannoMethodStructure: Column elpdmean not available.")
  #is column elpdsd available
  if(!"elpdsd" %in% cold) stop("evannoMethodStructure: Column elpdsd not available.")
  #is column minelpd available
  if(!"elpdmin" %in% cold) stop("evannoMethodStructure: Column elpdmin not available.")
  #is column maxelpd available
  if(!"elpdmax" %in% cold) stop("evannoMethodStructure: Column elpdmax not available.")
  
  
  err <- 0
  #atleast 3 values of K
  if(length(data$k) < 3) {warning("Error: The Evanno method not computed. Requires at least 3 values of K.\n"); err <- 1}
  #do loci vary
  if(!all(data$loci[1]==data$loci)) {warning("Error: The Evanno method not computed. Number of loci vary between runs.\n"); err <- 1}
  #do ind vary
  if(!all(data$ind[1]==data$ind)) {warning("Error: The Evanno method not computed. Number of individuals vary between runs.\n"); err <- 1}
  #are k values sequential
  is.sequential <- function(x) all(abs(diff(x))==1)
  if(!is.sequential(data$k)) {warning("Error: The Evanno method not computed. Requires increasing sequential values of K.\n"); err <- 1}
  #repeats of k<2
  if(any(data$runs < 2)) warning("evannoMethodStructure: Results may not be meaningful if repeats (runs) for any value of K is less than 2.")
  
  base_size <- basesize
  height1 <- height
  width1 <- width
  #if(is.na(units)) units <- "cm"
  #if(is.na(dpi)) dpi <- 300
  if(imgtype=="pdf") dpi <- 300
  if(imgtype=="pdf" && font=="") font <- "Helvetica"
  if(is.na(pointsize)) pointsize <- base_size*0.3
  if(is.na(linesize)) linesize <- base_size*0.04
  if(is.na(gridsize)) gridsize <- base_size*0.03
  #plotcol <- "grey30"
  #plotcol1 <- "steelblue"
  #pointsh <- 20
  #linewd <- 0.20
  #pointsz <- 1.5
  #linewd <- base_size*0.04
  #pointsz <- base_size*0.3
  
  if(err==1)
  {
    if(exportplot)
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
              plot.margin=grid::unit(c(0.15,0.15,0.15,0.15),"cm"))
      
      #show plot
      print(plist[[1]])
      
      #check image imgtype
      if(imgtype=="pdf") pdf(file="kPlot.pdf",height=height1,width=width1,fonts=font)
      if(imgtype=="png") png(filename="kPlot.png",height=height1,width=width1,res=dpi,units=units,type="cairo",family=font)
      if(imgtype=="jpeg") jpeg(filename="kPlot.jpg",height=height1,width=width1,res=dpi,units=units,quality=100,family=font)
      if(imgtype=="tiff") tiff(filename="kPlot.tiff",height=height1,width=width1,res=dpi,units=units,compression="lzw",type="cairo",family=font)
      
      print(plist[[1]])
      dev.off()
      
      if(imgtype=="tiff") cat("kPlot.tiff exported.\n")
      if(imgtype=="pdf") cat("kPlot.pdf exported.\n")
      if(imgtype=="png") cat("kPlot.png exported.\n")
      if(imgtype=="jpeg") cat("kPlot.jpg exported.\n")
    }
    stop("evannoMethodStructure: Evanno method not computed.")
  }
  
  #convert dataframe to list
  datal <- as.list(data)
  
  #Loop to get first derivative of l(K) and its sd
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
  
  #Loop to get second derivative of l(K) and its sd
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
  
  #add NA to SD vector 1 and 2
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
  
  #write table if opted
  if(writetable==TRUE | writetable=="T" | writetable=="TRUE")
  {
    #if(exportdataformat=="txt")
    #{
    write.table(data,"evannoMethodStructure.txt",quote=FALSE,row.names=FALSE,sep="\t",dec=".")
    cat("evannoMethodStructure.txt exported.\n")
    #}
    
    #if(exportdataformat=="excel")
    #{
    #xlsx::write.xlsx2(data,file="evannoMethodStructure.xlsx", sheetName="evannoMethodStructure", row.names=FALSE)
    #cat("evannoMethodStructure.xlsx exported.\n")
    #} 
  }
  
  #show plot
  if(exportplot)
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
    
    #plot1
    plist[[1]] <- ggplot2::ggplot(data,aes(x=k,y=elpdmean))+
      geom_path(colour=linecol,size=linesize,na.rm=na.rm)+
      geom_point(colour=pointcol,fill=pointcol,size=pointsize,shape=pointtype,na.rm=na.rm)+
      geom_errorbar(aes(x=k,ymax=elpdmax,ymin=elpdmin,width=ebwidth),size=linesize,colour=ebcol,na.rm=na.rm)+
      get(theme)(base_family=font)+
      labs(x=expression(paste(italic(K))),y=expression(paste("Mean L(",italic(K),") " %+-% " SD")),title="A")
    
    #plot 2
    plist[[2]] <- ggplot2::ggplot(data,aes(x=k,y=lnk1))+
      geom_path(colour=linecol,size=linesize,na.rm=na.rm)+
      geom_point(colour=pointcol,fill=pointcol,size=pointsize,shape=pointtype,na.rm=na.rm)+
      geom_errorbar(aes(x=k,ymax=lnk1max,ymin=lnk1min,width=ebwidth),
                    size=linesize,colour=ebcol,na.rm=na.rm)+
      get(theme)(base_family=font)+
      labs(x=expression(paste(italic(K))),y=expression(paste("L'(",italic(K),") " %+-% " SD")),title="B")
    
    #plot 3
    plist[[3]] <- ggplot2::ggplot(data,aes(x=k,y=lnk2))+
      geom_path(colour=linecol,size=linesize,na.rm=na.rm)+
      geom_point(colour=pointcol,fill=pointcol,size=pointsize,shape=pointtype,na.rm=na.rm)+
      geom_errorbar(aes(x=k,ymax=lnk2max,ymin=lnk2min,width=0.2),
                    size=linesize,colour=ebcol,na.rm=na.rm)+
      get(theme)(base_family=font)+
      labs(x=expression(paste(italic(K))),y=expression(paste("|L\"(",italic(K),")| " %+-% " SD")),title="C")
    
    #plot 4
    if(is.finite(sum(data$drv3,na.rm=TRUE)))
    {
      plist[[4]] <- ggplot2::ggplot(data,aes(x=k,y=deltaK))+
        geom_path(colour=linecol,size=linesize,na.rm=na.rm)+
        geom_point(colour=pointcol,fill=pointcol,size=pointsize,shape=pointtype,na.rm=na.rm)+
        get(theme)(base_family=font)+
        labs(x=expression(paste(italic(K))),y=expression(paste(Delta,italic(K))),title="D")
    }
    
    plen <- length(plist)
    for (r in 1:plen)
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
                                       plot.margin=grid::unit(c(0.1,0.1,0.1,0.1),"cm"))
    }
    
    #export image
    
    #check image imgtype  
    if(imgtype=="pdf") pdf("evannoMethodStructure.pdf",height=height1,width=width1,fonts=font)
    if(imgtype =="png") png("evannoMethodStructure.png",height=height1,width=width1,res=dpi,units=units,type="cairo",family=font)
    if(imgtype =="tiff") tiff("evannoMethodStructure.tiff",height=height1,width=width1,res=dpi,units=units,compression="lzw",type="cairo",family=font)
    if(imgtype=="jpeg") jpeg("evannoMethodStructure.jpg",height=height1,width=width1,res=dpi,units=units,quality=100,family=font)
    
    if(plen==3) gridExtra::grid.arrange(plist[[1]],plist[[2]],plist[[3]],ncol=2,nrow=2)
    if(plen==4) gridExtra::grid.arrange(plist[[1]],plist[[2]],plist[[3]],plist[[4]],ncol=2,nrow=2)
    dev.off()
    
    if(imgtype=="tiff") cat("evannoMethodStructure.tiff exported.\n")
    if(imgtype=="pdf") cat("evannoMethodStructure.pdf exported.\n")
    if(imgtype=="png") cat("evannoMethodStructure.png exported.\n")
    if(imgtype=="jpeg") cat("evannoMethodStructure.jpg exported.\n")
  }
  
  #return table
  return(data)
}


# clumppExport -----------------------------------------------------------------

#' @title Generate CLUMPP output from a qlist
#' @description Takes a qlist and combines several repeats for each K into a 
#' single file along with a parameter file suitable for input to CLUMPP. The two 
#' output files are organised into folders by K. CLUMPP is executed automatically 
#' when \code{useexe=T}, else the CLUMPP executable file can be copied to the 
#' output directories and run to reorder the clusters for each K.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param prefix A character prefix for folder names. By default, set to 'pop'.
#' @param parammode A numeric 1, 2 or 3 indicating the algorithm option for CLUMPP paramfile. Calculated 
#' automatically by default. Set this value to 3 if CLUMPP runs too long. See details.
#' @param paramrep A numeric indicating the number of repeats for CLUMPP paramfile. Calculated 
#' automatically by default. See details.
#' @param useexe A logical indicating if CLUMPP executable must be run automatically based on system OS (experimental). May not work on all OS and versions.
#' @return The combined file and paramfile are written into respective folders 
#' named by K.
#' @details When multiple repeats are run for each K in runs, the order of 
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
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' #generate input files for CLUMPP from STRUCTURE files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' clumppExport(readQ(sfiles))
#' 
#' #auto execute CLUMPP
#' clumppExport(readQ(sfiles),useexe=TRUE)
#' 
#' #generate input files for CLUMPP from ADMIXTURE files
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' clumppExport(readQ(afiles))
#' 
#' }
#' 
#' @export
#' 
clumppExport <- function(qlist=NULL,prefix=NA,parammode=NA,paramrep=NA,useexe=FALSE)
{
  #check input
  is.qlist(qlist)
  
  if(is.na(prefix)) prefix <- "pop"
  prefix <- paste0(prefix,"_K")
  if(!is.logical(useexe)) stop("clumppExport: Argument 'useexe' set incorrectly. Set as TRUE or FALSE.")
  
  #get tabulated runs
  df1 <- pophelper::tabulateQ(qlist)
  df2 <- pophelper::summariseQ(df1)
  df1l <- as.list(df1)
  df2l <- as.list(df2)
  
  if(is.null(names(qlist))) names(qlist) <- paste0("sample",1:length(qlist))
  
  #k val duplicated
  if(any(duplicated(df2l$k))) stop("clumppExport: Repeating values of K found.")
  #do ind vary
  if(!all(df2l$ind[1]==df2l$ind)) warning("clumppExport: Number of individuals vary between runs.")
  
  e <- 1
  p <- 1
  len1 <- length(df2l$k)
  while (e <= len1)
  {
    k <- df2l$k[e]
    ind <- df2l$ind[e]
    runs <- df2l$runs[e]
    
    ldata <- vector("list",length=runs)
    f <- 1
    for (f in 1:runs)
    {
      sel <- which(names(qlist)==as.character(df1l$file[p]))
      dframe1 <- qlist[[sel]]
      
      #generate df
      dframe3 <- as.matrix(data.frame(V1=paste0(1:ind,":"),dframe1,last=as.character(rep(1,ind)),stringsAsFactors=FALSE))
      
      #add dataframes to list
      ldata[[f]] <- dframe3
      rm(dframe3)
      p=p+1
    }
    
    if(runs > 1 & k > 1)
    {
      currwd <- getwd()
      if(as.numeric(file.access(currwd,2))==-1) stop(paste0("clumppExport: Directory ",currwd," has no write permission."))
      
      dir.create(paste0(currwd,"/",prefix,k))
      setwd(paste0(currwd,"/",prefix,k))
      cat(paste0("Folder created: ",basename(getwd()),"\n"))  
      out <- paste0(prefix,k,"-combined.txt")
      
      #File Output block
      
      #make 2 line space
      spacer <- matrix(rep("  ",(k+2)*2),nrow=2)
      
      #Write file
      write(t(format(ldata[[1]],nsmall=15)),paste(out),ncolumns=k+2)
      i=2
      for (i in 2:length(ldata))
      {
        write(t(spacer),paste(out),ncolumns=k+2,append=TRUE)
        write(t(format(ldata[[i]],nsmall=15)),append=TRUE,paste(out),ncolumns=k+2)
      }
      cat(paste0(out),"exported.\n")
      
      #PARAMFILE section
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
      
      if(useexe)
      {
        sysos <- pophelper:::getOS()
        if(sysos=="windows")
        {
          file.copy(system.file("bin/clumpp_windows_1.1.2b.exe",package="pophelper"),".")
          system("clumpp_windows_1.1.2b.exe")
          unlink("clumpp_windows_1.1.2b.exe",force=TRUE)
        }
        
        if(sysos=="mac")
        {
          file.copy(system.file("bin/clumpp_mac_1.1.2b",package="pophelper"),".")
          system("chmod 777 clumpp_mac_1.1.2b")
          system("./clumpp_mac_1.1.2b")
          unlink("clumpp_mac_1.1.2b",force=TRUE)
        }
        
        if(sysos=="unix32")
        {
          file.copy(system.file("bin/clumpp_linux_1.1.2b_32bit",package="pophelper"),".")
          system("chmod 777 clumpp_linux_1.1.2b_32bit")
          system("./clumpp_linux_1.1.2b_32bit")
          unlink("clumpp_linux_1.1.2b_32bit",force=TRUE)
        }
        
        if(sysos=="unix64")
        {
          file.copy(system.file("bin/clumpp_linux_1.1.2b_64bit",package="pophelper"),".")
          system("chmod 777 clumpp_linux_1.1.2b_64bit")
          system("./clumpp_linux_1.1.2b_64bit")
          unlink("clumpp_linux_1.1.2b_64bit",force=TRUE)
        }
        
        if(sysos=="unknown") warning("clumppExport: CLUMPP executable not run because system cannot be identified as windows, mac or linux.")
      }
      
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
#' @description Collect TESS cluster run files from multiple folders to one folder and rename each run by folder name
#' @param runsdir A character path indicating the directory containing TESS runs in multiple directories. Use \code{choose.dir()} for interactively selecting the directory. If NA, or no directory is selected, the current working directory is used.
#' @param newdir A character indicating the name of the new directory to be created with the collected runs. IF NA, the default name 'AllTESSRuns' is used. 
#' @param quiet A logical indicating if a message is to be displayed for directories without TESS runs and number of runs copied and renamed. 
#' @details Within each TESS run folder, the function searches for filename ending with 'TR.txt' as the cluster file. This file is copied to the new folder and renamed as the name of the respective run directory. Therefore, DO NOT manually rename original run files or directories.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @return Two integers are ruturned. The first denotes the number of TESS run files copied and renamed. The second number denotes number of directories without TESS run files.
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
  for (i in 1:len1)
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
#' @param prefix A character indicating the prefix of the CLUMPP directories before the underscore. For ex. if the directories are pop_K2, then prefix is pop.
#' @param filetype A character indicating the type of file to be copied. Options are 'aligned' to copy aligned files only, 'merged' to copy merged files only and 'both' to copy both files.
#' @param runsdir A character denoting the directory containing CLUMPP output files in multiple directories. Use \code{choose.dir()} for interactively selecting the directory. If NA, the current working directory is used.
#' @param newdir A character indicating the name of the new directory to be created with the collected runs. IF NA, the a directory name joining prefix and filetype is created. 
#' @param quiet A logical indicating if a message is to be displayed showing the number of folders processed and number of files processed. 
#' @details Within each CLUMPP output folder, the function searches for filenames containing combination of prefix and filetype. This file is copied to the new folder. Therefore, do not manually rename CLUMPP output files or output directories.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @return Two integers are ruturned. The first denotes the number of directories processed. The second number denotes the number files copied.
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
  
  #check imgoutput
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
  for (i in 1:len1)
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

# getDim -----------------------------------------------------------------------

#' @title Internal: Get dimensions for figures.
#' @description Internal: Generate height and width of figure based on number of individuals.
#' @param ind A numeric indicating the number of individuals.
#' @param units A character indicating the unit of dimension: "cm","mm","in".
#' @param height A numeric indicating the height of each plot.
#' @param width A numeric indicating the width of each plot.
#' @param dpi A numeric indicating the resolution of the figure.
#' @param imgtype A character denoting image format. "png", "jpeg" or "pdf".
#' @param grplabheight A numeric denoting the height of the label panel.
#' @param labs An integer denoting number of label groups.
#' @param plotnum A numeric indicating the number of plots in the figure.
#' @return a vector with height and width.
# @export
#'
getDim <- function(ind=NA,units=NA,height=NA,width=NA,dpi=NA,imgtype=NA,grplabheight=NA,labs=NA,plotnum=NA)
{
  if(is.na(units)) units <- "cm"
  if(is.na(units) && imgtype=="pdf") units <- "in"
  if(is.na(dpi)) dpi <- 300
  if(is.na(plotnum)) plotnum <- 1
  if(is.na(labs)) labs <- 1
  
  #height
  if(is.na(height))
  {
    if(plotnum==1) height <- 1.8
    if(plotnum > 1) height <- 1.2
    if(imgtype=="pdf") height <- pophelper:::unitConverter(value=height,fromunit="cm",tounit="in",dpi=dpi)
  }else{
    if(units=="mm" && imgtype != "pdf") height <- pophelper:::unitConverter(value=height,fromunit="mm",tounit="cm",dpi=dpi)
    if(units=="px" && imgtype != "pdf") height <- pophelper:::unitConverter(value=height,fromunit="px",tounit="cm",dpi=dpi)
    if(units=="in" && imgtype != "pdf") height <- pophelper:::unitConverter(value=height,fromunit="in",tounit="cm",dpi=dpi)
    if(units=="cm" && imgtype=="pdf") height <- pophelper:::unitConverter(value=height,fromunit="cm",tounit="in",dpi=dpi)
    if(units=="mm" && imgtype=="pdf") height <- pophelper:::unitConverter(value=height,fromunit="mm",tounit="in",dpi=dpi)
    if(units=="px" && imgtype=="pdf") height <- pophelper:::unitConverter(value=height,fromunit="px",tounit="in",dpi=dpi)
  }
  height <- height*plotnum
  
  #width
  if(is.na(width))
  {
    if(is.na(ind)) stop("getDim: Argument ind is empty.")
    width <- ind*0.020 
    if(width < 5) width <- 5
    if(width > 30) width <- 30
    if(imgtype=="pdf") width <- pophelper:::unitConverter(value=width,fromunit="cm",tounit="in",dpi=dpi)
  }else{
    if(units=="mm" && imgtype != "pdf") width <- pophelper:::unitConverter(value=width,fromunit="mm",tounit="cm",dpi=dpi)
    if(units=="px" && imgtype != "pdf") width <- pophelper:::unitConverter(value=width,fromunit="px",tounit="cm",dpi=dpi)
    if(units=="in" && imgtype != "pdf") width <- pophelper:::unitConverter(value=width,fromunit="in",tounit="cm",dpi=dpi)
    if(units=="cm" && imgtype=="pdf") width <- pophelper:::unitConverter(value=width,fromunit="cm",tounit="in",dpi=dpi)
    if(units=="mm" && imgtype=="pdf") width <- pophelper:::unitConverter(value=width,fromunit="mm",tounit="in",dpi=dpi)
    if(units=="px" && imgtype=="pdf") width <- pophelper:::unitConverter(value=width,fromunit="px",tounit="in",dpi=dpi)
  }
  
  #grplabheight
  if(is.na(grplabheight)) 
  {
    grplabheight <- 0.4
    grplabheight <- grplabheight * labs
    if(imgtype=="pdf") grplabheight <- pophelper:::unitConverter(value=grplabheight,fromunit="cm",tounit="in",dpi=dpi)
  }else{
    if(units=="mm" && imgtype != "pdf") grplabheight <- pophelper:::unitConverter(value=grplabheight,fromunit="mm",tounit="cm",dpi=dpi)
    if(units=="in" && imgtype != "pdf") grplabheight <- pophelper:::unitConverter(value=grplabheight,fromunit="in",tounit="cm",dpi=dpi)
    if(units=="px" && imgtype != "pdf") grplabheight <- pophelper:::unitConverter(value=grplabheight,fromunit="px",tounit="cm",dpi=dpi)
    if(units=="mm" && imgtype=="pdf") grplabheight <- pophelper:::unitConverter(value=grplabheight,fromunit="mm",tounit="in",dpi=dpi)
    if(units=="cm" && imgtype=="pdf") grplabheight <- pophelper:::unitConverter(value=grplabheight,fromunit="cm",tounit="in",dpi=dpi)
    if(units=="px" && imgtype=="pdf") grplabheight <- pophelper:::unitConverter(value=grplabheight,fromunit="px",tounit="in",dpi=dpi)
  }
  
  if(imgtype!="pdf") units1 <- "cm"
  if(imgtype=="pdf") units1 <- "in"
  
  lst <- list(height=round(height,2),width=round(width,2),grplabheight=round(grplabheight,2),units=units1)
  return(lst)
}

# getPlotParams ----------------------------------------------------------------

#' @title Internal: Generate parameters for plots with labels
#' @description Internal: Generates various parameters required for plotting with labels.
#' @param grplab A character vector of labels same length as number of individuals.
#' @param plotnum A numeric indicating the number of plots on the figure.
#' @param grplabsize A numeric indicating the size of the labels.
#' @param grplabangle A numeric indicating the angle/rotation of labels. 0 is horizontal while 90 is vertical.
#' @param grplabjust A numeric indicating the justification of labels. Defaults to 0.5 if grplabangle=0  or 1 if grplabangle between 20 and 135.
#' @param pointsize  A numeric indicating the size of points on label marker line.
#' @param linesize A numeric indicating the thickness of the label marker line.
#' @return A list with following plot parameters: grplab, plotnum, grplabsize, 
#' grplabangle, grplabjust, pointsize, linesize.
# @export
#' 
getPlotParams <- function(grplab=NA,plotnum=1,grplabsize=NA,grplabangle=NA,grplabjust=NA,pointsize=NA,linesize=NA)
{
  if(all(is.na(grplab))) stop("getPlotParams: Labels are empty.")
  
  #estimate ct based on number of labels/ind
  lp <- length(as.character(grplab))
  
  #calculate grplabangle, just and margins
  if(is.na(grplabangle)) grplabangle <- 0
  if(grplabangle==0)
  {
    if(is.na(grplabjust)) grplabjust <- 0.5
  }
  
  if(abs(grplabangle) > 20 & abs(grplabangle) < 135)
  {
    if(is.na(grplabjust)) grplabjust <- 1
    #bmar <- round(max(nchar(as.character(grplab)))/8, 2)+round(lp/900, 3)
    #if(all(is.na(fmar))) fmar <- c(0.2, 0.2, bmar, 0)
  }
  
  grplabsize1 <- grplabsize
  pointsize1 <- pointsize
  linesize1 <- linesize

  if(is.na(grplabsize)) grplabsize1 <- lp*0.00123
  if(is.na(pointsize)) pointsize1 <- lp*0.0015
  if(is.na(linesize)) linesize1 <- lp*0.0003
  
  if(is.na(grplabsize)) {if(grplabsize1 < 1.5) grplabsize1 <- 1.5}
  if(is.na(pointsize)) {if(pointsize1 < 1.2) pointsize1 <- 1.2}
  if(is.na(linesize)) {if(linesize1 < 0.3) linesize1 <- 0.3}
  
  if(is.na(grplabsize))  {if(grplabsize1 > 2.5) grplabsize1 <- 2.5}
  if(is.na(pointsize)) {if(pointsize1 > 3.2) pointsize1 <- 3.2}
  if(is.na(linesize)) {if(linesize1 > 0.6) linesize1 <- 0.6}
  
  dlist <- list(grplab=grplab,plotnum=plotnum,grplabsize=grplabsize1,grplabangle=grplabangle,grplabjust=grplabjust,
                pointsize=pointsize1,linesize=linesize1)
  return(dlist)
}

# grpLabels --------------------------------------------------------------------

#' @title Internal: Handles grp subset/order
#' @description Internal: Takes a q-matrix dataframe along with grp labels. Pop labels
#' can be reordered or subsetted. The function also creates labelpos and markerpos dfs.
#' @param df A q-matrix dataframe
#' @param grplab A dataframe with group labels
#' @param selgrp A character denoting selected group label set
#' @param ordergrp A logical indicating if individuals must be ordered by group labels
#' @param subsetgrp A character or character vector of grp name(s) to subset/reorder
#' @param grpmean A logical indicating if q-matrix must be converted from individual to group mean.
#' @param grplabpos A numeric indicating y-axis position of labels
#' @param linepos A numeric indicating y-axis position of label line
#' @return Returns a list with subsetted/reordered q-matrix, a character vector 
#' of original/subsetted/reordered grplab dataframe, grplabpos and linepos.
# @export
grpLabels <- function(dframe=NULL,grplab=NA,selgrp=NA,subsetgrp=NA,ordergrp=FALSE,grpmean=FALSE,grplabpos=NA,linepos=NA)
{
  if(is.null(dframe)) stop("grpLabels: Argument 'dframe' is empty.")
  if(!is.data.frame(dframe)) stop("grpLabels: Argument 'dframe' is not a data.frame datatype.")
  if(any(is.na(grplab))) stop("grpLabels: Argument 'grplab' contains NA.")
  if(!is.data.frame(grplab)) stop("grpLabels: Argument 'grplab' is not a data.frame datatype.")
  if(is.na(selgrp)) selgrp <- names(grplab)[1]
  if(!all(is.na(selgrp))) {if(!is.character(selgrp)) stop("grpLabels: Argument 'selgrp' is not a character datatype.")}
  if(!all(is.na(subsetgrp))) {if(!is.character(subsetgrp)) stop("grpLabels: Argument 'subsetgrp' is not a character datatype.")}
  if(is.na(grplabpos)) grplabpos <- 0.25
  if(is.na(linepos)) linepos <- 0.75
  
  #if(is.factor(grplab)) grplablev <- levels(grplab)
  #if(is.character(grplab)) grplablev <- factor(grplab,levels=c(rle(grplab)$values))
  gnames <- names(grplab)
  onames <- gnames[!gnames %in% selgrp]
  
  #order groups
  #orders dframe and all labels by selgrp
  if(ordergrp)
  {
    dfwork1 <- cbind(dframe,grplab)
    #dfwork1 <- dfwork1[order(dfwork1[,selgrp]),]
    dfwork1 <- dfwork1[do.call(order,dfwork1[,c(selgrp,onames),drop=FALSE]),,drop=FALSE]
    
    grplab1 <- dfwork1[,gnames,drop=FALSE]
    dfwork1[,gnames] <- NULL
  }else{
    dfwork1 <- dframe
    grplab1 <- grplab
  }
  
  #group mean by selgrp
  if(grpmean) dfwork1 <- as.data.frame(sapply(dfwork1,ave,unlist(grplab1[selgrp])),stringsAsFactors=F)
  
  #in case of subsetgrp
  if(!any(is.na(subsetgrp)))
  {
    if(any(duplicated(subsetgrp))) stop(paste0("grpLabels: Argument 'subsetgrp' contains duplicate values (",paste0(subsetgrp,collapse=', '),")."))
    subsetgrp <- as.character(subsetgrp)
    subsetlabs <- as.vector(unlist(grplab1[selgrp]))
    #rle grp
    rlegrp <- rle(subsetlabs)
    
    #checks
    if(any(duplicated(rlegrp$values))) stop("grpLabels: Duplicated contiguous block of labels in 'selgrp'. Change grplab, selgrp or use 'ordergrp=TRUE'.")
    if(!any(subsetgrp %in% rlegrp$values)) stop(paste0("grpLabels: Subset group label (",subsetgrp,") not present in group label set (",selgrp,") with following contiguous groups: (",paste0(rlegrp$values,collapse=", "),")."))
    
    #compute positions and labels of subset grps
    posvec=vector()
    for(k in 1:length(subsetgrp))
    {
      sgrp=subsetgrp[k]
      pos=which(subsetlabs==sgrp)
      posvec=c(posvec,pos)
    }
    
    if(length(intersect(colnames(dfwork1),colnames(grplab1)))!=0) stop(paste0("grpLabels: One or more header labels in the run file are duplicated in grplab header. Change labels to be unique. Following are the duplicate label(s): (",paste0(intersect(colnames(dfwork1),colnames(grplab1)),collapse=", "),")."))
    dfwork2 <- cbind(dfwork1,grplab1)
    dfwork2 <- dfwork2[posvec,,drop=FALSE]
    dfwork1 <- dfwork2
    dfwork1[,gnames] <- NULL
    grplab1 <- dfwork2[,gnames,drop=FALSE]
  }
  
  #create labelpos and markerpos
  markerlist <- vector("list",length=length(gnames))
  labellist <- vector("list",length=length(gnames))
  intlablist <- vector("list",length=length(gnames))
  for(k in 1:length(gnames))
  {
    rlegrp <- rle(unlist(grplab1[gnames[k]]))
    labelpos <- data.frame(label=rlegrp$values,freq=rlegrp$lengths,stringsAsFactors=F)
    markerpos <- data.frame(markerxpos=c(0,cumsum(labelpos$freq)),stringsAsFactors=F)
    labelpos$labxpos <- round((diff(markerpos$markerxpos)/2)+markerpos$markerxpos[1:length(markerpos$markerxpos)-1],1)
    labelpos$labypos <- rep(grplabpos,nrow(labelpos))
    markerpos$temp <- factor(rep(1,nrow(markerpos)))
    markerpos$markerypos <- rep(linepos,nrow(markerpos))
    
    markerpos$count <- gnames[k]
    markerlist[[k]] <- markerpos
    labelpos$count <- gnames[k]
    labellist[[k]] <- labelpos
  }
  
  markerposbind <- do.call("rbind",markerlist)
  markerposbind$count <- factor(markerposbind$count,levels=gnames)
  labelposbind <- do.call("rbind",labellist)
  labelposbind$count <- factor(labelposbind$count,levels=gnames)
  
  rownames(markerposbind) <- 1:nrow(markerposbind)
  rownames(labelposbind) <- 1:nrow(labelposbind)
  
  #adjust divider position
  markerposbind$divxpos <- markerposbind$markerxpos+0.5
  
  return(list(dframe=dfwork1,grplab=grplab1,labelpos=labelposbind,markerpos=markerposbind))
}

# sortInd ----------------------------------------------------------------------

#' @title Internal: Handles individual sorting
#' @description Internal: Handles individual sorting
#' @param dframe A q-matrix dataframe
#' @param grplab A dataframe with group labels
#' @param selgrp A single character denoting selected group label set. See details.
#' @param sortind A character indicating how individuals are sorted. Default is NA (Same order of individuals as in input file). Other options are 'all' (sorting by values of all clusters), by any one cluster (eg. 'Cluster1') or 'labels' (sorting by individual labels).
#' @param grplabpos A numeric indicating y-axis position of labels
#' @param linepos A numeric indicating y-axis position of label line
#' @return Returns a list with ordered q-matrix dataframe and ordered grplab.
#' @details When multiple group label sets are in use, \code{selgrp} defines which 
#' group label set is used for group ordering (\code{ordergrp}), subsetting (\code{subsetgrp}) 
#' and group mean (\code{grpmean}). \code{selgrp} is also used for plotting divider 
#' lines and and sorting (\code{sortind}). If \code{selgrp} is not specified, the 
#' first group label set is used by default.
# @export
sortInd <- function(dframe=NULL,grplab=NA,selgrp=NA,sortind=NA,grplabpos=NA,linepos=NA)
{
  if(is.null(dframe)) stop("sortInd: Argument 'dframe' is empty.")
  if(!all(is.na(grplab))) 
  {
    if(is.na(selgrp)) selgrp <- names(grplab)[1]
    if(!is.character(selgrp)) stop("sortInd: Argument 'selgrp' must be a character datatype.")
    if(length(selgrp)>1) stop("sortInd: Argument 'selgrp' must be a character datatype of length 1.")
    if(!any(selgrp %in% names(grplab))) stop(paste0("sortInd: Argument 'selgrp' contains (",selgrp,") which is not in the 'grplab' titles (",paste0(names(grpla),collapse=", "),")."))
  }
  if(all(!is.na(sortind)))
  {
    if(length(sortind) > 1) stop("sortInd: Argument 'sortind' must be of length 1. Use 'all','label' or a cluster name like 'Cluster1'.")
    #if(sortind != "all" && sortind != "label" && !grepl("Cluster[0-9+]",sortind)) stop("sortInd: Argument 'sortind' must be set to 'all', 'label' or a cluster like 'Cluster1'.")
  }
  
  #sorting without grplab
  if(any(is.na(grplab)))
  {
    if(!is.na(sortind))
    {
      if(sortind=="all")
      {
        maxval <- apply(dframe,1,max)
        matchval <- vector(length=nrow(dframe))
        for(j in 1:nrow(dframe)) matchval[j] <- match(maxval[j],dframe[j,])
        dftemp <- dframe
        dftemp$maxval <- maxval
        dftemp$matchval <- matchval
        
        dframe <- dframe[with(dftemp,order(matchval,-maxval)),,drop=FALSE]
      }
      
      if(sortind=="label") dframe[order(rownames(dframe)),,drop=FALSE]
      
      if(sortind!="all" && sortind!="label")
      {
        if(!(sortind %in% colnames(dframe))) stop(paste0("sortInd: 'sortind' value (",sortind,") not found in file header (",paste0(colnames(dframe),collapse=", "),")."))
        dframe <- dframe[order(dframe[[sortind]]),,drop=FALSE]
      }
    }
    labelposbind <- NA
    markerposbind <- NA
  }
  
  #sorting with grplab
  if(!all(is.na(grplab)))
  {
    gnames <- names(grplab)
    
    if(!is.na(sortind))
    {
      if(sortind=="all")
      {
        maxval <- apply(dframe,1,max)
        matchval <- vector(length=nrow(dframe))
        for(j in 1:nrow(dframe)) matchval[j] <- match(maxval[j],dframe[j,])
        dftemp <- dframe
        dftemp$maxval <- maxval
        dftemp$matchval <- matchval
        
        if(length(intersect(colnames(dftemp),colnames(grplab)))!=0) stop(paste0("sortInd: One or more header labels in the run file are duplicated in grplab header. Change labels to be unique. Following are the duplicate label(s): (",paste0(intersect(colnames(dftemp),colnames(grplab)),collapse=", "),")."))
        dftemp <- cbind(dftemp,grplab)
        
        rle1 <- rle(as.character(unlist(grplab[selgrp])))
        grplabnames <- rle1$values
        tovec <- cumsum(rle1$lengths)
        fromvec <- (tovec - rle1$lengths)+1
        dftemplist <- vector("list",length=length(grplabnames))
        for(k in 1:length(tovec))
        {
          dftemp1 <- dftemp[fromvec[k]:tovec[k],,drop=FALSE]
          dftemp1$grp <- NULL
          dftemplist[[k]] <- dftemp1[with(dftemp1,order(matchval,-maxval)),,drop=FALSE]
        }
        
        dframe <- do.call("rbind",dftemplist)
        grplab <- dframe[,gnames,drop=FALSE]
        dframe[,gnames] <- NULL
        dframe$maxval <- NULL
        dframe$matchval <- NULL
      }
      
      if(sortind=="label")
      {
        dftemp <- dframe
        if(length(intersect(colnames(dftemp),colnames(grplab)))!=0) stop(paste0("sortInd: One or more header labels in the run file are duplicated in grplab header. Change labels to be unique. Following are the duplicate label(s): (",paste0(intersect(colnames(dftemp),colnames(grplab)),collapse=", "),")."))
        dftemp <- cbind(dftemp,grplab)
        
        rle1 <- rle(as.character(unlist(grplab[selgrp])))
        grplabnames <- rle1$values
        tovec <- cumsum(rle1$lengths)
        fromvec <- (tovec - rle1$lengths)+1
        dftemplist <- vector("list",length=length(grplabnames))
        for(k in 1:length(tovec))
        {
          dftemp1 <- dftemp[fromvec[k]:tovec[k],,drop=FALSE]
          dftemp1$grp <- NULL
          dftemplist[[k]] <- dftemp1[order(rownames(dftemp1)),,drop=FALSE]
        }
        
        dframe <- do.call("rbind",dftemplist)
        grplab <- dframe[,gnames,drop=FALSE]
        dframe[,gnames] <- NULL
      }
      
      
      if(sortind!="all" && sortind!="label")
      {
        if(!(sortind %in% colnames(dframe))) stop(paste0("sortInd: 'sortind' value (",sortind,") not found in file header (",paste0(colnames(dframe),collapse=", "),")."))
        clnum <- which(sortind==colnames(dframe))
        dftemp <- dframe
        if(length(intersect(colnames(dftemp),colnames(grplab)))!=0) stop(paste0("sortInd: One or more header labels in the run file are duplicated in grplab header. Change labels to be unique. Following are the duplicate label(s): (",paste0(intersect(colnames(dftemp),colnames(grplab)),collapse=", "),")."))
        dftemp <- cbind(dftemp,grplab)
        
        rle1 <- rle(as.character(unlist(grplab[selgrp])))
        grplabnames <- rle1$values
        tovec <- cumsum(rle1$lengths)
        fromvec <- (tovec - rle1$lengths)+1
        dftemplist <- vector("list",length=length(grplabnames))
        for(k in 1:length(tovec))
        {
          dftemp1 <- dftemp[fromvec[k]:tovec[k],,drop=FALSE]
          dftemp1$grp <- NULL
          dftemplist[[k]] <- dftemp1[order(dftemp1[[sortind]]),,drop=FALSE]
        }
        
        dframe <- do.call("rbind",dftemplist)
        grplab <- dframe[,gnames,drop=FALSE]
        dframe[,gnames] <- NULL
      }
    }
    
    #create labelpos and markerpos
    gnames <- names(grplab)
    markerlist <- vector("list",length=length(gnames))
    labellist <- vector("list",length=length(gnames))
    intlablist <- vector("list",length=length(gnames))
    for(k in 1:length(gnames))
    {
      rlegrp <- rle(unlist(grplab[gnames[k]]))
      labelpos <- data.frame(label=rlegrp$values,freq=rlegrp$lengths,stringsAsFactors=F)
      markerpos <- data.frame(markerxpos=c(0,cumsum(labelpos$freq)),stringsAsFactors=F)
      labelpos$labxpos <- round((diff(markerpos$markerxpos)/2)+markerpos$markerxpos[1:length(markerpos$markerxpos)-1],1)
      labelpos$labypos <- rep(grplabpos,nrow(labelpos))
      markerpos$temp <- factor(rep(1,nrow(markerpos)))
      markerpos$markerypos <- rep(linepos,nrow(markerpos))
      
      markerpos$count <- gnames[k]
      markerlist[[k]] <- markerpos
      labelpos$count <- gnames[k]
      labellist[[k]] <- labelpos
    }
    
    markerposbind <- do.call("rbind",markerlist)
    markerposbind$count <- factor(markerposbind$count,levels=gnames)
    labelposbind <- do.call("rbind",labellist)
    labelposbind$count <- factor(labelposbind$count,levels=gnames)
    
    rownames(markerposbind) <- 1:nrow(markerposbind)
    rownames(labelposbind) <- 1:nrow(labelposbind)
    
    #adjust divider position
    markerposbind$divxpos <- markerposbind$markerxpos+0.5
  }
  

  
  return(list(dframe=dframe,grplab=grplab,labelpos=labelposbind,markerpos=markerposbind))
}

# plotQ ---------------------------------------------------------------------

#' @title Generate barplots from qlists.
#' @description Generate separate or joined barplots (group-level) from qlists.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param imgoutput A character with options: 'sep' or 'join'.If set to "sep", each run is plotted as separate image file. If set to "join", multiple runs are joined into a single image.
#' @param clustercol A vector of colours for clusters. If NA, colours are automatically generated. K=1 to K=12 are custom unique colours while K>12 are coloured by function \code{gplots::rich.colors()}.
#' @param sortind A character indicating how individuals are sorted. Default is NA (Same order of individuals as in input file). Other options are 'all' (sorting by values of all clusters), by any one cluster (eg. 'Cluster1') or 'labels' (sorting by individual labels). See details.
#' @param grplab A dataframe with one or more columns (group label sets), and rows equal to the number of individuals. See details.
#' @param selgrp A single character denoting a selected group label set. The selected label must be a group label title used in \code{grplab}. See details.
#' @param ordergrp A logical indicating if individuals must be grouped into contiguous blocks based on \code{grplab} starting with \code{selgrp}.
#' @param subsetgrp A character or character vector with group names to subset or reorder groups. Only applicable when \code{grplab} is in use. Default is NA. See details.
#' @param grpmean A logical indicating if q-matrix must be converted from individual values to group mean values. Applicable only when \code{grplab} is in use and mean is calculated over \code{selgrp}.
#' @param panelspacer A numeric indicating the spacing between barplot panels in cm. Defaults to 0.06cm.
#' @param showsp A logical indicating if strip panels on right side must be shown. Strip panel by default displays file name and K value. Defaults to TRUE.
#' @param sppos A character indicating position of strip panel. One of 'right' or 'left'. Defaults to 'right'.
#' @param splab A character or character vector denoting items displayed in the strip panels. Length must be equal to number of runs.
#' @param splabsize A numeric indicating the size of the strip panel label. Defaults to 4 points.
#' @param splabangle A numeric indicating angle/rotation of the strip panel label. Defaults to NULL. Automatically set to -90.
#' @param splabcol A character indicating the colour of the strip panel label. Defaults to "grey30".
#' @param splabface A character indicating the font face of strip panel label. One of 'plain', 'italic', 'bold' or 'bold.italic'. Defaults to 'plain'. Applicable only when \code{showsp=T}.
#' @param spbgcol A character denoting the background colour of the strip panel. Defaults to white.
#' @param showtitle A logical indicating if plot title must be shown on the top. Defaults to FALSE. If TRUE and \code{titlelab=NA}, file name is displayed by default.
#' @param titlelab A character or character vector for title text. Defaults to NA, and when \code{showtitle=TRUE} displays file name.
#' @param titlehjust A numeric denoting the horizontal justification of the title. Defaults to 0 (left).
#' @param titlevjust A numeric denoting the vertical justification of the title. Defaults to 0.5 (center).
#' @param titlesize A numeric indicating the size of the title text. Defaults to 5 points.
#' @param titlecol A colour character for title. Defaults to "grey30".
#' @param titleface A character indicating the font face of title label. One of 'plain', 'italic', 'bold' or 'bold.italic'. Defaults to 'plain'. Applicable only when \code{showtitle=T}.
#' @param titlespacer A numeric indicating the space below the title. Defaults to 1.2.
#' @param titleangle A numeric indicating the angle/rotation of the title. Defaults to 0.
#' @param showsubtitle A logical indicating if plot subtitle must be shown on the top. Defaults to FALSE. If TRUE and \code{subtitlelab=NA}, file name is displayed by default.
#' @param subtitlelab A character or character vector for subtitle text. Defaults to NA, and when \code{showsubtitle=TRUE} displays file name.
#' @param subtitlehjust A numeric denoting the horizontal justification of the subtitle. Defaults to 0 (left).
#' @param subtitlevjust A numeric denoting the vertical justification of the subtitle. Defaults to 0.5 (center).
#' @param subtitlesize A numeric indicating the size of the subtitle text. Defaults to 5 points.
#' @param subtitlecol A colour character for subtitle. Defaults to "grey30".
#' @param subtitleface A character indicating the font face of subtitle label. One of 'plain', 'italic', 'bold' or 'bold.italic'. Defaults to 'plain'. Applicable only when \code{showsubtitle=T}.
#' @param subtitlespacer A numeric indicating the space below the subtitle. Defaults to 1.2.
#' @param subtitleangle A numeric indicating the angle/rotation of the subtitle. Defaults to 0.
#' @param grplabspacer A numeric indicating the space between the plot panels and the group label area in cm. Defaults to 0cm. Applicable only when \code{grplab} are in use.
#' @param grplabheight A numeric indicating the height of the group label area in cm. Defaults to 0.4cm. Multiple group sets are multiplied by 0.4. Applicable only with \code{grplab}. See details.
#' @param grplabpos A numeric indicating the y position of the group labels. Applicable only with group labels. Defaults to 0.
#' @param grplabsize A numeric indicating the size of the group labels. Default range between 1.5 - 2.5 depending on number of individuals.
#' @param grplabangle A numeric indicating the angle/rotation of group labels. 0 is horizontal while 90 is vertical. Default is 0.
#' @param grplabjust A numeric indicating the justification of group labels. Defaults to 0.5 if grplabangle=0  or 1 if grplabangle between 20 and 135.
#' @param grplabcol A colour character for the colour of group labels. Defaults to "grey30".
#' @param grplabalpha A numeric between 0 and 1 denoting transparency of group labels. Defaults to 1.
#' @param showindlab A logical indicating if individual labels must be shown. See details.
#' @param useindlab A logical indicating if individual labels must be read from the rownames of qlist dataframes and used as individual labels. See details.
# @param indlabsingle A logical indicating if individual labels must be shown under each run panel (\code{indlabsingle=FALSE}) or a single bottom panel only (\code{indlabsingle=TRUE}. Applicable only when \code{imgoutput="join"} and \code{showindlab=TRUE}.
#' @param indlabsep A character used as separator when concatenating individual labels and group labels. Defaults to space \code{indlabsep=" "}.
#' @param indlabheight A numeric indicating height of the individual label panel height. Defaults to zero. If long ind labels are getting cut off, increase this value to 0.1, 0.2 etc.
#' @param indlabsize A numeric indicating the size of the individual labels.
#' @param indlabangle A numeric indicating the angle/rotation of individual labels. 0 is horizontal while 90 is vertical. Defaults to 90.
#' @param indlabvjust A numeric denoting vertical justification of the individual labels. Defaults to 0.5.
#' @param indlabhjust A numeric denoting the horizontal justification of the individual labels. Defaults to 1.
#' @param indlabcol A colour character for the colour of individual labels. Defaults to "grey30".
#' @param indlabspacer A numeric denoting space between the individual label and the plot area. Default set to 0.
#' @param pointsize A numeric indicating the size of points on label marker line. Default range between 1.2 - 3.2 depending on number of individuals.
#' @param pointcol A colour character for the colour of points on the label marker line. Defaults to "grey30".
#' @param pointbgcol A colour character for the background of marker point for certain point types.
#' @param pointtype A character or number for the type of points on the label marker line. Defaults to |. Same as pch in standard R.
#' @param pointalpha A numeric between 0 and 1 denoting transparency of the points. Defaults to 1.
#' @param linepos A numeric indicating the y position of the label marker line and the points. Applicable only with group labels. Defaults to 1.
#' @param linesize A numeric indicating the thickness of the label marker line. Default range between 0.3 and 0.6 depending on number of individuals.
#' @param linecol A colour character for the label marker line. Defaults to "grey30".
#' @param linetype A numeric indicating the type of line for marker line. Same as lty in standard R. Default value is 1.
#' @param linealpha A numeric between 0 and 1 denoting transparency of the marker line. Defaults to 1.
#' @param showdiv A logical indicating if divider lines between groups must be drawn. Applicable only when group labels are in use.
#' @param divgrp A character or character vector with one or more group label titles denoting which groups are used to draw divider lines. This must be a group label title used in \code{grplab}. If not provided, the value in \code{selgrp} is used by default.
#' @param divcol A character or hexadecimal colour denoting the colour of the divider line. Default is white.
#' @param divtype A numeric indicating the type of line for the divider line. Same as lty in standard R. Default value is '21'.
#' @param divsize A numeric indicating the thickness of the divider line. Default is 0.25.
#' @param divalpha A numeric between 0 and 1 denoting transparency of the divider line. Defaults to 1.
#' @param showlegend A logical indicating if legend denoting cluster colours must be plotted. Defaults to FALSE.
#' @param legendlab A character or character vector to for legend cluster labels. Must be equal to max number of clusters.
#' @param legendpos A character 'right' or 'left' denoting position of the legend. Defaults to 'left'.
#' @param legendkeysize A numeric indicating size of the legend key. Defaults to 4.
#' @param legendtextsize A numeric indicating size of the legend text. Defaults to 3.
#' @param barsize A numeric indicating the width of the bars. Defaults to 1.
#' @param barbordersize A numeric indicating border size of bars. Defaults to 0. Visible only when \code{barbordercolour} is not NA.
#' @param barbordercolour A single colour for bar border. Defaults to NA. Visible only when \code{barbordersize} is larger than zero and set to a colour other than NA.
# @param showyaxis A logical indicating if y-axis labels should be displayed or not. Defaults to FALSE. Y-axis size is same as \code{indlabsize}.
# @param showticks A logical indicating if ticks on axis should be displayed or not. Defaults to FALSE. Visible only when \code{showyaxis=TRUE}. Tick colour is same as \code{indlabcol}.
#' @param ticksize A numeric indicating size of ticks. Defaults to 0.2.
#' @param outputfilename A character or character vector denoting output file name without file extension. See details.
#' @param imgtype A character indicating output image file type. Possible options are "png","jpeg","tiff" or "pdf".
#' @param height A numeric indicating the height of a single run panel. By default, automatically generated based on number of runs. Separate plots use 1.8cm and joined plots use 1.2cm for single panel. See details.
#' @param width A numeric indicating the width of the whole plot. By default, automatically generated based on number of individuals. Ranges between 5cm and 30cm.
#' @param dpi A numeric indicating the image resolution in pixels per inch (PPI). Defaults to 300. If \code{imgtype="pdf"}, dpi is fixed at 300.
#' @param units A numeric indicating the units of height and width. Default set to "cm". Other options are 'px', 'in' or 'mm'.
#' @param theme A character indicating ggplot theme to be used. Use like "theme_grey", "theme_bw" etc.
#' @param font A character indicating font family to be used in the plots. Uses default system fonts by default for jpeg, png and tiff. Uses 'Helvetica' as default for pdf. Use package \code{extrafonts} to import custom fonts. See vignette for examples.
#' @param na.rm A logical indicating if NAs are removed from data, else \code{ggplot} prints warning messages for NAs. If set to TRUE, NAs are removed before plotting and \code{ggplot} NA warning is suppressed.
#' @return Nothing is returned.
#' @details 
#' 
#' \strong{sortind}\cr
#' This argument takes one character as input.  Default NA means individuals are 
#' plotted in the same order as input. Individuals can be ordered by any one cluster. 
#' For ex. \code{sortind="Cluster1"} or \code{sortind="Cluster2"}.
#' To order by all clusters as the 'Sort by Q' option in STRUCTURE software, 
#' use \code{sortind="all"}. When using \code{sortind="label"}, individuals are
#' sorted by individual labels (along with grplab if present). Individual
#' labels can be displayed using \code{showindlab=T}. When using \code{sortind} 
#' with \code{grplab}, individuals are sorted within the groups.\cr
#' 
#' \strong{grplab}\cr
#' \code{grplab} must be a data.frame. One or more label sets can be provided. Each 
#' label set must be a character vector equal to the number of individuals 
#' present in the qlist. 
#' For example, we can provide one group label set as such:\cr
#' \code{grplab=data.frame(labs=c("Grp A","Grp A","Grp B","Grp B"),stringsAsFactors=F)}\cr
#' 
#' Two group label sets can be provided as such:\cr
#' \code{grplab=data.frame(labs=c("Grp A","Grp A","Grp B","Grp B"),loc=c("Loc 1","Loc 2","Loc 2","Loc 2"),stringsAsFactors=F)}\cr
#' 
#' \strong{selgrp}\cr
#' When multiple group label sets are in use, \code{selgrp} defines which 
#' group label set is used for group ordering (\code{ordergrp}), subsetting (\code{subsetgrp}) 
#' and group mean (\code{grpmean}). \code{selgrp} is also used for plotting divider 
#' lines and and sorting (\code{sortind}). If \code{selgrp} is not specified, 
#' the first group label set is used by default.\cr
#'
#' \strong{ordergrp}\cr
#' When using \code{grplab}, labels may not be in contiguous blocks. Using \code{ordergrp=TRUE}, 
#' regroups individuals into contiguous blocks for all group label sets starting with \code{selgrp}.\cr
#'
#' \strong{subsetgrp}\cr
#' This argument takes one or more characters as input. Use only group labels used 
#' in one of the group label sets in \code{grplab}. For ex. In case of a group label 
#' set 'labs' with two grps in order 'Grp A' and 'Grp B', use \code{subsetgrp=c("Grp B","Grp A")} 
#' to change order of groups. Use \code{subsetgrp="Grp B"} to subset only Grp B. 
#' When using multiple group label sets, use \code{selgrp} to declare which group label set to subset.\cr
#' 
#' \strong{outputfilename}\cr
#' Default is \code{outputfilename=NA} which means that output file names are automatically generated. 
#' When \code{imgoutput="sep"}, the names of the qlist are used to create output labels. When \code{imgoutput="join"},
#' one output label is created for all input files in this format: JoinedNFiles-YYYYMMDDHHMMSS, 
#' where N stands for number of runs joined, and the ending stands for current system date and time. 
#' If \code{outputfilename} is provided, when \code{imgoutput="sep"}, \code{outputfilename} must be 
#' a character vector equal to length of input runs. When \code{imgoutput="join"}, \code{outputfilename} 
#' must be a character of length one. File extensions like .png etc must not be provided.\cr
#' 
#' \strong{height}\cr
#' Argument \code{height} denotes the height of one run panel. With joined plots, the \code{height} is multiplied by number of runs. The height does not include label panel. The \code{grplabheight} is used to define the full height of the lab panel. If \code{grplabheight} is not provided, it is calculated based on the number of group label sets.
#' final_image_height = (height*num_of_runs)+grplabheight
#' It is possible to set either height or width and leave other as default.\cr
#' 
#' \strong{indlab}\cr
#' When \code{showindlab=T}, individual labels are shown/displayed. When \code{showindlab=F}, 
#' individual labels are not shown/displayed on the graph, although they are present in the underlying data.
#' Therefore, \code{showindlab} only control display of labels on the plot and nothing to 
#' do with label control in the data.\cr
#' The default \code{useindlab=F}, creates labels numerically in the 
#' original order of data but with zero padding. For example, if there are 10 individuals,
#' labels are 01, 02 up to 10. if there are 100 individuals, then labels are 001, 002 up
#' to 100. Zero padding to ensure optimal sorting. When \code{useindlab=T}, labels
#' are used from rownames of qlist dataframes. They are usually labelled 1,2,3.. if
#' read in using \code{readQ()}. This can be an issue with sorting by labels \code{sortind="label"}.
#' For STRUCTURE files with individual labels, they can be read
#' in automatically using \code{readQ(indlabfromfile=T)}.\cr
#' When group labels are in use, \code{grplab}, they are added to the individual
#' labels in both cases \code{useindlab=T} and \code{useindlab=F} separated by \code{indlabsep}. 
#' Default \code{indlabsep=" "} adds a space between individual label and grplab. For example,
#' group labels 'popA', 'popA'... will be '01 popA', '02 popA'... when \code{useindlab=F}
#' and usually '1 popA', '2 popA'... when \code{useindlab=T}. When multiple group labels
#' are in use, the are similarly concatenated one after the other to individual names
#' in the order in which the group labels were provided.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @seealso \code{\link{plotQMultiline}}
#' @examples 
#' slist <- readQ(list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE))
#' 
#' # plot one separate figure
#' plotQ(qlist=slist[1])
#' 
#' # plot two separate figures
#' plotQ(qlist=slist[1:2])
#' 
#' # plot a joined figure with multiple plots
#' plotQ(qlist=slist[1:2],imgoutput="join")
#' 
#' # sort individuals
#' plotQ(qlist=slist[c(1,3)],sortind="all")
#' plotQ(qlist=slist[c(1,3)],sortind="Cluster1")
#' plotQ(qlist=slist[c(1,3)],sortind="label")
#' plotQ(qlist=slist[c(1,3)],sortind="all",imgoutput="join")
#' 
#' # read group labels
#' md <- read.delim(system.file("files/metadata.txt", package="pophelper"), header=T,stringsAsFactors=F)
#' 
#' # plot with one group label set
#' plotQ(qlist=slist[1],grplab=md[,2,drop=F])
#' plotQ(qlist=slist[1:2],grplab=md[,2,drop=F],imgoutput="join")
#' 
#' # sort within groups
#' plotQ(qlist=slist[1:2],grplab=md[,2,drop=F],imgoutput="join",sortind="all")
#' plotQ(qlist=slist[1:2],grplab=md[,2,drop=F],imgoutput="join",sortind="Cluster1")
#' plotQ(qlist=slist[1:2],grplab=md[,2,drop=F],imgoutput="join",sortind="label")
#' 
#' # reorder groups
#' plotQ(qlist=slist[1],grplab=md[,2,drop=F],subsetgrp=c("CatB","CatA"))
#' 
#' # multiple group label sets and ordergrp
#' plotQ(qlist=slist[1],grplab=md,ordergrp=TRUE)
#' plotQ(qlist=slist[1:2],grplab=md,ordergrp=TRUE,imgoutput="join")
#' 
#' # sort in second label group set cat
#' plotQ(qlist=slist[1],grplab=md,selgrp="cat",sortind="all")
#' 
#' # use default individual labels
#' plotQ(slist[1],showindlab=T,width=15)
#' 
#' # use custom individual labels
#' inds <- read.delim(system.file("files/structureindlabels.txt",package="pophelper"),header=FALSE,stringsAsFactors=FALSE)
#' rownames(slist[[1]]) <- inds$V1
#' plotQ(slist[1],showindlab=T,useindlab=T,width=15)
#' 
#' # change cluster colours
#' plotQ(slist[1],clustercol=c("steelblue","coral"))
#' 
#' # plot a custom dataframe
#' temp <- list("custom"=data.frame(Cluster1=c(0.2,0.3,0.6,0.8),Cluster2=c(0.8,0.7,0.4,0.2)))
#' plotQ(temp)
#' 
#' @import gtable
#' @import grid
#' @import gridExtra
#' @import tidyr
#' @export
#' 
plotQ <- function(qlist=NULL,imgoutput="sep",clustercol=NA,sortind=NA,grplab=NA,selgrp=NA,ordergrp=FALSE,subsetgrp=NA,grpmean=FALSE,panelspacer=0.06,
                    showsp=TRUE,sppos="right",splab=NA,splabsize=4,splabangle=NULL,splabcol="grey30",splabface="plain",spbgcol=NA,
                    showtitle=FALSE,titlelab=NA,titlehjust=0,titlevjust=0.5,titlesize=5,titlecol="grey30",titleface="plain",titlespacer=1.2,titleangle=0,
                    showsubtitle=FALSE,subtitlelab=NA,subtitlehjust=0,subtitlevjust=0.5,subtitlesize=4,subtitlecol="grey30",subtitleface="plain",subtitlespacer=1.2,subtitleangle=0,
                    grplabspacer=0,grplabheight=NA,grplabpos=0.25,grplabsize=NA,grplabangle=NA,grplabjust=NA,grplabcol="grey30",grplabalpha=1,
                    showindlab=FALSE,useindlab=FALSE,indlabsingle=TRUE,indlabsep=" ",indlabheight=0,indlabsize=3,indlabangle=90,indlabvjust=0.5,indlabhjust=1,indlabcol="grey30",indlabspacer=0,
                    pointsize=NA,pointcol="grey30",pointbgcol="grey30",pointtype="|",pointalpha=1,
                    linepos=0.75,linesize=NA,linecol="grey30",linetype=1,linealpha=1,
                    showdiv=TRUE,divgrp=NA,divcol="white",divtype="21",divsize=0.25,divalpha=1,
                    showlegend=FALSE,legendlab=NA,legendpos="right",legendkeysize=4,legendtextsize=3,
                    barsize=1,barbordersize=0,barbordercolour=NA,
                    outputfilename=NA,imgtype="png",height=NA,width=NA,dpi=300,units="cm",
                    theme="theme_grey",font="",na.rm=TRUE)
{
  #temporary
  showyaxis=FALSE
  showticks=FALSE
  ticksize=0.2
  
  #Validation
  #check qlist
  is.qlist(qlist)
  
  #check imgoutput
  imgoutput <- tolower(imgoutput)
  if(imgoutput != "sep" && imgoutput != "join") stop("plotQ: Argument 'imgoutput' set incorrectly. Set as 'sep' to export as separate plots. Set as 'join' to export as one joined plot.")
  
  #check imagetype
  imgtype <- tolower(imgtype)
  if(imgtype!="png" && imgtype != "pdf" && imgtype != "tiff" && imgtype != "jpeg") stop("plotQ: Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg', 'tiff' or 'pdf'.")
  
  #check sortind
  if(all(!is.na(sortind)))
  {
    if(length(sortind) > 1) stop("plotQ: Argument 'sortind' must be of length 1. Use 'all','label' or a cluster name like 'Cluster1'.")
    #if(sortind != "all" && sortind != "label" && !grepl("Cluster[0-9+]",sortind)) stop("plotQ: Argument 'sortind' must be set to 'all', 'label' or a cluster like 'Cluster1'.")
  }
  
  if(!any(is.na(clustercol))) {if(!is.character(clustercol)) stop("plotQ: Argument 'clustercol' must be a character datatype.")}
  if(!is.logical(useindlab)) stop("plotQ: Argument 'useindlab' set incorrectly. Set as TRUE or FALSE.")
  if(!is.character(indlabsep)) stop("plotQ: Argument 'indlabsep' must be a character datatype.")
  if(!is.logical(grpmean)) stop("plotQ: Argument 'grpmean' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(na.rm)) stop("plotQ: Argument 'na.rm' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(showsp)) stop("plotQ: Argument 'showsp' set incorrectly. Set as TRUE or FALSE.")
  if(!any(is.na(splab))) {if(!is.character(splab)) stop("plotQ: Argument 'splab' must be a character datatype.")}
  if(!any(is.na(splab))) {if(length(splab) != length(qlist)) stop("plotQ: Length of 'splab' is not equal to number of runs.")}
  if(!is.character(splabface)) stop("plotQ: Argument 'splabface' must be a character datatype.")
  if(is.character(splabface)) {if(!splabface %in% c("plain","italic","bold","bold.italic")) stop("plotQ: Argument 'splabface' must be 'plain', 'italic', 'bold' or 'bold.italic'.")}
  if((sppos != "right") && (sppos != "left")) stop("plotQ: Argument 'sppos' set incorrectly. Set as 'right' or 'left'.")
  if(!is.logical(showdiv)) stop("plotQ: Argument 'showdiv' set incorrectly. Set as TRUE or FALSE.")
  if(!any(is.na(legendlab))) {if(!is.character(legendlab)) stop("plotQ: Argument 'legendlab' must be a character datatype.")}
  if((legendpos != "right") && (legendpos != "left")) stop("plotQ: Argument 'legendpos' set incorrectly. Set as 'right' or 'left'.")
  if(!is.numeric(legendkeysize)) stop("plotQ: Argument 'legendkeysize' must be a numeric datatype.")
  if(!is.numeric(legendtextsize)) stop("plotQ: Argument 'legendtextsize' must be a numeric datatype.")
  if(!is.character(font)) stop("plotQ: Argument 'font' must be a character datatype.")
  if(imgtype=="pdf" && font=="") font <- "Helvetica"
  if(imgtype=="pdf") dpi <- 300
  if(!is.numeric(barsize)) stop("plotQ: Argument 'barsize' must be a numeric datatype.")
  if(barsize<0 | barsize>1) stop("plotQ: Argument 'barsize' must be a value between 0 and 1.")
  
  #ggplot version
  ggv <- as.numeric(gsub("\\.","",packageDescription("ggplot2", fields="Version")))
  if(ggv < 220) stop("plotQ: Package ggplot2 must be version 2.2.0 or above.")
  
  #defaults
  col3 <- "grey30"
  if(sppos=="left") sppos <- "y"
  if(sppos=="right") sppos <- NULL
  
  if(grplabpos > 1 | grplabpos < 0) stop("plotQ: Argument 'grplabpos' is set incorrectly. Set a numeric value between 0 and 1. To further increase space, adjust argument 'grplabheight'.")
  if(grplabpos > 1 | grplabpos < 0) stop("plotQ: Argument 'linepos' is set incorrectly. Set a numeric value between 0 and 1. To further increase space, adjust argument 'grplabheight'.")
  
  #check grplabels
  if(!all(is.na(grplab)))
  {
    verifyGrplab(grplab)
    grplablen <- length(grplab)
    grplabcheck <- TRUE

    if(length(selgrp)>1) stop("plotQ: Argument 'selgrp' must be of length 1.")
    if(any(is.na(selgrp))) selgrp <- names(grplab)[1]
    if(!is.character(selgrp)) stop("plotQ: Argument 'selgrp' must be a character datatype.")
    if(!any(selgrp %in% names(grplab))) stop(paste0("plotQ: Argument 'selgrp' contains (",selgrp,") which is not in the 'grplab' titles (",paste0(names(grplab),collapse=", "),")."))
    if(any(is.na(divgrp))) divgrp <- selgrp
    if(!is.character(divgrp)) stop("plotQ: Argument 'divgrp' must be a character datatype.")
    if(!any(divgrp %in% names(grplab))) stop(paste0("plotQ: Argument 'divgrp' contains one or more labels (",paste0(divgrp,collapse=", "),") not present in the 'grplab' titles (",paste0(names(grplab),collapse=", "),")."))
  }else{
    grplabcheck <- FALSE
  }
  
  #length of files
  flen <- length(qlist)
  
  # check outputfilename
  if(!all(is.na(outputfilename)))
  {
    if(imgoutput=="sep") if(length(outputfilename) != flen) stop(paste0("plotQ: Length of argument 'outputfilename' (",length(outputfilename),") is not equal to the number of runs (",flen,") when using 'imgoutput=sep'."))
    if(imgoutput=="join") if(length(outputfilename) != 1) stop("plotQ: Argument 'outputfilename' must be of length equal to 1 when 'imgoutput=join'.")
  }
  
  #check titlelab
  if(!all(is.na(titlelab)))
  {
    if(imgoutput=="sep") if(length(titlelab) != flen) stop(paste0("plotQ: Length of argument 'titlelab' (",length(titlelab),") is not equal to the number of runs (",flen,") when using 'imgoutput=sep'."))
    if(imgoutput=="join") if(length(titlelab) != 1) stop("plotQ: Argument 'titlelab' must be of length equal to 1 when 'imgoutput=join'.")
  }
  
  #check subtitlelab
  if(!all(is.na(subtitlelab)))
  {
    if(imgoutput=="sep") if(length(subtitlelab) != flen) stop(paste0("plotQ: Length of argument 'subtitlelab' (",length(subtitlelab),") is not equal to the number of runs (",flen,") when using 'imgoutput=sep'."))
    if(imgoutput=="join") if(length(subtitlelab) != 1) stop("plotQ: Argument 'subtitlelab' must be of length equal to 1 when 'imgoutput=join'.")
  }
  
  #make labeller function for facets
  if(ggv < 200) plotlabeller <- function(variable,value){return(facetnames[value])}
  
  #ss separate plots
  if(imgoutput=="sep")
  {
    for (i in 1:flen)
    {
      #output name
      fname <- names(qlist)[i]
      fname <- gsub(".txt$|.csv$|.tsv$|.meanq$|.meanQ$|.structure$","",fname)
      if(is.null(fname)) fname <- paste0("sample",i)
      
      # prepare outputfilename
      if(any(is.na(outputfilename))){
        outname <- fname
      }else{
        outname <- outputfilename[i]
      }
      
      # prepare title
      if(any(is.na(titlelab))){
        titlename <- fname
      }else{
        titlename <- titlelab[i]
      }

      #prepare subtitle
      if(any(is.na(subtitlelab))){
        subtitlename <- fname
      }else{
        subtitlename <- subtitlelab[i]
      }
      
      df1 <- qlist[[i]]
      grplabloop <- grplab
      
      #ordering grps
      if(grplabcheck)
      {
        templist <- pophelper:::grpLabels(dframe=df1,grplab=grplabloop,selgrp=selgrp,subsetgrp=subsetgrp,ordergrp=ordergrp,grpmean=grpmean,grplabpos=grplabpos,linepos=linepos)
        df1 <- templist$dframe
        grplabloop <- templist$grplab
        markerpos <- templist$markerpos
        labelpos <- templist$labelpos
        rm(templist)
      }
      
      #sorting individuals
      if(!is.na(sortind))
      {
        templist <- pophelper:::sortInd(dframe=df1,grplab=grplabloop,selgrp=selgrp,sortind=sortind,grplabpos=grplabpos,linepos=linepos)
        df1 <- templist$dframe
        grplabloop <- templist$grplab
        markerpos <- templist$markerpos
        labelpos <- templist$labelpos
        rm(templist)
      }


      #add rownames
      if(!useindlab) row.names(df1) <- sprintf(paste0("%",paste0(rep(0,nchar(nrow(df1))),collapse=""),nchar(nrow(df1)),"d"),1:nrow(df1))
      #concatenate grp labs to ind labs
      if(grplabcheck) rownames(df1) <- apply(as.data.frame(list(list(rn=rownames(df1)),as.list(grplabloop))),1,paste,collapse=indlabsep)
      
      k <- ncol(df1)
      Ind <- nrow(df1)
      #df1$Ind <- factor(1:nrow(df1))
      df1$Ind <- factor(rownames(df1),levels=rownames(df1))
      df1$Num <- factor(rep(i,nrow(df1)))
      df2 <- tidyr::gather(df1,"variable","value",-c(Ind,Num))
      df2 <- df2[rev(1:nrow(df2)),] 
      
      #legendlab
      if(any(is.na(legendlab))) 
      {
        legendlab1 <- levels(factor(as.character(df2$variable)))
      }else{
        legendlab1 <- legendlab
      }
      if(length(legendlab1) != length(levels(factor(as.character(df2$variable))))) stop("plotQ: Length of 'legendlab' is not equal to number of clusters.")
      
      # strip panel (showsp) labelling
      if(any(is.na(splab)))
      {
        facetnames <- paste0(fname,"\n","K=",k)
        names(facetnames) <- levels(df1$Num)
      }else{
        facetnames <- splab[i]
        names(facetnames) <- levels(df1$Num)
      }
      
      #get Colours
      coll <- clustercol
      if(any(is.na(clustercol))) coll <- pophelper:::getColours(as.integer(k))
      if(length(coll) < k) stop(paste0("plotQ: Number of colours (",length(coll),") less than number of clusters (",k,")."))
      
      #getDim
      dimtemp <- pophelper:::getDim(ind=Ind,height=height,width=width,dpi=dpi,units=units,imgtype=imgtype,
                                    grplabheight=grplabheight,labs=length(grplabloop),plotnum=1)
      height1 <- as.numeric(dimtemp[1])
      width1 <- as.numeric(dimtemp[2])
      grplabheight1 <- as.numeric(dimtemp[3])
      units1 <- as.character(dimtemp[4])
      
      if(!grplabcheck)
      {
        #plot
        p <- ggplot2::ggplot(data=df2,aes(x=Ind,y=value,fill=variable))+
                              geom_bar(width=barsize,size=barbordersize,colour=barbordercolour,
                                       stat="identity",position="fill",na.rm=na.rm)+
                              scale_x_discrete(expand=c(0,0))+
                              scale_y_continuous(expand=c(0,0))+
                              scale_fill_manual(values=coll,labels=legendlab1)+
                              facet_grid(Num~.,labeller=labeller(Num=facetnames),switch=sppos)+
                              get(theme)(base_family=font)
        
        #add_margin <- function(...)  grid::convertUnit(theme_get()[["plot.margin"]] + margin(...), "cm")
        
        p <- p + labs(x=NULL,y=NULL)+
                  theme(legend.position="top",
                        legend.direction="horizontal",
                        legend.title=element_blank(),
                        legend.key.size=grid::unit(legendkeysize,"points"),
                        legend.text=element_text(size=legendtextsize),
                        legend.spacing=grid::unit(0,"points"),
                        legend.justification=legendpos,
                        legend.margin=margin(0.2,0.2,0.2,0,"points"),
                        legend.box.spacing=grid::unit(1.5,"points"),
                        panel.grid=element_blank(),
                        panel.background=element_blank(),
                        axis.text.x=element_text(size=indlabsize,colour=indlabcol,
                                                 angle=indlabangle,vjust=indlabvjust,
                                                 hjust=indlabhjust,margin=margin(t=indlabspacer)),
                        axis.text.y=element_text(size=indlabsize,colour=indlabcol),
                        axis.ticks=element_line(size=ticksize,colour=indlabcol),
                        axis.ticks.x=element_blank(),
                        axis.line=element_blank(),
                        axis.title=element_blank(),
                        strip.text.y=element_text(size=splabsize,colour=splabcol,face=splabface,angle=splabangle),
                        strip.background=element_rect(colour=spbgcol,fill=spbgcol),
                        plot.margin=grid::unit(c(0.1,0.05,indlabheight,0),"cm"),
                        panel.spacing=grid::unit(panelspacer,"cm"))
        
        #add indlab, yaxis, ticks
        if(!showindlab) p <- p+theme(axis.text.x=element_blank())
        
        if(!showyaxis)
        {
          p <- p+theme(axis.ticks.y=element_blank(),
                       axis.text.y=element_blank(),
                       plot.margin=grid::unit(c(0.1,0.05,indlabheight,0),"cm"))
        }else{
          p <- p+theme(plot.margin=grid::unit(c(0.1,0.05,indlabheight,0.1),"cm"))
        }
          
        if(!showticks) p <- p+theme(axis.ticks=element_blank())
        
        #show title
        if(showtitle) p <- p+labs(title=titlename)+
                              theme(plot.title=element_text(size=titlesize,colour=titlecol,
                                                      angle=titleangle,hjust=titlehjust,face=titleface,
                                                      vjust=titlevjust,margin=margin(b=titlespacer)))
        #show subtitle
        if(showsubtitle) p <- p+labs(subtitle=subtitlename)+
                                theme(plot.subtitle=element_text(size=subtitlesize,colour=subtitlecol,
                                                        angle=subtitleangle,hjust=subtitlehjust,face=subtitleface,
                                                        vjust=subtitlevjust,margin=margin(b=subtitlespacer)))
                            
        #remove strip panels on right
        if(!showsp) p <- p + theme(strip.text=element_blank())
        
        #remove legend
        if(!showlegend) p <- p + theme(legend.position="none") 
        
        if(imgtype=="tiff") tiff(paste0(outname,".tiff"),height=height1,width=width1,res=dpi,units=units1,type="cairo",compression="lzw",family=font)
        if(imgtype=="png") png(paste0(outname,".png"),height=height1,width=width1,res=dpi,units=units1,type="cairo",family=font)
        if(imgtype=="jpeg") jpeg(paste0(outname,".jpg"),height=height1,width=width1,res=dpi,units=units1,quality=100,family=font)
        if(imgtype=="pdf") pdf(paste0(outname,".pdf"),height=height1,width=width1,fonts=font)
        print(p)
        dev.off()
        if(imgtype=="tiff") cat(paste0(outname,".tiff exported.\n"))
        if(imgtype=="png") cat(paste0(outname,".png exported.\n"))
        if(imgtype=="jpeg") cat(paste0(outname,".jpg exported.\n"))
        if(imgtype=="pdf") cat(paste0(outname,".pdf exported.\n"))
      }
      
      if(grplabcheck)
      {

        p1 <- ggplot2::ggplot(data=df2,aes(x=Ind,y=value,fill=variable))+
                              geom_bar(width=barsize,size=barbordersize,colour=barbordercolour,stat="identity",position="fill",na.rm=na.rm)+
                              scale_x_discrete(expand=c(0,0))+
                              scale_y_continuous(expand=c(0,0))+
                              scale_fill_manual(values=coll,labels=legendlab1)+
                              facet_grid(Num~.,labeller=labeller(Num=facetnames),switch=sppos)+
                              get(theme)(base_family=font)

        #add_margin <- function(...)  grid::convertUnit(theme_get()[["plot.margin"]] + margin(...), "cm")

        p1 <- p1 + labs(x=NULL,y=NULL)+
                    theme(legend.position="top",legend.direction="horizontal",
                          legend.title=element_blank(),
                          legend.key.size=grid::unit(legendkeysize,"points"),
                          legend.text=element_text(size=legendtextsize),
                          legend.spacing=grid::unit(0,"points"),
                          legend.justification=legendpos,
                          legend.margin=margin(0.2,0.2,0.2,0,"points"),
                          legend.box.spacing=grid::unit(1.5,"points"),
                          panel.grid=element_blank(),
                          panel.background=element_blank(),
                          axis.text.x=element_text(size=indlabsize,colour=indlabcol,
                                                 angle=indlabangle,vjust=indlabvjust,
                                                 hjust=indlabhjust,margin=margin(t=indlabspacer)),
                          axis.text.y=element_text(size=indlabsize,colour=indlabcol),
                          axis.ticks=element_line(size=ticksize,colour=indlabcol),
                          axis.ticks.x=element_blank(),
                          axis.line=element_blank(),
                          axis.title=element_blank(),
                          strip.text.y=element_text(size=splabsize,colour=splabcol,face=splabface,angle=splabangle),
                          strip.background=element_rect(colour=spbgcol,fill=spbgcol),
                          plot.margin=grid::unit(c(0.1,0.05,indlabheight,0),"cm"),
                          panel.spacing=grid::unit(panelspacer,"cm"))
        
        #add grp divider lines only if 2 grps or more
        if(showdiv)
        {
          markerpos1 <- markerpos[c(markerpos$count %in% divgrp),]
          if(nrow(markerpos1) > 2) p1 <- p1+geom_vline(xintercept=markerpos1$divxpos[-c(1,length(markerpos1$divxpos))],colour=divcol,linetype=divtype,size=divsize,alpha=divalpha)
        }
        
        #add showindlab
        if(!showindlab) p1 <- p1+theme(axis.text.x=element_blank())
        
        if(!showyaxis)
        {
          p1 <- p1+theme(axis.ticks.y=element_blank(),
                       axis.text.y=element_blank(),
                       plot.margin=grid::unit(c(0.1,0.05,indlabheight,0),"cm"))
        }else{
          p1 <- p1+theme(plot.margin=grid::unit(c(0.1,0.05,indlabheight,0.1),"cm"))
        }
        
        if(!showticks) p1 <- p1+theme(axis.ticks=element_blank())
        
        #remove strip panels on right
        if(!showsp) p1 <- p1+theme(strip.text=element_blank())
        
        #remove legend
        if(!showlegend) p1 <- p1 + theme(legend.position="none") 
        
        #show title
        if(showtitle) p1 <- p1+labs(title=titlename)+
          theme(plot.title=element_text(size=titlesize,colour=titlecol,
                                        angle=titleangle,hjust=titlehjust,face=titleface,
                                        vjust=titlevjust,margin=margin(b=titlespacer)))
        #show subtitle
        if(showsubtitle) p1 <- p1+labs(subtitle=subtitlename)+
          theme(plot.subtitle=element_text(size=subtitlesize,colour=subtitlecol,
                                           angle=subtitleangle,hjust=subtitlehjust,face=subtitleface,
                                           vjust=subtitlevjust,margin=margin(b=subtitlespacer)))
        
        ppar <- pophelper:::getPlotParams(grplab=grplabloop[[1]],plotnum=1,grplabsize=grplabsize,grplabangle=grplabangle,
                                          grplabjust=grplabjust,pointsize=pointsize,linesize=linesize)

        #fix facet labels in grplab plot
        cnl <- function(str) {length(strsplit(str,"\n")[[1]])-1}
        glabfacetnames <- names(grplabloop)
        names(glabfacetnames) <- names(grplabloop)
        gn <- max(as.vector(sapply(facetnames,cnl)))
        if(gn>0)
        {
          if(all(cnl(glabfacetnames)<gn))
          {
            pfn <- function(x,n) paste0(x,paste0(rep("\n",n),collapse=""))
            glabfacetnames <- as.vector(sapply(glabfacetnames,n=gn,pfn))
            names(glabfacetnames) <- names(grplabloop)
          }
        }
        
        #create bottom plot with labels
        p2 <- ggplot2::ggplot()+
              geom_blank(data=labelpos,aes(x=labxpos,y=labypos))+
              geom_text(data=labelpos,aes(x=labxpos,y=labypos),label=labelpos$label,angle=ppar$grplabangle,hjust=ppar$grplabjust,size=ppar$grplabsize,colour=grplabcol,alpha=grplabalpha,family=font)+
              geom_line(data=markerpos,aes(x=markerxpos,y=markerypos),colour=linecol,size=ppar$linesize,linetype=linetype,alpha=linealpha)+
              geom_point(data=markerpos,aes(x=markerxpos,y=markerypos),size=ppar$pointsize,colour=pointcol,shape=pointtype,fill=pointbgcol,alpha=pointalpha)+
              scale_x_continuous(expand=c(0,0))+
              scale_y_continuous(expand=c(0,0),limits=c(0,1))+
              labs(x=NULL,y=NULL)+
              facet_grid(count~.,switch=sppos,labeller=labeller(count=glabfacetnames))+
              get(theme)(base_family=font)+
              theme(legend.position="none",
                    panel.grid=element_blank(),
                    panel.background=element_rect(fill="white"),
                    axis.ticks=element_blank(),
                    axis.text=element_blank(),
                    axis.line=element_blank(),
                    axis.title=element_blank(),
                    strip.text.y=element_text(size=splabsize,colour=splabcol,face=splabface,angle=splabangle),
                    strip.background=element_rect(colour=spbgcol,fill=spbgcol),
                    plot.margin=grid::unit(c(grplabspacer,0.05,0,0),"cm"),
                    panel.spacing=grid::unit(panelspacer,"cm"))
        
        #remove strip panels on right
        if(!showsp) p2 <- p2+theme(strip.text=element_blank())
        if(showyaxis) p2 <- p2+theme(plot.margin=grid::unit(c(grplabspacer,0.05,0,0.1),"cm"))

        #gtable conversion
        gp1 <- ggplot_gtable(ggplot_build(p1))
        #turn off clipping
        gp1$layout$clip <- "off"
        
        #gtable conversion
        gp2 <- ggplot_gtable(ggplot_build(p2))
        #turn off clipping
        gp2$layout$clip <- "off"
        #change width of bottom plot to that of top plot
        #gp2 <- gtable::gtable_add_cols(gp2,gp1$widths[3])
        
        #calculate size of panels
        height2 <- height1 + grplabheight1
        #if(imgtype=="pdf") height2 <- height1-(pophelper:::unitConverter(value=grplabheight,fromunit="cm",tounit="in",dpi=dpi))
        
        if(imgtype=="tiff") tiff(paste0(outname,".tiff"),height=height2,width=width1,res=dpi,units=units1,type="cairo",compression="lzw",family=font)
        if(imgtype=="png") png(paste0(outname,".png"),height=height2,width=width1,res=dpi,units=units1,type="cairo",family=font)
        if(imgtype=="jpeg") jpeg(paste0(outname,".jpg"),height=height2,width=width1,res=dpi,units=units1,quality=100,family=font)
        if(imgtype=="pdf") pdf(paste0(outname,".pdf"),height=height2,width=width1,fonts=font)
        
        gridExtra::grid.arrange(gp1,gp2,heights=grid::unit(c(height1,grplabheight1),units1))
        dev.off()
        
        if(imgtype=="tiff") cat(paste0(outname,".tiff exported.\n"))
        if(imgtype=="png") cat(paste0(outname,".png exported.\n"))
        if(imgtype=="jpeg") cat(paste0(outname,".jpg exported.\n"))
        if(imgtype=="pdf") cat(paste0(outname,".pdf exported.\n"))
      }
    }
  }
  
  # joined plots
  if(imgoutput=="join")
  {
    #checks
    if(flen < 2) stop("plotQ: Joined plot cannot be created. Number of selected files less than 2.")
    
    tempdf <- tabulateQ(qlist)
    
    #checks if num of individuals differ between runs
    if(all(tempdf$ind[1] != tempdf$ind)) stop("plotQ: Joined plot not processed. Number of individuals differ between selected runs.")
    Ind <- tempdf$ind[1]
    rm(tempdf)
    
    # prepare outputfilename
    if(any(is.na(outputfilename))){
      outname <- paste0("Joined",flen,"Files-",as.character(format(Sys.time(),"%Y%m%d%H%M%S")))
    }else{
      outname <- outputfilename
    }
    
    # prepare title
    if(any(is.na(titlelab))){
      titlename <- outname
    }else{
      titlename <- titlelab
    }
    
    #prepare subtitle
    if(any(is.na(subtitlelab))){
      subtitlename <- outname
    }else{
      subtitlename <- subtitlelab
    }
    
    #loop to process selected files
    plist <- vector("list",length=flen)
    facetnames <- vector(length=flen)
    kvec <- vector(length=flen)
    for (i in 1:flen)
    {
      fname <- names(qlist)[i]
      fname <- gsub(".txt$|.csv$|.tsv$|.meanq$|.meanQ$|.structure$","",fname)
      if(is.null(fname)) fname <- paste0("sample",i)

      df1 <- qlist[[i]]
      grplabloop <- grplab
      
      #ordering grps
      if(grplabcheck)
      {
        templist <- pophelper:::grpLabels(dframe=df1,grplab=grplabloop,selgrp=selgrp,subsetgrp=subsetgrp,ordergrp=ordergrp,grpmean=grpmean,grplabpos=grplabpos,linepos=linepos)
        df1 <- templist$dframe
        grplabloop <- templist$grplab
        markerpos <- templist$markerpos
        labelpos <- templist$labelpos
        rm(templist)
      }

      #sorting individuals
      if(all(!is.na(sortind)))
      {
        templist <- pophelper:::sortInd(dframe=df1,grplab=grplabloop,selgrp=selgrp,sortind=sortind,grplabpos=grplabpos,linepos=linepos)
        df1 <- templist$dframe
        grplabloop <- templist$grplab
        markerpos <- templist$markerpos
        labelpos <- templist$labelpos
        rm(templist)
      }
      
      #add rownames
      if(!useindlab) row.names(df1) <- sprintf(paste0("%",paste0(rep(0,nchar(nrow(df1))),collapse=""),nchar(nrow(df1)),"d"),1:nrow(df1))
      # concatenate grp labs to ind labs
      if(grplabcheck) rownames(df1) <- apply(as.data.frame(list(list(rn=rownames(df1)),as.list(grplabloop))),1,paste,collapse=indlabsep)
      
      k <- ncol(df1)
      df1$Ind <- factor(rownames(df1),levels=rownames(df1))
      df1$Num <- factor(rep(i,nrow(df1)))
      
      #strip panel labelling
      if(any(is.na(splab)))
      {
        facetnames[[i]] <- paste0(fname,"\n","K=",k)
        names(facetnames[[i]]) <- levels(df1$Num)[i]
      }else{
        facetnames[[i]] <- splab[i]
        names(facetnames[[i]]) <- levels(df1$Num)[i]
      }
      
      kvec[[i]] <- k
      df2 <- tidyr::gather(df1,"variable","value",-c(Ind,Num))
      plist[[i]] <- df2[rev(1:nrow(df2)),]
      rm(df2)
    }
    
    Ind <- nrow(df1)
    #combine list to one dataframe 
    df3 <- do.call("rbind",plist)
    names(facetnames) <- levels(factor(as.character(df3$Num)))
    
    #legendlab
    if(any(is.na(legendlab))) 
    {
      legendlab1 <- levels(factor(as.character(df3$variable)))
    }else{
      legendlab1 <- legendlab
    }
    if(length(legendlab1) != length(levels(factor(as.character(df3$variable))))) stop("plotQ: Length of 'legendlab' is not equal to max number of clusters.")
    
    #get Dim
    dimtemp <- pophelper:::getDim(ind=Ind,height=height,width=width,dpi=dpi,units=units,imgtype=imgtype,
                                  grplabheight=grplabheight,labs=length(grplabloop),plotnum=flen)
    height1 <- as.numeric(dimtemp[1])
    width1 <- as.numeric(dimtemp[2])
    grplabheight1 <- as.numeric(dimtemp[3])
    units1 <- as.character(dimtemp[4])
    
    #Get Col
    coll <- clustercol
    if(any(is.na(clustercol))) coll <- pophelper:::getColours(as.integer(max(kvec)))
    if(length(coll) < max(kvec)) stop(paste0("plotQ: Number of colours (",length(coll),") is less than the number of clusters (",max(kvec),")."))
    
    if(!grplabcheck)
    {
      
      #ggplot
      p <- ggplot2::ggplot(data=df3,aes(x=Ind,y=value,fill=variable))+
        geom_bar(width=barsize,size=barbordersize,colour=barbordercolour,stat="identity",position="fill",na.rm=na.rm)+
        scale_x_discrete(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        scale_fill_manual(values=coll,labels=legendlab1)+
        facet_grid(Num~.,labeller=labeller(Num=facetnames),switch=sppos,scales="free_x")+
        get(theme)(base_family=font)
      
      #add_margin <- function(...)  grid::convertUnit(theme_get()[["plot.margin"]] + margin(...), "cm")

      p <- p + labs(x=NULL,y=NULL)+
        theme(legend.position="top",
              legend.direction="horizontal",
              legend.title=element_blank(),
              legend.key.size=grid::unit(legendkeysize,"points"),
              legend.text=element_text(size=legendtextsize),
              legend.spacing=grid::unit(0,"points"),
              legend.justification=legendpos,
              legend.margin=margin(0.2,0.2,0.2,0,"points"),
              legend.box.spacing=grid::unit(1.5,"points"),
              panel.grid=element_blank(),
              panel.background=element_blank(),
              axis.text.x=element_text(size=indlabsize,colour=indlabcol,
                                        angle=indlabangle,vjust=indlabvjust,
                                        hjust=indlabhjust,margin=margin(t=indlabspacer)),
              axis.text.y=element_text(size=indlabsize,colour=indlabcol),
              axis.ticks=element_line(size=ticksize,colour=indlabcol),
                        axis.ticks.x=element_blank(),
              axis.line=element_blank(),
              axis.title=element_blank(),
              strip.text.y=element_text(size=splabsize,colour=splabcol,face=splabface,angle=splabangle),
              strip.background=element_rect(colour=spbgcol,fill=spbgcol),
              plot.margin=grid::unit(c(0.1,0.05,indlabheight,0),"cm"),
              panel.spacing=grid::unit(panelspacer,"cm"))
      
      #add showindlab
      if(!showindlab) p <- p+theme(axis.text.x=element_blank())

      if(!showyaxis)
        {
          p <- p+theme(axis.ticks.y=element_blank(),
                       axis.text.y=element_blank(),
                       plot.margin=grid::unit(c(0.1,0.05,indlabheight,0),"cm"))
        }else{
          p <- p+theme(plot.margin=grid::unit(c(0.1,0.05,indlabheight,0.1),"cm"))
        }

      if(!showticks) p <- p+theme(axis.ticks=element_blank())
      
      #remove strip panels on right
      if(!showsp) p <- p + theme(strip.text=element_blank())
      
      #remove legend
      if(!showlegend) p <- p + theme(legend.position="none")
      
      #show title
      if(showtitle) p <- p+labs(title=titlename)+
        theme(plot.title=element_text(size=titlesize,colour=titlecol,
                                      angle=titleangle,hjust=titlehjust,face=titleface,
                                      vjust=titlevjust,margin=margin(b=titlespacer)))
      #show subtitle
      if(showsubtitle) p <- p+labs(subtitle=subtitlename)+
        theme(plot.subtitle=element_text(size=subtitlesize,colour=subtitlecol,
                                         angle=subtitleangle,hjust=subtitlehjust,face=subtitleface,
                                         vjust=subtitlevjust,margin=margin(b=subtitlespacer)))
      #turn off clipping
      #gp <- ggplot_gtable(ggplot_build(p))
      #gp$layout$clip <- "off"
      
      if(imgtype=="tiff") tiff(paste0(outname,".tiff"),height=height1,width=width1,res=dpi,units=units1,type="cairo",compression="lzw",family=font)
      if(imgtype=="png") png(paste0(outname,".png"),height=height1,width=width1,res=dpi,units=units1,type="cairo",family=font)
      if(imgtype=="jpeg") jpeg(paste0(outname,".jpg"),height=height1,width=width1,res=dpi,units=units1,quality=100,family=font)
      if(imgtype=="pdf") pdf(paste0(outname,".pdf"),height=height1,width=width1,fonts=font)
      print(p)
      #grid::grid.draw(gp)
      dev.off()
      if(imgtype=="tiff") cat(paste0(outname,".tiff exported.\n"))
      if(imgtype=="png") cat(paste0(outname,".png exported.\n"))
      if(imgtype=="jpeg") cat(paste0(outname,".jpg exported.\n"))
      if(imgtype=="pdf") cat(paste0(outname,".pdf exported.\n"))
    }
    
    if(grplabcheck)
    {
      
      #create top plot with multiple barplots
      p1 <- ggplot2::ggplot(data=df3,aes(x=Ind,y=value,fill=variable))+
        geom_bar(width=barsize,size=barbordersize,colour=barbordercolour,stat="identity",position="fill",na.rm=na.rm)+
        scale_x_discrete(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        scale_fill_manual(values=coll,labels=legendlab1)+
        facet_grid(Num~.,labeller=labeller(Num=facetnames),switch=sppos)+
        get(theme)(base_family=font)
      
      #add_margin <- function(...)  grid::convertUnit(theme_get()[["plot.margin"]] + margin(...), "cm")

      p1 <- p1 + labs(x=NULL,y=NULL)+
        theme(legend.position="top",
              legend.direction="horizontal",
              legend.title=element_blank(),
              legend.key.size=grid::unit(legendkeysize,"points"),
              legend.text=element_text(size=legendtextsize),
              legend.spacing=grid::unit(0,"points"),
              legend.justification=legendpos,
              legend.margin=margin(0.2,0.2,0.2,0,"points"),
              legend.box.spacing=grid::unit(1.5,"points"),
              panel.grid=element_blank(),
              panel.background=element_blank(),
              axis.text.x=element_text(size=indlabsize,colour=indlabcol,
                                        angle=indlabangle,vjust=indlabvjust,
                                        hjust=indlabhjust,margin=margin(t=indlabspacer)),
              axis.text.y=element_text(size=indlabsize,colour=indlabcol),
              axis.ticks=element_line(size=ticksize,colour=indlabcol),
              axis.ticks.x=element_blank(),
              axis.line=element_blank(),
              axis.title=element_blank(),
              strip.text.y=element_text(size=splabsize,colour=splabcol,face=splabface,angle=splabangle),
              strip.background=element_rect(colour=spbgcol,fill=spbgcol),
              plot.margin=grid::unit(c(0.1,0.05,indlabheight,0),"cm"),
              panel.spacing=grid::unit(panelspacer,"cm"))
      
      #add grp divider lines only if 2 grps or more
      if(showdiv)
      {
        markerpos1 <- markerpos[c(markerpos$count %in% divgrp),]
        if(nrow(markerpos1) > 2) p1 <- p1+geom_vline(xintercept=markerpos1$divxpos[-c(1,length(markerpos1$divxpos))],colour=divcol,linetype=divtype,size=divsize,alpha=divalpha)
      }
      
      #add showindlab
      if(!showindlab) p1 <- p1+theme(axis.text.x=element_blank())
      
      if(!showyaxis)
        {
          p1 <- p1+theme(axis.ticks.y=element_blank(),
                       axis.text.y=element_blank(),
                       plot.margin=grid::unit(c(0.1,0.05,indlabheight,0),"cm"))
        }else{
          p1 <- p1+theme(plot.margin=grid::unit(c(0.1,0.05,indlabheight,0.1),"cm"))
        }

      if(!showticks) p1 <- p1+theme(axis.ticks=element_blank())
      
      #remove strip panels on right
      if(!showsp) p1 <- p1 + theme(strip.text=element_blank())
      
      #remove legend
      if(!showlegend) p1 <- p1 + theme(legend.position="none")
      
      #show title
      if(showtitle) p1 <- p1+labs(title=titlename)+
        theme(plot.title=element_text(size=titlesize,colour=titlecol,
                                      angle=titleangle,hjust=titlehjust,face=titleface,
                                      vjust=titlevjust,margin=margin(b=titlespacer)))
      #show subtitle
      if(showsubtitle) p1 <- p1+labs(subtitle=subtitlename)+
        theme(plot.subtitle=element_text(size=subtitlesize,colour=subtitlecol,
                                         angle=subtitleangle,hjust=subtitlehjust,face=subtitleface,
                                         vjust=subtitlevjust,margin=margin(b=subtitlespacer)))
      #plot with labels
      ppar <- pophelper:::getPlotParams(grplab=grplabloop[[1]],plotnum=flen,grplabsize=grplabsize,grplabangle=grplabangle,grplabjust=grplabjust,
                                        pointsize=pointsize,linesize=linesize)
      
      #fix facet labels in grplab plot
      cnl <- function(str) {length(strsplit(str,"\n")[[1]])-1}
      glabfacetnames <- names(grplabloop)
      names(glabfacetnames) <- names(grplabloop)
      gn <- max(as.vector(sapply(facetnames,cnl)))
      if(gn>0)
      {
        if(all(cnl(glabfacetnames)<gn))
        {
          pfn <- function(x,n) paste0(x,paste0(rep("\n",n),collapse=""))
          glabfacetnames <- as.vector(sapply(glabfacetnames,n=gn,pfn))
          names(glabfacetnames) <- names(grplabloop)
        }
      }
      
      #create bottom plot with labels
      p2 <- ggplot2::ggplot()+
        geom_blank(data=labelpos,aes(x=labxpos,y=labypos))+
        geom_text(data=labelpos,aes(x=labxpos,y=labypos),label=labelpos$label,angle=ppar$grplabangle,hjust=ppar$grplabjust,size=ppar$grplabsize,colour=grplabcol,alpha=grplabalpha,family=font)+
        geom_line(data=markerpos,aes(x=markerxpos,y=markerypos),colour=linecol,size=ppar$linesize,linetype=linetype,alpha=linealpha)+
        geom_point(data=markerpos,aes(x=markerxpos,y=markerypos),size=ppar$pointsize,colour=pointcol,shape=pointtype,fill=pointbgcol,alpha=pointalpha)+
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0),limits=c(0,1))+
        labs(x=NULL,y=NULL)+
        facet_grid(count~.,switch=sppos,labeller=labeller(count=glabfacetnames))+
        get(theme)(base_family=font)+
        theme(legend.position="none",
              panel.grid=element_blank(),
              panel.background=element_blank(),
              axis.ticks=element_blank(),
              axis.text=element_blank(),
              axis.line=element_blank(),
              axis.title=element_blank(),
              strip.text.y=element_text(size=splabsize,colour=splabcol,face=splabface,angle=splabangle),
              strip.background=element_rect(colour=spbgcol,fill=spbgcol),
              plot.margin=grid::unit(c(grplabspacer,0.05,0,0),"cm"),
              panel.spacing=grid::unit(panelspacer,"cm"))
      
      #remove strip panels on right
      if(!showsp) p2 <- p2+theme(strip.text=element_blank())
      if(showyaxis) p2 <- p2+theme(plot.margin=grid::unit(c(grplabspacer,0.05,0,0.1),"cm"))
      
      #gtable conversion
      gp1 <- ggplot_gtable(ggplot_build(p1))
      #turn off clipping
      #gp1$layout$clip[gp1$layout$name=="panel"] <- "off"
      gp1$layout$clip <- "off"
      
      #gtable conversion
      gp2 <- ggplot_gtable(ggplot_build(p2))
      #turn off clipping
      gp2$layout$clip <- "off"
      #change width of bottom plot to that of top plot
      #gp2 <- gtable::gtable_add_cols(gp2, gp1$widths[5]) 
      
      #calculate size of panels
      height2 <- height1+grplabheight1
      
      if(imgtype=="tiff") tiff(paste0(outname,".tiff"),height=height2,width=width1,res=dpi,units=units1,type="cairo",compression="lzw",family=font)
      if(imgtype=="png") png(paste0(outname,".png"),height=height2,width=width1,res=dpi,units=units1,type="cairo",family=font)
      if(imgtype=="jpeg") jpeg(paste0(outname,".jpg"),height=height2,width=width1,res=dpi,units=units1,quality=100,family=font)
      if(imgtype=="pdf") pdf(paste0(outname,".pdf"),height=height2,width=width1,fonts=font)
      
      gridExtra::grid.arrange(gp1,gp2,heights=grid::unit(c(height1,grplabheight1),units1))
      dev.off()
      
      if(imgtype=="tiff") cat(paste0(outname,".tiff exported.\n"))
      if(imgtype=="png") cat(paste0(outname,".png exported.\n"))
      if(imgtype=="jpeg") cat(paste0(outname,".jpg exported.\n"))
      if(imgtype=="pdf") cat(paste0(outname,".pdf exported.\n"))
    }
  }
}

# plotQMultiline ----------------------------------------------------------------

#' @title Plot a qlist as individual-level multiline barplot
#' @description Plot a qlist as individual-level barplot with multiple lines.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param spl An integer indicating samples per line. Defaults to 60.
#' @param lpp An integer indicating lines per page. Defaults to 11.
#' @param clustercol A character vector of colours for clusters.
#' @param sortind A character indicating how individuals are sorted. Default is NA (Same order of individuals as in input file). Other options are 'all' (sorting by values of all clusters), by any one cluster (eg. 'Cluster1') or 'labels' (sorting by individual labels). See details.
#' @param grplab A dataframe with one or more columns (group label sets), and rows equal to the number of individuals. See details.
#' @param selgrp A single character denoting a selected group label set. The selected label must be a group label title used in \code{grplab}. See details.
#' @param ordergrp A logical indicating if individuals must be grouped into contiguous blocks based on \code{grplab} starting with \code{selgrp}.
#' @param subsetgrp A character or character vector with group names to subset or reorder groups. Only applicable when \code{grplab} is in use. Default is NA. See details.
#' @param grpmean A logical indicating if q-matrix must be converted from individual values to group mean values. Applicable only when \code{grplab} is in use and mean is calculated over \code{selgrp}.
#' @param showindlab A logical indicating if individual labels must be shown below the bars. To hide labels, set \code{showindlab=FALSE}. See details.
#' @param useindlab A logical indicating if individual labels must be read from the rownames of qlist dataframes and used as labels. See details.
#' @param indlabsep A character used as separator when concatenating individual labels and group labels. Defaults to space \code{indlabsep=" "}.
#' @param indlabsize A numeric denoting size of the individual labels. Defaults to 5.
#' @param indlabangle A numeric denoting the angle of the individual labels. Defaults to 90.
#' @param indlabvjust A numeric denoting vertical justification of the individual labels. Defaults to 0.5.
#' @param indlabhjust A numeric denoting the horizontal justification of the individual labels. Defaults to 1.
#' @param indlabcol A colour for individual labels. Defaults to 'grey30'.
#' @param indlabspacer A numeric denoting space between the individual label and the plot area. Default set to 0.
#' @param showgrplab A logical indicating if group labels \code{grplab} must be displayed on the plot.
#' @param grplabsize A numeric denoting size of the group labels. Defaults to 7.
#' @param grplabcol A colour for group labels. Defaults to 'grey30'.
#' @param grplabbgcol A colour for group label background. Defaults to 'white'.
#' @param titlelab A character or character vector for title text. Defaults to NA, and when \code{showtitle=TRUE} displays file name.
#' @param titlehjust A numeric denoting the horizontal justification of the title. Defaults to 0 (left).
#' @param titlevjust A numeric denoting the vertical justification of the title. Defaults to 0.5 (center).
#' @param titlesize A numeric indicating the size of the title text. Defaults to 5 points.
#' @param titlecol A colour character for title. Defaults to "grey30".
#' @param titleface A character indicating the font face of title label. One of 'plain', 'italic', 'bold' or 'bold.italic'. Defaults to 'plain'. Applicable only when \code{showtitle=T}.
#' @param titlespacer A numeric indicating the space below the title. Defaults to 1.2.
#' @param titleangle A numeric indicating the angle/rotation of the title. Defaults to 0.
#' @param showsubtitle A logical indicating if plot subtitle must be shown on the top. Defaults to FALSE. If TRUE and \code{subtitlelab=NA}, file name is displayed by default.
#' @param subtitlelab A character or character vector for subtitle text. Defaults to NA, and when \code{showsubtitle=TRUE} displays file name.
#' @param subtitlehjust A numeric denoting the horizontal justification of the subtitle. Defaults to 0 (left).
#' @param subtitlevjust A numeric denoting the vertical justification of the subtitle. Defaults to 0.5 (center).
#' @param subtitlesize A numeric indicating the size of the subtitle text. Defaults to 5 points.
#' @param subtitlecol A colour character for subtitle. Defaults to "grey30".
#' @param subtitleface A character indicating the font face of subtitle label. One of 'plain', 'italic', 'bold' or 'bold.italic'. Defaults to 'plain'. Applicable only when \code{showsubtitle=T}.
#' @param subtitlespacer A numeric indicating the space below the subtitle. Defaults to 1.2.
#' @param subtitleangle A numeric indicating the angle/rotation of the subtitle. Defaults to 0.
#' @param barsize A numeric indicating the width of the bars. Defaults to 0.9.
#' @param barbordersize A numeric indicating border size of bars. Defaults to 0. Visible only when \code{barbordercolour} is not NA.
#' @param barbordercolour A single colour for bar border. Defaults to NA. Visible only when \code{barbordersize} is larger than zero and set to a colour other than NA.
#' @param showticks A logical indicating if ticks on axis should be displayed or not. Defaults to FALSE.
#' @param showyaxis A logical indicating if y-axis labels should be displayed or not. Defaults to FALSE.
#' @param outputfilename A character or character vector denoting output file name without file extension. See details. 
#' @param imgtype A character denoting figure output format. Options are 'png', 'jpeg', 'tiff' or 'pdf'.
#' @param height A numeric denoting height of the full figure. If NA, height is set to 29.7cm (A4 height).
#' @param width A numeric denoting width of the full figure. If NA, width is set to 21cm (A4 width).
#' @param dpi A numeric denoting resolution of the figure. Default is 300. If \code{imgtype="pdf"}, dpi is fixed at 300 and does not have any effect..
#' @param units A character denoting the units of dimension of the figure. Default is "cm". Other options are 'px', 'in' or 'mm'. 
#' @param mar A four number vector denoting distance of top, right, bottom and left margins in \code{units}.
#' @param theme A character indicating ggplot theme to be used. Use like "theme_grey", "theme_bw" etc.
#' @param font A character indicating font family to be used in the plots. Uses default system fonts by default for jpeg, png and tiff. Uses 'Helvetica' as default for pdf. Use package \code{extrafonts} to import custom fonts. See vignette for examples.
#' @param na.rm Default set to FALSE. NAs are not removed from data, therefore \code{ggplot} prints warning messages for NAs. If set to TRUE, NAs are removed before plotting and \code{ggplot} NA warning is suppressed.

#' @details Figures are always created to A4 size. Any plotted row will span the width of the figure. 
#' Note that this function is slow and may take several minutes when plotting mutiple runs.
#' 
#' \strong{indlab}\cr
#' \code{plotQMultiline()} labels each individual separately. When \code{showindlab=T}, 
#' individual labels are shown/displayed. When \code{showindlab=F}, individual labels are
#' not shown/displayed on the graph, although they are present in the underlying data.
#' Therefore, \code{showindlab} only control display of labels on the plot and nothing to 
#' do with label control in the data.\cr
#' The default \code{useindlab=F}, creates labels numerically in the 
#' original order of data but with zero padding. For example, if there are 10 individuals,
#' labels are 01, 02 up to 10. if there are 100 individuals, then labels are 001, 002 up
#' to 100. Zero padding to ensure optimal sorting. When \code{useindlab=T}, labels
#' are used from rownames of qlist dataframes. They are usually labelled 1,2,3.. if
#' read in using \code{readQ()}. This can be an issue with sorting by labels \code{sortind="label"}.
#' For STRUCTURE files with individual labels, they can be read
#' in automatically using \code{readQ(indlabfromfile=T)}.\cr
#' When group labels are in use, \code{grplab}, they are added to the individual
#' labels in both cases \code{useindlab=T} and \code{useindlab=F} separated by \code{indlabsep}. 
#' Default \code{indlabsep=" "} adds a space between individual label and grplab. For example,
#' group labels 'popA', 'popA'... will be '01 popA', '02 popA'... when \code{useindlab=F}
#' and usually '1 popA', '2 popA'... when \code{useindlab=T}. When multiple group labels
#' are in use, the are similarly concatenated one after the other to individual names
#' in the order in which the group labels were provided.
#' 
#' \strong{sortind}\cr
#' This argument takes one character as input.  Default NA means individuals are 
#' plotted in the same order as input. Individuals can be ordered by any one cluster. 
#' For ex. \code{sortind="Cluster1"} or \code{sortind="Cluster2"}.
#' To order by all clusters as the 'Sort by Q' option in STRUCTURE software, 
#' use \code{sortind="all"}. To order by individual labels, use \code{sortind="label"}.
#' When using \code{sortind} with \code{grplab}, individuals
#' are sorted within the groups.\cr
#' 
#' \strong{grplab}\cr
#' \code{grplab} must be a list. One or more label sets can be provided. Each 
#' label set must be a character vector equal to the number of individuals 
#' present in the qlist. 
#' For example, we can provide one set of grp labels as such:\cr
#' \code{labs1 <- c("Grp A","Grp A","Grp B","Grp B")}\cr
#' \code{grplab=list("grp"=labs1)}\cr
#' 
#' A second set of grp labels can be provided as such:
#' \code{labs2 <- c("Loc 1","Loc 1","Loc 2","Loc 3")}\cr
#' \code{grplab=list("population"=labs1,"location"=labs2)}\cr
#' 
#' \strong{subsetgrp}\cr
#' This argument takes one or more characters as input. Use only group labels 
#' exactly as used in the \code{grplab} vector. For ex. In case of two grps in 
#' order 'Pop A' and 'Pop B', use \code{subsetgrp=c("Pop B","Pop A")} to change 
#' order of groups. Use \code{subsetgrp="Pop B"} to subset only Pop B.\cr
#' 
#' \strong{outputfilename}\cr
#' Output file names are created automatically by default using the input qlist names.
#' When number of individuals exceed one page and extra pages are created, incremental 
#' numbers are added to the run name like so: -1, -2 etc. Custom file name can be provided
#' to \code{outputfilename}. The number of labels must be equal to the number of input 
#' runs. Incremental numbers are still added if extra pages are created.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @examples 
#' \dontrun{
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' slist <- readQ(sfiles)
#' 
#' # basic
#' plotQMultiline(slist[1])
#' 
#' # multiple files
#' plotQMultiline(slist[1:3])
#' 
#' # adjust samples per line (spl) and lines per page (lpp)
#' plotQMultiline(slist[1],spl=30)
#' plotQMultiline(slist[1],lpp=8)
#' plotQMultiline(slist[1],spl=75,lpp=10)
#' 
#' # sort individuals
#' plotQMultiline(slist[1],sortind="all")
#' plotQMultiline(slist[1],sortind="Cluster1")
#' plotQMultiline(slist[1],sortind="label")
#' 
#' # use custom individual labels
#' inds <- read.delim(system.file("files/structureindlabels.txt",package="pophelper"),header=FALSE,stringsAsFactors=FALSE)
#' rownames(slist[[1]]) <- inds$V1
#' plotQMultiline(slist[1],useindlab=T)
#' 
#' # change cluster colours
#' plotQMultiline(slist[1],clustercol=c("steelblue","coral"))
#' 
#' # change bar width and height
#' plotQMultiline(slist[1],barsize=1,spl=149,indlabsize=3,height=5)
#' 
#' # read group labels
#' md <- read.delim(system.file("files/metadata.txt", package="pophelper"), header=T,stringsAsFactors=F)
#' 
#' # plot with one group label set
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=F])
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=F],useindlab=T)
#' 
#' # sort ind within groups
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=F],sortind="Cluster1")
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=F],sortind="all")
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=F],sortind="label")
#' 
#' # subset or reorder groups
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=F],subsetgrp=c("CatB"))
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=F],subsetgrp=c("Cat B","CatA"))
#' 
#' # using multiple group label sets
#' plotQMultiline(qlist=slist[1],grplab=md,ordergrp=TRUE)
#' 
#' # subset on a group from second group label set
#' plotQMultiline(qlist=slist[1],grplab=md,selgrp="cat",subsetgrp="CatB")
#' 
#' }
#' @import tidyr
#' @import gridExtra
#' @export
#'
plotQMultiline <- function(qlist=NULL,spl=NA,lpp=NA,clustercol=NA,sortind=NA,grplab=NA,selgrp=NA,ordergrp=FALSE,subsetgrp=NA,grpmean=FALSE,
                            showindlab=TRUE,useindlab=FALSE,indlabsep=" ",
                            indlabsize=5,indlabangle=90,indlabvjust=0.5,indlabhjust=1,indlabcol="grey30",indlabspacer=0,
                            showgrplab=TRUE,grplabsize=7,grplabcol="grey30",grplabbgcol="#DCDCDC",
                            showtitle=FALSE,titlelab=NA,titlehjust=0,titlevjust=0.5,titlesize=9,titlecol="grey30",titleface="plain",titlespacer=3,titleangle=0,
                            showsubtitle=FALSE,subtitlelab=NA,subtitlehjust=0,subtitlevjust=0.5,subtitlesize=7,subtitlecol="grey30",subtitleface="plain",subtitlespacer=4,subtitleangle=0,
                            barsize=0.9,barbordersize=0,barbordercolour=NA,
                            showticks=FALSE,showyaxis=FALSE,
                            outputfilename=NA,imgtype="png",height=NA,width=NA,dpi=300,units="cm",mar=c(0.1,0.5,0.1,0.5),
                            theme="theme_grey",font="",na.rm=FALSE)
{
  #check input
  is.qlist(qlist)
  
  #check
  #check imagetype
  imgtype <- tolower(imgtype)
  if(imgtype!="png" && imgtype != "pdf" && imgtype != "tiff" && imgtype != "jpeg") stop("plotQMultiline: Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg', 'tiff' or 'pdf'.")
  if(!any(is.na(clustercol))) {if(!is.character(clustercol)) stop("plotQMultiline: Argument 'clustercol' must be a character datatype.")}
  if(!is.logical(showindlab)) stop("plotQMultiline: Argument 'showindlab' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(useindlab)) stop("plotQMultiline: Argument 'useindlab' set incorrectly. Set as TRUE or FALSE.")
  if(!is.character(indlabsep)) stop("plotQMultiline: Argument 'indlabsep' must be a character datatype.")
  if(!is.logical(na.rm)) stop("plotQMultiline: Argument 'na.rm' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(showticks)) stop("plotQMultiline: Argument 'showticks' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(showyaxis)) stop("plotQMultiline: Argument 'showyaxis' set incorrectly. Set as TRUE or FALSE.")
  if(all(!is.na(sortind)))
  {
    if(length(sortind) > 1) stop("plotQMultiline: Argument 'sortind' must be of length 1. Use 'all', 'label' or a cluster name like 'Cluster1'.")
    #if(sortind != "all" && sortind != "label" && !grepl("Cluster[0-9+]",sortind)) stop("plotQMultiline: Argument 'sortind' must be set to 'all','label' or a cluster like 'Cluster1'.")
  }
  if(!is.character(font)) stop("plotQMultiline: Argument 'font' must be a character datatype.")
  if(imgtype=="pdf" && font=="") font <- "Helvetica"
  if(!is.numeric(barsize)) stop("plotQMultiline: Argument 'barsize' must be a numeric datatype.")
  if(barsize<0 | barsize>1) stop("plotQMultiline: Argument 'barsize' must be a value between 0 and 1.")

  #check grplabels
  if(!all(is.na(grplab)))
  {
    verifyGrplab(grplab)
    grplablen <- length(grplab)
    grplabcheck <- TRUE
    
    if(!is.logical(showgrplab)) stop("Argument 'showgrplab' must be a logical TRUE or FALSE.")
    if(length(selgrp)>1) stop("plotQMultiline: Argument 'selgrp' must be of length 1.")
    if(any(is.na(selgrp))) selgrp <- names(grplab)[1]
    if(!is.character(selgrp)) stop("plotQMultiline: Argument 'selgrp' must be a character datatype.")
    if(!any(selgrp %in% names(grplab))) stop(paste0("plotQMultiline: Argument 'selgrp' contains (",selgrp,") which is not in the 'grplab' titles (",paste0(names(grplab),collapse=", "),")."))
  }else{
    grplabcheck <- FALSE
  }
  
  #set NA values
  #if(is.na (dpi)) dpi <- 300
  if(imgtype=="pdf") dpi <- 300
  #if(is.na (units)) units <- "cm"
  if(is.na (height)) height <- 29.7 
  if(is.na (width)) width <- 21 
  if(imgtype=="pdf") height <- pophelper:::unitConverter(height,units,"in",dpi)
  if(imgtype=="pdf") width <- pophelper:::unitConverter(width,units,"in",dpi)
  
  len1 <- length(qlist)
  
  # check outputfilename
  if(!all(is.na(outputfilename))) 
  {
    if(length(outputfilename) != len1) stop(paste0("plotQMultiline: Length of argument 'outputfilename' (",length(outputfilename),") is not equal to the number of runs (",len1,")."))
  }
  
  #check titlelab
  if(!all(is.na(titlelab)))
  {
    if(length(titlelab) != len1) stop(paste0("plotQMultiline: Length of argument 'titlelab' (",length(titlelab),") is not equal to the number of runs (",len1,")."))
  }
  
  #check subtitlelab
  if(!all(is.na(subtitlelab)))
  {
    if(length(subtitlelab) != len1) stop(paste0("plotQMultiline: Length of argument 'subtitlelab' (",length(subtitlelab),") is not equal to the number of runs (",len1,")."))
  }
  
  for (i in 1:len1)
  {
    #sample name
    fname <- names(qlist)[i]
    fname <- gsub(".txt$|.csv$|.tsv$|.meanq$|.meanQ$|.structure$","",fname)
    if(is.null(fname)) fname <- paste0("sample",i)

    # prepare outputfilename
    if(any(is.na(outputfilename))){
      outname <- fname
    }else{
      outname <- outputfilename[i]
    }
    
    # prepare title
    if(any(is.na(titlelab))){
      titlename <- fname
    }else{
      titlename <- titlelab[i]
    }
    
    #prepare subtitle
    if(any(is.na(subtitlelab))){
      subtitlename <- fname
    }else{
      subtitlename <- subtitlelab[i]
    }
    
    #check files
    dff <- qlist[[i]]
    grplabloop <- grplab
    
    #ordering grps
    if(grplabcheck)
    {
      templist <- pophelper:::grpLabels(dframe=dff,grplab=grplabloop,selgrp=selgrp,subsetgrp=subsetgrp,ordergrp=ordergrp,grpmean=grpmean)
      dff <- templist$dframe
      grplabloop <- templist$grplab
      if(any(duplicated(rle(grplabloop[,selgrp])$values))) stop("plotQMultiline: Group labels cannot be used due to non-contiguous blocks of labels. Change grplab, selgrp or use 'ordergrp=TRUE'.")
      rm(templist)
    }
      
      #ordering individuals
      if(!is.na(sortind))
      {
        templist <- pophelper:::sortInd(dframe=dff,grplab=grplabloop,selgrp=selgrp,sortind=sortind)
        dff <- templist$dframe
        grplabloop <- templist$grplab
        rm(templist)
      }
      
    #add rownames
    if(!useindlab) row.names(dff) <- sprintf(paste0("%",paste0(rep(0,nchar(nrow(dff))),collapse=""),nchar(nrow(dff)),"d"),1:nrow(dff))
    # concatenate grp labs to ind labs
    if(grplabcheck) rownames(dff) <- apply(as.data.frame(list(list(rn=rownames(dff)),grplabloop)),1,paste,collapse=indlabsep)

      #primary calculation of spl
      nr1 <- nrow(dff)
      
      #numrows <- floor(nr1/spl)
      #numextra <- nr1-(spl*numrows)
      #nr2 <- numrows
      #if(numextra > 0) nr2 <- nr2+1
      
      if(!is.na(spl))
      {
        if(spl > nr1) stop("plotQMultiline: Samples per line (spl) is greater than total number of samples.")
        spl1 <- spl
        numrows <- floor(nr1/spl1)
        numextra <- nr1-(spl1*numrows)
      }
      
      #optimise spl
      if(is.na(spl))
      {
        if(nr1 <= 60) {spl1 <- nr1} else {spl1 <- 60}
        
        #automatically optimise number of rows and spl
        numextra <- 0
        while(numextra < 0.70*spl1)
        {
          numrows <- floor(nr1/spl1)
          numextra <- nr1-(spl1*numrows)
          if(numextra < 0.70*spl1) spl1=spl1+1
          if(spl1 > nr1) 
          {
            spl1 <- nr1 
            break
          }
        }
      }

      nr2 <- numrows
      if(numextra > 0) nr2 <- nr2+1
      
      #get colours
      coll <- clustercol
      if(any(is.na(clustercol))) coll <- pophelper:::getColours(as.integer(ncol(dff)))
      if(length(coll) < ncol(dff)) stop(paste0("plotQMultiline: Number of colours (",length(coll),") is less than the number of clusters (",ncol(dff),")."))
      
      dff$ind <- factor(rownames(dff),levels=rownames(dff))
      dff$rows <- factor(c(rep(1:numrows,each=spl1),rep(nr2,each=numextra)))
      if(grplabcheck) 
      {
        if(length(intersect(colnames(dff),colnames(grplabloop)))!=0) stop(paste0("sortInd: One or more header labels in the run file are duplicated in grplab header. Change labels to be unique. Following are the duplicate label(s): (",paste0(intersect(colnames(dff),colnames(grplabloop)),collapse=", "),")."))
        dff <- cbind(dff,grplabloop)
        dff[[selgrp]] <- grplabloop[,selgrp]
        dff[[selgrp]] <- factor(dff[[selgrp]],levels=rle(dff[[selgrp]])$values)
      }
      
      #split and plot rows
      dlist <- split(dff,dff$rows)
      plist <- vector("list",length=nr2)
      #widthsvec <- vector(length=nr2)
      for (j in 1: nr2)
      {
        dlist[[j]]$rows <- NULL

        if(grplabcheck) 
        {
          scols <- setdiff(colnames(dlist[[j]]),c("ind",names(grplabloop)))
          df2 <- tidyr::gather_(dlist[[j]],key_col="variable",value="value",gather_cols=scols)
        }else{
          df2 <- tidyr::gather(dlist[[j]],"variable","value",-ind)
        }

        #df2 <- df2[rev(1:nrow(df2)),]
        plist[[j]] <- ggplot2::ggplot(data=df2,aes(x=ind,y=value,fill=variable))+
          geom_bar(width=barsize,size=barbordersize,colour=barbordercolour,stat="identity",position="fill",na.rm=na.rm)+
          scale_x_discrete(expand=c(0,0))+
          scale_y_continuous(expand=c(0,0),limits=c(0,1))+
          scale_fill_manual(values=coll)+
          labs(x=NULL,y=NULL)+
          #facet_wrap(~selgrp,nrow=1,scales="free_x")+
          get(theme)(base_family=font)+
          theme(legend.position="none",
                panel.grid=element_blank(),
                panel.background=element_blank(),
                axis.ticks=element_line(size=0.25),
                axis.text.y=element_text(size=indlabsize,colour=indlabcol),
                axis.line=element_blank(),
                axis.title=element_blank(),
                axis.text.x=element_text(size=indlabsize,angle=indlabangle,vjust=indlabvjust,hjust=indlabhjust,colour=indlabcol,margin=margin(t=indlabspacer)),
                plot.margin=grid::unit(mar,units))
        
        if(grplabcheck) 
        {
          plist[[j]] <- plist[[j]] + facet_grid(as.formula(paste0("~",paste0(names(grplabloop),collapse="+"))),scales="free_x",space="free_x")+
            theme(strip.background=element_rect(fill=grplabbgcol),
                  strip.text=element_text(size=grplabsize,colour=grplabcol))
          if(!showgrplab) plist[[j]] <- plist[[j]] + theme(strip.text=element_blank())
        }

        if(!showyaxis) plist[[j]] <- plist[[j]] + theme(axis.ticks.y=element_blank(),axis.text.y=element_blank())
        if(!showindlab) plist[[j]] <- plist[[j]] + theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
        if(!showticks) plist[[j]] <- plist[[j]] + theme(axis.ticks=element_blank())
        
        #show title
        if(j==1)
        {
          if(showtitle) plist[[j]] <- plist[[j]]+labs(title=titlename)+
              theme(plot.title=element_text(size=titlesize,colour=titlecol,
                                            angle=titleangle,hjust=titlehjust,face=titleface,
                                            vjust=titlevjust,margin=margin(b=titlespacer)))
          #show subtitle
          if(showsubtitle) plist[[j]] <- plist[[j]]+labs(subtitle=subtitlename)+
              theme(plot.subtitle=element_text(size=subtitlesize,colour=subtitlecol,
                                               angle=subtitleangle,hjust=subtitlehjust,face=subtitleface,
                                               vjust=subtitlevjust,margin=margin(b=subtitlespacer)))
        }

        #calculate widths. not implemented.
        #widthsvec[j] <- nrow(dlist[[j]])/spl1
      }
      
      #lpp calculations
      if(!is.na(lpp)) lpp1 <- lpp
      if(is.na(lpp)) 
      {
        lpp1 <- 11
        if(lpp1 > nr2) lpp1 <- nr2
      }
      numpages <- ceiling(nr2/lpp1)
      #numpagesextra <- nr2-(lpp*numpages)
      #numpages1 <- numpages
      #if(numpagesextra > 0) numpages1 <- numpages1+1
      
      e <- 0
      r <- 1
      while (r <= numpages)
      {
        start1 <- e + 1
        stop1 <- e + lpp1
        if(stop1 > length(plist)) stop1 <- length(plist)
        
        #widths <- widthsvec[start1:stop1]
        alist <- c(plist[start1:stop1],lpp1,1)
        names(alist) <- c(as.character(start1:stop1),"nrow","ncol")
        if(numpages>1) outname1 <- paste0(outname,"-",r)
        if(numpages==1) outname1 <- outname
        
        if(imgtype=="tiff") tiff(paste0(outname1,".tiff"),height=height,width=width,res=dpi,units=units,type="cairo",compression="lzw",family=font)
        if(imgtype=="png") png(paste0(outname1,".png"),height=height,width=width,res=dpi,units=units,type="cairo",family=font)
        if(imgtype=="jpeg") jpeg(paste0(outname1,".jpg"),height=height,width=width,res=dpi,units=units,quality=100,family=font)
        if(imgtype=="pdf") pdf(paste0(outname1,".pdf"),height=height,width=width,fonts=font)
        
        do.call(gridExtra::grid.arrange,alist)
        dev.off()
        
        if(imgtype=="tiff") cat(paste0(outname1,".tiff exported.\n"))
        if(imgtype=="png") cat(paste0(outname1,".png exported.\n"))
        if(imgtype=="jpeg") cat(paste0(outname1,".jpg exported.\n"))
        if(imgtype=="pdf") cat(paste0(outname1,".pdf exported.\n"))
        
        e <- stop1
        r=r+1
      }
      rm(nr1,nr2,numrows,numextra,numpages,start1,stop1,e,r,dlist,plist,df2,dff)
  }
}

# analyseQ ------------------------------------------------------------------

#' @title Analyse STRUCTURE, TESS or BASIC text runs. Wrapper around several smaller functions.
#' @description A single function to analyse STRUCTURE, TESS or BASIC text runs. Converts runs to a qlist, then tabulate, summarise, perform evanno method, export clumpp and plots runs.
#' @param files A character or character vector of one or more STRUCTURE, TESS or BASIC run files. Use \code{choose.files(multi=TRUE)} to choose interactively.
#' @param evannomethod A logical indicating if evanno method should be performed. Applies only to STRUCTURE runs.
#' @param clumppexport A logical indicating if files must be exported for clumpp.
#' @param plotruns A logical indicating if selected files should be exported as barplots.
#' @param imgoutput A character indicating if files are plotted as separate image files ("sep") or joined into a single image ("join").
#' @param grplab A dataframe with one or more columns (group label sets), and rows equal to the number of individuals.
#' @param clustercol A character vector of colours for colouring clusters. If NA, colours are automatically generated. K 1 to 12 are custom unique colours while K>12 are coloured by function \code{rich.color()}.
#' @param writetable A logical T or F. Setting to TRUE writes the output table to the working directory.
# @param exportdataformat A character to indicate format of exported data. Set as 'excel' to export an Excel .xlsx file or set as 'txt' to export a tab-delimited .txt text file.
#' @param sorttable A logical indicating if the output table must be sorted. Sorts table by loci, ind and K when available.
#' @details The function \code{analyseQ} is a wrapper around several other \code{pophelper} functions. All arguments for all these other functions are not available. If more arguments/options are required, consider running the functions separately.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @return If a single file is selected, a single dataframe is returned. If 
#' multiple files are selected, a list with multiple dataframes is returned.
#' @examples 
#' \dontrun{
#' #structure files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE)
#' analyseQ(sfiles)
#' 
#' #tess files
#' tfiles <- list.files(path=system.file("files/tess",package="pophelper"),full.names=TRUE)
#' analyseQ(tfiles)
#' 
#' #admixture files
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),full.names=TRUE)
#' analyseQ(afiles)
#' }
#' @export
#' 
analyseQ <- analyzeQ <- function(files=NULL,evannomethod=TRUE,clumppexport=TRUE,plotruns=TRUE,
                      imgoutput="sep",grplab=NA,clustercol=NA,writetable=TRUE,sorttable=TRUE)
{
  if(is.null(files) | (length(files)==0)) stop("analyseQ: No input files.")
  if(!is.character(files)) stop("analyseQ: Input is not character dataype.")
  if(!is.logical(evannomethod)) stop("analyseQ: Argument 'evannoMethod' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(clumppexport)) stop("analyseQ: Argument 'clumppExport' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(plotruns)) stop("analyseQ: Argument 'plotQ' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(writetable)) stop("analyseQ: Argument 'writetable' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(sorttable)) stop("analyseQ: Argument 'sorttable' set incorrectly. Set as TRUE or FALSE.")
  
  chk1 <- unique(pophelper:::checkQ(files=files,warn=FALSE)$type)
  if(length(chk1) > 1) stop("analyseQ: Mixed runs selected.")
  if("CLUMPP" %in% chk1) stop("analyseQ: CLUMPP files cannot be used in this function.")
  if("UNIDENTIFIED" %in% chk1) stop("analyseQ: Input file has incorrect format. Check if selected files are STRUCTURE, TESS or BASIC runs.")
  if(imgoutput != "sep" && imgoutput != "join") stop("analyseQ: Argument 'imgoutput' set incorrectly. Set as 'sep' to export as separate plots. Set as 'join' to export as one joined plot.")
  if((chk1 != "STRUCTURE") && (chk1 != "TESS") && (chk1 != "BASIC")) stop("analyseQ: Input files are not STRUCTURE, TESS or BASIC run formats.")
  
  if(chk1=="STRUCTURE")
  {
    rlist <- pophelper::readQ(files,filetype="structure",indlabfromfile=TRUE)
    df1 <- pophelper::tabulateQ(rlist,writetable=writetable,sorttable=sorttable)
    df2 <- pophelper::summariseQ(df1,writetable=writetable)
    if(evannomethod) pophelper::evannoMethodStructure(df2,writetable=writetable,exportplot=TRUE)
    if(clumppexport) pophelper::clumppExport(rlist)
    if(plotruns) pophelper::plotQ(rlist,imgoutput=imgoutput,grplab=grplab,clustercol=clustercol)
    return(rlist)
  }
  
  if(chk1=="TESS")
  {
    rlist <- pophelper::readQ(files,filetype="tess")
    df1 <- pophelper::tabulateQ(rlist,writetable=writetable,sorttable=sorttable)
    df2 <- pophelper::summariseQ(df1,writetable=writetable)
    if(clumppexport) pophelper::clumppExport(rlist)
    if(plotruns) pophelper::plotQ(rlist,imgoutput=imgoutput,grplab=grplab,clustercol=clustercol)
    return(rlist)
  }
  
  if(chk1=="BASIC")
  {
    rlist <- pophelper::readQ(files,filetype="basic")
    df1 <- pophelper::tabulateQ(rlist,writetable=writetable,sorttable=sorttable)
    df2 <- pophelper::summariseQ(df1,writetable=writetable)
    if(clumppexport) pophelper::clumppExport(rlist)
    if(plotruns) pophelper::plotQ(rlist,imgoutput=imgoutput,grplab=grplab,clustercol=clustercol)
    return(rlist)
  }
}

# distructExport ---------------------------------------------------------------

#' @title Generate files for DISTRUCT.
#' @description Create DISTRUCT input files from a qlist.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param grplabbottom A character vector of group labels to be plotted below the plot. The vector must be the same length as number of individuals. See details.
#' @param grplabtop An optional character vector of group labels to be plotted above the plot. The vector must be the same length as number of individuals. See details.
#' @param grpmean A logical indicating if individual values are to be plotted (F) or group means are to be plotted (T).
#' @param overwritedirs A logical indicating if existing directories must be overwritten (T) automatically or not (F).
#' @param printtitle A logical indicating if the filename must be printed as the title on the plot.
#' @param clustercol A character vector of colours equal to the number of clusters. Note these are not R colours. Use \code{distructColours()} or refer to DISTRUCT manual for colours.  
#' With multiple files, number of colours must equal input file with highest number of clusters. 
#' @param grayscale A logical indicating if clusters must be shown in grayscale.
#' @param printcolorbrewer A logical indicating if the colours provided in \code{clustercol} are ColorBrewer colours. See details.
#' @param sepline A logical indicating if divider lines must be drawn between groups (T).
#' @param seplinewidth A numeric indicating width of sepline.
#' @param borderlinewidth A numeric indicating width of border around the plot.
#' @param indlinewidth A numeric indicating width of border around individual bars and ticks.
#' @param fontsize A numeric indicating font size of group labels.
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
#' @param useexe A logical indicating if DISTRUCT executable is automatically run based on system OS (experimental). Does not work on UNIX systems.
#' @return The function does not return anything. The function creates directories for each input file and 
#' populates it with files necessary to run DISTRUCT. The files are individual q-matrix file (xx-indq.txt),
#' population q-matrix file (xx-popq.txt), a cluster colour file (xx-colours.txt) and drawparams file. If 
#' group labels were defined, then (xx-poplab-bottom.txt) or (xx-poplab-top.txt) are also exported. The
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
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#  @import xlsx
#' @examples 
#' \dontrun{
#' #read some data
#' slist <- readQ(list.files(path=system.file("files/structure",package="pophelper"),full.names=TRUE))
#' grps1 <- read.delim(system.file("files/structuregrplabels.txt",package="pophelper"),header=FALSE, stringsAsFactor=F)
#' grps2 <- read.delim(system.file("files/structuregrplabels2.txt",package="pophelper"),header=FALSE, stringsAsFactor=F)
#' 
#' #plot without labels
#' distructExport(slist[1])
#' 
#' #plot with bottom group label
#' distructExport(slist[1],grplabbottom=grps1$V1)
#' 
#' #plot with top group label
#' distructExport(slist[1],grplabtop=grps2$V1)
#' 
#' #plot group mean values
#' distructExport(slist[1],grplabbottom=grps1$V1,grpmean=T)
#' 
#' #automatically run DESTRUCT
#' distructExport(slist[1],useexe=T)
#' }
#' @import tidyr
#' @export
#' 
distructExport <- function(qlist=NULL,grplabbottom=NA,grplabtop=NA,grpmean=FALSE,overwritedirs=FALSE,
                           printtitle=FALSE,clustercol=NA,grayscale=FALSE,printcolorbrewer=FALSE,
                           sepline=T,seplinewidth=0.2,borderlinewidth=1.2,indlinewidth=0.2,
                           fontsize=6,topdist=5,bottomdist=-7,figheight=36,indwidth=1,
                           orientation=0,xorigin=NA,yorigin=NA,xscale=1,yscale=1,toplabangle=60,bottomlabangle=60,
                           echodata=TRUE,printdata=FALSE,quiet=FALSE,useexe=FALSE)
{
  #check input
  is.qlist(qlist)
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
  if(!is.logical(useexe)) stop("distructExport: Argument 'useexe' set incorrectly. Set as TRUE or FALSE.")
  
  #number of files selected
  flen <- length(qlist)
  if(!quiet) cat(paste0("Number of runs in the qlist: ",flen,"\n"))
  
  currwd <- getwd()
  if(as.numeric(file.access(currwd,2))==-1) stop(paste0("distructExport: Directory ",currwd," has no write permission."))
  
  for(i in 1:flen)
  {
    #sample name
    fname <- names(qlist)[i]
    if(is.null(fname)) fname <- paste0("sample",i)
    if(!quiet) cat(paste0("Computing Distruct files for ",fname,"\n"))
    
    #check file
    df <- qlist[[i]]
    if(!is.data.frame(df)) stop(paste0("distructExport: List item ",fname," is not a data.frame object."))
    if(!any(sapply(df,is.numeric))) stop(paste0("distructExport: List item ",fname," has non-numeric columns."))
    tab_runs <- 1
    
    for(j in 1:tab_runs)
    {
      
      dirname <- paste0(fname,"-distruct")
      dop_k <- ncol(df)
      dop_n <- nrow(df)
      
      #top labels & bottom labels
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
        
        #indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 grp=sprintf("%06.0f",as.numeric(as.character(facb))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=F),df)
        
        #grpq
        dfb <- df
        dfb$grp <- facb
        dfb1 <- tidyr::gather(dfb,"variable","value",-grp)
        dfb2 <- cbind(aggregate(value~grp+variable,data=dfb1,FUN=mean),aggregate(value~grp+variable,data=dfb1,FUN=length)[,3,drop=F])
        colnames(dfb2) <- c("grp","variable","mean","len")
        dfb3 <- tidyr::spread(dfb2,"variable","mean")
        rm(dfb1,dfb2)
        dfb3$length <- dfb3$len
        dfb3 <- dfb3[,!(colnames(dfb3) %in% c("grp","len"))]
        pnames <- sprintf("%06.0f",1:nrow(dfb3))
        grpq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=F),dfb3)
        
        #bottomlabels
        plb <- data.frame(sample=pnames,label=rlevalb$values,stringsAsFactors=F)
        dop_m <- nrow(plb)
        
        #grpq
        dft <- df
        dft$grp <- facb
        dft1 <- tidyr::gather(dft,"variable","value",-grp)
        dft2 <- cbind(aggregate(value~grp+variable,data=dft1,FUN=mean),aggregate(value~grp+variable,data=dft1,FUN=length)[,3,drop=F])
        colnames(dft2) <- c("grp","variable","mean","len")
        dft3 <- tidyr::spread(dft2,"variable","mean")
        rm(dft1,dft2)
        dft3$length <- dft3$len
        dft3 <- dft3[,!(colnames(dft3) %in% c("grp","len"))]
        pnamest <- sprintf("%06.0f",1:nrow(dft3))
        #grpq <- cbind(data.frame(sample=paste0(pnamesb,":"),stringsAsFactors=F),dft3)
        
        #tolabels
        plt <- data.frame(sample=pnamest,label=rlevalt$values,stringsAsFactors=F)
        #dop_m <- nrow(plb)
        fname_plb <- paste0(fname,"-grplab-bottom.txt")
        fname_plt <- paste0(fname,"-grplab-top.txt")
      }
      
      #bottom labels
      if((!any(is.na(grplabbottom))) && (any(is.na(grplabtop))))
      {
        grplabbottom <- as.character(grplabbottom)
        if(dop_n != length(grplabbottom)) {message(paste0("distructExport: Number of rows of data (",dop_n,") not equal to length of bottom group labels (",length(grplabbottom),").")); next;}
        
        dop_bottomlabel <- TRUE
        dop_toplabel <- FALSE
        rleval <- rle(grplabbottom)
        fac <- factor(rep(1:length(rleval$values),rleval$lengths))
        
        #indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 grp=sprintf("%06.0f",as.numeric(as.character(fac))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=F),df)
        
        #grpq
        df$grp <- fac
        df1 <- tidyr::gather(df,"variable","value",-grp)
        df2 <- cbind(aggregate(value~grp+variable,data=df1,FUN=mean),aggregate(value~grp+variable,data=df1,FUN=length)[,3,drop=F])
        colnames(df2) <- c("grp","variable","mean","len")
        df3 <- tidyr::spread(df2,"variable","mean")
        rm(df1,df2)
        df3$length <- df3$len
        df3 <- df3[,!(colnames(df3) %in% c("grp","len"))]
        pnames <- sprintf("%06.0f",1:nrow(df3))
        grpq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=F),df3)
        
        #bottomlabels
        plb <- data.frame(sample=pnames,label=rleval$values,stringsAsFactors=F)
        dop_m <- nrow(plb)
        
        fname_plb <- paste0(fname,"-grplab-bottom.txt")
        fname_plt <- "null"
      }
      
      #top labels
      if((!any(is.na(grplabtop))) && (any(is.na(grplabbottom))))
      {
        grplabtop<- as.character(grplabtop)
        if(dop_n != length(grplabtop)) {message(paste0("distructExport: Number of rows of data (",dop_n,") not equal to length of bottom group labels (",length(grplabtop),").")); next;}
        
        dop_toplabel <- TRUE
        dop_bottomlabel <- FALSE
        
        rleval <- rle(grplabtop)
        fac <- factor(rep(1:length(rleval$values),rleval$lengths))
        
        #indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 grp=sprintf("%06.0f",as.numeric(as.character(fac))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=F),df)
        
        #grpq
        df$grp <- fac
        df1 <- tidyr::gather(df,"variable","value",-grp)
        df2 <- cbind(aggregate(value~grp+variable,data=df1,FUN=mean),aggregate(value~grp+variable,data=df1,FUN=length)[,3,drop=F])
        colnames(df2) <- c("grp","variable","mean","len")
        df3 <- tidyr::spread(df2,"variable","mean")
        rm(df1,df2)
        df3$length <- df3$len
        df3 <- df3[,!(colnames(df3) %in% c("grp","len"))]
        pnames <- sprintf("%06.0f",1:nrow(df3))
        grpq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=F),df3)
        
        #toplabels
        plt <- data.frame(sample=pnames,label=rleval$values,stringsAsFactors=F)
        dop_m <- nrow(plt)
        
        fname_plb <- "null"
        fname_plt <- paste0(fname,"-grplab-top.txt")
      }
      
      #top and bottom labels absent
      if((any(is.na(grplabbottom))) && (any(is.na(grplabtop))))
      {
        
        #indq
        inames <- sprintf("%08.0f",1:nrow(df))
        indq <- cbind(data.frame(id1=1:nrow(df),sample=inames,id2=1:nrow(df),
                                 grp=sprintf("%06.0f",as.numeric(as.character(rep(1,nrow(df))))),
                                 colon=rep(":",nrow(df)),stringsAsFactors=F),df)
        
        df1 <- tidyr::gather(df,"variable","value")
        df2 <- cbind(aggregate(value~variable,data=df1,FUN=mean),aggregate(value~variable,data=df1,FUN=length)[,2,drop=F])
        colnames(df2) <- c("variable","mean","len")
        df3 <- tidyr::spread(df2,"variable","mean")
        
        rm(df1,df2)
        df3$length <- df3$len
        df3 <- df3[,!(colnames(df3) %in% c("len"))]
        pnames <- sprintf("%06.0f",1:nrow(df3))
        grpq <- cbind(data.frame(sample=paste0(pnames,":"),stringsAsFactors=F),df3)
        
        dop_m <- 1
        dop_toplabel <- FALSE
        dop_bottomlabel <- FALSE
        fname_plb <- "null"
        fname_plt <- "null"
      }
      
      #colours
      if(any(is.na(clustercol))){
        colsdf <- data.frame(sample=1:dop_k,cols=pophelper:::distructColours()[1:dop_k],stringsAsFactors=F)
      }else{
        if(length(clustercol) != length(pnames)) stop(paste0("distructExport: Length of colours (",length(clustercol),") not equal to length of groups (",length(pnames),"). Change number of colours in 'clustercol' or set 'clustercol=NA'."))
        #if(any(!clustercol %in% pophelper:::distructColours())) stop(paste0("distructExport: One or more colours provided (",clustercol[which(!clustercol %in% pophelper:::distructColours())],") is not a standard Distruct colour."))
        colsdf <- data.frame(sample=pnames,cols=clustercol,stringsAsFactors=F)
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
        if(orientation==0) xorigin <- 72
        else if(orientation==1) xorigin <- 360
        else if(orientation==2) xorigin <- 540
        else if(orientation==3) xorigin <- 288
      }
      
      #yorigin
      if(is.na(yorigin))
      {
        if(orientation==0) yorigin <- 288
        else if(orientation==1) yorigin <- 72
        else if(orientation==2) yorigin <- 504
        else if(orientation==3) yorigin <- 720
      }
      
      #output file names
      fname_grpq <- paste0(fname,"-grpq.txt")
      fname_indq <- paste0(fname,"-indq.txt")
      fname_col <- paste0(fname,"-colours.txt")
      fname_out <- paste0(fname,".ps")
      
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
      
      write.table(grpq,fname_grpq,row.names=F,col.names=F,quote=F,dec=".",sep="\t")
      if(!quiet) cat(paste0(fname_grpq," exported.\n"))
      write.table(indq,fname_indq,row.names=F,col.names=F,quote=F,dec=".",sep="\t")
      if(!quiet) cat(paste0(fname_indq," exported.\n"))
      if(!any(is.na(grplabbottom))) 
      {
        write.table(plb,fname_plb,row.names=F,col.names=F,quote=F,dec=".",sep="\t")
        if(!quiet) cat(paste0(fname_plb," exported.\n"))
      }
      if(!any(is.na(grplabtop))) 
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
        if(sysos=="windows")
        {
          file.copy(system.file("bin/distruct_windows_1.1.exe",package="pophelper"),".")
          system("distruct_windows_1.1.exe")
          unlink("distruct_windows_1.1.exe",force=TRUE)
        }
        
        if(sysos=="mac")
        {
          file.copy(system.file("bin/distruct_macosx_1.1_2013",package="pophelper"),".")
          system("chmod 777 distruct_macosx_1.1_2013")
          system("./distruct_macosx_1.1_2013")
          unlink("distruct_macosx_1.1_2013",force=TRUE)
        }
        
        if(sysos=="unix64")
        {
          file.copy(system.file("bin/distruct_linux_1.1_64bit",package="pophelper"),".")
          system("chmod 777 distruct_linux_1.1_64bit")
          system("./distruct_linux_1.1_64bit")
          unlink("distruct_linux_1.1_64bit",force=TRUE)
        }
        if(sysos=="unix32") warning("distructExport: DISTRUCT executable not run because system was identified as 32 bit while the executable is 64 bit.")
        if(sysos=="unknown") warning("distructExport: DISTRUCT executable not run because system cannot be identified as windows, mac or linux.")
      }
      
      setwd(currwd)
    }
  }
  if(!quiet) cat("===============================\n")
}

# distructColours --------------------------------------------------------------

#' @title Internal: Vector of 90 Distruct colours
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

# getOS ------------------------------------------------------------------------

#' @title Internal: Find current OS
#' @description Find current OS
#' @return Returns a character in lowercase with OS name 'windows', 'mac' or 'linux'.
#' 
getOS <- function() {
  if(tolower(.Platform$OS.type)=="windows") { 
    return("windows")
  } else if(tolower(Sys.info()["sysname"])=="darwin") {
    return("mac")
  } else if(tolower(.Platform$OS.type)=="unix") { 
    if(grepl("x86_64",as.character(sessionInfo()["platform"]))) {return("unix64")}else{return("unix32")}
  } else {
    return("unknown")
  }
}

analyzeQ <- analyseQ
summarizeQ <- summariseQ

# On Load ----------------------------------------------------------------------

#ON LOAD
.onLoad <- function(...) {
  packageStartupMessage("pophelper v2.2.2 ready.")
}

# End --------------------------------------------------------------------------
