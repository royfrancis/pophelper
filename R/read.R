# checkQ --------------------------------------------------------------------

#' @title Internal: Check input filetype.
#' @description Internal: Check input filetype.
#' @param files A character or character vector of one or more input text files 
#' or a list of dataframes.
#' @param warn A logical indicating if a warning must be displayed for file 
#' those are not STRUCTURE, TESS or BASIC file.
#' @return A character or character vector indicating input type 'STRUCTURE', 
#' 'TESS', 'BASIC', 'CLUMPP', 'list', 'data.frame', 'UNIDENTIFIED' for all 
#' selected files.
#' @noRd
#' @keywords internal
#' @import utils
#' 
checkQ <- function(files=NULL,warn=FALSE)
{
  if(is.null(files)) stop("checkQ: Input is empty.")
  if(class(files) != "list" && class(files) != "character") stop("checkQ: Input is not a character or list datatype.")
  
  len1 <- length(files)
  
  checkvec <- rep("UNIDENTIFIED",length=len1)
  subtype <- rep(NA,length=len1)
  for(i in seq_along(files))
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
      
      # read BAPS file
      if(!chk)
      {
        chk <- any(grepl("RESULTS OF ADMIXTURE ANALYSIS BASED",toupper(read1)))
        if(chk) checkvec[i] <- "BAPS"
      }
      
      # read TESS file
      if(!chk)
      {
        chk <- grepl("ESTIMATED CLUSTERING PROBABILITIES",toupper(read1)[1])
        if(chk) checkvec[i] <- "TESS"
      }
      
      # read STRUCTURE file
      if(!chk)
      {
        chk <- grepl("STRUCTURE BY PRITCHARD",toupper(read1)[4])
        if(chk) checkvec[i] <- "STRUCTURE"
      }
      
      # read BASIC files
      
      rm(read1)
      
      if(!chk)
      {
        seps <- c("","\t",",")
        subtypes <- c("SPACE","TAB","COMMA")
        k=1
        while(!chk)
        {
          if(class(try(suppressWarnings(utils::read.table(files[i],header=FALSE,sep=seps[k],nrows=1,quote="",stringsAsFactors=FALSE))))!="try-error")
          {
            df <- utils::read.table(files[i],header=FALSE,sep=seps[k],nrows=1,quote="",stringsAsFactors=FALSE)
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
          if(k>3) break
        }
      }
    }
    if((!chk) && warn) warning(paste0("checkQ: ",files[i]," is not a STRUCTURE, TESS, BAPS, BASIC or CLUMPP file.\n"))
  }
  
  return(list(type=checkvec,subtype=subtype))
}

# readQ ------------------------------------------------------------------------

#' @title Convert run files (q-matrices) to qlist.
#' @description Takes one or more STRUCTURE, TESS, BAPS, BASIC (numeric delimited runs) 
#' or CLUMPP format files and converts them to a qlist (list of dataframes).
#' @param files A character or character vector of one or more files.
#' @param filetype A character indicating input filetype. Options are 'auto',
#' 'structure','tess2','baps','basic' or 'clumpp'. See details.
#' @param indlabfromfile A logical indicating if individual labels must be read 
#' from input file and used as row names for resulting dataframe. Spaces in 
#' labels may be replaced with _. Currently only applicable to STRUCTURE runs.
#' @param readci A logical indicating if confidence intervals from the STRUCTURE
#' run file (if available) should be read. Set to FALSE by default as it take up 
#' excess space. This argument is only applicable to STRUCTURE run files.
#' @return A list of lists with dataframes is returned. List items are named by 
#' input filenames. File extensions such as '.txt','.csv','.tsv' and '.meanQ' 
#' are removed from filename. In case filenames are missing or not available, 
#' lists are named sample1, sample2 etc. For STRUCTURE runs, if individual 
#' labels are present in the run file and \code{indlabfromfile=TRUE}, they are 
#' added to the dataframe as row names. Structure metadata including loci, 
#' burnin, reps, elpd, mvll, and vll is added as attributes to each dataframe. 
#' When \code{readci=TRUE} and if CI data is available in STRUCTURE run files, 
#' it is read in and attached as attribute named ci.
#' For CLUMPP files, multiple runs within one file are suffixed by -1, -2 etc.
#' @details
#' STRUCTURE, TESS2 and BAPS run files have unique layout and format (See 
#' vignette). BASIC files can be Admixture run files, fastStructure meanQ files 
#' or any tab-delimited, space-delimited or comma-delimited tabular data 
#' without a header. CLUMPP files can be COMBINED, ALIGNED or MERGED files. 
#' COMBINED files are generated from \code{clumppExport}. ALIGNED and 
#' MERGED files are generated by CLUMPP.
#' 
#' To convert TESS3 R objects to pophelper qlist, see \code{\link{readQTess3}}.
#' 
#' See the \href{http://royfrancis.github.io/pophelper/}{vignette} for more details.
#' 
#' @seealso \code{\link{readQTess3}}
#' 
#' @examples 
#' 
#' # STRUCTURE files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' # create a qlist of all runs
#' slist <- readQ(sfiles)
#' slist <- readQ(sfiles,filetype="structure")
#' 
#' # use ind names from file
#' slist <- readQ(sfiles[1],indlabfromfile=TRUE)
#' 
#' # access the first run
#' slist <- readQ(sfiles)[[1]]
#' 
#' # access names of runs
#' names(slist)
#' 
#' # get attributes of a run
#' attributes(slist[[1]])
#' 
#' # get attributes of all runs
#' lapply(slist,attributes)
#' 
#' # TESS files
#' tfiles <- list.files(path=system.file("files/tess",package="pophelper"),
#' full.names=TRUE)
#' # create a qlist
#' tlist <- readQ(tfiles)
#' 
#' # BASIC files
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),
#' full.names=TRUE)
#' # create a qlist
#' alist <- readQ(afiles)
#' 
#' # CLUMPP files
#' cfiles1 <- system.file("files/STRUCTUREpop_K4-combined.txt",
#' package="pophelper")
#' cfiles2 <- system.file("files/STRUCTUREpop_K4-combined-aligned.txt",
#' package="pophelper")
#' cfiles3 <- system.file("files/STRUCTUREpop_K4-combined-merged.txt",
#' package="pophelper")
#' 
#' # create a qlist
#' clist1 <- readQ(cfiles1)
#' clist2 <- readQ(cfiles2)
#' clist3 <- readQ(cfiles3)
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
readQ <- function(files=NULL,filetype="auto",indlabfromfile=FALSE,readci=FALSE)
{
  if(is.null(files) || (length(files)==0)) stop("readQ: No input files.")
  if(!is.character(files)) stop("readQ: Argument 'files' is not a character datatype.")
  flen <- length(files)
  
  len <- length(files)
  dlist <- vector("list")
  for (i in seq_along(files))
  {
    # check file
    if(filetype=="auto") 
    {
      chk <- tolower(checkQ(files[i])$type)
      
      if(chk %in% c("structure","tess","baps","basic","clumpp")) 
      {
        if(chk=="structure") dfr <- pophelper::readQStructure(files[i],indlabfromfile=indlabfromfile,readci=readci)
        if(chk=="tess") dfr <- pophelper::readQTess(files[i])
        if(chk=="basic") dfr <- pophelper::readQBasic(files[i])
        if(chk=="clumpp") dfr <- pophelper::readQClumpp(files[i])
        if(chk=="baps") dfr <- pophelper::readQBaps(files[i])
        dlist <- append(dlist,dfr)
      }else{
        warning(paste0("readQ: Input file ",files[i]," was not identified as a STRUCTURE, TESS, BAPS, BASIC or CLUMPP filetype. Specify 'filetype' manually or check input.\n"))
      }
    }else{
      if(filetype=="structure") dfr <- pophelper::readQStructure(files[i],indlabfromfile=indlabfromfile,readci=readci)
      if(filetype=="tess") dfr <- pophelper::readQTess(files[i])
      if(filetype=="basic") dfr <- pophelper::readQBasic(files[i])
      if(filetype=="clumpp") dfr <- pophelper::readQClumpp(files[i])
      if(filetype=="baps") dfr <- pophelper::readQBaps(files[i])
      dlist <- append(dlist,dfr)
    }
  }
  return(dlist)
}

# readQStructure ------------------------------------------------------------

#' @title Convert STRUCTURE run files to qlist.
#' @description Takes one or more STRUCTURE run files and converts them to a 
#' list of dataframes.
#' @param files A character or character vector of one or more STRUCTURE run 
#' files. Use \code{choose.files(multi=TRUE)} 
#' to select interactively.
#' @param indlabfromfile A logical indicating if individual labels must be read 
#' from input file and used as row names for resulting dataframe. Spaces in 
#' labels may be replaced with _.
#' @param readci A logical indicating if confidence intervals from the structure
#' file (if available) should be read. Set to FALSE by default as it take up 
#' excess space.
#' @return A list of lists with dataframes is returned. If individual labels are 
#' present in the STRUCTURE file, they are added to the dataframe as row names. 
#' Structure metadata including loci, burnin, reps, elpd, mvll, and vll is added 
#' as attributes to each dataframe. When \code{readci=TRUE} and if CI data is
#' available, it is read in and attached as attribute named ci. List items are 
#' named by input filenames.
#' @examples 
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' # create a qlist of all runs
#' slist <- readQStructure(sfiles)
#' 
#' # use ind names from file
#' slist <- readQStructure(sfiles[1],indlabfromfile=TRUE)
#' 
#' # access the first run
#' slist <- readQStructure(sfiles)[[1]]
#' 
#' # access names of runs
#' names(slist)
#' @import utils
#' @export
#' 
readQStructure <- function(files=NULL,indlabfromfile=FALSE,readci=FALSE)
{
  if(is.null(files) || (length(files)==0)) stop("readQStructure: No input files.")
  # number of files selected
  flen <- length(files)
  
  #check file
  if(any(checkQ(files)$type != "STRUCTURE")) stop("readQStructure: Input may be in incorrect format.")
  
  i <- 1
  dlist <- vector("list",length=flen)
  len1 <- length(files)
  for (i in seq_along(files))
  {
    fname <- basename(files[i])
    file1 <- readLines(as.character(files[i]),warn=FALSE)
    
    # find individuals and get number of individuals
    ind <- as.numeric(as.character(base::gsub("\\D","",grep("\\d individuals",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1])))
    if(is.na(ind)) cat(paste0("Number of individuals is NA in file: ",fname,"\n"))
    
    # get value of k & error check
    k <- as.numeric(as.character(base::gsub("\\D","",grep("\\d populations assumed",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1])))
    if(is.na(k)) cat(paste0("Value of K is NA in file: ",fname,"\n"))
    
    # get number of loci & error check
    loci <- as.numeric(base::gsub("\\D","",grep("\\d loci",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1]))
    if(is.na(loci)) cat(paste0("Number of Loci is NA in file: ",files[i],"\n"))
    
    # get burn-in value & error check
    burnin <- as.numeric(base::gsub("\\D","",grep("\\d Burn-in period",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1]))
    if(is.na(burnin)) cat(paste0("Burn-in value is NA in file: ",files[i],"\n"))
    
    # get burn-in value & error check
    reps <- as.numeric(base::gsub("\\D","",grep("\\d Reps",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1]))
    if(is.na(reps)) cat(paste0("Reps value is NA in file: ",files[i],"\n"))
    
    # get est ln prob of data & error check
    elpd <- as.numeric(base::gsub("=","",base::gsub("Estimated Ln Prob of Data","",grep("Estimated Ln Prob of Data",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1])))
    if(is.na(elpd)) cat(paste0("Estimated Ln Prob of Data is NA in file: ",files[i],"\n"))
    
    # get mn value of ln likelihood & error check
    mvll <- as.numeric(base::gsub("=","",base::gsub("Mean value of ln likelihood","",grep("Mean value of ln likelihood",file1,perl=TRUE,ignore.case=TRUE,value=TRUE)[1])))
    if(is.na(mvll)) cat(paste0("Mean value of ln likelihood is NA in file: ",files[i],"\n"))
    
    # get Variance of ln likelihood else NA
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
    
    # error check
    tc_file_a <- textConnection(file_a)
    file_b <- utils::read.delim(tc_file_a,header=FALSE,sep="",stringsAsFactors=FALSE)
    close(tc_file_a)
    
    suppressWarnings(
      errorcheck <- try(
        file_b[1,as.integer(grep(":",file_b[1,])+1):as.integer(max(grep("^[0-9]|[.]+$",file_b[1,]))),drop=FALSE],
        silent=TRUE)
    )
    rm(file_b)
    
    if(class(errorcheck)=="try-error")
    {
      # using manual substring
      file_a <- base::gsub("\\([0-9.,]+\\)","",file_a)
      file_b <- base::gsub(":  ","",base::substr(file_a,base::regexpr(":\\W+\\d\\.\\d+",file_a),base::nchar(file_a)-1))
      file_b <- base::sub("\\s+$","",base::sub("^\\s+","",file_b))
      rm(file_a)
      file_c <- as.vector(as.numeric(as.character(unlist(base::strsplit(file_b," ")))))
      rm(file_b)
      dframe <- as.data.frame(matrix(file_c,nrow=ind,byrow=TRUE),stringsAsFactors=FALSE)
    }else{
      # using textconnection
      tc_file_a <- textConnection(file_a)
      file_b <- utils::read.delim(tc_file_a,header=FALSE,sep="",stringsAsFactors=FALSE)
      close(tc_file_a)
      dframe <- file_b[,as.integer(grep(":",file_b[1,])+1):as.integer(max(grep("^[0-9]|[.]+$",file_b[1,]))),drop=FALSE]
    }
    
    dframe <- as.data.frame(sapply(dframe,as.numeric),stringsAsFactors=FALSE)
    colnames(dframe) <- paste0("Cluster",1:ncol(dframe))
    row.names(dframe) <- 1:nrow(dframe)
    #row.names(dframe) <- sprintf(paste0("%",paste0(rep(0,nchar(nrow(dframe))),collapse=""),nchar(nrow(dframe)),"d"),1:nrow(dframe))
    
    # add labels
    if(indlabfromfile)
    {
      labeldf <- file_b[,(grep("[0-9]",file_b[1,])[1]+1):(grep("[(]",file_b[1,])[1]-1),drop=FALSE]
      
      if(ncol(labeldf) > 1) labeldf <- data.frame(V2=do.call(paste,c(labeldf,sep="_")),stringsAsFactors=FALSE)
      if(nrow(labeldf)==nrow(dframe))
      {
        if(any(duplicated(labeldf[,1]))) 
        {
          warning(paste0("readQStructure: Individual names in file ",fname," not used due to presence of duplicate names.\n"))
        }else{
          row.names(dframe) <- as.character(labeldf[,1])
        }
      }else{
        warning(paste0("readQStructure: Individual names in file ",fname," not used due to incorrect length.\n"))
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
    
    # confidence intervals
    if(readci) {
      cichk <- base::grep("([0-9.]+,[0-9.]+)",file_b[1,])
      if(length(cichk)!=0) {
        file_b <- apply(file_b[,cichk,drop=FALSE],1,paste0,collapse="")
        file_b <- base::gsub("[()]","",base::gsub(")(",",",file_b,fixed=T))
        cframe <- as.data.frame(matrix(as.numeric(unlist(base::strsplit(file_b,","))),ncol=ncol(dframe)*2,byrow=TRUE))
        colnames(cframe) <- as.vector(t(outer(paste0("Cluster",1:ncol(dframe)),c("L","H"),paste,sep="")))
        row.names(cframe) <- row.names(dframe)
        attr(dframe,"ci") <- cframe
      }else{
        warning("plotQStructure: Confidence intervals could not be read.\n")
      }
    }
    
    dlist[[i]] <- dframe
  }
  
  fnames <- sub(".txt","",basename(files))
  names(dlist) <- fnames
  return(dlist)
}

# readQTess -----------------------------------------------------------------

#' @title Convert TESS cluster files to qlist.
#' @description Takes one or more TESS cluster run files and converts them to a 
#' list of dataframes.
#' @param files A character or character vector of one or more TESS cluster run 
#' files. Use \code{choose.files(multi=TRUE)} to select interactively.
#' @return A list of lists with dataframes is returned. List items are named by 
#' input filename.
#' @details Use collectRunsTess() to collect TESS runs into one directory.
#' @examples 
#' tfiles <- list.files(path=system.file("files/tess",package="pophelper"),
#' full.names=TRUE)
#' # create a qlist
#' tlist <- readQTess(tfiles)
#' @import utils
#' @export
#'
readQTess <- function(files=NULL)
{
  if(is.null(files) || (length(files)==0)) stop("readQTess: No input files.")
  # number of files selected
  flen <- length(files)
  
  # check file
  if(any(checkQ(files)$type != "TESS")) warning("readQTess: Input may contain incorrect input format.\n")
  
  i <- 1
  dlist <- vector("list",length=flen)
  len1 <- length(files)
  for (i in seq_along(files))
  {
    # read whole file in
    file1 <- readLines(files[i],warn=FALSE)
    
    # extract the cluster table part
    file1 <- file1[3:c(grep("Estimated Allele Frequencies",file1)-1)]
    
    # remove empty lines
    file1 <- file1[file1 != ""]
    
    # create a text connection
    tc_file1 <- textConnection(file1)
    
    # read as a table
    file2 <- utils::read.delim(tc_file1,header=FALSE,sep="\t",stringsAsFactors=FALSE)
    
    # close text connection
    close(tc_file1)
    
    # choose columns 2 to numofcols-2
    dframe <- file2[,2:(ncol(file2)-2)]
    
    # remove temporary files
    rm(file1,file2)
    
    # convert all columns to numeric
    dframe <- as.data.frame(sapply(dframe,as.numeric),stringsAsFactors=FALSE)
    
    # add column names
    colnames(dframe) <- paste0("Cluster",1:ncol(dframe))
    
    # add attributes
    attr(dframe,"ind") <- nrow(dframe)
    attr(dframe,"k") <- ncol(dframe)
    
    # add to list
    dlist[[i]] <- dframe
  }
  
  # add file names as qlist names
  fnames <- sub(".txt","",basename(files))
  names(dlist) <- fnames
  
  return(dlist)
}

# readQBasic ---------------------------------------------------------------

#' @title Convert delimited text files to qlist.
#' @description Takes one or more delimited numeric text files and converts each 
#' of them to separate dataframes.
#' @param files A character or character vector of one or more delimited text 
#' files. Use \code{choose.files(multi=TRUE)} to select interactively.
#' @return A list of lists with dataframes is returned. List items are named by 
#' input filename.
#' @details Input files can be Admixture run files, fastStructure meanQ files. 
#' or any tab-delimited, space-delimited or comma-delimited tabular data without 
#' header.
#' @examples 
#' afiles <- list.files(path=system.file("files/admixture",package="pophelper"),
#' full.names=TRUE)
#' # create a qlist
#' alist <- readQBasic(afiles)
#' @import utils
#' @export
#'
readQBasic <- function(files=NULL)
{
  if(is.null(files) || (length(files)==0)) stop("readQBasic: No input files.")
  
  # number of files selected
  flen <- length(files)
  
  # check input file type
  chk <- checkQ(files)
  if(any(chk$type != "BASIC")) warning("readQBasic: Input may be in incorrect format.\n")
  if(any(is.na(chk$subtype))) warning("readQBasic: Input may be in incorrect format.\n")
  
  i <- 1
  dlist <- vector("list",length=flen)
  len1 <- length(files)
  for (i in seq_along(files))
  {
    # read in delimited files
    if(chk$subtype[i]=="SPACE") dframe <- utils::read.delim(files[i],header=FALSE,sep="",dec=".",stringsAsFactors=FALSE)
    if(chk$subtype[i]=="TAB") dframe <- utils::read.delim(files[i],header=FALSE,sep="\t",dec=".",stringsAsFactors=FALSE)
    if(chk$subtype[i]=="COMMA") dframe <- utils::read.delim(files[i],header=FALSE,sep=",",dec=".",stringsAsFactors=FALSE)
    
    # error if columns contain non-numeric
    if(!all(sapply(dframe,is.numeric))) stop("readQBasic: One or more columns are not numeric.")
    colnames(dframe) <- paste0("Cluster",1:ncol(dframe))
    
    # add attributes
    attr(dframe,"ind") <- nrow(dframe)
    attr(dframe,"k") <- ncol(dframe)
    
    dlist[[i]] <- dframe
  }
  
  # remove file name extensions
  fnames <- sub(".txt","",basename(files))
  fnames <- sub(".tsv","",basename(files))
  fnames <- sub(".csv","",basename(files))
  fnames <- sub(".meanQ","",basename(files))
  
  # add file names to qlist
  names(dlist) <- fnames
  
  return(dlist)
}

# readQClumpp ---------------------------------------------------------------

#' @title Convert CLUMPP format numeric text files to qlist.
#' @description Takes one or more CLUMPP format numeric text files and converts
#' them to a list of dataframes.
#' @param files A character or character vector of one or more COMBINED, ALIGNED 
#' or MERGED files. COMBINED files are generated from \code{clumppExport}. 
#' ALIGNED and MERGED files are generated by CLUMPP. 
#' Use \code{choose.files(multi=TRUE)} to select interactively.
#' @return A list of lists with dataframes is returned. Each list item is named 
#' by input filename. Multiple runs within one file are suffixed by -1, -2 etc.
#' @examples 
#' cfiles1 <- system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper")
#' cfiles2 <- system.file("files/STRUCTUREpop_K4-combined-aligned.txt",
#' package="pophelper")
#' cfiles3 <- system.file("files/STRUCTUREpop_K4-combined-merged.txt",
#' package="pophelper")
#' 
#' # create a qlist
#' clist1 <- readQClumpp(cfiles1)
#' clist2 <- readQClumpp(cfiles2)
#' clist3 <- readQClumpp(cfiles3)
#' 
#' @import utils
#' @export
#'
readQClumpp <- function(files=NULL)
{
  if(is.null(files) || (length(files)==0)) stop("readQClumpp: No input files.")
  
  # number of files selected
  flen <- length(files)
  
  # check file
  chk <- checkQ(files)
  if(any(chk$type != "CLUMPP")) warning("readQClumpp: Input may be in incorrect format.\n")
  
  i <- 1
  k <- 1
  dlist <- vector("list")
  snames <- vector()
  for (i in seq_along(files))
  {
    fname <- base::gsub(".txt","",basename(files[i]))
    
    df1 <- utils::read.table(files[i],header=FALSE,sep="",dec=".",quote="",stringsAsFactors=FALSE)
    if(class(df1)!="data.frame") stop("readQClumpp: Read error. Check input format.")
    
    df1[,1] <- factor(df1[ ,1])
    indlev <- levels(df1[,1])
    
    # error check
    if((nrow(df1) %% length(indlev)) != 0) stop("readQClumpp: Number of individuals is not a multiple of the total number of rows.")
    
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

# readQTess3 -------------------------------------------------------------------

#' @title Convert TESS3 R object to pophelper qlist.
#' @description Takes a TESS3 R object and convert to pophelper qlist for use 
#' with pophelper.
#' @param t3list A TESS3 object. An output from function \code{tess3()} from 
#' package \code{tess3r}. This is not an external object. If imported from outside
#' of R, it must be read in as an Rdata or Rds file.
#' @param progressbar A logical indicating if execution progress must be shown.
#' @return A list of lists with dataframes (qlist) is returned. Each list item 
#' is named by as sample1, sample2 etc. Within TESS3, 'tess3Main' attributes L, 
#' n, ploidy, K, rmse and crossentropy are preserved as attributes in the qlist 
#' dataframe.
#' @details See the \href{http://royfrancis.github.io/pophelper/}{vignette} for 
#' more details.
#' @import utils
#' @export
#' 
readQTess3 <- function(t3list=NULL,progressbar=FALSE)
{
  if(is.null(t3list)) stop("readQTess3: Input is empty.")
  if(!any("tess3" %in% class(t3list))) warning("readQTess3: Input cannot be identified as a valid tess3 class object.\n")
  length(t3list)
  
  # initialise loop variables
  len <- length(t3list)
  qlist <- vector("list",length=len)
  if(progressbar) pb <- utils::txtProgressBar(min=0,max=len,style=3)
  
  # loop to read in data
  for(i in seq_along(t3list))
  {
    if(progressbar) utils::setTxtProgressBar(pb,i)
    if(!"tess3.run" %in% names(t3list[[i]])) stop("readQTess3: 'tess3.run' slot not found in list item ",i,".")
    dlist <- t3list[[i]]$tess3.run[[1]]
    dframe <- as.data.frame(dlist$Q,stringsAsFactors=FALSE)
    colnames(dframe) <- paste0("Cluster",1:ncol(dframe))
    
    # attribute values as added if available else set to NA
    if("n" %in% names(dlist)) {attr(dframe,"ind") <- dlist$n} else {attr(dframe,"ind") <-NA}
    if("K" %in% names(dlist)) {attr(dframe,"k") <- dlist$K} else {attr(dframe,"k") <- NA}
    if("L" %in% names(dlist)) {attr(dframe,"loci") <- dlist$L} else {attr(dframe,"loci") <- NA}
    if("gif" %in% names(dlist)) {attr(dframe,"gif") <- dlist$gif} else {attr(dframe,"gif") <- NA}
    if("rmse" %in% names(dlist)) {attr(dframe,"rmse") <- dlist$rmse} else {attr(dframe,"rmse") <- NA}
    if("crossentropy" %in% names(dlist)) {attr(dframe,"crossentropy") <- dlist$crossentropy} else {attr(dframe,"crossentropy") <- NA}
    if("ploidy" %in% names(dlist)) {attr(dframe,"ploidy") <- dlist$ploidy} else {attr(dframe,"ploidy") <- NA}
    
    qlist[[i]] <- dframe
  }
  if(progressbar) close(pb)
  # list items are labelled
  names(qlist) <- paste0("sample",1:len)
  
  return(qlist)
}

# readQBaps -----------------------------------------------------------------

#' @title Convert BAPS cluster files to qlist.
#' @description Takes one or more BAPS cluster run files and converts them to a 
#' list of dataframes.
#' @param files A character or character vector of one or more BAPS cluster run 
#' files. Use \code{choose.files(multi=TRUE)} to select interactively.
#' @return A list of lists with dataframes is returned. List items are named by 
#' input filename.
#' @details See the \href{http://royfrancis.github.io/pophelper/}{vignette} for 
#' more details.
#' @examples 
#' bfiles <- list.files(path=system.file("files/baps",package="pophelper"),
#' full.names=TRUE)
#' # create a qlist
#' blist <- readQBaps(bfiles)
#' @import utils
#' @export
#'
readQBaps <- function(files=NULL)
{
  if(is.null(files) || (length(files)==0)) stop("readQBaps: No input files.")
  # number of files selected
  flen <- length(files)
  
  # check if file type is BAPS
  if(any(checkQ(files)$type != "BAPS")) warning("readQBaps: Input may be in incorrect format.\n")
  
  i <- 1
  dlist <- vector("list",length=flen)
  for (i in seq_along(files))
  {
    # read in all lines from file
    file1 <- readLines(files[i],warn=FALSE)
    
    # extract the cluster table part
    file1 <- file1[grep("^1:",file1):length(file1)]
    
    # read table using delimiter : and use column V2
    tc_file1 <- textConnection(file1)
    file2 <- utils::read.delim(tc_file1,sep=":",header=FALSE,stringsAsFactors=FALSE)$V2
    
    # read table using delimiter space
    tc_file2 <- textConnection(file2)
    dframe <- utils::read.delim(tc_file2,sep="",header=FALSE,stringsAsFactors=FALSE)
    
    # close text connections
    close(tc_file1,tc_file2)
    
    # remove temporary objects
    rm(file1,file2)
    
    # convert all columns to numeric
    dframe <- as.data.frame(sapply(dframe,as.numeric),stringsAsFactors=FALSE)
    
    # create valid column names
    colnames(dframe) <- paste0("Cluster",1:ncol(dframe))
    
    # attach attributes to dataframe
    attr(dframe,"ind") <- nrow(dframe)
    attr(dframe,"k") <- ncol(dframe)
    
    # place dataframe in a list
    dlist[[i]] <- dframe
  }
  
  # remove .txt in all file names
  fnames <- sub(".txt","",basename(files))
  # label qlist objects with file names
  names(dlist) <- fnames
  
  return(dlist)
}
