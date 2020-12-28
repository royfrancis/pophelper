# is.qlist --------------------------------------------------------------------

#' @title Verify if a qlist is formatted correctly.
#' @description Verify if a qlist is formatted correctly.
#' @param qlist A qlist object.
#' @return Nothing.
#' @details Returns error if the object is not a list, contents are not data.frames or NA list elements or missing run labels. Warnings are generated for non standard column labels or missing attributes 'ind' and 'k'. Warnings can be fixed by running \code{\link{as.qlist}}.
#' @examples 
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' slist <- readQ(sfiles)
#' is.qlist(slist)
#' @seealso \code{\link{as.qlist}}
#' @export
#' 
is.qlist <- function(qlist=NULL) {
  
  if(is.null(qlist)) stop("is.qlist: Input is empty.")
  
  # is it a list?
  if(!is.list(qlist)) stop("is.qlist: Input is not a list object.")
  
  # does it have dataframes?
  if(!all(sapply(qlist,is.data.frame))) stop("is.qlist: One or more list elements are not data.frame datatype.")
  
  # any NA?
  if(any(is.na(qlist))) stop("is.qlist: One or more list elements are NA.")
  
  # any NAs in dataframe?
  if(any(unlist(lapply(qlist,sapply,is.na)))) warning("is.qlist: One or more qlist dataframes have NA in data.")
  
  # are dataframes all numeric?
  if(!all(unlist(lapply(qlist,sapply,is.numeric)))) stop("is.qlist: One or more qlist dataframes have non-numeric columns.")
  
  # are there duplicated rownames?
  if(any(unlist(lapply(lapply(qlist,rownames),duplicated)))) stop("is.qlist: One or more qlist dataframes have duplicated row names.")
  
  # are there duplicated column names?
  if(any(unlist(lapply(lapply(qlist,colnames),duplicated)))) stop("is.qlist: One or more qlist dataframes have duplicated column names.")
  
  # are dataframe lists named?
  if(is.null(names(qlist))) stop("is.qlist: List elements are not named. Add names or run as.qlist().")
  
  # are dataframe lists named?
  if(any(is.na(names(qlist)))) stop("is.qlist: One or more list element name is NA. Add names or run as.qlist().")
  
  # are all dataframe lists named?
  if(any(nchar(names(qlist))==0)) stop("is.qlist: One or more list elements are not named. Add names or run as.qlist().")
  
  # are there duplicated list names?
  if(any(duplicated(names(qlist)))) stop("is.qlist: One or more list element names are duplicated. Correct the names or run as.qlist().")
  
  # do column names have format Cluster?
  if(!all(grepl("Cluster[0-9]+$",unlist(lapply(qlist,colnames))))) warning("is.qlist: One or more qlist dataframes have column names that do not conform to format 'ClusterNumber' like 'Cluster1', 'Cluster12' etc. Correct columns or run as.qlist().\n")
  
  # are attributes present?
  a <- lapply(qlist,function(x) as.matrix(unlist(attributes(x))))
  if(any(!sapply(a,function(x) any(grepl("ind",rownames(x)))))) {
    warning("is.qlist: Attribute 'ind' is missing in one or more runs. Run as.qlist().\n")
  }
  if(any(!sapply(a,function(x) any(grepl("k",rownames(x)))))) {
    warning("is.qlist: Attribute 'k' is missing in one or more runs. Run as.qlist().\n")
  }
}

# as.qlist --------------------------------------------------------------------

#' @title Convert a qlist or qlist-like list to a valid qlist.
#' @description Convert a list to a valid qlist. Adds run labels if needed. 
#' Renames cluster columns and adds basic attributes 'ind' and 'k' if needed.
#' @param qlist A qlist or a list of runs (data.frames)
#' @param debug Logical for internal use
#' @return A qlist object (list of data.frames)
#' @examples
#' 
#' # create two data frames
#' df1 <- data.frame(A=c(0.2,0.4,0.6,0.2), B=c(0.8,0.6,0.4,0.8))
#' df2 <- data.frame(A=c(0.3,0.1,0.5,0.6), B=c(0.7,0.9,0.5,0.4))
#' 
#' # create qlist
#' q1 <- list(df1,df2)
#' q1
#' # is.qlist(q1) # error because list elements are not named
#' 
#' q2 <- as.qlist(q1)
#' is.qlist(q2) # no error
#' q2
#' 
#' @importFrom utils str
#' @export
#' 
as.qlist <- function(qlist,debug=FALSE) {
  
  # add names to runs
  if(any(is.null(names(qlist))) | any(names(qlist)=="")) {
    names(qlist) <- paste0("run",1:length(qlist))
  }
  
  if(debug) print(qlist)
  
  fun1 <- function(x) as.matrix(unlist(attributes(x)))
  a <- lapply(qlist,fun1)
  
  # if ind or k is missing from any run, add it to all runs
  if(any(!sapply(a,function(x) any(grepl("ind",rownames(x))))) | any(!sapply(a,function(x) any(grepl("k",rownames(x)))))) {
    for(i in 1:length(qlist)) {
      attr(qlist[[i]],"ind") <- nrow(qlist[[i]])
      attr(qlist[[i]],"k") <- ncol(qlist[[i]])
    }
  }
  
  if(debug) print(str(qlist))
  #a <- as.data.frame(t(as.matrix(do.call("cbind",))),stringsAsFactors=FALSE)
  #fun2 <- function(x,y) attr(x,"ind") <- y
  #mapply(fun2,x=qlist,y=sapply(qlist,nrow))
  
  # if column names do not match format, rename all
  if(any(!unlist(lapply(qlist,function(x) grepl("Cluster[0-9]+",colnames(x)))))) {
    mk <- max(sapply(qlist,ncol))
    for(i in 1:length(qlist)) {
      colnames(qlist[[i]]) <- paste0("Cluster",paste0(sprintf(paste0("%0",nchar(mk),"d"),1:ncol(qlist[[i]]))))
    }
  }
  
  return(qlist)
}

# print.qlist ------------------------------------------------------------------

#' @title print.qlist
#' @description Custom print function for qlist class. Print a summary of qlist contents.
#' @param x A qlist class object
#' @return NULL
#' @examples 
#' 
#' # STRUCTURE files
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' # create a qlist of all runs
#' slist <- readQ(sfiles)
#' 
#' @noRd
#' 
print.qlist <- function(x) {
  message(paste0("qlist S3 class object\n",
             "=====================\n",
             # num of runs
             "Runs: ",length(x),"\n",
             # range of K
             "K range: ",paste(range(sapply(x,ncol)),collapse="-"),"\n",
             # range of ind
             "Ind range: ",paste(range(sapply(x,nrow)),collapse="-"),"\n",
             # names
             "Run labels: ",ifelse(length(x)>3,paste0(paste0(names(x)[1:3],collapse=", ")," ..."),paste0(names(x),collapse=", ")),"\n",
             "Cluster labels: ",ifelse(length(colnames(x[[1]]))>3,paste0(paste0(colnames(x[[1]])[1:3],collapse=", ")," ..."),paste0(colnames(x[[1]]),collapse=", ")),"\n",
             "Ind labels: ",ifelse(length(rownames(x[[1]]))>3,paste0(paste0(rownames(x[[1]])[1:3],collapse=", ")," ..."),paste0(rownames(x[[1]]),collapse=", ")),"\n",
             "Attributes: ",paste(rownames(sapply(s2,attributes))[-c(1:3)],collapse=", "),"\n"))
}

# head.title -------------------------------------------------------------------

#' @title head.title
#' @description Custom head function for qlist class object.
#' @param x A qlist class object
#' @param n An integer denoting number of runs to display
#' @noRd
#' 
head.qlist <- function(x,n) {
  if(missing(n)) {
    n <- ifelse(length(x)>3,3,length(x))
  }
  x <- x[1:n]
  fun <- function(x) x[1:ifelse(nrow(x)>2,2,nrow(x)),1:ifelse(ncol(x)>6,6,ncol(x))]
  return(lapply(x,fun))
}

# joinQ ------------------------------------------------------------------------

#' @title Combine two or more qlist objects into one
#' @description Combine two or more qlist objects
#' @param ... Two or more qlist objects separated by comma (,)
#' @return A joined qlist object
#' @examples
#' 
#' slist <- readQ(list.files(path=system.file("files/structure",package="pophelper"), full.names=TRUE))
#' tlist <- readQ(list.files(path=system.file("files/tess",package="pophelper"), full.names=TRUE))
#' jlist <- joinQ(slist,tlist)
#' length(slist)
#' length(tlist)
#' length(jlist)
#' 
#' @export
#' 
joinQ <- function(...) {
  return(c(...))
}

# mergeQ -----------------------------------------------------------------------

#' @title Merge replicate runs of the same K
#' @description Merge replicate runs of the same K
#' @param qlist A qlist object (with aligned clusters)
#' @details Make sure clusters are aligned between replicate runs before merging 
#' them.
#' @return A merged qlist object. Runs will be ordered by K in the output. Output
#' runs are labelled by K value.
#' @examples
#' 
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"), full.names=TRUE)
#' slist <- readQ(sfiles)
#' slist <- alignK(slist)
#' length(slist)
#' length(mergeQ(slist))
#' @seealso \code{\link{alignK}}
#' @export
#' 
mergeQ <- function(qlist) {
  
  is.qlist(qlist)
  if(diff(range(as.integer(tabulateQ(qlist)$ind)))!=0) stop("mergeQ: Number of individuals differ between runs.")
  
  # Computes mean cell-wise across dataframes
  # x A list of numeric dataframes
  # 
  mergy <- function(x) {
    return(list(Reduce(`+`, x)/length(x)))
  }
  
  # if all runs have same K, merge as is
  if(diff(range(as.integer(tabulateQ(qlist)$k)))==0) {
    labels <- summariseQ(tabulateQ(qlist))$k
    x <- mergy(qlist)
    names(x) <- labels
  }else{
    # if runs have different K, split them and merge within sublists
    qlist <- sortQ(qlist)
    labels <- summariseQ(tabulateQ(qlist,sorttable=FALSE))$k
    x <- unlist(lapply(splitQ(qlist),mergy),recursive=FALSE)
    names(x) <- labels
  }
  
  return(as.qlist(x))
}

# splitQ -----------------------------------------------------------------------

#' @title Split a qlist into sublists by attribute
#' @description Split a qlist into sublists by attribute
#' @param qlist A qlist object
#' @param by A valid attribute name. Generally k or ind. Structure runs can take 
#' loci, burin, reps etc. Any attribute name returned by \code{sapply(qlist,attributes)}
#' should work. Defaults to 'k'.
#' @return Returns a list of qlist objects
#' @examples
#' 
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"), full.names=TRUE)
#' slist <- readQ(sfiles)
#' slist_2 <- splitQ(slist)
#' str(slist,max.level=0)
#' str(slist_2,max.level=1)
#' 
#' @export
#' 
splitQ <- function(qlist,by="k") {
  
  is.qlist(qlist)
  if(length(by)==0) stop("splitQ: Argument 'by' must not be length zero.")
  if(length(by)>1) stop("splitQ: Argument 'by' must be a single character.")
  if(!is.character(by)) stop("splitQ: Argument 'by' must be a character.")
  if(by=="") stop("splitQ: Argument 'by' must not be empty.")
  
  fun1 <- function(x) as.matrix(unlist(attributes(x)))
  a <- lapply(qlist,fun1)
  if(any(!sapply(a,function(x) any(grepl(by,rownames(x)))))) {
    stop(paste0("Attribute provided in by (",by,") is missing in one or more runs. If 'ind' or 'k' is missing, use 'as.qlist()' to add them."))
  }
  
  # match by to attributes
  fun2 <- function(x,y) attr(x,y)
  b <- sapply(qlist,fun2,by)
  
  # split by by
  return(split(qlist,as.vector(b)))
}

# sortQ ------------------------------------------------------------------------

#' @title Sort runs in a qlist based on an attribute
#' @description Sort runs in a qlist based on an attribute
#' @param qlist A qlist object
#' @param by A valid attribute name. Generally 'k' or 'ind'. Structure runs can take 
#' loci, burin, reps etc. Can take more than one name to arrange by multiple fields. For example \code{by=c("ind","k")}.
#' See details.
#' @param decreasing A logical indicating the direction of sorting.
#' @param debug Logical for internal use
#' @details Argument 'by' can be any attribute name returned by \code{sapply(qlist,attributes)} except 'names', 'class' and 'row.names'. This is usually similar to \code{tabulateQ(qlist,sorttable=FALSE)}.
#' @return A sorted qlist object
#' @examples
#' 
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"), full.names=TRUE)
#' slist <- readQ(sfiles)
#' slist_1 <- sortQ(slist)
#' names(slist)
#' names(slist_1)
#' 
#' @importFrom utils str
#' @export
#' 
# order by more than one by value
sortQ <- function(qlist,by="k",decreasing=FALSE,debug=FALSE) {
  
  is.qlist(qlist)
  if(length(by)==0) stop("sortQ: Argument 'by' must not be length zero.")
  if(!is.character(by)) stop("sortQ: Argument 'by' must be a character.")
  if(!is.logical(decreasing)) stop("sortQ: Argument 'decreasing' must be a logical datatype.")
  
  fun1 <- function(x) as.matrix(unlist(attributes(x)))
  a <- lapply(qlist,fun1)
  if(debug) print(a)
  if(any(!sapply(a,function(x) any(grepl(paste0(by,collapse="|"),rownames(x)))))) {
    stop(paste0("One or more of the attributes provided in by (",by,") is missing in one or more runs. If 'ind' or 'k' is missing, use 'as.qlist()' to add them."))
  }
  
  # get df of attributes
  b <- as.data.frame(t(as.data.frame(lapply(a,function(x,y) x[y,],by),stringAsFactors=FALSE)),stringsAsFactors=FALSE)
  fun2 <- function(x) if(all(!is.na(as.numeric(as.character(x))))) {return(as.numeric(as.character(x)))}else{return(x)}
  b <- as.data.frame(sapply(b,fun2),stringAsFactors=FALSE)
  
  if(debug) {print(str(b)); print(b)}
  
  # order
  if(length(by)==1) ord <- order(b[,by,drop=FALSE])
  if(length(by)>1) ord <- do.call(order,b[,by,drop=FALSE])
  if(decreasing) ord <- rev(ord)
  # sort qlist
  return(qlist[ord])
}
