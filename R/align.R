# alignKStephens -----------------------------------------------------------

#' @title Internal: Corrects switched labels between repeated runs within a K.
#' @description Internal: Corrects switched labels between repeated runs within a K.
#' @param qlist A qlist object with two or more runs having same number of individuals and clusters (K).
#' @return A qlist where the clusters have been rearranged if needed.
#' @import label.switching
#' @noRd
#' @keywords internal
#' 
alignKStephens <- function(qlist){
  
  pophelper::is.qlist(qlist)
  # if there is only 1 run, just return it
  if(length(qlist)==1) {
    qlist1 <- qlist
  } else {
    # if num of inds or K differ, throw error
    if(diff(range(as.integer(pophelper::tabulateQ(qlist)$k)))!=0) stop("alignKStephens: Number of clusters differ between runs.")
    if(diff(range(as.integer(pophelper::tabulateQ(qlist)$ind)))!=0) stop("alignKStephens: Number of individuals differ between runs.")
    
    # if all runs have K=1, just return it
    if(unique(as.integer(pophelper::tabulateQ(qlist)$k))==1) {
      qlist1 <- qlist
    } else {
      qmat <- lapply(qlist,as.matrix)
      p <- aperm(simplify2array(qmat), c(3,1,2))
      perm <- label.switching::stephens(p)
      
      # reorder and rename columns
      qlist1 <- lapply(seq_len(dim(p)[1]),
                       function(i) {
                         q_perm <- qmat[[i]][, perm$permutations[i,,drop=FALSE],drop=FALSE]
                         q_perm <- as.data.frame(q_perm)
                         attributes(q_perm) <- attributes(qlist[[i]])
                         q_perm
                       }
      )
      names(qlist1) <- names(qlist)
    }
  }

  return(qlist1)
}

# alignWithinK -----------------------------------------------------------

#' @title Internal: Aligns clusters between repeated runs within K.
#' @description Internal: Aligns clusters between repeated runs within K.
#' @param qlist A qlist object
#' @param method A character denoting method. Current option is only 'stephens'.
#' @return A qlist object with clusters switched where necessary.
#' @noRd
#' @keywords internal
#' 
alignWithinK <- function(qlist,method="stephens") {
  
  pophelper::is.qlist(qlist)
  # if all runs have same K, align as is
  if(diff(range(as.integer(pophelper::tabulateQ(qlist)$k)))==0) {
    x <- alignKStephens(qlist)
  }else{
    # if runs have different K, split them and align within sublists
    x <- unlist(lapply(splitQ(qlist),alignKStephens),recursive=FALSE)
    names(x) <- sub("^[0-9]+[\\.]","",names(x)) 
  }
  
  return(x)
}

# alignAcrossK -----------------------------------------------------------

#' @title Internal: Aligns clusters across K.
#' @description Internal: Aligns clusters across K.
#' @param qlist A qlist object
#' @return A qlist object with clusters switched where necessary.
#' @noRd
#' @keywords internal
#' 
alignAcrossK <- function(qlist,debug=FALSE) {
  
  pophelper::is.qlist(qlist)
  if(diff(range(as.integer(pophelper::tabulateQ(qlist)$k)))==0) stop("alignAcrossK: All runs belong to a single K.")
  
  ql <- suppressWarnings(alignWithinK(qlist))
  tq <- pophelper::tabulateQ(ql,sorttable=F)
  kvec <- unique(tq$k)
  
  # loop over
  lvec <- vector("list",length=length(kvec))
  i=1
  while(i<length(kvec)) {
    if(debug) cat(paste("======================\n"))
    if(debug) cat(paste("Loop (i): ",i,"\n"))
    if(debug) cat("Saved list:\n")
    if(debug) cat(str(lvec,max.level=1))
    k1 <- kvec[i]
    k2 <- kvec[i+1]
    if(debug) cat(paste("k1: ",k1,"\n"))
    if(debug) cat(paste("k2: ",k2,"\n"))
    
    
    if(i==1) {
      q1 <- ql[sapply(ql,function(x) ncol(x)==k1)]
      lvec[[1]] <- q1
    }
    if(i>1) q1 <- lvec[[i]]
    q2 <- ql[sapply(ql,function(x) ncol(x)==k2)]
    
    if(debug) cat(paste("Cols of runs of q1 before zeroing: ",paste0(sapply(q1,ncol),collapse=","),"\n"))
    if(debug) cat(paste("Cols of runs of q2 before zeroing: ",paste0(sapply(q2,ncol),collapse=","),"\n"))
    
    kdiff <- k2-k1
    j=1
    while(j<=kdiff) {
      cname <- paste0("TEMP",j)
      fun1 <- function(x,y) {x[,y] <- 0; x}
      q1 <- lapply(q1,fun1,y=cname)
      j <- j+1
    }
    
    if(debug) cat(paste("Cols of runs of q1 after zeroing: ",paste0(sapply(q1,ncol),collapse=","),"\n"))
    if(debug) cat(paste("Cols of runs of q2 after zeroing: ",paste0(sapply(q2,ncol),collapse=","),"\n"))
    
    qx <- c(q1,q2)
    qx <- suppressWarnings(alignWithinK(qx))
    
    qx <- lapply(qx,function(x) {
      g <- grep("TEMP", colnames(x))
      if(length(g)!=0) {
        x[,-g,drop=FALSE]
      }else{
        x
      }
    })
    if(debug) print(str(qx))
    lvec[[i+1]] <- qx[sapply(qx,function(x) ncol(x)==k2)]
    i <- i+1
  }
  
  return(unlist(lvec,recursive=FALSE))
}

# alignK ------------------------------------------------------------------

#' @title Align clusters
#' @description Aligns clusters within or across K.
#' @param qlist A qlist object
#' @param type A character denoting whether clusters must be aligned within K 
#' or across K. Select 'auto', 'within' or 'across'. Defaults to 'auto'.
#' @return A qlist object with clusters switched where necessary.
#' @details When 'within' is selected, clusters are aligned across replicate runs
#' within a given K. When 'across' is selected, clusters are aligned across 
#' replicate runs within a given K and then across K. When 'auto' is selected,
#' if all runs belong to a single K, clusters are aligned within that K, and if 
#' multiple K is present, the 'across' method is used. 
#' @examples
#' 
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"), full.names=TRUE)
#' slist <- readQ(sfiles)
#' xlist <- alignK(slist)
#' 
#' @export
#' 
alignK <- function(qlist,type="auto") {
  pophelper::is.qlist(qlist)
  if(type=="across") return(alignAcrossK(qlist))
  if(type=="within") return(alignWithinK(qlist))
  if(type=="auto") {
    if(diff(range(as.integer(pophelper::tabulateQ(qlist)$k)))==0) {
      return(alignWithinK(qlist))
    } else {
      return(alignAcrossK(qlist))
    }
  }
}
