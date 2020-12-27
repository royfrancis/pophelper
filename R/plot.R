# getDim -----------------------------------------------------------------------

#' @title Internal: Get dimensions for figures.
#' @description Internal: Generate height and width of figure based on number
#' of individuals.
#' @param ind A numeric indicating the number of individuals.
#' @param units A character indicating the unit of dimension: "cm","mm","in".
#' @param height A numeric indicating the height of each plot.
#' @param width A numeric indicating the width of each plot.
#' @param dpi A numeric indicating the resolution of the figure.
#' @param imgtype A character denoting image format. "png", "jpeg" or "pdf".
#' @param grplabheight A numeric denoting the height of the label panel.
#' @param labs An integer denoting number of label groups.
#' @param plotnum A numeric indicating the number of plots in the figure.
#' @param showindlab A logical indicating of indlab is displayed.
#' @param sharedindlab A logical indicating if shared indlab is in use.
#' @return a vector with height and width.
#' @noRd
#' @keywords internal
#'
getDim <- function(ind=NA,units=NA,height=NA,width=NA,dpi=NA,imgtype=NA,grplabheight=NA,labs=NA,plotnum=NA,showindlab=FALSE,sharedindlab=TRUE)
{
  if(is.na(units)) units <- "cm"
  if(is.na(units) && imgtype=="pdf") units <- "in"
  if(is.na(dpi)) dpi <- 300
  if(is.na(plotnum)) plotnum <- 1
  if(is.na(labs)) labs <- 1

  # height
  if(is.na(height))
  {
    if(plotnum==1) height <- 1.8
    if(plotnum > 1) height <- 1.2
    if(showindlab)
    {
      if(!sharedindlab) height <- height+0.5
    }

    if(imgtype=="pdf") height <- unitConverter(value=height,fromunit="cm",tounit="in",dpi=dpi)
  }else{
    if(units=="mm" && imgtype != "pdf") height <- unitConverter(value=height,fromunit="mm",tounit="cm",dpi=dpi)
    if(units=="px" && imgtype != "pdf") height <- unitConverter(value=height,fromunit="px",tounit="cm",dpi=dpi)
    if(units=="in" && imgtype != "pdf") height <- unitConverter(value=height,fromunit="in",tounit="cm",dpi=dpi)
    if(units=="cm" && imgtype=="pdf") height <- unitConverter(value=height,fromunit="cm",tounit="in",dpi=dpi)
    if(units=="mm" && imgtype=="pdf") height <- unitConverter(value=height,fromunit="mm",tounit="in",dpi=dpi)
    if(units=="px" && imgtype=="pdf") height <- unitConverter(value=height,fromunit="px",tounit="in",dpi=dpi)
  }
  height <- height*plotnum
  if(showindlab) {if(sharedindlab) height <- height+0.5}

  # width
  if(is.na(width))
  {
    if(is.na(ind)) stop("getDim: Argument ind is empty.")
    width <- ind*0.020
    if(width < 5) width <- 5
    if(width > 30) width <- 30
    if(imgtype=="pdf") width <- unitConverter(value=width,fromunit="cm",tounit="in",dpi=dpi)
  }else{
    if(units=="mm" && imgtype != "pdf") width <- unitConverter(value=width,fromunit="mm",tounit="cm",dpi=dpi)
    if(units=="px" && imgtype != "pdf") width <- unitConverter(value=width,fromunit="px",tounit="cm",dpi=dpi)
    if(units=="in" && imgtype != "pdf") width <- unitConverter(value=width,fromunit="in",tounit="cm",dpi=dpi)
    if(units=="cm" && imgtype=="pdf") width <- unitConverter(value=width,fromunit="cm",tounit="in",dpi=dpi)
    if(units=="mm" && imgtype=="pdf") width <- unitConverter(value=width,fromunit="mm",tounit="in",dpi=dpi)
    if(units=="px" && imgtype=="pdf") width <- unitConverter(value=width,fromunit="px",tounit="in",dpi=dpi)
  }

  # grplabheight
  if(is.na(grplabheight))
  {
    if(labs==1) grplabheight <- 0.6
    if(labs>1) grplabheight <- 0.5
    grplabheight <- grplabheight * labs
    if(imgtype=="pdf") grplabheight <- unitConverter(value=grplabheight,fromunit="cm",tounit="in",dpi=dpi)
  }else{
    if(units=="mm" && imgtype != "pdf") grplabheight <- unitConverter(value=grplabheight,fromunit="mm",tounit="cm",dpi=dpi)
    if(units=="in" && imgtype != "pdf") grplabheight <- unitConverter(value=grplabheight,fromunit="in",tounit="cm",dpi=dpi)
    if(units=="px" && imgtype != "pdf") grplabheight <- unitConverter(value=grplabheight,fromunit="px",tounit="cm",dpi=dpi)
    if(units=="mm" && imgtype=="pdf") grplabheight <- unitConverter(value=grplabheight,fromunit="mm",tounit="in",dpi=dpi)
    if(units=="cm" && imgtype=="pdf") grplabheight <- unitConverter(value=grplabheight,fromunit="cm",tounit="in",dpi=dpi)
    if(units=="px" && imgtype=="pdf") grplabheight <- unitConverter(value=grplabheight,fromunit="px",tounit="in",dpi=dpi)
  }

  if(imgtype!="pdf") units1 <- "cm"
  if(imgtype=="pdf") units1 <- "in"

  lst <- list(height=round(height,2),width=round(width,2),grplabheight=round(grplabheight,2),units=units1)
  return(lst)
}

# getPlotParams ----------------------------------------------------------------

#' @title Internal: Generate parameters for plots with labels
#' @description Internal: Generates various parameters required for plotting
#' with labels.
#' @param grplab A character vector of labels same length as number of
#' individuals.
#' @param plotnum A numeric indicating the number of plots on the figure.
#' @param grplabsize A numeric indicating the size of the labels.
#' @param grplabangle A numeric indicating the angle/rotation of labels. 0 is
#' horizontal while 90 is vertical.
#' @param grplabjust A numeric indicating the justification of labels. Defaults
#' to 0.5 if grplabangle=0  or 1 if grplabangle between 20 and 135.
#' @param pointsize  A numeric indicating the size of points on label marker
#' line.
#' @param linesize A numeric indicating the thickness of the label marker line.
#' @return A list with following plot parameters: grplab, plotnum, grplabsize,
#' grplabangle, grplabjust, pointsize, linesize.
#' @noRd
#' @keywords internal
#'
getPlotParams <- function(grplab=NA,plotnum=1,grplabsize=NA,grplabangle=NA,grplabjust=NA,pointsize=NA,linesize=NA)
{
  if(all(is.na(grplab))) stop("getPlotParams: Labels are empty.")

  # estimate ct based on number of labels/ind
  lp <- length(as.character(grplab))

  # calculate grplabangle, just and margins
  if(is.na(grplabangle)) grplabangle <- 0
  if(grplabangle==0)
  {
    if(is.na(grplabjust)) grplabjust <- 0.5
  }

  if(abs(grplabangle) > 20 && abs(grplabangle) < 135)
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
#' @description Internal: Takes a q-matrix dataframe along with group labels.
#' Group labels can be reordered or subsetted. The function also creates
#' label_position and marker_position dfs.
#' @param dframe A q-matrix dataframe
#' @param grplab A dataframe with group labels
#' @param selgrp A character denoting selected group label set
#' @param ordergrp A logical indicating if individuals must be ordered by group
#' labels
#' @param subsetgrp A character or character vector of grp name(s) to
#' subset/reorder
#' @param grpmean A logical indicating if q-matrix must be converted from
#' individual to group mean
#' @param grplabpos A numeric indicating y-axis position of labels
#' @param linepos A numeric indicating y-axis position of label line
#' @param indlabwithgrplab A logical indicating if grp labels are added to
#' indlab
#' @param indlabsep A character specifying seperation in indlab
#' @param runid A numeric indicating run ID
#' @param corder current order of individuals
#' @return Returns a list with subsetted/reordered q-matrix, a character vector
#' of original/subsetted/reordered grplab dataframe, grplabpos and linepos.
#' @noRd
#' @keywords internal
#' @importFrom stats ave
#'
grpLabels <- function(dframe=NULL,grplab=NA,selgrp=NA,subsetgrp=NA,ordergrp=FALSE,grpmean=FALSE,grplabpos=NA,linepos=NA,indlabwithgrplab=TRUE,indlabsep="_",runid=NULL,corder=NA)
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

  #check length
  if(nrow(dframe) != nrow(grplab)) stop(paste0("grpLabels: Number of individuals (",nrow(dframe),") differ from number of labels in 'grplab' (",nrow(grplab),")."))
  #if(is.factor(grplab)) grplablev <- levels(grplab)
  #if(is.character(grplab)) grplablev <- factor(grplab,levels=c(rle(grplab)$values))

  #concatenate grp labs to ind labs
  if(indlabwithgrplab) rownames(dframe) <- apply(as.data.frame(list(list(rn=rownames(dframe)),as.list(grplab))),1,paste,collapse=indlabsep)

  gnames <- names(grplab)
  onames <- setdiff(gnames,selgrp)

  # get original order
  if(any(is.na(corder))) corder <- 1:nrow(dframe)
  if(length(corder)!=nrow(dframe)) stop("grpLabels: Length of 'corder' not equal to number of individuals.")

  # order groups
  # orders dframe and all labels by selgrp
  if(ordergrp)
  {
    dfwork1 <- cbind(dframe,grplab)
    #dfwork1 <- dfwork1[order(dfwork1[,selgrp]),]
    dfwork1$corder <- corder
    dfwork1 <- dfwork1[do.call(order,dfwork1[,c(selgrp,onames),drop=FALSE]),,drop=FALSE]

    corder <- dfwork1$corder
    dfwork1$corder <- NULL
    grplab1 <- dfwork1[,gnames,drop=FALSE]
    dfwork1[,gnames] <- NULL
  }else{
    dfwork1 <- dframe
    grplab1 <- grplab
  }

  # group mean by selgrp
  if(grpmean) dfwork1 <- as.data.frame(sapply(dfwork1,ave,unlist(grplab1[selgrp])),stringsAsFactors=FALSE)

  # in case of subsetgrp
  if(!any(is.na(subsetgrp)))
  {
    if(any(duplicated(subsetgrp))) stop(paste0("grpLabels: Argument 'subsetgrp' contains duplicate values (",paste0(subsetgrp,collapse=', '),")."))
    subsetgrp <- as.character(subsetgrp)
    subsetlabs <- as.vector(unlist(grplab1[selgrp]))
    #rle grp
    rlegrp <- rle(subsetlabs)

    # checks
    if(any(duplicated(rlegrp$values))) stop("grpLabels: Duplicated contiguous block of labels in 'selgrp'. Change grplab, selgrp or use 'ordergrp=TRUE'.")
    if(any(duplicated(rlegrp$values))) {cont <- "non-contiguous"}else{cont <- "contiguous"}
    if(!any(subsetgrp %in% rlegrp$values)) stop(paste0("grpLabels: Subset group label (",subsetgrp,") not present in group label set (",selgrp,") with following ",cont," groups: (",paste0(rlegrp$values,collapse=", "),")."))

    # compute positions and labels of subset grps
    posvec=vector()
    for(k in 1:length(subsetgrp))
    {
      sgrp=subsetgrp[k]
      pos=which(subsetlabs==sgrp)
      posvec=c(posvec,pos)
    }

    if(length(intersect(colnames(dfwork1),colnames(grplab1)))!=0) stop(paste0("grpLabels: One or more header labels in the run file are duplicated in grplab header. Change labels to be unique. Following are the duplicate label(s): (",paste0(intersect(colnames(dfwork1),colnames(grplab1)),collapse=", "),")."))
    dfwork2 <- cbind(dfwork1,grplab1)
    dfwork2$corder <- corder
    dfwork2 <- dfwork2[posvec,,drop=FALSE]
    corder <- dfwork2$corder
    dfwork2$corder <- NULL
    dfwork1 <- dfwork2
    dfwork1[,gnames] <- NULL
    grplab1 <- dfwork2[,gnames,drop=FALSE]
  }

  # create label_position and marker_position
  marker_position_list <- vector("list",length=length(gnames))
  label_position_list <- vector("list",length=length(gnames))
  intlablist <- vector("list",length=length(gnames))
  for(k in seq_along(gnames))
  {
    rlegrp <- rle(unlist(grplab1[gnames[k]]))
    label_position_df <- data.frame(label=rlegrp$values,freq=rlegrp$lengths,stringsAsFactors=FALSE)
    marker_position_df <- data.frame(markerxpos=c(0,cumsum(label_position_df$freq)),stringsAsFactors=FALSE)
    label_position_df$labxpos <- round((diff(marker_position_df$markerxpos)/2)+marker_position_df$markerxpos[1:length(marker_position_df$markerxpos)-1],1)
    label_position_df$labypos <- rep(grplabpos,nrow(label_position_df))
    #marker_position_df$temp <- factor(rep(1,nrow(marker_position_df)))
    marker_position_df$markerypos <- rep(linepos,nrow(marker_position_df))

    marker_position_df$count <- gnames[k]
    marker_position_list[[k]] <- marker_position_df
    label_position_df$count <- gnames[k]
    label_position_list[[k]] <- label_position_df
  }

  marker_position <- do.call("rbind",marker_position_list)
  marker_position$count <- factor(marker_position$count,levels=gnames)
  label_position <- do.call("rbind",label_position_list)
  label_position$count <- factor(label_position$count,levels=gnames)

  rownames(marker_position) <- 1:nrow(marker_position)
  rownames(label_position) <- 1:nrow(label_position)

  # adjust divider position
  marker_position$divxpos <- marker_position$markerxpos+0.5
  # add runid
  marker_position$run <- runid

  return(list(dframe=dfwork1,grplab=grplab1,label_position=label_position,
              marker_position=marker_position,corder=corder))
}

# sortInd ----------------------------------------------------------------------

#' @title Internal: Handles individual sorting
#' @description Internal: Handles individual sorting
#' @param dframe A q-matrix dataframe
#' @param grplab A dataframe with group labels
#' @param selgrp A single character denoting selected group label set.
#' See details.
#' @param ordergrp A logical indicating if individuals must be ordered by all
#' group labels
#' @param sortind A character indicating how individuals are sorted. Default is
#' NA (Same order of individuals as in input file). Other options are 'all'
#' (sorting by values of all clusters), by any one cluster (eg. 'Cluster1') or
#' 'labels' (sorting by individual labels).
#' @param grplabpos A numeric indicating y-axis position of labels
#' @param linepos A numeric indicating y-axis position of label line
#' @param corder Current order of individuals
#' @return Returns a list with ordered q-matrix dataframe and ordered grplab.
#' @details When multiple group label sets are in use, \code{selgrp} defines
#' which group label set is used for group ordering (\code{ordergrp}),
#' subsetting (\code{subsetgrp}) and group mean (\code{grpmean}). \code{selgrp}
#' is also used for plotting divider lines and and sorting (\code{sortind}).
#' If \code{selgrp} is not specified, the first group label set is used by
#' default.
#' @noRd
#' @keywords internal
#'
sortInd <- function(dframe=NULL,grplab=NA,selgrp=NA,ordergrp=FALSE,sortind=NA,grplabpos=NA,linepos=NA,corder=NA)
{
  if(is.null(dframe)) stop("sortInd: Argument 'dframe' is empty.")
  if(!all(is.na(grplab)))
  {
    if(is.na(selgrp)) selgrp <- names(grplab)[1]
    if(!is.character(selgrp)) stop("sortInd: Argument 'selgrp' must be a character datatype.")
    if(length(selgrp)>1) stop("sortInd: Argument 'selgrp' must be a character datatype of length 1.")
    if(!any(selgrp %in% names(grplab))) stop(paste0("sortInd: Argument 'selgrp' contains (",selgrp,") which is not in the 'grplab' titles (",paste0(names(grplab),collapse=", "),")."))
  }
  if(all(!is.na(sortind)))
  {
    if(length(sortind) > 1) stop("sortInd: Argument 'sortind' must be of length 1. Use 'all','label' or a cluster name like 'Cluster1'.")
    #if(sortind != "all" && sortind != "label" && !grepl("Cluster[0-9+]",sortind)) stop("sortInd: Argument 'sortind' must be set to 'all', 'label' or a cluster like 'Cluster1'.")
  }
  if(is.na(grplabpos)) grplabpos <- 0.25
  if(is.na(linepos)) linepos <- 0.75

  # get original order
  if(any(is.na(corder))) corder <- 1:nrow(dframe)
  if(length(corder)!=nrow(dframe)) stop("grpLabels: Length of 'corder' not equal to number of individuals.")
  # sorting without grplab
  if(any(is.na(grplab)))
  {
    if(!is.na(sortind))
    {
      if(sortind=="all")
      {
        dftemp <- dframe
        dftemp$maxval <- as.numeric(apply(dframe,1,max))
        dftemp$matchval <- as.numeric(apply(dframe,1,FUN=function(x) match(max(x),x)))
        dframe$corder <- corder
        dframe <- dframe[with(dftemp,order(matchval,-maxval)),,drop=FALSE]
        corder <- dframe$corder
        dframe$corder <- NULL
        rm(dftemp)
      }

      if(sortind=="label")
      {
        dframe$corder <- corder
        dframe <- dframe[order(rownames(dframe)),,drop=FALSE]
        corder <- dframe$corder
        dframe$corder <- NULL
      }

      if(sortind!="all" && sortind!="label")
      {
        if(!(sortind %in% colnames(dframe))) stop(paste0("sortInd: 'sortind' value (",sortind,") not found in file header (",paste0(colnames(dframe),collapse=", "),")."))
        dframe$corder <- corder
        dframe <- dframe[order(dframe[[sortind]]),,drop=FALSE]
        corder <- dframe$corder
        dframe$corder <- NULL
      }

    }
    label_position <- NA
    marker_position <- NA
  }

  # sorting with grplab
  if(!all(is.na(grplab)))
  {
    gnames <- names(grplab)
    onames <- setdiff(gnames,selgrp)

    if(!is.na(sortind))
    {
      # sort by all
      if(sortind=="all")
      {
        dftemp <- dframe
        # find the max value for each individual
        dftemp$maxval <- as.numeric(apply(dframe,1,max))
        # pick cluster with max value
        dftemp$matchval <- as.numeric(apply(dframe,1,FUN=function(x) match(max(x),x)))
        dftemp$corder <- corder

        if(length(intersect(colnames(dftemp),colnames(grplab)))!=0) stop(paste0("sortInd: One or more header labels in the run file are duplicated in grplab header. Change labels to be unique. Following are the duplicate label(s): (",paste0(intersect(colnames(dftemp),colnames(grplab)),collapse=", "),")."))
        dftemp <- cbind(dftemp,grplab)

        if(ordergrp)
        {
          sort_asc <- c(selgrp,onames,"matchval")
          sort_desc <- "maxval"
          dframe <- dftemp[do.call(order,c(as.list(dftemp[sort_asc]),lapply(dftemp[sort_desc],function(x) -xtfrm(x)))),]
        }else{
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
        }

        corder <- dframe$corder
        dframe$corder <- NULL
        grplab <- dframe[,gnames,drop=FALSE]
        dframe[,gnames] <- NULL
        dframe$maxval <- NULL
        dframe$matchval <- NULL
      }

      # sort by label
      if(sortind=="label")
      {
        if(length(intersect(colnames(dframe),colnames(grplab)))!=0) stop(paste0("sortInd: One or more header labels in the run file are duplicated in grplab header. Change labels to be unique. Following are the duplicate label(s): (",paste0(intersect(colnames(dframe),colnames(grplab)),collapse=", "),")."))
        dftemp <- cbind(dframe,grplab)
        dftemp$corder <- corder

        if(ordergrp)
        {
          dftemp$label <- rownames(dftemp)
          sort_asc <- c(selgrp,onames,"label")
          dframe <- dftemp[do.call(order,dftemp[,sort_asc]),]
          dframe$label <- NULL
        }else{
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
        }

        corder <- dframe$corder
        dframe$corder <- NULL
        grplab <- dframe[,gnames,drop=FALSE]
        dframe[,gnames] <- NULL
      }

      # sort by cluster
      if(sortind!="all" && sortind!="label")
      {
        if(!(sortind %in% colnames(dframe))) stop(paste0("sortInd: 'sortind' value (",sortind,") not found in file header (",paste0(colnames(dframe),collapse=", "),")."))
        # checks if sortind variable is a column in dframe
        if(length(intersect(colnames(dframe),colnames(grplab)))!=0) stop(paste0("sortInd: One or more header labels in the run file are duplicated in grplab header. Change labels to be unique. Following are the duplicate label(s): (",paste0(intersect(colnames(dframe),colnames(grplab)),collapse=", "),")."))
        dftemp <- cbind(dframe,grplab)
        dftemp$corder <- corder

        if(ordergrp)
        {
          sort_asc <- c(selgrp,onames,sortind)
          dframe <- dftemp[do.call(order,dftemp[,sort_asc]),]
        }else{
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
        }

        corder <- dframe$corder
        dframe$corder <- NULL
        grplab <- dframe[,gnames,drop=FALSE]
        dframe[,gnames] <- NULL
      }
    }

    # create label_position and marker_position
    gnames <- names(grplab)
    marker_position_list <- vector("list",length=length(gnames))
    label_position_list <- vector("list",length=length(gnames))
    intlablist <- vector("list",length=length(gnames))
    for(k in seq_along(gnames))
    {
      rlegrp <- rle(unlist(grplab[gnames[k]]))
      label_position_df <- data.frame(label=rlegrp$values,freq=rlegrp$lengths,stringsAsFactors=FALSE)
      marker_position_df <- data.frame(markerxpos=c(0,cumsum(label_position_df$freq)),stringsAsFactors=FALSE)
      label_position_df$labxpos <- round((diff(marker_position_df$markerxpos)/2)+marker_position_df$markerxpos[1:length(marker_position_df$markerxpos)-1],1)
      label_position_df$labypos <- rep(grplabpos,nrow(label_position_df))
      #marker_position_df$temp <- factor(rep(1,nrow(marker_position_df)))
      marker_position_df$markerypos <- rep(linepos,nrow(marker_position_df))

      marker_position_df$count <- gnames[k]
      marker_position_list[[k]] <- marker_position_df
      label_position_df$count <- gnames[k]
      label_position_list[[k]] <- label_position_df
    }

    marker_position <- do.call("rbind",marker_position_list)
    marker_position$count <- factor(marker_position$count,levels=gnames)
    label_position <- do.call("rbind",label_position_list)
    label_position$count <- factor(label_position$count,levels=gnames)

    rownames(marker_position) <- 1:nrow(marker_position)
    rownames(label_position) <- 1:nrow(label_position)

    #adjust divider position
    marker_position$divxpos <- marker_position$markerxpos+0.5
  }



  return(list(dframe=dframe,grplab=grplab,label_position=label_position,
              marker_position=marker_position,corder=corder))
}

# plotQ ---------------------------------------------------------------------

#' @title Generate barplots from qlists.
#' @description Generate separate or joined barplots (group-level) from qlists.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param imgoutput A character with options: 'sep' or 'join'.If set to "sep",
#' each run is plotted as separate image file. If set to "join", multiple runs
#' are joined into a single image.
#' @param clustercol A vector of colours for clusters. If NA, colours are
#' automatically generated. K=1 to K=12 are custom unique colours while K>12
#' are coloured by function \code{gplots::rich.colors()}.
#' @param sortind A character indicating how individuals are sorted. Default
#' is NA (Same order of individuals as in input file). Other options are 'all'
#' (sorting by values of all clusters), by any one cluster (eg. 'Cluster1') or
#' 'label' (sorting by individual labels). See details.
#' @param grplab A dataframe with one or more columns (group label sets), and
#' rows equal to the number of individuals. See details.
#' @param selgrp A single character denoting a selected group label set. The
#' selected label must be a group label title used in \code{grplab}. See details.
#' @param ordergrp A logical indicating if individuals must be grouped into
#' contiguous blocks based on \code{grplab} starting with \code{selgrp}.
#' @param subsetgrp A character or character vector with group names to subset
#' or reorder groups. Only applicable when \code{grplab} is in use. Default is
#' NA. See details.
#' @param grpmean A logical indicating if q-matrix must be converted from
#' individual values to group mean values. Applicable only when \code{grplab} is
#' in use and mean is calculated over \code{selgrp}.
#' @param showgrplab A logical indicating if group labels must be displayed. Only
#' applicable when \code{grplab} is in use. Defaults to TRUE.
#' @param panelspacer A numeric indicating the spacing between barplot panels in
#' cm. Defaults to 0.06cm.
#' @param showsp A logical indicating if strip panels on right side must be
#' shown. Strip panel by default displays file name and K value. Defaults to
#' TRUE.
#' @param sppos A character indicating position of strip panel. One of 'right'
#' or 'left'. Defaults to 'right'.
#' @param splab A character or character vector denoting items displayed in the
#' strip panels. Length must be equal to number of runs.
#' @param splabsize A numeric indicating the size of the strip panel label.
#' Computed automatically when set to NULL. Note that overall text size can be
#' controlled using \code{basesize}.
#' @param splabangle A numeric indicating angle/rotation of the strip panel
#' label. Defaults to NULL. Automatically set to -90.
#' @param splabcol A character indicating the colour of the strip panel label.
#' Defaults to "grey30".
#' @param splabface A character indicating the font face of strip panel label.
#' One of 'plain', 'italic', 'bold' or 'bold.italic'. Defaults to 'plain'.
#' Applicable only when \code{showsp=TRUE}.
#' @param spbgcol A character denoting the background colour of the strip panel.
#' Defaults to white.
#' @param showtitle A logical indicating if plot title must be shown on the top.
#' Defaults to FALSE. If TRUE and \code{titlelab=NA}, file name is displayed by
#' default.
#' @param titlelab A character or character vector for title text. Defaults to
#' NA, and when \code{showtitle=TRUE} displays file name.
#' @param titlehjust A numeric denoting the horizontal justification of the
#' title. Defaults to 0 (left).
#' @param titlevjust A numeric denoting the vertical justification of the title.
#' Defaults to 0.5 (center).
#' @param titlesize A numeric indicating the size of the title text. Computed
#' automatically when set to NULL. Note that overall text size can be controlled
#' using \code{basesize}.
#' @param titlecol A colour character for title. Defaults to "grey30".
#' @param titleface A character indicating the font face of title label. One of
#' 'plain', 'italic', 'bold' or 'bold.italic'. Defaults to 'plain'. Applicable
#' only when \code{showtitle=TRUE}.
#' @param titlespacer A numeric indicating the space below the title. Defaults
#' to 1.2.
#' @param titleangle A numeric indicating the angle/rotation of the title.
#' Defaults to 0.
#' @param showsubtitle A logical indicating if plot subtitle must be shown on
#' the top. Defaults to FALSE. If TRUE and \code{subtitlelab=NA}, file name is
#' displayed by default.
#' @param subtitlelab A character or character vector for subtitle text.
#' Defaults to NA, and when \code{showsubtitle=TRUE} displays file name.
#' @param subtitlehjust A numeric denoting the horizontal justification of the
#' subtitle. Defaults to 0 (left).
#' @param subtitlevjust A numeric denoting the vertical justification of the
#' subtitle. Defaults to 0.5 (center).
#' @param subtitlesize A numeric indicating the size of the subtitle text.
#' Computed automatically when set to NULL. Note that overall text size can be
#' controlled using \code{basesize}.
#' @param subtitlecol A colour character for subtitle. Defaults to "grey30".
#' @param subtitleface A character indicating the font face of subtitle label.
#' One of 'plain', 'italic', 'bold' or 'bold.italic'. Defaults to 'plain'.
#' Applicable only when \code{showsubtitle=TRUE}.
#' @param subtitlespacer A numeric indicating the space below the subtitle.
#' Defaults to 1.2.
#' @param subtitleangle A numeric indicating the angle/rotation of the subtitle.
#' Defaults to 0.
#' @param grplabspacer A numeric indicating the space between the plot panels
#' and the group label area in cm. Defaults to 0cm. Applicable only when
#' \code{grplab} are in use.
#' @param grplabheight A numeric indicating the height of the group label area
#' in cm. Defaults to 0.4cm. Multiple group sets are multiplied by 0.4.
#' Applicable only with \code{grplab}. See details.
#' @param grplabpos A numeric indicating the y position of the group labels.
#' Applicable only with group labels. Defaults to 0.
#' @param grplabsize A numeric indicating the size of the group labels. Default
#' range between 1.5 - 2.5 depending on number of individuals. This text size is
#' not affected by \code{basesize}.
#' @param grplabangle A numeric indicating the angle/rotation of group labels.
#' 0 is horizontal while 90 is vertical. Default is 0.
#' @param grplabjust A numeric indicating the justification of group labels.
#' Defaults to 0.5 if grplabangle=0  or 1 if grplabangle between 20 and 135.
#' @param grplabcol A colour character for the colour of group labels. Defaults
#' to "grey30".
#' @param grplabalpha A numeric between 0 and 1 denoting transparency of group
#' labels. Defaults to 1.
#' @param grplabface A character specifying font face. Either 'plain', 'italic',
#' 'bold' or 'bold.italic'.
#' @param showindlab A logical indicating if individual labels must be shown.
#' See details.
#' @param sharedindlab A logical indicating if only one set of shared individual
#' labels must be shown below all plots. Applicable only when
#' \code{imgoutput="join"}. Individual labels are visible only when
#' \code{showindlab=TRUE}.
#' @param useindlab A logical indicating if individual labels must be read from
#' the rownames of qlist dataframes and used as individual labels. See details.
#' @param indlabwithgrplab A logical indicating if individual labels must be
#' concatenated with grplab. Applies only when grplab is in use. Relevant for
#' sorting by label.
#' @param indlabsep A character used as separator when concatenating individual
#' labels and group labels. Defaults to space \code{indlabsep=" "}.
#' @param indlabheight A numeric indicating space below the individual label
#' panel. Increase to 0.1, 0.2 etc if labels are clipped off.
#' @param indlabsize A numeric indicating the size of the individual labels.
#' Computed automatically when set to NULL.  Note that overall text size can be
#' controlled using \code{basesize}.
#' @param indlabangle A numeric indicating the angle/rotation of individual
#' labels. 0 is horizontal while 90 is vertical. Defaults to 90.
#' @param indlabvjust A numeric denoting vertical justification of the
#' individual labels. Defaults to 0.5.
#' @param indlabhjust A numeric denoting the horizontal justification of the
#' individual labels. Defaults to 1.
#' @param indlabcol A colour character for the colour of individual labels.
#' Defaults to "grey30".
#' @param indlabspacer A numeric denoting space between the individual label
#' and the plot area. Default set to 0.
#' @param pointsize A numeric indicating the size of points on label marker
#' line. Default range between 1.2 - 3.2 depending on number of individuals.
#' @param pointcol A colour character for the colour of points on the label
#' marker line. Defaults to "grey30".
#' @param pointbgcol A colour character for the background of marker point for
#' certain point types.
#' @param pointtype A character or number for the type of points on the label
#' marker line. Defaults to |. Same as pch in standard R.
#' @param pointalpha A numeric between 0 and 1 denoting transparency of the
#' points. Defaults to 1.
#' @param linepos A numeric indicating the y position of the label marker line
#' and the points. Applicable only with group labels. Defaults to 1.
#' @param linesize A numeric indicating the thickness of the label marker line.
#' Default range between 0.3 and 0.6 depending on number of individuals.
#' @param linecol A colour character for the label marker line. Defaults to
#' "grey30".
#' @param linetype A numeric indicating the type of line for marker line. Same
#' as lty in standard R. Default value is 1.
#' @param linealpha A numeric between 0 and 1 denoting transparency of the
#' marker line. Defaults to 1.
#' @param showdiv A logical indicating if divider lines between groups must be
#' drawn. Applicable only when group labels are in use.
#' @param divgrp A character or character vector with one or more group label
#' titles denoting which groups are used to draw divider lines. This must be a
#' group label title used in \code{grplab}. If not provided, the value in
#' \code{selgrp} is used by default.
#' @param divcol A character or hexadecimal colour denoting the colour of the
#' divider line. Default is white.
#' @param divtype A numeric indicating the type of line for the divider line.
#' Same as lty in standard R. Default value is '21'.
#' @param divsize A numeric indicating the thickness of the divider line.
#' Default is 0.25.
#' @param divalpha A numeric between 0 and 1 denoting transparency of the
#' divider line. Defaults to 1.
#' @param showlegend A logical indicating if legend denoting cluster colours
#' must be plotted. Defaults to FALSE.
#' @param legendlab A character or character vector to for legend cluster
#' labels. Must be equal to max number of clusters.
#' @param legendpos A character 'right' or 'left' denoting position of the
#' legend. Defaults to 'left'.
#' @param legendkeysize A numeric indicating size of the legend key. Defaults
#' to 4.
#' @param legendtextsize A numeric indicating size of the legend text. Defaults
#' to 3.
#' @param legendspacing Spacing between legend items.
#' @param legendrow Number of rows of legend.
#' @param legendmargin A numeric vector of length 4 indicating top, right,
#' bottom and left margins of the legend.
#' @param barsize A numeric indicating the width of the bars. Defaults to 1.
#' @param barbordersize A numeric indicating border size of bars. Defaults to 0.
#' Visible only when \code{barbordercolour} is not NA.
#' @param barbordercolour A single colour for bar border. Defaults to NA.
#' Visible only when \code{barbordersize} is larger than zero and set to a
#' colour other than NA.
#' @param showyaxis A logical indicating if y-axis labels should be displayed
#' or not. Defaults to FALSE. Y-axis size is same as \code{indlabsize}.
#' @param showticks A logical indicating if ticks on axis should be displayed
#' or not. Defaults to FALSE. Applies to x and y axis. Y-axis ticks are visible
#' only when \code{showyaxis=TRUE}. Tick colour is same as \code{indlabcol}.
#' @param ticksize A numeric indicating size of ticks. Defaults to 0.2. Applies
#' to both x and y axis.
#' @param ticklength A numeric indicating length of tick marks in cm. Defaults
#' to 0.03. Applies to both x and y axis.
#' @param outputfilename A character or character vector denoting output file
#' name without file extension. See details.
#' @param imgtype A character indicating output image file type. Possible
#' options are "png","jpeg","tiff" or "pdf".
#' @param height A numeric indicating the height of a single run panel. By
#' default, automatically generated based on number of runs. Separate plots use
#' 1.8cm and joined plots use 1.2cm for single panel. See details.
#' @param width A numeric indicating the width of the whole plot. By default,
#' automatically generated based on number of individuals. Ranges between 5cm
#' and 30cm.
#' @param dpi A numeric indicating the image resolution in pixels per inch
#' (PPI). Defaults to 300. If \code{imgtype="pdf"}, dpi is fixed at 300.
#' @param units A numeric indicating the units of height and width. Default set
#' to "cm". Other options are 'px', 'in' or 'mm'.
#' @param theme A character indicating ggplot theme to be used. Use like
#' "theme_grey", "theme_bw" etc.
#' @param basesize A numeric indicating overall text size. Defaults to 5
#' suitable for export. Set to 11 for returned plot.
#' @param font A character indicating font family to be used in the plots. Uses
#' default system fonts by default for jpeg, png and tiff. Uses 'Helvetica' as
#' default for pdf. Use package \code{extrafonts} to import custom fonts. See
#' vignette for examples.
#' @param na.rm A logical indicating if NAs are removed from data, else
#' \code{ggplot} prints warning messages for NAs. If set to TRUE, NAs are
#' removed before plotting and \code{ggplot} NA warning is suppressed.
#' @param panelratio A two value integer vector denoting ratio of plot panel to
#' grplab panel. Defaults to \code{c(3,1)}. Applicable only when \code{grplab}
#' is in use.
#' @param exportplot A logical indicating if a plot image must be exported into
#' the working directory.
#' @param returnplot A logical indicating if ggplot plot objects must be
#' returned. See 'Value'.
#' @param returndata A logical indicating if processed data must be returned.
#' See 'Value'.
#' @param exportpath A path to where content must be exported. For example,
#' \code{exportpath="./dir/anotherdir"}. To use the current working directory, set \code{path=getwd()}.
#' directory.
#' @return When \code{returnplot=TRUE}, plot object(s) are returned. When
#' \code{grplab=NA},
#' a ggplot2 object is returned. When \code{grplab} is in use, a gtable (output
#' from gridExtra::arrangeGrob())
#' list is returned. When \code{returndata=TRUE}, the input qlist is modified
#' (sorted, subsetted etc)
#' and returned. If \code{grplab} is in use, a list of modified qlist and grplab
#' is returned.
#' If \code{returnplot=TRUE} and \code{returndata=TRUE} are both set, then a
#' named list
#' (plot,data) is returned. The plot item contains the ggplot2 object or gtable
#' and the data contains qlist (and grplab).
#'
#' @details
#'
#' \strong{sortind}\cr
#' This argument takes one character as input.  Default NA means individuals are
#' plotted in the same order as input. Individuals can be ordered by any one
#' cluster. For ex. \code{sortind="Cluster1"} or \code{sortind="Cluster2"}.
#' To order by all clusters as the 'Sort by Q' option in STRUCTURE software,
#' use \code{sortind="all"}. When using \code{sortind="label"}, individuals are
#' sorted by individual labels (and within groups if \code{grplab} is in use). Individual
#' labels can be displayed using \code{showindlab=TRUE}. When using \code{sortind}
#' with \code{grplab}, individuals are sorted within the groups.\cr
#'
#' \strong{grplab}\cr
#' \code{grplab} must be a data.frame. One or more label sets can be provided.
#' Each label set must be a character vector equal to the number of individuals
#' present in the \code{qlist}.
#' For example, we can provide one group label set as such:\cr
#' \code{grplab=data.frame(labs=c("Grp A","Grp A","Grp B","Grp B"),
#' stringsAsFactors=FALSE)}\cr
#'
#' Two group label sets can be provided as such:\cr
#' \code{grplab=data.frame(labs=c("Grp A","Grp A","Grp B","Grp B"),loc=
#' c("Loc 1","Loc 2","Loc 2","Loc 2"),stringsAsFactors=FALSE)}\cr
#'
#' \strong{selgrp}\cr
#' When multiple group label sets are in use, \code{selgrp} defines which
#' group label set is used for group ordering ( \code{ordergrp} ), subsetting
#' ( \code{subsetgrp} ) and group mean ( \code{grpmean} ). \code{selgrp} is also
#' used for plotting divider lines and and sorting ( \code{sortind} ). If
#' \code{selgrp} is not specified, the first group label set is used by
#' default.\cr
#'
#' \strong{ordergrp}\cr
#' When using \code{grplab}, labels may not be in contiguous blocks. Using
#' \code{ordergrp=TRUE}, regroups individuals into contiguous blocks for all
#' group label sets starting with \code{selgrp}.\cr
#'
#' \strong{subsetgrp}\cr
#' This argument takes one or more characters as input. Use only group labels
#' used in one of the group label sets in \code{grplab}. For ex. In case of a
#' group label set 'labs' with two grps in order 'Grp A' and 'Grp B', use
#' \code{subsetgrp=c("Grp B","Grp A")} to change order of groups. Use
#' \code{subsetgrp="Grp B"} to subset only Grp B. When using multiple group
#' label sets, use \code{selgrp} to declare which group label set to subset.\cr
#'
#' \strong{outputfilename}\cr
#' Default is \code{outputfilename=NA} which means that output file names are
#' automatically generated. When \code{imgoutput="sep"}, the names of the qlist
#' are used to create output labels. When \code{imgoutput="join"}, one output
#' label is created for all input files in this format:
#' JoinedNFiles-YYYYMMDDHHMMSS, where N stands for number of runs joined, and
#' the ending stands for current system date and time. If \code{outputfilename}
#' is provided, when \code{imgoutput="sep"}, \code{outputfilename} must be a
#' character vector equal to length of input runs. When \code{imgoutput="join"},
#' \code{outputfilename} must be a character of length one. File extensions
#' like .png etc must not be provided.\cr
#'
#' \strong{height}\cr
#' Argument \code{height} denotes the height of one run panel. With joined
#' plots, the \code{height} is multiplied by number of runs. The height does
#' not include label panel. The \code{grplabheight} is used to define the full
#' height of the lab panel. If \code{grplabheight} is not provided, it is
#' calculated based on the number of group label sets.\cr
#' \code{final_image_height = (height*num_of_runs)+grplabheight}\cr
#' It is possible to set either height or width and leave other as default.\cr
#'
#' \strong{indlab}\cr
#' When \code{showindlab=TRUE}, individual labels are shown/displayed. When
#' \code{showindlab=FALSE},
#' individual labels are not shown/displayed on the graph, although they are
#' present in the underlying data. Therefore, \code{showindlab} only control
#' display of labels on the plot and nothing to do with label control in the
#' data.\cr
#' The default \code{useindlab=FALSE}, creates labels numerically in the
#' original order of data but with zero padding. For example, if there are 10
#' individuals, labels are 01, 02 up to 10. if there are 100 individuals, then
#' labels are 001, 002 up to 100. Zero padding to ensure optimal sorting. When
#' \code{useindlab=TRUE}, labels are used from rownames of qlist dataframes. They
#' are usually labelled 1,2,3.. if read in using \code{readQ()}. This can be an
#' issue with sorting by labels \code{sortind="label"}.For STRUCTURE files with
#' individual labels, they can be read in automatically using
#' \code{readQ(indlabfromfile=TRUE)}.\cr
#' When group labels are in use, \code{grplab}, they are added to the individual
#' labels in both cases \code{useindlab=TRUE} and \code{useindlab=FALSE} separated by
#' \code{indlabsep}. Default \code{indlabsep=" "} adds a space between
#' individual label and grplab. For example, group labels 'popA', 'popA'...
#' will be '01 popA', '02 popA'... when \code{useindlab=FALSE} and usually '1 popA',
#' '2 popA'... when \code{useindlab=TRUE}. When multiple group labels are in use,
#' the are similarly concatenated one after the other to individual names in the
#' order in which the group labels were provided.
#'
#' See the \href{http://royfrancis.github.io/pophelper/articles/index.html#plotq}{vignette} for more details and examples.
#'
#' @seealso \code{\link{plotQMultiline}}
#' @examples
#' \dontrun{
#' slist <- readQ(list.files(path=system.file("files/structure",
#' package="pophelper"),full.names=TRUE))
#'
#' # plot one separate figure
#' plotQ(qlist=slist[1],exportpath=getwd())
#'
#' # plot two separate figures
#' plotQ(qlist=slist[1:2],exportpath=getwd())
#'
#' # plot a joined figure with multiple plots
#' plotQ(qlist=slist[1:2],imgoutput="join",exportpath=getwd())
#'
#' # sort individuals
#' plotQ(qlist=slist[c(1,3)],sortind="all",exportpath=getwd())
#' plotQ(qlist=slist[c(1,3)],sortind="Cluster1",exportpath=getwd())
#' plotQ(qlist=slist[c(1,3)],sortind="label",exportpath=getwd())
#' plotQ(qlist=slist[c(1,3)],sortind="all",imgoutput="join",sharedindlab=FALSE,exportpath=getwd())
#'
#' # read group labels
#' md <- read.delim(system.file("files/metadata.txt", package="pophelper"),
#' header=TRUE,stringsAsFactors=FALSE)
#'
#' # plot with one group label set
#' plotQ(qlist=slist[1],grplab=md[,2,drop=FALSE],exportpath=getwd())
#' plotQ(qlist=slist[1:2],grplab=md[,2,drop=FALSE],imgoutput="join",exportpath=getwd())
#'
#' # sort within groups
#' plotQ(qlist=slist[1:2],grplab=md[,2,drop=FALSE],imgoutput="join",sortind="all",
#' sharedindlab=FALSE,exportpath=getwd())
#' plotQ(qlist=slist[1:2],grplab=md[,2,drop=FALSE],imgoutput="join",
#' sortind="Cluster1",sharedindlab=FALSE,exportpath=getwd())
#' plotQ(qlist=slist[1:2],grplab=md[,2,drop=FALSE],imgoutput="join",sortind="label",exportpath=getwd())
#'
#' # reorder groups
#' plotQ(qlist=slist[1],grplab=md[,2,drop=FALSE],subsetgrp=c("CatB","CatA"),exportpath=getwd())
#'
#' # multiple group label sets and ordergrp
#' plotQ(qlist=slist[1],grplab=md,ordergrp=TRUE,exportpath=getwd())
#' plotQ(qlist=slist[1:2],grplab=md,ordergrp=TRUE,imgoutput="join",exportpath=getwd())
#'
#' # sort in second label group set cat
#' plotQ(qlist=slist[1],grplab=md,selgrp="cat",sortind="all",exportpath=getwd())
#'
#' # use default individual labels
#' plotQ(slist[1],showindlab=TRUE,width=15,exportpath=getwd())
#'
#' # use custom individual labels
#' inds <- read.delim(system.file("files/structureindlabels.txt",
#' package="pophelper"),header=FALSE,stringsAsFactors=FALSE)
#' rownames(slist[[1]]) <- inds$V1
#' plotQ(slist[1],showindlab=TRUE,useindlab=TRUE,width=15,exportpath=getwd())
#'
#' # change cluster colours
#' plotQ(slist[1],clustercol=c("steelblue","coral"),exportpath=getwd())
#'
#' # plot a custom dataframe
#' temp <- list("custom"=data.frame(Cluster1=c(0.2,0.3,0.6,0.8),
#' Cluster2=c(0.8,0.7,0.4,0.2)))
#' plotQ(temp)
#' }
#' @importFrom ggplot2 ggplot aes geom_bar geom_vline geom_blank geom_text geom_line geom_point scale_x_continuous scale_y_continuous scale_fill_manual guides guide_legend facet_wrap labeller labs element_blank element_text element_line element_rect unit margin ggplotGrob theme theme_grey
#' @importFrom grDevices png pdf jpeg tiff dev.off
#' @importFrom grid unit.pmax
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom tidyr pivot_longer
#' @importFrom utils packageDescription
#' @export
#'
plotQ <- function(qlist=NULL,imgoutput="sep",clustercol=NA,sortind=NA,grplab=NA,selgrp=NA,ordergrp=FALSE,subsetgrp=NA,grpmean=FALSE,showgrplab=TRUE,panelspacer=0.1,
                  showsp=TRUE,sppos="right",splab=NA,splabsize=NULL,splabangle=NULL,splabcol="grey30",splabface="plain",spbgcol=NA,
                  showtitle=FALSE,titlelab=NA,titlehjust=0,titlevjust=0.5,titlesize=NULL,titlecol="grey30",titleface="plain",titlespacer=1.4,titleangle=0,
                  showsubtitle=FALSE,subtitlelab=NA,subtitlehjust=0,subtitlevjust=0.5,subtitlesize=NULL,subtitlecol="grey30",subtitleface="plain",subtitlespacer=1.5,subtitleangle=0,
                  grplabspacer=0,grplabheight=NA,grplabpos=0.25,grplabsize=NA,grplabangle=NA,grplabjust=NA,grplabcol="grey30",grplabalpha=1,grplabface="plain",
                  showindlab=FALSE,sharedindlab=TRUE,useindlab=FALSE,indlabwithgrplab=FALSE,indlabspacer=1.5,indlabheight=0.2,indlabsep=" ",indlabsize=NULL,indlabangle=90,indlabvjust=0.5,indlabhjust=1,indlabcol="grey30",
                  pointsize=NA,pointcol="grey30",pointbgcol="grey30",pointtype="|",pointalpha=1,
                  linepos=0.75,linesize=NA,linecol="grey30",linetype=1,linealpha=1,
                  showdiv=TRUE,divgrp=NA,divcol="white",divtype="21",divsize=0.25,divalpha=1,
                  showlegend=FALSE,legendlab=NA,legendpos="right",legendkeysize=4,legendtextsize=3,legendspacing=2,legendrow=NULL,legendmargin=c(0.5,0.5,0.5,0),
                  barsize=1,barbordersize=0,barbordercolour=NA,
                  showyaxis=FALSE,showticks=FALSE,ticksize=0.1,ticklength=0.03,
                  outputfilename=NA,imgtype="png",height=NA,width=NA,dpi=300,units="cm",
                  theme="theme_grey",basesize=5,font="",na.rm=TRUE,panelratio=c(3,1),
                  exportplot=TRUE,returnplot=FALSE,returndata=FALSE,exportpath=NULL)
{
  # check qlist
  is.qlist(qlist)

  # check exportpath
  if(exportplot) {
    if(is.null(exportpath)) stop("plotQ: Argument 'exportpath' not set. To use current working directory, set 'exportpath=getwd()'.")
  }
  
  # check imgoutput
  imgoutput <- tolower(imgoutput)
  if(imgoutput != "sep" && imgoutput != "join") stop("plotQ: Argument 'imgoutput' set incorrectly. Set as 'sep' to export as separate plots. Set as 'join' to export as one joined plot.")

  # check imagetype
  imgtype <- tolower(imgtype)
  if(imgtype!="png" && imgtype != "pdf" && imgtype != "tiff" && imgtype != "jpeg") stop("plotQ: Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg', 'tiff' or 'pdf'.")

  # check sortind
  if(length(sortind) > 1) stop("plotQ: Argument 'sortind' must be of length 1. Use 'all','label' or a cluster name like 'Cluster1'.")
  if(is.na(sortind)) {sortindcheck <- "empty"}else{sortindcheck <- sortind}
  # if sort not label, set sharedindlab in use, error
  if(imgoutput=="join" && sortindcheck != "label" && sortindcheck != "empty" && sharedindlab) stop("plotQ: With 'joined' plots, when 'sortind' is 'all' or a cluster, 'sharedindlab' must be set to FALSE.")

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
  if(barsize<0 || barsize>1) stop("plotQ: Argument 'barsize' must be a value between 0 and 1.")
  if(is.na(basesize)) stop("plotQ: Argument 'basesize' is NA.")
  if(length(panelratio)!=2) stop("plotQ: Argument 'panelratio' must be an integer vector of length 2.")
  panelratio <- as.integer(panelratio)
  if(grplabpos > 1 || grplabpos < 0) stop("plotQ: Argument 'grplabpos' is set incorrectly. Set a numeric value between 0 and 1. To further increase space, adjust argument 'grplabheight'.")
  if(linepos > 1 || linepos < 0) stop("plotQ: Argument 'linepos' is set incorrectly. Set a numeric value between 0 and 1. To further increase space, adjust argument 'grplabheight'.")

  # ggplot version
  ggv <- as.numeric(gsub("\\.","",packageDescription("ggplot2", fields="Version")))
  if(ggv < 220) stop("plotQ: Package ggplot2 must be version 2.2.0 or above.")

  # defaults
  col3 <- "grey30"

  # check grplabels
  if(!all(is.na(grplab)))
  {
    verifyGrplab(grplab)
    grplablen <- ncol(grplab)
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

  # length of files
  flen <- length(qlist)

  # check outputfilename
  if(!all(is.na(outputfilename)))
  {
    if(exportplot) {if(imgoutput=="sep") if(length(outputfilename) != flen) stop(paste0("plotQ: Length of argument 'outputfilename' (",length(outputfilename),") is not equal to the number of runs (",flen,") when using 'imgoutput=sep'."))}
    if(imgoutput=="join") if(length(outputfilename) != 1) stop("plotQ: Argument 'outputfilename' must be of length equal to 1 when 'imgoutput=join'.")
  }

  # check titlelab
  if(!all(is.na(titlelab)))
  {
    if(imgoutput=="sep") if(length(titlelab) != flen) stop(paste0("plotQ: Length of argument 'titlelab' (",length(titlelab),") is not equal to the number of runs (",flen,") when using 'imgoutput=sep'."))
    if(imgoutput=="join") if(length(titlelab) != 1) stop("plotQ: Argument 'titlelab' must be of length equal to 1 when 'imgoutput=join'.")
  }

  # check subtitlelab
  if(!all(is.na(subtitlelab)))
  {
    if(imgoutput=="sep") if(length(subtitlelab) != flen) stop(paste0("plotQ: Length of argument 'subtitlelab' (",length(subtitlelab),") is not equal to the number of runs (",flen,") when using 'imgoutput=sep'."))
    if(imgoutput=="join") if(length(subtitlelab) != 1) stop("plotQ: Argument 'subtitlelab' must be of length equal to 1 when 'imgoutput=join'.")
  }

  ## sep + sep indlab ----------------------------------------------------------
  if(imgoutput=="sep")
  {
    if(returndata)
    {
      list_qlist <- vector("list",length=flen)
      list_grplab <- vector("list",length=flen)
    }
    if(returnplot) list_plot <- vector("list",length=flen)
    for (i in seq_along(qlist))
    {
      #output name
      fname <- names(qlist)[i]
      fname <- gsub(".txt$|.csv$|.tsv$|.meanq$|.meanQ$|.structure$","",fname)
      if(is.null(fname)) fname <- paste0("sample",i)

      # prepare outputfilename
      if(exportplot)
      {
        if(any(is.na(outputfilename))){
          outname <- fname
        }else{
          outname <- outputfilename[i]
        }
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

      # add rownames
      if(!useindlab) row.names(df1) <- sprintf(paste0("%",paste0(rep(0,nchar(nrow(df1))),collapse=""),nchar(nrow(df1)),"d"),1:nrow(df1))

      #ordering grps
      if(grplabcheck)
      {
        templist <- grpLabels(dframe=df1,grplab=grplabloop,selgrp=selgrp,
                                          subsetgrp=subsetgrp,ordergrp=ordergrp,grpmean=grpmean,
                                          grplabpos=grplabpos,linepos=linepos,
                                          indlabwithgrplab=indlabwithgrplab,
                                          indlabsep=indlabsep,runid=i)
        df1 <- templist$dframe
        grplabloop <- templist$grplab
        marker_position <- templist$marker_position
        label_position <- templist$label_position
        rm(templist)
      }

      # sorting individuals
      if(!is.na(sortind))
      {
        templist <- sortInd(dframe=df1,grplab=grplabloop,selgrp=selgrp,ordergrp=ordergrp,
                                        sortind=sortind,grplabpos=grplabpos,linepos=linepos)
        df1 <- templist$dframe
        grplabloop <- templist$grplab
        marker_position <- templist$marker_position
        label_position <- templist$label_position
        rm(templist)
      }

      # save modified data for return
      if(returndata)
      {
        list_qlist[[i]] <- df1
        names(list_qlist)[i] <- fname
        if(grplabcheck) {list_grplab[[i]] <- grplabloop}else{list_grplab[[i]] <- NA}
      }

      k <- ncol(df1)
      Ind <- nrow(df1)
      df1$ind <- as.character(rownames(df1))
      df1$run <- as.integer(rep(i,nrow(df1)))
      df1$order_ind <- seq(from=1,to=Ind)
      #df2 <- gather(df1,"variable","value",-c(ind,run,order_ind))
      df2 <- pivot_longer(data=df1,cols=colnames(df1)[c(!colnames(df1) %in% c("ind","run","order_ind"))],names_to="variable",values_to="value")
      
      # legendlab
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
      }else{
        facetnames <- splab[i]
      }

      #names of facets are run id which is i
      names(facetnames) <- i

      # get colours
      coll <- clustercol
      if(any(is.na(clustercol))) coll <- getColours(as.integer(k))
      if(length(coll) < k) stop(paste0("plotQ: Number of colours (",length(coll),") less than number of clusters (",k,")."))

      # get dimensions
      dimtemp <- getDim(ind=Ind,height=height,width=width,dpi=dpi,units=units,imgtype=imgtype,
                                    grplabheight=grplabheight,labs=length(grplabloop),plotnum=1,
                                    showindlab=showindlab)
      height1 <- as.numeric(dimtemp[1])
      width1 <- as.numeric(dimtemp[2])
      grplabheight1 <- as.numeric(dimtemp[3])
      units1 <- as.character(dimtemp[4])

      ## COMMON PLOT TOP PANEL ---------------------------------------------------
      # create plot
      gg_plot_panel <- ggplot(data=df2,aes(x=order_ind,y=value,fill=variable))+
        geom_bar(width=barsize,size=barbordersize,colour=barbordercolour,
                 stat="identity",position="fill",na.rm=na.rm)+
        scale_x_continuous(breaks=df2$order_ind,labels=df2$ind,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        scale_fill_manual(values=coll,labels=legendlab1)+
        guides(fill=guide_legend(nrow=legendrow,byrow=TRUE))+
        facet_wrap(~run,labeller=labeller(run=facetnames),strip.position=sppos,scales="fixed",nrow=1)+
        get(theme)(base_family=font,base_size=basesize)

      gg_plot_panel <- gg_plot_panel + labs(x=NULL,y=NULL)+
        theme(legend.position="top",
              legend.direction="horizontal",
              legend.title=element_blank(),
              legend.key.size=unit(legendkeysize,"points"),
              legend.text=element_text(size=legendtextsize,colour=indlabcol),
              legend.spacing.x=unit(legendspacing,"points"),
              legend.spacing=unit(0,"points"),
              legend.justification=legendpos,
              legend.margin=margin(legendmargin[1],legendmargin[2],legendmargin[3],legendmargin[4],"points"),
              legend.box.spacing=unit(1.5,"points"),
              panel.grid=element_blank(),
              panel.background=element_blank(),
              axis.text.x=element_text(size=indlabsize,colour=indlabcol,
                                       angle=indlabangle,vjust=indlabvjust,
                                       hjust=indlabhjust,margin=margin(t=indlabspacer)),
              axis.text.y=element_text(size=indlabsize,colour=indlabcol,margin=margin(r=indlabspacer)),
              axis.ticks=element_line(size=ticksize,colour=indlabcol),
              axis.ticks.length=unit(ticklength,"cm"),
              axis.line=element_blank(),
              axis.title=element_blank(),
              strip.text.y=element_text(size=splabsize,colour=splabcol,face=splabface,angle=splabangle),
              strip.background=element_rect(colour=spbgcol,fill=spbgcol),
              plot.margin=unit(c(0.2,0.05,indlabheight,0),"cm"),
              panel.spacing=unit(panelspacer,"cm"))

      # remove indlab
      if(!showindlab) gg_plot_panel <- gg_plot_panel+theme(axis.text.x=element_blank())

      # remove y-axis labels
      if(!showyaxis)
      {
        gg_plot_panel <- gg_plot_panel+theme(axis.ticks.y=element_blank(),
                                             axis.text.y=element_blank(),
                                             plot.margin=unit(c(0.2,0.05,indlabheight,0),"cm"))
      }else{
        gg_plot_panel <- gg_plot_panel+theme(plot.margin=unit(c(0.2,0.05,indlabheight,0.1),"cm"))
      }

      # remove ticks
      if(!showticks) gg_plot_panel <- gg_plot_panel+theme(axis.ticks=element_blank())

      # show title
      if(showtitle) gg_plot_panel <- gg_plot_panel+labs(title=titlename)+
        theme(plot.title=element_text(size=titlesize,colour=titlecol,
                                      angle=titleangle,hjust=titlehjust,face=titleface,
                                      vjust=titlevjust,margin=margin(b=titlespacer)))
      # show subtitle
      if(showsubtitle) gg_plot_panel <- gg_plot_panel+labs(subtitle=subtitlename)+
        theme(plot.subtitle=element_text(size=subtitlesize,colour=subtitlecol,
                                         angle=subtitleangle,hjust=subtitlehjust,face=subtitleface,
                                         vjust=subtitlevjust,margin=margin(b=subtitlespacer)))

      # remove strip panels on right
      if(!showsp) gg_plot_panel <- gg_plot_panel+theme(strip.text=element_blank())

      # remove legend
      if(!showlegend) gg_plot_panel <- gg_plot_panel+theme(legend.position="none")

      if(!grplabcheck)
      {
        if(exportplot)
        {
          message("Drawing plot ...")
          if(imgtype=="tiff") tiff(file.path(exportpath,paste0(outname,".tiff")),height=height1,width=width1,res=dpi,units=units1,compression="lzw",family=font)
          if(imgtype=="png") png(file.path(exportpath,paste0(outname,".png")),height=height1,width=width1,res=dpi,units=units1,family=font)
          if(imgtype=="jpeg") jpeg(file.path(exportpath,paste0(outname,".jpg")),height=height1,width=width1,res=dpi,units=units1,quality=100,family=font)
          if(imgtype=="pdf") pdf(file.path(exportpath,paste0(outname,".pdf")),height=height1,width=width1,fonts=font)
          print(gg_plot_panel)
          dev.off()
          if(imgtype=="tiff") message(file.path(exportpath,paste0(outname,".tiff exported.")))
          if(imgtype=="png") message(file.path(exportpath,paste0(outname,".png exported.")))
          if(imgtype=="jpeg") message(file.path(exportpath,paste0(outname,".jpg exported.")))
          if(imgtype=="pdf") message(file.path(exportpath,paste0(outname,".pdf exported.")))
        }
        if(returnplot) list_plot[[i]] <- gg_plot_panel
      }

      if(grplabcheck)
      {
        # top panel
        # add grp divider lines only if 2 grps or more
        if(showdiv)
        {
          div_position <- marker_position[c(marker_position$count %in% divgrp),]
          if(nrow(div_position) > 2) gg_plot_panel <- gg_plot_panel+geom_vline(xintercept=div_position$divxpos[-c(1,length(div_position$divxpos))],colour=divcol,linetype=divtype,size=divsize,alpha=divalpha)
        }

        if(!showgrplab)
        {
          if(exportplot)
          {
            message("Drawing plot ...")
            if(imgtype=="tiff") tiff(file.path(exportpath,paste0(outname,".tiff")),height=height1,width=width1,res=dpi,units=units1,compression="lzw",family=font)
            if(imgtype=="png") png(file.path(exportpath,paste0(outname,".png")),height=height1,width=width1,res=dpi,units=units1,family=font)
            if(imgtype=="jpeg") jpeg(file.path(exportpath,paste0(outname,".jpg")),height=height1,width=width1,res=dpi,units=units1,quality=100,family=font)
            if(imgtype=="pdf") pdf(file.path(exportpath,paste0(outname,".pdf")),height=height1,width=width1,fonts=font)
            print(gg_plot_panel)
            dev.off()
            if(imgtype=="tiff") message(file.path(exportpath,paste0(outname,".tiff exported.")))
            if(imgtype=="png") message(file.path(exportpath,paste0(outname,".png exported.")))
            if(imgtype=="jpeg") message(file.path(exportpath,paste0(outname,".jpg exported.")))
            if(imgtype=="pdf") message(file.path(exportpath,paste0(outname,".pdf exported.")))
          }
          if(returnplot) list_plot[[i]] <- gg_plot_panel
        }

        if(showgrplab)
        {
          # bottom panel
          ppar <- getPlotParams(grplab=grplabloop[[1]],plotnum=1,grplabsize=grplabsize,grplabangle=grplabangle,
                                            grplabjust=grplabjust,pointsize=pointsize,linesize=linesize)

          # fix facet labels in grplab plot
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

          # create bottom plot with labels
          gg_plot_grplab <- ggplot()+
            geom_blank(data=label_position,aes(x=labxpos,y=labypos))+
            geom_text(data=label_position,aes(x=labxpos,y=labypos),label=label_position$label,size=ppar$grplabsize,angle=ppar$grplabangle,hjust=ppar$grplabjust,colour=grplabcol,alpha=grplabalpha,family=font,fontface=grplabface)+
            geom_line(data=marker_position,aes(x=markerxpos,y=markerypos),colour=linecol,size=ppar$linesize,linetype=linetype,alpha=linealpha)+
            geom_point(data=marker_position,aes(x=markerxpos,y=markerypos),size=ppar$pointsize,colour=pointcol,shape=pointtype,fill=pointbgcol,alpha=pointalpha)+
            scale_x_continuous(expand=c(0,0))+
            scale_y_continuous(expand=c(0,0),limits=c(0,1))+
            labs(x=NULL,y=NULL)+
            #facet_grid(count~.,switch=sppos,labeller=labeller(count=glabfacetnames))+
            facet_wrap(~count,labeller=labeller(count=glabfacetnames),strip.position=sppos,scales="fixed",nrow=length(unique(label_position$count)))+
            get(theme)(base_family=font,base_size=basesize)+
            theme(legend.position="none",
                  panel.grid=element_blank(),
                  panel.background=element_rect(fill="white"),
                  axis.ticks=element_blank(),
                  axis.text=element_blank(),
                  axis.line=element_blank(),
                  axis.title=element_blank(),
                  strip.text.y=element_text(size=splabsize,colour=splabcol,face=splabface,angle=splabangle),
                  strip.background=element_rect(colour=spbgcol,fill=spbgcol),
                  plot.margin=unit(c(grplabspacer,0.05,0.05,0),"cm"),
                  panel.spacing=unit(panelspacer,"cm"))

          # remove strip panels on right
          if(!showsp) gg_plot_grplab <- gg_plot_grplab+theme(strip.text=element_blank())

          # add margin to left if y-axis is on
          if(showyaxis) gg_plot_grplab <- gg_plot_grplab+theme(plot.margin=unit(c(grplabspacer,0.05,0.05,0.1),"cm"))

          # gtable conversion
          ggg_plot_panel <- ggplotGrob(gg_plot_panel)
          ggg_plot_panel$layout$clip <- "off"
          ggg_plot_grplab <- ggplotGrob(gg_plot_grplab)
          ggg_plot_grplab$layout$clip <- "off"

          #set y-axis width same for bars and grplabs
          maxWidth <- unit.pmax(ggg_plot_panel$widths[2:5],ggg_plot_grplab$widths[2:5])
          ggg_plot_panel$widths[2:5] <- as.list(maxWidth)
          ggg_plot_grplab$widths[2:5] <- as.list(maxWidth)

          # calculate size of panels
          height2 <- height1 + grplabheight1

          if(exportplot)
          {
            message("Drawing plot ...")
            if(imgtype=="tiff") tiff(file.path(exportpath,paste0(outname,".tiff")),height=height2,width=width1,res=dpi,units=units1,compression="lzw",family=font)
            if(imgtype=="png") png(file.path(exportpath,paste0(outname,".png")),height=height2,width=width1,res=dpi,units=units1,family=font)
            if(imgtype=="jpeg") jpeg(file.path(exportpath,paste0(outname,".jpg")),height=height2,width=width1,res=dpi,units=units1,quality=100,family=font)
            if(imgtype=="pdf") pdf(file.path(exportpath,paste0(outname,".pdf")),height=height2,width=width1,fonts=font)

            grid.arrange(ggg_plot_panel,ggg_plot_grplab,layout_matrix=matrix(c(rep(1,panelratio[1]),rep(2,panelratio[2]*length(grplabloop))),ncol=1,byrow=TRUE))
            dev.off()

            if(imgtype=="tiff") message(file.path(exportpath,paste0(outname,".tiff exported.")))
            if(imgtype=="png") message(file.path(exportpath,paste0(outname,".png exported.")))
            if(imgtype=="jpeg") message(file.path(exportpath,paste0(outname,".jpg exported.")))
            if(imgtype=="pdf") message(file.path(exportpath,paste0(outname,".pdf exported.")))
          }

          px <- arrangeGrob(ggg_plot_panel,ggg_plot_grplab,layout_matrix=matrix(c(rep(1,panelratio[1]),rep(2,panelratio[2]*length(grplabloop))),ncol=1,byrow=TRUE))
          if(returnplot) list_plot[[i]] <- px
        }
      }

    }

    if(returnplot && !returndata) return(list(plot=list_plot,data=list(qlist=NA,grplab=NA)))
    if(!returnplot && returndata) return(list(plot=NA,data=list(qlist=list_qlist,grplab=list_grplab)))
    if(returnplot && returndata) return(list(plot=list_plot,data=list(qlist=list_qlist,grplab=list_grplab)))
  }

  ## join + shared indlab ------------------------------------------------------
  if(imgoutput=="join")
  {
    # checks
    if(flen < 2) stop("plotQ: Joined plot cannot be created. Number of selected files less than 2.")
    tempdf <- tabulateQ(qlist,sorttable=FALSE)

    # checks if num of individuals differ between runs
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

    # prepare subtitle
    if(any(is.na(subtitlelab))){
      subtitlename <- outname
    }else{
      subtitlename <- subtitlelab
    }

    # loop to process selected files
    plist <- vector("list",length=flen)
    facetnames <- vector(length=flen)
    kvec <- vector(length=flen)
    strt <- 1
    div_position_list <- vector("list",length=flen)
    if(returndata)
    {
      list_qlist <- vector("list",length=flen)
      list_grplab <- vector("list",length=flen)
    }
    div_multiplier <- seq(from=0,to=(flen-1))
    for (i in seq_along(qlist))
    {
      fname <- names(qlist)[i]
      fname <- gsub(".txt$|.csv$|.tsv$|.meanq$|.meanQ$|.structure$","",fname)
      if(is.null(fname)) fname <- paste0("sample",i)

      df1 <- qlist[[i]]
      grplabloop <- grplab

      # add rownames
      if(!useindlab) row.names(df1) <- sprintf(paste0("%",paste0(rep(0,nchar(nrow(df1))),collapse=""),nchar(nrow(df1)),"d"),1:nrow(df1))

      # ordering grps
      if(grplabcheck)
      {
        templist <- grpLabels(dframe=df1,grplab=grplabloop,selgrp=selgrp,
                                          subsetgrp=subsetgrp,ordergrp=ordergrp,grpmean=grpmean,
                                          grplabpos=grplabpos,linepos=linepos,
                                          indlabwithgrplab=indlabwithgrplab,
                                          indlabsep=indlabsep,runid=i)

        df1 <- templist$dframe
        grplabloop <- templist$grplab
        marker_position <- templist$marker_position
        label_position <- templist$label_position
        rm(templist)
      }

      # sorting individuals
      if(!is.na(sortind))
      {
        templist <- sortInd(dframe=df1,grplab=grplabloop,selgrp=selgrp,ordergrp=ordergrp,
                                        sortind=sortind,grplabpos=grplabpos,linepos=linepos)
        df1 <- templist$dframe
        grplabloop <- templist$grplab
        marker_position <- templist$marker_position
        label_position <- templist$label_position
        rm(templist)
      }

      # save modified data for return
      if(returndata)
      {
        list_qlist[[i]] <- df1
        names(list_qlist)[i] <- fname
        if(grplabcheck) list_grplab[[i]] <- grplabloop
      }

      k <- ncol(df1)
      # num of ind, ind changes when subsetting
      Ind <- nrow(df1)
      #df1$ind <- factor(rownames(df1),levels=rownames(df1))
      df1$ind <- as.character(rownames(df1))
      df1$run <- as.integer(rep(i,nrow(df1)))
      df1$order_ind <- seq(from=1,to=Ind)

      # cumulative numbering
      if(i==1) {
        start <- 1
        end <- Ind
      }else{
        start <- end+1
        end <- Ind*i
      }

      df1$order_cumulative <- seq(from=start,to=end)

      # strip panel labelling
      if(any(is.na(splab)))
      {
        facetnames[[i]] <- paste0(fname,"\n","K=",k)
        #names(facetnames[[i]]) <- levels(df1$run)[i]
      }else{
        facetnames[[i]] <- splab[i]
        #names(facetnames[[i]]) <- levels(df1$run)[i]
      }

      #names of facets are run ids which is just i
      names(facetnames)[i] <- i

      #oclist[[i]]$run <- facetnames[[i]]
      kvec[[i]] <- k
      #df2 <- gather(df1,"variable","value",-c(ind,run,order_ind,order_cumulative))
      df2 <- pivot_longer(data=df1,cols=colnames(df1)[c(!colnames(df1) %in% c("ind","run","order_ind","order_cumulative"))],names_to="variable",values_to="value") 
      #plist[[i]] <- df2[rev(1:nrow(df2)),]
      plist[[i]] <- df2

      # div position
      # positions of div lines are selected from group label marker positions.
      # top and bottom marker positions are removed since ends do not need div lines
      # for joined plots with runs>1, the div positions are readjusted
      if(grplabcheck)
      {
        div_position <- marker_position[c(marker_position$count %in% divgrp),]
        div_position <- div_position[seq(from=2,to=(nrow(div_position)-1)),c("count","divxpos"),drop=FALSE]
        div_position$run <- i
        if(sortindcheck!="label" && sortindcheck!="empty") div_position$divxpos <- div_position$divxpos+(Ind*div_multiplier[i])
        div_position_list[[i]] <- div_position
      }
      rm(df2)
    }

    # combine list to one dataframe
    df3 <- do.call("rbind",plist)
    if(grplabcheck) div_position <- do.call("rbind",div_position_list)
    #names(facetnames) <- levels(factor(as.character(df3$run)))

    # legendlab
    if(any(is.na(legendlab)))
    {
      legendlab1 <- levels(factor(as.character(df3$variable)))
    }else{
      legendlab1 <- legendlab
    }
    if(length(legendlab1) != length(levels(factor(as.character(df3$variable))))) stop("plotQ: Length of 'legendlab' is not equal to max number of clusters.")

    # get dimensions
    dimtemp <- getDim(ind=Ind,height=height,width=width,dpi=dpi,units=units,imgtype=imgtype,
                                  grplabheight=grplabheight,labs=length(grplabloop),plotnum=flen,
                                  showindlab=showindlab,sharedindlab=sharedindlab)
    height1 <- as.numeric(dimtemp[1])
    width1 <- as.numeric(dimtemp[2])
    grplabheight1 <- as.numeric(dimtemp[3])
    units1 <- as.character(dimtemp[4])

    # get colours
    coll <- clustercol
    if(any(is.na(clustercol))) coll <- getColours(as.integer(max(kvec)))
    if(length(coll) < max(kvec)) stop(paste0("plotQ: Number of colours (",length(coll),") is less than the number of clusters (",max(kvec),")."))

    ## COMMON PLOT TOP PANEL ----------------------------------------------------
    # create plot
    # when unsorted or sorting is by label, x-axis is by individual order
    # when sorting is by cluster or all, x-axis is set to cumulative order
    # in both cases, x-axis text is overridden by actual individual labels
    if(sortindcheck=="label" || sortindcheck=="empty")
    {
      gg_plot_panel <- ggplot(data=df3,aes(x=order_ind,y=value,fill=variable))+
        geom_bar(width=barsize,size=barbordersize,colour=barbordercolour,stat="identity",position="fill",na.rm=na.rm)+
        scale_x_continuous(breaks=df3$order_ind,labels=df3$ind,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        scale_fill_manual(values=coll,labels=legendlab1)+
        guides(fill=guide_legend(nrow=legendrow,byrow=TRUE))+
        get(theme)(base_family=font,base_size=basesize)
    }else{
      gg_plot_panel <- ggplot(data=df3,aes(x=order_cumulative,y=value,fill=variable))+
        geom_bar(width=barsize,size=barbordersize,colour=barbordercolour,stat="identity",position="fill",na.rm=na.rm)+
        scale_x_continuous(breaks=df3$order_cumulative,labels=df3$ind,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        scale_fill_manual(values=coll,labels=legendlab1)+
        guides(fill=guide_legend(nrow=legendrow,byrow=TRUE))+
        get(theme)(base_family=font,base_size=basesize)
    }

    # shared indlab
    # when ind labs are shared, x-axis is fixed
    # when ind labels are not shared, x-axis is free
    if(sharedindlab) gg_plot_panel <- gg_plot_panel+facet_wrap(~run,labeller=labeller(run=facetnames),strip.position=sppos,scales="fixed",nrow=flen,drop=TRUE)
    if(!sharedindlab) gg_plot_panel <- gg_plot_panel+facet_wrap(~run,labeller=labeller(run=facetnames),strip.position=sppos,scales="free_x",nrow=flen,drop=TRUE)

    gg_plot_panel <- gg_plot_panel+
      labs(x=NULL,y=NULL)+
      theme(legend.position="top",
            legend.direction="horizontal",
            legend.title=element_blank(),
            legend.key.size=unit(legendkeysize,"points"),
            legend.text=element_text(size=legendtextsize,colour=indlabcol),
            legend.spacing.x=unit(legendspacing,"points"),
            legend.spacing=unit(0,"points"),
            legend.justification=legendpos,
            legend.margin=margin(legendmargin[1],legendmargin[2],legendmargin[3],legendmargin[4],"points"),
            legend.box.spacing=unit(1.5,"points"),
            panel.grid=element_blank(),
            panel.background=element_blank(),
            axis.text.x=element_text(size=indlabsize,colour=indlabcol,
                                     angle=indlabangle,vjust=indlabvjust,
                                     hjust=indlabhjust,margin=margin(t=indlabspacer)),
            axis.text.y=element_text(size=indlabsize,colour=indlabcol),
            axis.ticks=element_line(size=ticksize,colour=indlabcol),
            axis.ticks.length=unit(ticklength,"cm"),
            axis.line=element_blank(),
            axis.title=element_blank(),
            strip.text.y=element_text(size=splabsize,colour=splabcol,face=splabface,angle=splabangle),
            strip.background=element_rect(colour=spbgcol,fill=spbgcol),
            plot.margin=unit(c(0.2,0.05,indlabheight,0),"cm"),
            panel.spacing=unit(panelspacer,"cm"))

    # remove indlab
    if(!showindlab) gg_plot_panel <- gg_plot_panel+theme(axis.text.x=element_blank())

    # remove y-axis labels
    if(!showyaxis)
    {
      gg_plot_panel <- gg_plot_panel+theme(axis.ticks.y=element_blank(),
                                           axis.text.y=element_blank(),
                                           plot.margin=unit(c(0.2,0.05,indlabheight,0),"cm"))
    }else{
      gg_plot_panel <- gg_plot_panel+theme(plot.margin=unit(c(0.2,0.05,indlabheight,0.1),"cm"))
    }

    # remove axis ticks
    if(!showticks) gg_plot_panel <- gg_plot_panel+theme(axis.ticks=element_blank())

    # remove strip panels on right
    if(!showsp) gg_plot_panel <- gg_plot_panel+theme(strip.text=element_blank())

    # remove legend
    if(!showlegend) gg_plot_panel <- gg_plot_panel+theme(legend.position="none")

    # show title
    if(showtitle) gg_plot_panel <- gg_plot_panel+labs(title=titlename)+
      theme(plot.title=element_text(size=titlesize,colour=titlecol,
                                    angle=titleangle,hjust=titlehjust,face=titleface,
                                    vjust=titlevjust,margin=margin(b=titlespacer)))
    # show subtitle
    if(showsubtitle) gg_plot_panel <- gg_plot_panel+labs(subtitle=subtitlename)+
      theme(plot.subtitle=element_text(size=subtitlesize,colour=subtitlecol,
                                       angle=subtitleangle,hjust=subtitlehjust,face=subtitleface,
                                       vjust=subtitlevjust,margin=margin(b=subtitlespacer)))

    if(!grplabcheck)
    {
      if(exportplot)
      {
        message("Drawing plot ...")
        if(imgtype=="tiff") tiff(file.path(exportpath,paste0(outname,".tiff")),height=height1,width=width1,res=dpi,units=units1,compression="lzw",family=font)
        if(imgtype=="png") png(file.path(exportpath,paste0(outname,".png")),height=height1,width=width1,res=dpi,units=units1,family=font)
        if(imgtype=="jpeg") jpeg(file.path(exportpath,paste0(outname,".jpg")),height=height1,width=width1,res=dpi,units=units1,quality=100,family=font)
        if(imgtype=="pdf") pdf(file.path(exportpath,paste0(outname,".pdf")),height=height1,width=width1,fonts=font)
        print(gg_plot_panel)
        dev.off()
        if(imgtype=="tiff") message(file.path(exportpath,paste0(outname,".tiff exported.")))
        if(imgtype=="png") message(file.path(exportpath,paste0(outname,".png exported.")))
        if(imgtype=="jpeg") message(file.path(exportpath,paste0(outname,".jpg exported.")))
        if(imgtype=="pdf") message(file.path(exportpath,paste0(outname,".pdf exported.")))
      }

      if(returnplot) px <- gg_plot_panel
    }

    if(grplabcheck)
    {
      # add grp divider lines only if 2 grps or more
      if(showdiv)
      {
        if(nrow(div_position) > 0) gg_plot_panel <- gg_plot_panel+geom_vline(data=div_position,aes(xintercept=divxpos),colour=divcol,linetype=divtype,size=divsize,alpha=divalpha)
      }

      if(!showgrplab)
      {
        if(exportplot)
        {
          message("Drawing plot ...")
          if(imgtype=="tiff") tiff(file.path(exportpath,paste0(outname,".tiff")),height=height1,width=width1,res=dpi,units=units1,compression="lzw",family=font)
          if(imgtype=="png") png(file.path(exportpath,paste0(outname,".png")),height=height1,width=width1,res=dpi,units=units1,family=font)
          if(imgtype=="jpeg") jpeg(file.path(exportpath,paste0(outname,".jpg")),height=height1,width=width1,res=dpi,units=units1,quality=100,family=font)
          if(imgtype=="pdf") pdf(file.path(exportpath,paste0(outname,".pdf")),height=height1,width=width1,fonts=font)
          print(gg_plot_panel)
          dev.off()
          if(imgtype=="tiff") message(file.path(exportpath,paste0(outname,".tiff exported.")))
          if(imgtype=="png") message(file.path(exportpath,paste0(outname,".png exported.")))
          if(imgtype=="jpeg") message(file.path(exportpath,paste0(outname,".jpg exported.")))
          if(imgtype=="pdf") message(file.path(exportpath,paste0(outname,".pdf exported.")))
        }

        if(returnplot) px <- gg_plot_panel
      }

      if(showgrplab)
      {
        # plot with grp labels
        ppar <- getPlotParams(grplab=grplabloop[[1]],plotnum=flen,grplabsize=grplabsize,grplabangle=grplabangle,grplabjust=grplabjust,
                                          pointsize=pointsize,linesize=linesize)

        # fix facet labels in grplab plot
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

        # create bottom plot with labels
        gg_plot_grplab <- ggplot()+
          geom_blank(data=label_position,aes(x=labxpos,y=labypos))+
          geom_text(data=label_position,aes(x=labxpos,y=labypos),label=label_position$label,angle=ppar$grplabangle,hjust=ppar$grplabjust,size=ppar$grplabsize,colour=grplabcol,alpha=grplabalpha,family=font,fontface=grplabface)+
          geom_line(data=marker_position,aes(x=markerxpos,y=markerypos),colour=linecol,size=ppar$linesize,linetype=linetype,alpha=linealpha)+
          geom_point(data=marker_position,aes(x=markerxpos,y=markerypos),size=ppar$pointsize,colour=pointcol,shape=pointtype,fill=pointbgcol,alpha=pointalpha)+
          scale_x_continuous(expand=c(0,0))+
          scale_y_continuous(expand=c(0,0),limits=c(0,1))+
          labs(x=NULL,y=NULL)+
          facet_wrap(~count,labeller=labeller(count=glabfacetnames),strip.position=sppos,scales="fixed",nrow=length(unique(label_position$count)))+
          get(theme)(base_family=font,base_size=basesize)+
          theme(legend.position="none",
                panel.grid=element_blank(),
                panel.background=element_blank(),
                axis.ticks=element_blank(),
                axis.text=element_blank(),
                axis.line=element_blank(),
                axis.title=element_blank(),
                strip.text.y=element_text(size=splabsize,colour=splabcol,face=splabface,angle=splabangle),
                strip.background=element_rect(colour=spbgcol,fill=spbgcol),
                plot.margin=unit(c(grplabspacer,0.05,0.05,0),"cm"),
                panel.spacing=unit(panelspacer,"cm"))

        # remove strip panels on right
        if(!showsp) gg_plot_grplab <- gg_plot_grplab+theme(strip.text=element_blank())

        # add margin on left if y-axis is on
        if(showyaxis) gg_plot_grplab <- gg_plot_grplab+theme(plot.margin=unit(c(grplabspacer,0.05,0.05,0.1),"cm"))

        # gtable conversion
        ggg_plot_panel <- ggplotGrob(gg_plot_panel)
        ggg_plot_panel$layout$clip <- "off"
        ggg_plot_grplab <- ggplotGrob(gg_plot_grplab)
        ggg_plot_grplab$layout$clip <- "off"

        #set y-axis width same for bars and grplabs
        maxWidth <- unit.pmax(ggg_plot_panel$widths[2:5],ggg_plot_grplab$widths[2:5])
        ggg_plot_panel$widths[2:5] <- as.list(maxWidth)
        ggg_plot_grplab$widths[2:5] <- as.list(maxWidth)

        # calculate size of panels
        height2 <- height1+grplabheight1

        if(exportplot)
        {
          message("Drawing plot ...")
          if(imgtype=="tiff") tiff(file.path(exportpath,paste0(outname,".tiff")),height=height2,width=width1,res=dpi,units=units1,compression="lzw",family=font)
          if(imgtype=="png") png(file.path(exportpath,paste0(outname,".png")),height=height2,width=width1,res=dpi,units=units1,family=font)
          if(imgtype=="jpeg") jpeg(file.path(exportpath,paste0(outname,".jpg")),height=height2,width=width1,res=dpi,units=units1,quality=100,family=font)
          if(imgtype=="pdf") pdf(file.path(exportpath,paste0(outname,".pdf")),height=height2,width=width1,fonts=font)

          grid.arrange(ggg_plot_panel,ggg_plot_grplab,layout_matrix=matrix(c(rep(1,panelratio[1]*flen),rep(2,panelratio[2]*length(grplabloop))),ncol=1,byrow=TRUE))
          dev.off()

          if(imgtype=="tiff") message(file.path(exportpath,paste0(outname,".tiff exported.")))
          if(imgtype=="png") message(file.path(exportpath,paste0(outname,".png exported.")))
          if(imgtype=="jpeg") message(file.path(exportpath,paste0(outname,".jpg exported.")))
          if(imgtype=="pdf") message(file.path(exportpath,paste0(outname,".pdf exported.")))
        }

        if(returnplot) px <- arrangeGrob(ggg_plot_panel,ggg_plot_grplab,layout_matrix=matrix(c(rep(1,panelratio[1]*flen),rep(2,panelratio[2]*length(grplabloop))),ncol=1,byrow=TRUE))
      }
    }

    if(returnplot && !returndata) return(list(plot=list(px),data=list(qlist=NA,grplab=NA)))
    if(!returnplot && returndata) return(list(plot=NA,data=list(qlist=list_qlist,grplab=list_grplab)))
    if(returnplot && returndata) return(list(plot=list(px),data=list(qlist=list_qlist,grplab=list_grplab)))
  }
}

# plotQMultiline ----------------------------------------------------------------

#' @title Plot a qlist as individual-level multiline barplot
#' @description Plot a qlist as individual-level barplot with multiple lines.
#' @param qlist A qlist (list of dataframes). An output from \code{\link{readQ}}.
#' @param spl An integer indicating samples per line. Defaults to 60.
#' @param lpp An integer indicating lines per page. Defaults to 11.
#' @param clustercol A character vector of colours for clusters.
#' @param sortind A character indicating how individuals are sorted. Default is
#' NA (Same order of individuals as in input file). Other options are 'all'
#' (sorting by values of all clusters), by any one cluster (eg. 'Cluster1') or
#' 'labels' (sorting by individual labels). See details.
#' @param grplab A dataframe with one or more columns (group label sets), and
#' rows equal to the number of individuals. See details.
#' @param selgrp A single character denoting a selected group label set. The
#' selected label must be a group label title used in \code{grplab}. See details.
#' @param ordergrp A logical indicating if individuals must be grouped into
#' contiguous blocks based on \code{grplab} starting with \code{selgrp}.
#' @param subsetgrp A character or character vector with group names to subset
#' or reorder groups. Only applicable when \code{grplab} is in use. Default is
#' NA. See details.
#' @param grpmean A logical indicating if q-matrix must be converted from
#' individual values to group mean values. Applicable only when \code{grplab}
#' is in use and mean is calculated over \code{selgrp}.
#' @param showindlab A logical indicating if individual labels must be shown
#' below the bars. To hide labels, set \code{showindlab=FALSE}. See details.
#' @param useindlab A logical indicating if individual labels must be read from
#' the rownames of qlist dataframes and used as labels. See details.
#' @param indlabwithgrplab A logical indicating if individual labels must be
#' concatenated with grplab. Applies only when grplab is in use. Relevant for
#' sorting by label.
#' @param indlabsep A character used as separator when concatenating individual
#' labels and group labels. Defaults to space \code{indlabsep=" "}.
#' @param indlabsize A numeric denoting size of the individual labels. Defaults
#' to 5.
#' @param indlabangle A numeric denoting the angle of the individual labels.
#' Defaults to 90.
#' @param indlabvjust A numeric denoting vertical justification of the
#' individual labels. Defaults to 0.5.
#' @param indlabhjust A numeric denoting the horizontal justification of the
#' individual labels. Defaults to 1.
#' @param indlabcol A colour for individual labels. Defaults to 'grey30'.
#' @param indlabspacer A numeric denoting space between the individual label
#' and the plot area. Default set to 0.
#' @param showgrplab A logical indicating if group labels \code{grplab} must be
#' displayed on the plot.
#' @param grplabsize A numeric denoting size of the group labels. Defaults to 7.
#' @param grplabcol A colour for group labels. Defaults to 'grey30'.
#' @param grplabbgcol A colour for group label background. Defaults to 'white'.
#' @param showtitle A logical indicating if plot title must be shown on the top.
#' Defaults to FALSE. If TRUE and \code{titlelab=NA}, file name is displayed by
#' default.
#' @param titlelab A character or character vector for title text. Defaults to
#' NA, and when \code{showtitle=TRUE} displays file name.
#' @param titlehjust A numeric denoting the horizontal justification of the
#' title. Defaults to 0 (left).
#' @param titlevjust A numeric denoting the vertical justification of the title.
#' Defaults to 0.5 (center).
#' @param titlesize A numeric indicating the size of the title text. Defaults
#' to 5 points.
#' @param titlecol A colour character for title. Defaults to "grey30".
#' @param titleface A character indicating the font face of title label. One of
#' 'plain', 'italic', 'bold' or 'bold.italic'. Defaults to 'plain'. Applicable
#' only when \code{showtitle=TRUE}.
#' @param titlespacer A numeric indicating the space below the title. Defaults
#' to 1.2.
#' @param titleangle A numeric indicating the angle/rotation of the title.
#' Defaults to 0.
#' @param showsubtitle A logical indicating if plot subtitle must be shown on
#' the top. Defaults to FALSE. If TRUE and \code{subtitlelab=NA}, file name is
#' displayed by default.
#' @param subtitlelab A character or character vector for subtitle text.
#' Defaults to NA, and when \code{showsubtitle=TRUE} displays K value like K=2.
#' @param subtitlehjust A numeric denoting the horizontal justification of the
#' subtitle. Defaults to 0 (left).
#' @param subtitlevjust A numeric denoting the vertical justification of the
#' subtitle. Defaults to 0.5 (center).
#' @param subtitlesize A numeric indicating the size of the subtitle text.
#' Defaults to 5 points.
#' @param subtitlecol A colour character for subtitle. Defaults to "grey30".
#' @param subtitleface A character indicating the font face of subtitle label.
#' One of 'plain', 'italic', 'bold' or 'bold.italic'. Defaults to 'plain'.
#' Applicable only when \code{showsubtitle=TRUE}.
#' @param subtitlespacer A numeric indicating the space below the subtitle.
#' Defaults to 1.2.
#' @param subtitleangle A numeric indicating the angle/rotation of the subtitle.
#' Defaults to 0.
#' @param showlegend A logical indicating if legend denoting cluster colours
#' must be plotted. Defaults to FALSE.
#' @param legendlab A character or character vector to for legend cluster
#' labels. Must be equal to max number of clusters.
#' @param legendpos A character 'right' or 'left' denoting position of the
#' legend. Defaults to 'left'.
#' @param legendkeysize A numeric indicating size of the legend key. Defaults
#' to 4.
#' @param legendtextsize A numeric indicating size of the legend text. Defaults
#' to 3.
#' @param legendmargin A numeric vector of length 4 indicating top, right,
#' bottom and left margins of the legend.
#' @param barsize A numeric indicating the width of the bars. Defaults to 0.9.
#' @param barbordersize A numeric indicating border size of bars. Defaults to 0.
#' Visible only when \code{barbordercolour} is not NA.
#' @param barbordercolour A single colour for bar border. Defaults to NA.
#' Visible only when \code{barbordersize} is larger than zero and set to a
#' colour other than NA.
#' @param showticks A logical indicating if ticks on axis should be displayed or
#' not. Defaults to FALSE.
#' @param showyaxis A logical indicating if y-axis labels should be displayed or
#' not. Defaults to FALSE.
#' @param ticksize A numeric indicating size of ticks. Defaults to 0.2. Applies
#' to both x and y axis.
#' @param ticklength A numeric indicating length of tick marks in cm. Defaults
#' to 0.03. Applies to both x and y axis.
#' @param outputfilename A character or character vector denoting output file
#' name without file extension. See details.
#' @param imgtype A character denoting figure output format. Options are 'png',
#' 'jpeg', 'tiff' or 'pdf'.
#' @param height A numeric denoting height of the full figure. If NA, height is
#' set to 29.7cm (A4 height).
#' @param width A numeric denoting width of the full figure. If NA, width is set
#' to 21cm (A4 width).
#' @param dpi A numeric denoting resolution of the figure. Default is 300. If
#' \code{imgtype="pdf"}, dpi is fixed at 300 and does not have any effect..
#' @param units A character denoting the units of dimension of the figure.
#' Default is "cm". Other options are 'px', 'in' or 'mm'.
#' @param mar A four number vector denoting distance of top, right, bottom and
#' left margins in \code{units}.
#' @param theme A character indicating ggplot theme to be used. Use like
#' "theme_grey", "theme_bw" etc.
#' @param basesize A numeric indicating overall text size. Defaults to 5
#' suitable for export. Set to 11 for returned plot.
#' @param font A character indicating font family to be used in the plots. Uses
#' default system fonts by default for jpeg, png and tiff. Uses 'Helvetica' as
#' default for pdf. Use package \code{extrafonts} to import custom fonts. See
#' vignette for examples.
#' @param na.rm Default set to FALSE. NAs are not removed from data, therefore
#' \code{ggplot} prints warning messages for NAs. If set to TRUE, NAs are
#' removed before plotting and \code{ggplot} NA warning is suppressed.
#' @param exportplot A logical indicating if a plot image must be exported into
#' the working directory.
#' @param returnplot A logical indicating if ggplot plot objects must be
#' returned. See 'Value'.
#' @param returndata A logical indicating if processed data must be returned.
#' See 'Value'.
#' @param exportpath A path to where content must be exported. For example,
#' \code{exportpath='/path'}. To use current working directory, set \code{exportpath=getwd()}.
#' @return When \code{returnplot=TRUE}, plot object(s) are returned. When
#' \code{grplab=NA}, a ggplot2 object is returned. When \code{returndata=TRUE},
#' the input qlist is modified (sorted, subsetted etc) and returned. If
#' \code{grplab} is in use, a list of modified qlist and grplab is returned.
#' If \code{returnplot=TRUE} and \code{returndata=TRUE} are both set, then a
#' named list (plot,data) is returned. The plot item contains the ggplot2 object
#' and the data contains qlist (and grplab).
#'
#' @details Figures are always created to A4 size. Any plotted row will span the
#' width of the figure. Note that this function is slow and may take several
#' minutes when plotting mutiple runs.
#'
#' \strong{indlab}\cr
#' \code{plotQMultiline()} labels each individual separately. When
#' \code{showindlab=TRUE},
#' individual labels are shown/displayed. When \code{showindlab=FALSE}, individual
#' labels are not shown/displayed on the graph, although they are present in the
#' underlying data. Therefore, \code{showindlab} only control display of labels
#' on the plot and nothing to do with label control in the data.\cr
#' The default \code{useindlab=FALSE}, creates labels numerically in the
#' original order of data but with zero padding. For example, if there are 10
#' individuals, labels are 01, 02 up to 10. if there are 100 individuals, then
#' labels are 001, 002 upto 100. Zero padding to ensure optimal sorting. When
#' \code{useindlab=TRUE}, labels are used from rownames of qlist dataframes. They
#' are usually labelled 1,2,3.. if read in using \code{readQ()}. This can be an
#' issue with sorting by labels \code{sortind="label"}. For STRUCTURE files with
#' individual labels, they can be read in automatically using
#' \code{readQ(indlabfromfile=TRUE)}.\cr
#' When group labels are in use, \code{grplab}, they are added to the individual
#' labels in both cases \code{useindlab=TRUE} and \code{useindlab=FALSE} separated by
#' \code{indlabsep}. Default \code{indlabsep=" "} adds a space between
#' individual label and grplab. For example, group labels 'popA', 'popA'... will
#' be '01 popA', '02 popA'... when \code{useindlab=FALSE} and usually '1 popA',
#' '2 popA'... when \code{useindlab=TRUE}. When multiple group labels are in use,
#' the are similarly concatenated one after the other to individual names in the
#'  order in which the group labels were provided.
#'
#' \strong{sortind}\cr
#' This argument takes one character as input.  Default NA means individuals are
#' plotted in the same order as input. Individuals can be ordered by any one
#' cluster. For ex. \code{sortind="Cluster1"} or \code{sortind="Cluster2"}.
#' To order by all clusters as the 'Sort by Q' option in STRUCTURE software,
#' use \code{sortind="all"}. To order by individual labels, use
#' \code{sortind="label"}. When using \code{sortind} with \code{grplab},
#' individuals are sorted within the groups.\cr
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
#' Output file names are created automatically by default using the input qlist
#' names. When number of individuals exceed one page and extra pages are
#' created, incremental numbers are added to the run name like so: -1, -2 etc.
#' Custom file name can be provided to \code{outputfilename}. The number of
#' labels must be equal to the number of input runs. Incremental numbers are
#' still added if extra pages are created.
#'
#' See the \href{http://royfrancis.github.io/pophelper/articles/index.html#plotqmultiline}{vignette} for more details and examples.
#'
#' @examples
#' \dontrun{
#' sfiles <- list.files(path=system.file("files/structure",package="pophelper"),
#' full.names=TRUE)
#' slist <- readQ(sfiles)
#'
#' # basic
#' plotQMultiline(slist[1],exportpath=getwd())
#'
#' # multiple files
#' plotQMultiline(slist[1:3],exportpath=getwd())
#'
#' # adjust samples per line (spl) and lines per page (lpp)
#' plotQMultiline(slist[1],spl=30,exportpath=getwd())
#' plotQMultiline(slist[1],lpp=8,exportpath=getwd())
#' plotQMultiline(slist[1],spl=75,lpp=10,exportpath=getwd())
#'
#' # sort individuals
#' plotQMultiline(slist[1],sortind="all",exportpath=getwd())
#' plotQMultiline(slist[1],sortind="Cluster1",exportpath=getwd())
#' plotQMultiline(slist[1],sortind="label",exportpath=getwd())
#'
#' # use custom individual labels
#' inds <- read.delim(system.file("files/structureindlabels.txt",
#' package="pophelper"),header=FALSE,stringsAsFactors=FALSE)
#' rownames(slist[[1]]) <- inds$V1
#' plotQMultiline(slist[1],useindlab=TRUE,exportpath=getwd())
#'
#' # change cluster colours
#' plotQMultiline(slist[1],clustercol=c("steelblue","coral"),exportpath=getwd())
#'
#' # change bar width and height
#' plotQMultiline(slist[1],barsize=1,spl=149,indlabsize=3,height=5,exportpath=getwd())
#'
#' # read group labels
#' md <- read.delim(system.file("files/metadata.txt", package="pophelper"),
#' header=TRUE,stringsAsFactors=FALSE)
#'
#' # plot with one group label set
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=FALSE],exportpath=getwd())
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=FALSE],useindlab=TRUE,exportpath=getwd())
#'
#' # sort ind within groups
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=FALSE],sortind="Cluster1",exportpath=getwd())
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=FALSE],sortind="all",exportpath=getwd())
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=FALSE],sortind="label",exportpath=getwd())
#'
#' # subset or reorder groups
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=FALSE],subsetgrp=c("CatB"),exportpath=getwd())
#' plotQMultiline(qlist=slist[1],grplab=md[,2,drop=FALSE],
#' subsetgrp=c("Cat B","CatA"),exportpath=getwd())
#'
#' # using multiple group label sets
#' plotQMultiline(qlist=slist[1],grplab=md,ordergrp=TRUE,exportpath=getwd())
#'
#' # subset on a group from second group label set
#' plotQMultiline(qlist=slist[1],grplab=md,selgrp="cat",subsetgrp="CatB",exportpath=getwd())
#'
#' }
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_discrete scale_y_continuous scale_fill_manual facet_grid labs element_blank element_text element_line element_rect margin unit theme theme_grey
#' @importFrom grDevices png pdf jpeg tiff dev.off
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom stats as.formula
#' @importFrom tidyr pivot_longer
#' @importFrom utils packageDescription
#' @export
#'
plotQMultiline <- function(qlist=NULL,spl=NA,lpp=NA,clustercol=NA,sortind=NA,grplab=NA,selgrp=NA,ordergrp=FALSE,subsetgrp=NA,grpmean=FALSE,
                           showindlab=TRUE,useindlab=FALSE,indlabwithgrplab=FALSE,indlabsep=" ",
                           indlabsize=5,indlabangle=90,indlabvjust=0.5,indlabhjust=1,indlabcol="grey30",indlabspacer=1.5,
                           showgrplab=TRUE,grplabsize=7,grplabcol="grey30",grplabbgcol="#DCDCDC",
                           showtitle=FALSE,titlelab=NA,titlehjust=0,titlevjust=0.5,titlesize=9,titlecol="grey30",titleface="plain",titlespacer=3,titleangle=0,
                           showsubtitle=FALSE,subtitlelab=NA,subtitlehjust=0,subtitlevjust=0.5,subtitlesize=7,subtitlecol="grey30",subtitleface="plain",subtitlespacer=4,subtitleangle=0,
                           showlegend=FALSE,legendlab=NA,legendpos="right",legendkeysize=6,legendtextsize=8,legendmargin=c(1,1,1,0),
                           barsize=0.9,barbordersize=0,barbordercolour=NA,
                           showticks=FALSE,showyaxis=FALSE,ticksize=0.1,ticklength=0.03,
                           outputfilename=NA,imgtype="png",height=NA,width=NA,dpi=300,units="cm",mar=c(0.1,0.5,0.1,0.5),
                           theme="theme_grey",basesize=5,font="",na.rm=FALSE,
                           exportplot=TRUE,returnplot=FALSE,returndata=FALSE,exportpath=NULL)
{
  # check input
  is.qlist(qlist)

  # check exportpath
  if(exportplot) {
    # check exportpath
    if(is.null(exportpath)) stop("plotQMultiline: Argument 'exportpath' not set. To use current working directory, set 'exportpath=getwd()'.")
  }

  # check imagetype
  imgtype <- tolower(imgtype)
  if(imgtype!="png" && imgtype != "pdf" && imgtype != "tiff" && imgtype != "jpeg") stop("plotQMultiline: Argument 'imgtype' set incorrectly. Set as 'png', 'jpeg', 'tiff' or 'pdf'.")
  if(!any(is.na(clustercol))) {if(!is.character(clustercol)) stop("plotQMultiline: Argument 'clustercol' must be a character datatype.")}
  if(!is.logical(showindlab)) stop("plotQMultiline: Argument 'showindlab' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(useindlab)) stop("plotQMultiline: Argument 'useindlab' set incorrectly. Set as TRUE or FALSE.")
  if(!is.character(indlabsep)) stop("plotQMultiline: Argument 'indlabsep' must be a character datatype.")
  if(!is.logical(na.rm)) stop("plotQMultiline: Argument 'na.rm' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(showticks)) stop("plotQMultiline: Argument 'showticks' set incorrectly. Set as TRUE or FALSE.")
  if(!is.logical(showyaxis)) stop("plotQMultiline: Argument 'showyaxis' set incorrectly. Set as TRUE or FALSE.")
  if(length(sortind) > 1) stop("plotQMultiline: Argument 'sortind' must be of length 1. Use 'all','label' or a cluster name like 'Cluster1'.")
  if(is.na(sortind)) {sortindcheck <- "empty"}else{sortindcheck <- sortind}

  if(!is.character(font)) stop("plotQMultiline: Argument 'font' must be a character datatype.")
  if(imgtype=="pdf" && font=="") font <- "Helvetica"
  if(!is.numeric(barsize)) stop("plotQMultiline: Argument 'barsize' must be a numeric datatype.")
  if(barsize<0 || barsize>1) stop("plotQMultiline: Argument 'barsize' must be a value between 0 and 1.")

  # ggplot version
  ggv <- as.numeric(gsub("\\.","",packageDescription("ggplot2", fields="Version")))
  if(ggv < 220) stop("plotQMultiline: Package ggplot2 must be version 2.2.0 or above.")

  # check grplabels
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

  # set NA values
  #if(is.na (dpi)) dpi <- 300
  if(imgtype=="pdf") dpi <- 300
  #if(is.na (units)) units <- "cm"
  if(is.na (height)) height <- 29.7
  if(is.na (width)) width <- 21
  if(imgtype=="pdf") height <- unitConverter(height,units,"in",dpi)
  if(imgtype=="pdf") width <- unitConverter(width,units,"in",dpi)

  len1 <- length(qlist)

  # check outputfilename
  if(!all(is.na(outputfilename)))
  {
    if(length(outputfilename) != len1) stop(paste0("plotQMultiline: Length of argument 'outputfilename' (",length(outputfilename),") is not equal to the number of runs (",len1,")."))
  }

  # check titlelab
  if(!all(is.na(titlelab)))
  {
    if(length(titlelab) != len1) stop(paste0("plotQMultiline: Length of argument 'titlelab' (",length(titlelab),") is not equal to the number of runs (",len1,")."))
  }

  # check subtitlelab
  if(!all(is.na(subtitlelab)))
  {
    if(length(subtitlelab) != len1) stop(paste0("plotQMultiline: Length of argument 'subtitlelab' (",length(subtitlelab),") is not equal to the number of runs (",len1,")."))
  }

  # legendlab
  maxk <- max(tabulateQ(qlist)$k)
  if(all(!is.na(legendlab)))
  {
    legendlab1 <- legendlab
    if(length(legendlab1) < maxk) stop(paste0("plotQ: Length of 'legendlab' (",length(legendlab),") is less than the number of max clusters (",maxk,")."))
  }

  if(returndata)
  {
    list_qlist <- vector("list",length=len1)
    list_grplab <- vector("list",length=len1)
  }
  if(returnplot) list_plot <- vector("list",length=len1)
  for (i in seq_along(qlist))
  {
    # sample name
    fname <- names(qlist)[i]
    fname <- gsub(".txt$|.csv$|.tsv$|.meanq$|.meanQ$|.structure$","",fname)
    if(is.null(fname)) fname <- paste0("sample",i)

    # prepare outputfilename
    if(any(is.na(outputfilename))){
      outname <- fname
    }else{
      outname <- outputfilename[i]
    }

    # check files
    dff <- qlist[[i]]
    grplabloop <- grplab

    # add rownames
    if(!useindlab) row.names(dff) <- sprintf(paste0("%",paste0(rep(0,nchar(nrow(dff))),collapse=""),nchar(nrow(dff)),"d"),1:nrow(dff))

    # ordering grps
    if(grplabcheck)
    {
      templist <- grpLabels(dframe=dff,grplab=grplabloop,selgrp=selgrp,
                                        subsetgrp=subsetgrp,ordergrp=ordergrp,grpmean=grpmean,
                                        indlabwithgrplab=indlabwithgrplab,indlabsep=indlabsep,runid=i)
      dff <- templist$dframe
      grplabloop <- templist$grplab
      if(any(duplicated(rle(grplabloop[,selgrp])$values))) stop("plotQMultiline: Group labels cannot be used due to non-contiguous blocks of labels. Change grplab, selgrp or use 'ordergrp=TRUE'.")
      rm(templist)
    }

    # ordering individuals
    if(!is.na(sortind))
    {
      templist <- sortInd(dframe=dff,grplab=grplabloop,selgrp=selgrp,
                                      ordergrp=ordergrp,sortind=sortind)
      dff <- templist$dframe
      grplabloop <- templist$grplab
      rm(templist)
    }

    # prepare title
    if(any(is.na(titlelab))){
      titlename <- fname
    }else{
      titlename <- titlelab[i]
    }

    # prepare subtitle
    if(any(is.na(subtitlelab))){
      subtitlename <- paste0("K=",ncol(dff))
    }else{
      subtitlename <- subtitlelab[i]
    }

    # save modified data for return
    if(returndata)
    {
      list_qlist[[i]] <- dff
      names(list_qlist)[i] <- fname
      if(grplabcheck) {list_grplab[[i]] <- grplabloop}else{list_grplab[[i]] <- NA}
    }

    # primary calculation of spl
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

    # optimise spl
    if(is.na(spl))
    {
      if(nr1 <= 60) {spl1 <- nr1} else {spl1 <- 60}

      # automatically optimise number of rows and spl
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

    # get colours
    coll <- clustercol
    if(any(is.na(clustercol))) coll <- getColours(as.integer(ncol(dff)))
    if(length(coll) < ncol(dff)) stop(paste0("plotQMultiline: Number of colours (",length(coll),") is less than the number of clusters (",ncol(dff),")."))

    if(any(is.na(legendlab))) legendlab1 <- colnames(dff)

    dff$ind <- factor(rownames(dff),levels=rownames(dff))
    #dff$ind <- as.character(rownames(dff))
    dff$rows <- factor(c(rep(1:numrows,each=spl1),rep(nr2,each=numextra)))
    #dff$order_ind <- seq(from=1,to=nr1)
    if(grplabcheck)
    {
      if(length(intersect(colnames(dff),colnames(grplabloop)))!=0) stop(paste0("sortInd: One or more header labels in the run file are duplicated in grplab header. Change labels to be unique. Following are the duplicate label(s): (",paste0(intersect(colnames(dff),colnames(grplabloop)),collapse=", "),")."))
      dff <- cbind(dff,grplabloop)
      dff[[selgrp]] <- grplabloop[,selgrp]
      dff[[selgrp]] <- factor(dff[[selgrp]],levels=rle(dff[[selgrp]])$values)
    }

    # split and plot rows
    dlist <- split(dff,dff$rows)
    plist <- vector("list",length=nr2)
    #widthsvec <- vector(length=nr2)
    for (j in 1: nr2)
    {
      dlist[[j]]$rows <- NULL

      if(grplabcheck)
      {
        scols <- setdiff(colnames(dlist[[j]]),c("ind",names(grplabloop)))
        #df2 <- gather_(dlist[[j]],key_col="variable",value="value",gather_cols=scols)
        df2 <- pivot_longer(data=dlist[[j]],cols=scols,names_to="variable",values_to="value") 
      }else{
        #df2 <- gather(dlist[[j]],"variable","value",-ind)
        df2 <- pivot_longer(data=dlist[[j]],cols=colnames(dlist[[j]])[c(!colnames(dlist[[j]]) %in% c("ind"))],names_to="variable",values_to="value") 
      }

      #df2 <- df2[rev(1:nrow(df2)),]
      plist[[j]] <- ggplot(data=df2,aes(x=ind,y=value,fill=variable))+
        geom_bar(width=barsize,size=barbordersize,colour=barbordercolour,stat="identity",position="fill",na.rm=na.rm)+
        scale_x_discrete(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0),limits=c(0,1))+
        scale_fill_manual(values=coll,labels=legendlab1)+
        labs(x=NULL,y=NULL)+
        #facet_wrap(~selgrp,nrow=1,scales="free_x")+
        get(theme)(base_family=font,base_size=basesize)+
        theme(legend.position="none",
              panel.grid=element_blank(),
              panel.background=element_blank(),
              axis.line=element_blank(),
              axis.title=element_blank(),
              axis.text.x=element_text(size=indlabsize,colour=indlabcol,angle=indlabangle,vjust=indlabvjust,hjust=indlabhjust,margin=margin(t=indlabspacer)),
              axis.text.y=element_text(size=indlabsize,colour=indlabcol,margin=margin(r=indlabspacer)),
              axis.ticks=element_line(size=ticksize,colour=indlabcol),
              axis.ticks.length=unit(ticklength,"cm"),
              plot.margin=unit(mar,units))

      if(grplabcheck)
      {
        plist[[j]] <- plist[[j]] + facet_grid(as.formula(paste0("~",paste0(names(grplabloop),collapse="+"))),scales="free_x",space="free_x")+
          theme(strip.background=element_rect(fill=grplabbgcol),
                strip.text=element_text(size=grplabsize,colour=grplabcol))
        if(!showgrplab) plist[[j]] <- plist[[j]] + theme(strip.text=element_blank())
      }

      # remove y-axis
      if(!showyaxis) plist[[j]] <- plist[[j]] + theme(axis.ticks.y=element_blank(),axis.text.y=element_blank())
      # remove indlab
      if(!showindlab) plist[[j]] <- plist[[j]] + theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
      # remove ticks
      if(!showticks) plist[[j]] <- plist[[j]] + theme(axis.ticks=element_blank())

      # show title
      if(j==1)
      {
        if(showtitle) plist[[j]] <- plist[[j]]+labs(title=titlename)+
            theme(plot.title=element_text(size=titlesize,colour=titlecol,
                                          angle=titleangle,hjust=titlehjust,face=titleface,
                                          vjust=titlevjust,margin=margin(b=titlespacer)))
        # show subtitle
        if(showsubtitle) plist[[j]] <- plist[[j]]+labs(subtitle=subtitlename)+
            theme(plot.subtitle=element_text(size=subtitlesize,colour=subtitlecol,
                                             angle=subtitleangle,hjust=subtitlehjust,face=subtitleface,
                                             vjust=subtitlevjust,margin=margin(b=subtitlespacer)))
        # show legend
        if(showlegend) plist[[j]] <- plist[[j]]+
            theme(legend.position="top",
                  legend.direction="horizontal",
                  legend.title=element_blank(),
                  legend.key.size=unit(legendkeysize,"points"),
                  legend.text=element_text(size=legendtextsize,colour=indlabcol),
                  legend.spacing=unit(0,"points"),
                  legend.justification=legendpos,
                  legend.margin=margin(legendmargin[1],legendmargin[2],legendmargin[3],legendmargin[4],"points"),
                  legend.box.spacing=unit(1.5,"points"))
      }

      # calculate widths. not implemented.
      #widthsvec[j] <- nrow(dlist[[j]])/spl1
    }

    # lpp calculations
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
    list_panel <- vector("list",length=numpages)
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

      if(exportplot)
      {
        message("Drawing plot ...")
        if(imgtype=="tiff") tiff(file.path(exportpath,paste0(outname1,".tiff")),height=height,width=width,res=dpi,units=units,compression="lzw",family=font)
        if(imgtype=="png") png(file.path(exportpath,paste0(outname1,".png")),height=height,width=width,res=dpi,units=units,family=font)
        if(imgtype=="jpeg") jpeg(file.path(exportpath,paste0(outname1,".jpg")),height=height,width=width,res=dpi,units=units,quality=100,family=font)
        if(imgtype=="pdf") pdf(file.path(exportpath,paste0(outname1,".pdf")),height=height,width=width,fonts=font)

        do.call(grid.arrange,alist)
        dev.off()

        if(imgtype=="tiff") message(file.path(exportpath,paste0(outname1,".tiff exported.")))
        if(imgtype=="png") message(file.path(exportpath,paste0(outname1,".png exported.")))
        if(imgtype=="jpeg") message(file.path(exportpath,paste0(outname1,".jpg exported.")))
        if(imgtype=="pdf") message(file.path(exportpath,paste0(outname1,".pdf exported.")))
      }

      if(returnplot)
      {
        px <- do.call(arrangeGrob,alist)
        list_panel[[r]] <- px
        names(list_panel)[r] <- outname1
      }

      e <- stop1
      r=r+1
    }
    if(returnplot) list_plot[[i]] <- list_panel
    rm(nr1,nr2,numrows,numextra,numpages,start1,stop1,e,r,dlist,plist,df2,dff)
  }

  if(returnplot && !returndata) return(list(plot=list_plot,data=list(qlist=NA,grplab=NA)))
  if(!returnplot && returndata) return(list(plot=NA,data=list(qlist=list_qlist,grplab=list_grplab)))
  if(returnplot && returndata) return(list(plot=list_plot,data=list(qlist=list_qlist,grplab=list_grplab)))
}
