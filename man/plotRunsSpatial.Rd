\name{plotRunsSpatial}
\alias{plotRunsSpatial}
\title{plotRunsSpatial}
\usage{
plotRunsSpatial(datafile = NULL, coordsfile = NULL, popcol = NA,
  exportplot = TRUE, imgtype = "png", height = NA, width = NA,
  units = "cm", res = 200, showaxis = FALSE, pointcol = "grey10",
  pointtype = "+", pointsize = 4, pointtransp = 0.9, chull = FALSE,
  chulltransp = 0.02, chullsize = 0.4, chulltype = 1, ellipse = TRUE,
  ellconf = 0.95, ellsize = 0.4, elltype = 1, ellpoints = 100,
  legend = TRUE, legendlabels = NA, legendpos = c(1, 1),
  legendjust = c(1, 1), legendsize = NA, legendtextsize = NA,
  plottitle = NULL, filename = NA, setutm = FALSE, dataout = FALSE)
}
\arguments{
  \item{datafile}{One STRUCTURE or TESS output file. Input
  is either a character or a dataframe. If character, then
  a path pointing to location of the datafile. Can use
  `choose.files()`. If a dataframe, then an output from
  `runsToDfStructure()` or ` runsToDfTess()`.}

  \item{coordsfile}{A character or a dataframe. If
  character, then a path pointing to location of the
  coordsfile. It must be a tab-delimited text file with x
  and y coordinates of the samples. The number of rows must
  be equal to the number of samples in datafile. The
  coordsfiles must have no header and 2 columns in the
  order: x (latitude) and then y (longitude). Coordinates
  must be in standard longitude latitude (LL) decimals.}

  \item{popcol}{A vector of colours for the clusters. R
  colour names or hexadecimal values. If NA, colours are
  automatically generated. K 1 to 12 are custom unique
  colours while K>12 are coloured by function
  rich.color().}

  \item{exportplot}{If set to FALSE, no image is exported.}

  \item{imgtype}{The export format for figures. Options are
  "png", "jpeg" or "pdf". If pdf, height and width must be
  in inches and res argument is ignored (set to 300).}

  \item{height}{The height of export figure in cm unless
  units are changed. If `imgtype` is pdf, then height must
  be in inches.}

  \item{width}{The width of export figure in cm unless
  units are changed. If `imgtype` is pdf, then height must
  be in inches.}

  \item{units}{The units of measurement for figure
  dimensions. "cm", "mm" or "in".}

  \item{res}{The pixel resolution of the export image. Set
  to 200 by default.}

  \item{showaxis}{If TRUE, then axis text, axis ticks and
  plot border are plotted.}

  \item{pointcol}{The colour of sample points. An R colour
  or hexadecimal value.}

  \item{pointtype}{The shape/pch of sample points. A
  numeric or a character. By default, "+" is used for all
  points. If NA, then each cluster is plotted using a
  different shape. If a numeric or character of length one
  is used, then it is used for all points. If a vector is
  used, then it must be equal to number clusters.}

  \item{pointsize}{The size of sample points. A number
  usually 0.4,0.8,1,3 etc.}

  \item{pointtransp}{The transparency of points. A value
  betwwen 0 to 1.}

  \item{chull}{If TRUE, then the convex hull is computed
  for each cluster. The outer points of each cluster are
  connected by lines. If less than 3 points are available
  in a cluster, then convex hull is not computed and a
  warning is shown.}

  \item{chulltransp}{The transparency of the convex hull. A
  value between 0 and 1.}

  \item{chullsize}{The thickness of the convex hull
  border.}

  \item{chulltype}{The line type of the convex hull
  border.}

  \item{ellipse}{If TRUE, draws an ellipse around the
  clusters.}

  \item{ellconf}{The confidence interval of the ellipse.
  Defaults to 0.95.}

  \item{ellsize}{The thickness of the ellipse line.}

  \item{elltype}{The linetype for the ellipse.}

  \item{ellpoints}{The number of points on the ellipse.}

  \item{legend}{If TRUE, the legend for the colours is
  plotted.}

  \item{legendlabels}{A vector of labels for the clusters.
  Defaults to cluster numbers.}

  \item{legendpos}{Position of the legend. If
  "right","left","top" or "bottom", then, legend is plotted
  outside the plot area. To plot inside plot area use a 2
  vale vector. If a vector like c(1,1), first value denotes
  x-axis from 0 to 1 and second value denotes y-axis from 0
  to 1. For ex. to plot in bottom left corner, use c(0,0).}

  \item{legendjust}{The x and y axis justification of the
  legend. A 2 value vector.}

  \item{legendsize}{The size of the legend in cm. Usually
  values like 0.5,0.7,1.2 etc. The legendsize does not
  control the text in the legend.}

  \item{legendtextsize}{The size of the text in the
  legend.}

  \item{plottitle}{The title for the plot. NULL by
  default.}

  \item{filename}{A name for the export figure.
  Automatically computed if NA.}

  \item{setutm}{If TRUE, then LL coordinates are converted
  to UTM coordinates. The midpoint of the longitude within
  the dataset is used to determine the UTM zone.}

  \item{dataout}{If set to TRUE, a list of one or more
  ggplot gtable elements are returned. This output can be
  modified using ggplot themes() for more figure control if
  required.}
}
\value{
If dataout=T, a list of one or more ggplot gtable output is
returned for more theme control if required.
}
\description{
Plot STRUCTURE or TESS runs spatial and colour by
population clusters
}
\details{
The coordinates must always be provided as standard
longitude-latitude (LL) decimal format.
}

