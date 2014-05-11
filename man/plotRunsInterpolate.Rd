\name{plotRunsInterpolate}
\alias{plotRunsInterpolate}
\title{Interpolate STRUCTURE and TESS runs spatially}
\usage{
plotRunsInterpolate(datafile = NULL, coordsfile = NULL, method = "krig",
  duplicate = "mean", idwpower = 2, clusters = NA, gridsize = 60,
  imgoutput = "join", colours = NA, nrow = NA, ncol = NA,
  exportplot = TRUE, imgtype = "png", height = NA, width = NA,
  units = "cm", res = 200, showaxis = FALSE, addpoints = TRUE,
  pointcol = "grey10", pointtype = "+", pointsize = 4, legend = TRUE,
  legendpos = c(1, 1), legendjust = c(1, 1), legendsize = NA,
  legendtextsize = NA, dataout = FALSE)
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
  order: x (latitude) and then y (longitude).}

  \item{method}{The method employed for interpolation.
  Options are "bilinear", "bicubic", "krig" (Kriging),
  "idw" (Inverse distance weighting) or "nn" (nearest
  neighbour). See Details for more information.}

  \item{duplicate}{How to deal with duplicate spatial
  locations. This is only applicable to 'bilinear' and
  'bicubic' methods. Options are "error" (error message if
  duplicate points), "strip" (remove all duplicate points),
  "mean" (mean of duplicate points), "median" (median of
  duplicate points).}

  \item{idwpower}{The power of inverse distance weighting
  if method is set to "idw".}

  \item{clusters}{Which clusters to plot? If NA, all
  clusters are plotted. For ex. If set to 2, cluster 2 is
  plotted. If set to 2:4, clusters 2, 3 and 4 are plotted.
  If set to c(1,4,5), clusters 1, 4 and 5 are plotted.}

  \item{gridsize}{The size of the image grid on which
  interpolation is to be carried out. Set to 60 by default.
  Higher values produces less pixelated grids, but more
  computationally intensive.}

  \item{imgoutput}{To plot each cluster as a seperate
  figure, set to "sep". To plot multiple clusters in a
  single figure, set to "join". If number of clusters is
  less than 2, then automatically set to "sep".}

  \item{colours}{A vector of colours. R colour names or
  hexadecimal values. Set to 9 value 'Blues' palette from
  RColorBrewer by default.}

  \item{nrow}{Number of rows of plots in a joined plot.
  Determined automatically if number of plots <20 and
  nrow=NA.}

  \item{ncol}{Number of columns of plots in a joined plot.
  Determined automatically if number of plots <20 and
  ncol=NA.}

  \item{exportplot}{If set to FALSE, no image is exported.}

  \item{imgtype}{The export format for figures. Options are
  "png", "jpeg" or "pdf".}

  \item{height}{The height of export figure in cm unless
  units are changed.}

  \item{width}{The width of export figure in cm unless
  units are changed.}

  \item{units}{The units of measurement for figure
  dimensions.}

  \item{res}{The pixel resolution of the export image. Set
  to 200 by default.}

  \item{showaxis}{If TRUE, then axis text, axis ticks and
  plot border are plotted.}

  \item{addpoints}{If TRUE, then sample coordinates are
  overplotted on interpolated grid.}

  \item{pointcol}{The colour of sample points. An R colour
  or hexadecimal value.}

  \item{pointtype}{The shape/pch of sample points. A
  numeric or a character.}

  \item{pointsize}{The size of sample points. A number
  usually 0.4,0.8,1,3 etc.}

  \item{legend}{If TRUE, the legend for the colours is
  plotted.}

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
Interpolate clusters from STRUCTURE and TESS runs spatially
using coordinates.
}
\details{
The "bilinear", "bicubic", "idw" and "nn" are essentially
direct interpolation between spatial points. The "krig"
option is predictive rather than direct interpolation. For
more details of methods, see R package 'akima' function
'interp' for "bilinear" and "bicubic" methods, see R
package 'fields' function 'Krig' for "krig" method, see R
package 'spatstat' function 'idw' for "idw" and function
'nnmark' for "nn" method. The model for "krig" is
automatically determined and may produce warning messages
if the GCV algorithm does not converge optimally. This can
be ignored for more purposes. All methods require full
coordinate data. No missing data allowed in coordsfile.
}

