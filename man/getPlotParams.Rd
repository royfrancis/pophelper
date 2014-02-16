\name{getPlotParams}
\alias{getPlotParams}
\title{Internal: Generate parameters for plots with labels}
\usage{
  getPlotParams(poplab = NA, plotnum = 1, labpos = NA,
    labsize = NA, labangle = NA, labjust = NA, labcol = NA,
    pointsize = NA, pointcol = NA, pointtype = NA,
    linepos = NA, linethick = NA, linecol = NA, fmar = NA)
}
\arguments{
  \item{poplab}{A character vector of labels same length as
  number of individuals.}

  \item{plotnum}{Number of plots on the figure.}

  \item{labpos}{The y position of the labels.}

  \item{labsize}{Size of the labels.}

  \item{labangle}{Angle/Rotation of labels. 0 is horizontal
  while 90 is vertical.}

  \item{labjust}{Justification of labels. Defaults to 0.5
  if labangle=0 or 1 if labangle between 20 and 135.}

  \item{labcol}{Colour of labels. Defaults to "grey30".}

  \item{pointsize}{Size of points on label marker line.}

  \item{pointcol}{Colour of the points on the label marker
  line. Defaults to "grey30".}

  \item{pointtype}{Type of points on the label marker line.
  Defaults to |.}

  \item{linepos}{The y position of the label marker line
  and the points.}

  \item{linethick}{Thickness of the label marker line.}

  \item{linecol}{Colour of the label marker line. Defaults
  to "grey30".}

  \item{fmar}{Figure margins in 'lines' unit. A vector of 4
  numbers for top, right, bottom and left margins. ex.
  c(0.2,0.2,0.2,0).}
}
\value{
  A list with following plot parameters: poplab, plotnum,
  labpos, labsize, labangle, labjust, labcol, pointsize,
  pointcol, pointtype, linepos, linethick, linecol, fmar
}
\description{
  Generates various parameters required for plotting with
  labels. Internal function.
}

