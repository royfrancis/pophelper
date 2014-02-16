\name{plotRuns}
\alias{plotRuns}
\title{Plot STRUCTURE, TESS or table files as barplots.}
\usage{
plotRuns(files = NULL, imgoutput = "sep", poplab = NA, popcol = NA,
  imgtype = "png", height = NA, width = NA, dpi = NA, units = NA,
  flabsize = NA, flabcol = NA, flabbackcol = NA, labpos = NA,
  labsize = NA, labangle = NA, labjust = NA, labcol = NA,
  pointsize = NA, pointcol = NA, pointtype = NA, linepos = NA,
  linethick = NA, linecol = NA, fmar = NA)
}
\arguments{
  \item{files}{One or more STRUCTURE/TESS/table files}

  \item{imgoutput}{Options are 'sep','join' or'tab'.If
  output="sep", STRUCTURE/TESS run files are plotted as
  separate image files. If output="join", STRUCTURE/TESS
  run files are joined into a single image. If
  output="tab", combined/aligned/merged files are plotted
  into separate or joined plots based on number of tables
  within each file.}

  \item{poplab}{A character vector of population labels
  equal to length of individuals. Each pop name must repeat
  to the number of individuals present in each pop.}

  \item{popcol}{A vector of colours (representing
  populations) for colouring clusters. If NA, colours are
  automatically generated. K 1 to 12 are custom unique
  colours while K>12 are coloured by function
  rich.color().}

  \item{imgtype}{Image file type. Possible options are
  "png","jpeg" or "pdf". For pdf, height and width are in
  inches and res does not apply.}

  \item{height}{Height of individual run plot. By default,
  automatically generated based on number of Individuals.
  Ranges between 2.5cm and 4.6cm.}

  \item{width}{Width of individual run plot. By default,
  automatically generated based on number of individuals.
  Ranges between 5cm and 30cm.}

  \item{dpi}{Image resolution. Defaults to 300.}

  \item{units}{Units of height and width. Default set to
  "cm". If type is eps, units must be "in".}

  \item{flabsize}{The size of the filename label. Defaults
  to 4.}

  \item{flabcol}{The colour of the filename label. Defaults
  to "grey10".}

  \item{flabbackcol}{The background colour of the filename
  label. Defaults to white}

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
Nothing is returned.
}
\description{
Plot one or more STRUCTURE/TESS output files or table files
(aligned/combined/merged) files. The STRUCTURE and TESS
files can be plotted individually or joined together. The
table files will be plotted based on number of runs in each
file.
}
\details{
It is possible to set either height or width and leave
other as default.
}

