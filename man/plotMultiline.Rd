\name{plotMultiline}
\alias{plotMultiline}
\title{Plot STRUCTURE/TESS/table run files in multiline}
\usage{
plotMultiline(files = NA, spl = NA, lpp = NA, popcol = NA,
  na.rm = FALSE, barwidth = 0.9, barspace = 0.1, indlabs = TRUE,
  labsize = 5, labangle = 90, labvjust = 0.5, labhjust = 1,
  imgtype = "png", height = NA, width = NA, res = NA, units = NA)
}
\arguments{
  \item{files}{One or more STRUCTURE output files. Use
  \code{choose.files(multi=TRUE)} for interactive
  selection.}

  \item{spl}{samples per line. Defaults to 60.}

  \item{lpp}{lines per page. Defaults to 11.}

  \item{popcol}{A vector of colours for populations.}

  \item{na.rm}{Default set to FALSE. NAs are not removed
  from data, therefore ggplot prints warning messages for
  NAs. If set to TRUE, NAs are removed before plotting and
  ggplot NA warning is suppressed.}

  \item{barwidth}{The width of the bars.}

  \item{barspace}{The space between the bars.}

  \item{indlabs}{By default, \code{indlabs=TRUE}, then
  individual labels 1, 2, 3.. are indicated below bars. To
  hide labels, set \code{indlabs=FALSE}}

  \item{labsize}{The size of the labels.}

  \item{labangle}{The angle of labels.}

  \item{labvjust}{The vertical justification of the
  labels.}

  \item{labhjust}{The horizontal justification of the
  labels.}

  \item{imgtype}{Figure output format. Options are 'png',
  'jpeg' or 'pdf'. If pdf, height and width must be in
  inches and res argument is ignored.}

  \item{height}{Height of the full figure. If NA, height is
  set to 29.7cm (A4 height).}

  \item{width}{Width of the full figure. If NA, width is
  set to 21cm (A4 width).}

  \item{res}{Resolution of the figure. Default is 300.}

  \item{units}{Units of dimension of the figure. Default is
  cm.}
}
\description{
Plot STRUCTURE/TESS run files as barplots with multiple
lines
}
\details{
Figures are always created to A4 size. Any plotted row will
span the width of the figure. Note that this function is
slow and may take several minutes when plotting mutiple
tables.
}

