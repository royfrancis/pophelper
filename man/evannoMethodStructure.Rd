\name{evannoMethodStructure}
\alias{evannoMethodStructure}
\title{Perform the Evanno method}
\usage{
  evannoMethodStructure(data = NULL, writetable = F,
    showplot = T, exportplot = F, imgtype = "png",
    height = NA, width = NA, res = NA, units = NA)
}
\arguments{
  \item{data}{A dataframe with summarised runs. An output
  from summariseRunsStructure(). Must have minimum 7
  columns named meanelpd, sd, k, runs, loci, maxelpd and
  minelpd.}

  \item{writetable}{Set to F by default. Setting to T
  writes the output table as a tab-delimited text file in
  the working directory.}

  \item{showplot}{If set to T, shows the Evanno plots in
  the graphical device. If Evanno method cannot be
  computed, a kPlot (elpd over k) is shown instead.}

  \item{exportplot}{If set to T, exports the Evanno plots
  as image in the working directory. If Evanno method
  cannot be computed, a kPlot (elpd over k) is exported
  instead.}

  \item{imgtype}{Type of exported image. Default set to
  png. Other possible options are jpeg or pdf.}

  \item{height}{Height of exported image. Default units in
  px. If imgtype is pdf, height must be in inches.}

  \item{width}{Width of exported image. Default units in
  px. If imgtype is pdf, height must be in inches.}

  \item{res}{Resolution of exported image. Default set to
  200. If imgtype is pdf, this option does not apply.}

  \item{units}{Units of measure of the export image. Some
  options include 'px','in','cm','mm' etc. By default,
  units is set to 'px' for png and jpeg and to 'in' if
  imgtype is 'pdf'.}
}
\value{
  Returns a dataframe with all values sorted by K. The
  table has 16 columns namely Mean estimated ln probability
  of data, Standard deviation, Value of K, Number of runs
  for each K, Number of runs for each K, Number of
  individuals for each K, Number of loci for each K,
  Estimated ln probability of data plus standard deviation,
  Estimated ln probability of data minus standard
  deviation, First derivative, Max error of first
  derivative, Min error of first derivative, Second
  derivative, Max error of second derivative, Min error of
  second derivative, Third derivative and Best value of K.
}
\description{
  The Evanno method for detecting the appropriate number of
  population clusters from STRUCTURE results. Creates table
  and figure with Evanno method derivatives. Refer to
  return for detailed list of columns. See details for
  Evanno method reference.
}
\details{
  The Evanno method is based on the paper: Evanno, G.,
  Regnaut, S., & Goudet, J. (2005). Detecting the number of
  clusters of individuals using the software STRUCTURE: a
  simulation study. Molecular ecology, 14(8), 2611-2620.
  The Evanno plot generated from this function can be
  recreated from the returned dataframe if furthur
  customisation is required.
}

