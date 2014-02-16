\name{plotMultiline}
\alias{plotMultiline}
\title{Plot STRUCTURE/TESS run files in multiline}
\usage{
  plotMultiline(files = NA, spl = NA, lpp = NA,
    popcol = NA, height = NA, width = NA)
}
\arguments{
  \item{files}{One or more STRUCTURE output files.}

  \item{spl}{samples per line.}

  \item{lpp}{lines per page.}

  \item{popcol}{A vector of colours for populations.}

  \item{height}{Height of the plot in pixels. If NA, height
  is set to 500px by default.}

  \item{width}{Width of the plot in pixels. If NA, width is
  determined by number of individuals.}
}
\description{
  Plot STRUCTURE/TESS run files as barplots with multiple
  lines
}

