\name{clumppExportTess}
\alias{clumppExportTess}
\title{Combine TESS runs and export files for use with software CLUMPP}
\usage{
  clumppExportTess(files = NULL, prefix = NA)
}
\arguments{
  \item{files}{A character vector of TESS cluster run files
  to be tabulated. Use choose.files(multi=TRUE) for
  interactive selection.}

  \item{prefix}{A character prefix for folder names.}
}
\value{
  The combined file and paramfile are written into
  respective folders named by population. A tabulated table
  is returned from function same as tabulateRunsTess().
}
\description{
  Takes multiple TESS runs and combines several repeats for
  each K into a single file along with a parameter file.
  The two output files are organised into folders by K. The
  CLUMPP.exe file can simply be copied to this folder and
  run to reorder the clusters for each K.
}
\details{
  When multiple repeats are run for each K in TESS, the
  order of clusters may be jumbled for each run. Therefore,
  when plotting multiple runs within each K, the colours
  cannot be assigned correctly. The software CLUMPP helps
  to overcome this issue by reordering the clusters
  correctly. This function clumppExportTess() takes
  multiple runs for each K and combines them into a single
  file and generates a parameter file for easy use with
  CLUMPP. Further details for CLUMPP can be found here:
  Jakobsson, M., & Rosenberg, N. A. (2007). CLUMPP: a
  cluster matching and permutation program for dealing with
  label switching and multimodality in analysis of
  population structure. Bioinformatics, 23(14), 1801-1806.
}

