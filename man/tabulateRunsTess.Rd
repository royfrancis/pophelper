\name{tabulateRunsTess}
\alias{tabulateRunsTess}
\title{Tabulate TESS runs}
\usage{
  tabulateRunsTess(files = NULL, writetable = F, quiet = F)
}
\arguments{
  \item{files}{A character vector of TESS cluster files to
  be tabulated. Use \code{choose.files(multi=TRUE)} for
  interactive selection. Use \code{collectRunsTess()} to
  collect TESS runs from multiple folders into one.}

  \item{writetable}{Set to F by default. Setting to T
  writes the output table as a tab-delimited text file in
  the same folder as the STRUCTURE run files.}

  \item{quiet}{If set to T, then number of selected files
  are not printed.}
}
\value{
  Returns a dataframe with filenames, K and number of
  individuals of all runs sorted by K.
}
\description{
  Creates a table from TESS output files with filenames, K
  and number of individuals.
}

