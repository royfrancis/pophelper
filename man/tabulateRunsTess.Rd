\name{tabulateRunsTess}
\alias{tabulateRunsTess}
\title{Tabulate TESS runs}
\usage{
tabulateRunsTess(files = NULL, writetable = FALSE, quiet = FALSE)
}
\arguments{
  \item{files}{A character vector of TESS cluster files to
  be tabulated. Use \code{choose.files(multi=TRUE)} for
  interactive selection. Use \code{collectRunsTess()} to
  collect TESS runs from multiple folders into one.}

  \item{writetable}{Set to FALSE by default. Setting to
  TRUE writes the output table as a tab-delimited text file
  in the same folder as the STRUCTURE run files.}

  \item{quiet}{If set to TRUE, then number of selected
  files are not printed.}
}
\value{
Returns a dataframe with filenames, K and number of
individuals of all runs sorted by ind and K.
}
\description{
Creates a table from TESS output files with filenames, K
and number of individuals.
}
\details{
The row numbers of the output table denotes the file number
selected. This is helpful if a particular file from the
table needs to be identified in the selection vector.
}

