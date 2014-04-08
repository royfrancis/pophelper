\name{tabulateRunsStructure}
\alias{tabulateRunsStructure}
\title{Tabulate STRUCTURE runs}
\usage{
tabulateRunsStructure(files = NULL, writetable = FALSE, sorttable = TRUE,
  quiet = FALSE)
}
\arguments{
  \item{files}{A character vector of STRUCTURE output files
  to be tabulated. Use \code{choose.files(multi=TRUE)} for
  interactive selection.}

  \item{writetable}{Set to FALSE by default. Setting to
  TRUE writes the output table as a tab-delimited text file
  in the working directory.}

  \item{sorttable}{Default set to T. Sorts table by loci,
  ind and K.}

  \item{quiet}{Set to FALSE by default to print number of
  selected files. If set to TRUE, then number of selected
  files are not printed.}
}
\value{
Returns a dataframe with all runs sorted by loci, ind and K
(if sorttable=T). The table has 10 columns namely file
name, value of K, number of individuals, number of loci,
estimated ln probability of data, mean value of ln
likelihood, variance of ln likelihood, mean value of alpha,
number of burn-in and number of repeats. Missing values are
given NA.
}
\description{
Creates a table from STRUCTURE output files with various
STRUCTURE parameters. Refer to return for detailed list of
columns.
}
\details{
The row numbers of the output table denotes the file number
selected. This is helpful if a particular file from the
table needs to be identified in the selection vector.
}

