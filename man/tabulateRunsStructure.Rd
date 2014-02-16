\name{tabulateRunsStructure}
\alias{tabulateRunsStructure}
\title{Tabulate STRUCTURE runs}
\usage{
tabulateRunsStructure(files = NULL, writetable = FALSE, quiet = FALSE)
}
\arguments{
  \item{files}{A character vector of STRUCTURE output files
  to be tabulated. Use \code{choose.files(multi=TRUE)} for
  interactive selection.}

  \item{writetable}{Set to FALSE by default. Setting to
  TRUE writes the output table as a tab-delimited text file
  in the working directory.}

  \item{quiet}{Set to FALSE by default to print number of
  selected files. If set to TRUE, then number of selected
  files are not printed.}
}
\value{
Returns a dataframe with all runs sorted by K. The table
has 10 columns namely file name, value of K, Number of
individuals, Number of loci, Estimated ln probability of
data, Mean value of ln likelihood, Variance of ln
likelihood, Mean value of alpha, Number of burn-in and
Number of repeats. Missing values are given NA.
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

