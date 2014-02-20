\name{summariseRunsStructure}
\alias{summariseRunsStructure}
\title{Summarise STRUCTURE runs}
\usage{
summariseRunsStructure(data = NULL, writetable = FALSE)
}
\arguments{
  \item{data}{A dataframe with tabulated runs. An output
  from \code{tabulateRunsStructure()}. Must have minimum 4
  columns named k, ind, loci and elpd.}

  \item{writetable}{Set to FALSE by default. Setting to
  TRUE writes the output table as a tab-delimited text file
  in the working directory.}
}
\value{
Returns a dataframe with all values of K sorted by loci,
ind and K. The table has 6 columns namely mean estimated ln
probability of data, standard deviation, value of K, Number
of runs for each K, number of individuals, number of loci,
estimated ln probability of data plus standard deviation,
estimated ln probability of data minus standard deviation.
}
\description{
Creates a summary table of several STRUCTURE runs with
means and std deviation. Refer to return for detailed list
of columns.
}

