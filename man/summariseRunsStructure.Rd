\name{summariseRunsStructure}
\alias{summariseRunsStructure}
\title{Summarise STRUCTURE runs}
\usage{
  summariseRunsStructure(data = NULL, writetable = F)
}
\arguments{
  \item{data}{A dataframe with tabulated runs. An output
  from tabulateRunsStructure(). Must have minimum 4 columns
  named k, ind, loci and elpd.}

  \item{writetable}{Set to F by default. Setting to T
  writes the output table as a tab-delimited text file in
  the working directory.}
}
\value{
  Returns a dataframe with all values of K sorted by K. The
  table has 6 columns namely Mean estimated ln probability
  of data, Standard deviation, Value of K, Number of runs
  for each K, Estimated ln probability of data plus
  standard deviation, Estimated ln probability of data
  minus standard deviation.
}
\description{
  Creates a summary table of several STRUCTURE runs with
  means and std deviation. Refer to return for detailed
  list of columns.
}

