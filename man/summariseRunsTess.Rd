\name{summariseRunsTess}
\alias{summariseRunsTess}
\title{Summarise TESS runs}
\usage{
summariseRunsTess(data = NULL, writetable = FALSE)
}
\arguments{
  \item{data}{A dataframe with tabulated runs. An output
  from \code{tabulateRunsTess()}. Must have minimum 2
  columns named k and ind.}

  \item{writetable}{Set to FALSE by default. Setting to
  TRUE writes the output table as a tab-delimited text file
  in the working directory.}
}
\value{
Returns a dataframe with all values of K sorted by K. The
table has 3 columns namely value of K, number of runs for
each K and number of individuals.
}
\description{
Creates a summary table of several TESS runs with k, number
of runs and individuals.
}

