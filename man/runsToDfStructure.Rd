\name{runsToDfStructure}
\alias{runsToDfStructure}
\title{Convert STRUCTURE run files to R dataframes.}
\usage{
runsToDfStructure(files = NA)
}
\arguments{
  \item{files}{One or more STRUCTURE run files. Use
  \code{choose.files(multi=TRUE)} to select interactively.}
}
\value{
If a single file is selected, a single dataframe is
returned. If multiple files are selected, a list with
multiple dataframes is returned.
}
\description{
Takes one or more STRUCTURE output files and converts each
of them to separate dataframes.
}

