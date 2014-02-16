\name{collectClumppOutput}
\alias{collectClumppOutput}
\title{Collect CLUMPP output files from multiple folders}
\usage{
collectClumppOutput(prefix = "STRUCTUREpop", filetype = "aligned",
  runsdir = NA, newdir = NA, quiet = FALSE)
}
\arguments{
  \item{prefix}{The prefix of the CLUMPP directories before
  the underscore. For ex. if the directories are
  STRUCTUREpop_K2, then prefix is STRUCTUREpop.}

  \item{filetype}{the type of file to be copied. Options
  are 'aligned' to copy aligned files only, 'merged' to
  copy merged files only and 'both' to copy both files.}

  \item{runsdir}{The directory containing CLUMPP output
  files in multiple directories. Use \code{choose.dir()}
  for interactively selecting the directory. If NA, the
  current working directory is used.}

  \item{newdir}{The name of the new directory to be created
  with the collected runs. IF NA, the a directory name
  joining prefix and filetype is created.}

  \item{quiet}{Set to FALSE by default. Prints number of
  folders processed and number of files processed.}
}
\value{
Two integers are ruturned. The first denotes the number of
directories processed. The second number denotes the number
files copied.
}
\description{
Collect CLUMPP output files from multiple folders to one
folder
}
\details{
Within each CLUMPP output folder, the function searches for
filenames containing combination of prefix and filetype.
This file is copied to the new folder. Therefore, do not
manually rename CLUMPP output files or output directories.
}

