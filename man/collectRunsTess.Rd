\name{collectRunsTess}
\alias{collectRunsTess}
\title{Collect TESS cluster run files from multiple folders}
\usage{
collectRunsTess(runsdir = NA, newdir = NA, quiet = FALSE)
}
\arguments{
  \item{runsdir}{The directory containing TESS runs in
  multiple directories. Use \code{choose.dir()} for
  interactively selecting the directory. If NA, or no
  directory is selected, the current working directory is
  used.}

  \item{newdir}{The name of the new directory to be created
  with the collected runs. IF NA, the default name
  'AllTESSRuns' is used.}

  \item{quiet}{Set to FALSE by default. Prints directories
  without TESS runs and number of runs copied and renamed.}
}
\value{
Two integers are ruturned. The first denotes the number of
TESS run files copied and renamed. The second number
denotes number of directories without TESS run files.
}
\description{
Collect TESS cluster run files from multiple folders to one
folder and rename by run
}
\details{
Within each TESS run folder, the function searches for
filename ending with 'TR.txt' as the cluster file. This
file is copied to the new folder and renamed as the name of
the respective run directory. Therefore, do not manually
rename original run files or directories.
}

