# pophelper v2.0.0
# Deprecated functions
# 22-Dec-2016

# tabulateRunsStructure --------------------------------------------------------

#' @title Deprecated
#' @description Deprecated
#' @seealso \code{\link{tabulateQ}},
#' @export
#' 
tabulateRunsStructure <- function(files = NULL, writetable = FALSE, sorttable = TRUE, quiet = FALSE)
{
  stop("tabulateRunsStructure() is deprecated. Use tabulateQ().")
}

# tabulateRunsTess -------------------------------------------------------------

#' @title Deprecated
#' @description Deprecated
#' @seealso \code{\link{tabulateQ}},
#' @export
#'
tabulateRunsTess <- function(files = NULL, writetable = FALSE, sorttable = TRUE, quiet = FALSE)
{
  stop("tabulateRunsTess() is deprecated. Use tabulateQ().")
}

# tabulateRunsAdmixture --------------------------------------------------------

#' @title Deprecated
#' @description Deprecated
#' @seealso \code{\link{tabulateQ}},
#' @export
#'
tabulateRunsAdmixture <- function(files = NULL, writetable = FALSE, sorttable = TRUE, quiet = FALSE)
{
  stop("tabulateRunsAdmixture() is deprecated. Use tabulateQ().")
}

# tabulateRunsMatrix -----------------------------------------------------------

#FUNCTION tabulateRunsMatrix
#' @title Deprecated
#' @description Deprecated
#' @seealso \code{\link{tabulateQ}},
#' @export
#'
tabulateRunsMatrix <- function(files = NULL, writetable = FALSE, sorttable = TRUE, quiet = FALSE)
{
  stop("tabulateRunsMatrix() is deprecated. Use tabulateQ().")
}

# summariseRunsStructure -------------------------------------------------------

#' @title Deprecated
#' @description Deprecated.
#' @seealso \code{\link{summariseQ}},
#' @export
#' 
summariseRunsStructure <- summarizeRunsStructure <- function(data = NULL, writetable = FALSE)
{
  stop("summariseRunsStructure() is deprecated. Use summariseQ().")
}

# summariseRunsTess ------------------------------------------------------------

#' @title Deprecated
#' @description Deprecated
#' @seealso \code{\link{summariseQ}},
#' @export
#' 
summariseRunsTess <- summarizeRunsTess <- function(data = NULL, writetable = FALSE)
{
  stop("summariseRunsTess() is deprecated. Use summariseQ().")
}

# summariseRunsAdmixture -------------------------------------------------------

#' @title Deprecated
#' @description Deprecated
#' @seealso \code{\link{summariseQ}},
#' @export
#' 
summariseRunsAdmixture <- summarizeRunsAdmixture <- function(data = NULL, writetable = FALSE)
{
  stop("summariseRunsAdmixture() is deprecated. Use summariseQ().")
}

# summariseRunsMatrix ----------------------------------------------------------

#' @title Deprecated
#' @description Deprecated
#' @seealso \code{\link{summariseQ}},
#' @export
#' 
summariseRunsMatrix <- summarizeRunsMatrix <- function(data = NULL, writetable = FALSE)
{
  stop("summariseRunsMatrix() is deprecated. Use summariseQ().")
}

# clumppExportStructure --------------------------------------------------------

#' @title Deprecated
#' @description Deprecated 
#' @seealso \code{\link{clumppExport}}
#' @export
#' 
clumppExportStructure <- function(files = NULL, prefix = NA, parammode = NA, paramrep = NA, useexe=FALSE)
{
  stop("clumppExportStructure() is deprecated. Use clumppExport().")
  #clumppExport(files = readQStructure(files), prefix = prefix, parammode = parammode, paramrep = paramrep, useexe=useexe)
}

# clumppExportTess -------------------------------------------------------------

#' @title Deprecated
#' @description Deprecated 
#' @seealso \code{\link{clumppExport}}
#' @export
#' 
clumppExportTess <- function(files = NULL, prefix = NA, parammode = NA, paramrep = NA, useexe=FALSE)
{
  stop("clumppExportTess() is deprecated. Use clumppExport().")
  #clumppExport(files = readQTess(files), prefix = prefix, parammode = parammode, paramrep = paramrep, useexe=useexe)
}

# clumppExportAdmixture --------------------------------------------------------

#' @title Deprecated
#' @description Deprecated 
#' @seealso \code{\link{clumppExport}}
#' @export
#' 
clumppExportAdmixture <- function(files = NULL, prefix = NA, parammode = NA, paramrep = NA, useexe=FALSE)
{
  stop("clumppExportAdmixture() is deprecated. Use clumppExport().")
  #clumppExport(files = readQSimple(files), prefix = prefix, parammode = parammode, paramrep = paramrep, useexe=useexe)
}

# clumppExportMatrix -----------------------------------------------------------

#' @title Deprecated
#' @description Deprecated 
#' @seealso \code{\link{clumppExport}}
#' @export
#' 
clumppExportMatrix <- function(files = NULL, prefix = NA, parammode = NA, paramrep = NA, useexe=FALSE)
{
  stop("clumppExportMatrix() is deprecated. Use clumppExport().")
  #clumppExport(files = readQSimple(files), prefix = prefix, parammode = parammode, paramrep = paramrep, useexe=useexe)
}

# runsToDfStructure ------------------------------------------------------------

#' @title Deprecated.
#' @description Deprecated.
#' @seealso \code{\link{readQ}}
#' @export
#'
runsToDfStructure <- function(files = NULL)
{
  stop("runsToDfStructure() is deprecated. Use readQ().")
}

# runsToDfTess ------------------------------------------------------------

#' @title Deprecated.
#' @description Deprecated.
#' @seealso \code{\link{readQ}}
#' @export
#'
runsToDfTess <- function(files = NULL)
{
  stop("runsToDfTess() is deprecated. Use readQ().")
}

# runsToDfAdmixture ------------------------------------------------------------

#' @title Deprecated.
#' @description Deprecated.
#' @seealso \code{\link{readQ}}
#' @export
#'
runsToDfAdmixture <- function(files = NULL)
{
  stop("runsToDfStructure() is deprecated. Use readQ().")
}

# runsToDfMatrix ------------------------------------------------------------

#' @title Deprecated.
#' @description Deprecated.
#' @seealso \code{\link{readQ}}
#' @export
#'
runsToDfMatrix <- function(files = NULL)
{
  stop("runsToDfMatrix() is deprecated. Use readQ().")
}

# plotRuns ------------------------------------------------------------

#' @title Deprecated.
#' @description Deprecated.
#' @seealso \code{\link{plotQ}}
#' @export
#'
plotRuns <- function(files = NULL)
{
  stop("plotRuns() is deprecated. Use plotQ().")
}

# plotMultiline ------------------------------------------------------------

#' @title Deprecated.
#' @description Deprecated.
#' @seealso \code{\link{plotQMultiline}}
#' @export
#'
plotMultiline <- function(files = NULL)
{
  stop("plotMultiline() is deprecated. Use plotQMultiline().")
}

# analyseRuns ------------------------------------------------------------

#' @title Deprecated.
#' @description Deprecated.
#' #' @seealso \code{\link{analyseQ}}
#' @export
#'
analyseRuns <- function(files = NULL)
{
  stop("analyseRuns() is deprecated. Use analyseQ().")
}

# plotRunsSpatial ------------------------------------------------------------

#' @title Deprecated.
#' @description Deprecated.
#' @export
#'
plotRunsSpatial <- function(files = NULL)
{
  stop("plotRunsSpatial() is deprecated and no longer part of this package. Use plotQSpatial() from package pophelperSpatial.")
}

# plotRunsInterpolate ------------------------------------------------------------

#' @title Deprecated.
#' @description Deprecated.
#' @export
#'
plotRunsInterpolate <- function(files = NULL)
{
  stop("plotRunsInterpolate() is deprecated and no longer part of this package. Use plotQInterpolate() from package pophelperSpatial.")
}
