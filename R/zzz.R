# On Load ----------------------------------------------------------------------

.onLoad <- function(...){
  packageStartupMessage("pophelper v2.3.0 ready.")
}

#' @importFrom grDevices colorRampPalette dev.off jpeg pdf png tiff
#' @importFrom stats aggregate as.formula ave sd
#' @importFrom utils packageDescription read.delim read.table setTxtProgressBar str txtProgressBar write.table