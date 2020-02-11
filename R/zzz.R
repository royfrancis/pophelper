
globalVariables(c("grp","k","elpdmean","elpdmax","elpdmin","lnk1","lnk1max","lnk1min","lnk2","lnk2max","lnk2min","deltaK","ind","run","order_ind","value","variable","labxpos","labypos","markerxpos","markerypos","divxpos","s2","grplab","order_cumulative"))

.onAttach <- function(...){
  packageStartupMessage("pophelper v2.3.1 ready.")
}

