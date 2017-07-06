# pophelper 2.2.0

`pophelper` is an R package and web app to analyse and visualise population structure. `pophelper` curently supports output run files generated from population analysis programs such as STRUCTURE, TESS and numeric delimited formats such as ADMIXTURE or fastSTRUCTURE. The `pophelper` package can be used to read run files to R, tabulate runs, summarise runs, estimate *K* using the Evanno method, export files for CLUMPP, export files for DISTRUCT and generate barplot figures.  

For a detailed demonstration and walkthrough, refer the online [vignette](http://royfrancis.github.io/pophelper/). New versions and updates are shown only on this GitHub page.

## Latest version

`ggplot2` version must be 2.2.0 or above. This version 2.2.0 has several breaking changes compared to previous version 2.1.0.  Some of the important changes are listed below:

+ Order of arguments have changed in `plotQ()` and `plotQMultiline()`
+ `plotQ()` and `plotQMultiline()` allow ordering of group labels using argument `ordergrp`
+ `plotQ()` and `plotQMultiline()` allows subsetting with multiple group label sets using argument `selgrp`
+ Group labels are provided as data.frame rather than lists
+ `plotQ()` and `plotQMultiline()` support naming of output files using argument `outputfilename`
+ `plotQ()` supports display and customisation of individual labels
+ `plotQMultiline()` displays group labels in strip panels
+ Several existing arguments in `plotQ()` and `plotQMultiline()` have changed

For full list of changes, refer to the [NEWS](https://github.com/royfrancis/pophelper/blob/master/NEWS).

## Installation  
You need to have R (> 3.3.0) statistical package installed on your system. [R](https://www.r-project.org/) is open-source and freely available to download for Windows, Mac and other OS. Then, install the `devtools` and other dependency packages. Then, you can install `pophelper` from `github` using the `devtools` package.  

```coffee
# install dependencies and devtools
install.packages(c("Cairo","ggplot2","gridExtra","gtable","tidyr","devtools"),dependencies=T)

# load package
library(devtools)

# install pophelper package from GitHub
install_github('royfrancis/pophelper')

# load library for use
library(pophelper)
```

Note that `pophelper 1.2.0` and later includes binary executables for CLUMPP and DISTRUCT. This is experimental and may not work on all OS and versions.

`pophelper 2.2.0` has been tested on the following systems: 

+ Windows 10 64bit, R 3.3.3
+ Windows 7 64bit, R 3.3.3 (DISTRUCT is unstable)
+ Scientific Linux 6.8 (Carbon) 64bit, R 3.3.2

## Web App   
An online interactive version of pophelper is available at [pophelper.com](http://www.pophelper.com). The web app is quite outdated and limited in terms of functionality and flexibility. The web app must not be used for major work or large datasets. The web app is also limited in computational power and working hours. If anyone interested has the resources or funding to host `pophelper` web app on a solid server, please get in touch.  
The web app will be automatically restricted after 100 hours of use per month. The cycle starts on the 11th of every month and the app is available until 100 hours are used, which is approximately (as of now) till 4th of next month. This, of course, varies and depends on the usage of the app. I will update info about unavailability here:

__Web app is currently unavailable from 4th July to 11th July.__

## List of Functions  

For help on any function, use  
`?tabulateQ`  
`?evannoMethodStructure`  

```coffee
readQ()                   # Read q-matrix run files to R qlist
tabulateQ()               # Tabulate a qlist
summariseQ()              # Summarise an output from tabulateQ()
clumppExport()            # Generate CLUMPP input/output files
collectClumppOutput()     # Collect CLUMPP output into a common directory
plotQ()                   # Create single-line barplots from qlist
PlotQMultiline()          # Create multi-line barplots from qlist
distructExport()          # Export files for DISTRUCT from qlist

evannoMethodStructure()   # Perform the Evanno method for STRUCTURE data
collectRunsTess()         # Collect TESS output from multiple directories into one

analyseQ()                # A wrapper function to quickly tabulate, summarise, 
                          # perform evanno method, clumpp output and generate
                          # barplots from filenames/paths.
```

## Sample figures

![Workflow](vignettes/workflow.png)  
__Fig:__ *Workflow for all filetypes.*  

![evanno-method](vignettes/evanno-plot.png)  
__Fig:__ *Plots from Evanno method.*  

![plotq](vignettes/plotq.png)  
__Fig:__ *Singleline barplots from q-matrices with individual and group labelling.*  

![plotq-multiline](vignettes/plotqmultiline-1.png)  
__Fig:__ *Multiline barplots from q-matrices with individual and group labelling.* 

For detailed demonstration and description, refer the [vignette](http://royfrancis.github.io/pophelper/).

### Disclaimer

The `pophelper` R package is offered free and without warranty of any kind, either expressed or implied. I will not be held liable to you for any damage arising out of the use, modification or inability to use this program. `pophelper` R package can be used, redistributed and/or modified freely for non-commercial purposes subject to the original source being properly cited. Licensed under GPL-3. Please make sure you verify all your results.  

### Contact

If you have an comments, suggestions, corrections or ideas on ways to improve or extend this package, feel free to contact me. Submit a report on the [Github issues page](https://github.com/royfrancis/pophelper/issues).  

2017 Roy M Francis  
