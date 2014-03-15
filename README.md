# pophelper 1.0.0

`pophelper` is an R package to analyse output files generated from population analysis programs such as STRUCTURE and TESS. The `pophelper` package can be used to tabulate runs, summarise runs, perform the Evanno method, export files for CLUMPP and generate barplot figures. A brief introduction and tutorial is provided here. For more detailed demonstration, refer the [vignette](https://github.com/royfrancis/pophelper/blob/master/inst/doc/Vignette.pdf).

Also, try the video demo [here](https://www.youtube.com/watch?v=iOqxXcQReJI).

## Installation  
You can install `pophelper` from `github` using the `devtools` package

```coffee
require(devtools)
install_github('pophelper', 'royfrancis')
require(pophelper)
```
For help on any function, use  
`?tabulateRunsStructure`  
`?evannoMethodStructure`

For functions where one or more files need to be selected, the selection can be performed interactively. Windows users can use `choose.files(multi=T)`. Mac users can use `file.choose()` for single selection and `tk_choose.files()` from `tcltk` package for multiple selection.

## Functions and workflow 
A list of important functions are shown below. Once the package is installed, and you have set a working directory, you can follow the mini tutorial below.

### 1. Tabulate runs  
Select multiple STRUCTURE or TESS runs and tabulate them into a table.

```coffee
#read sample STRUCTURE files from R package
flist<-list.files(path=system.file("/files/structure",package="pophelper"),full.names=T)
#read sample TESS files from R package
flist1<-list.files(path=system.file("/files/tess",package="pophelper"),full.names=T)
#Or select your own files
flist<-choose.files()

#tabulate STRUCTURE runs
df<-tabulateRunsStructure(files=flist)
#tabulate TESS runs
tabulateRunsTess(files=flist1)
```
### 2. Summarise runs  
The tabulated runs can be further condensed by repeats.

```coffee
#summarise STRUCTURE runs
df1<-summariseRunsStructure(df)
summariseRunsTess()
```

### 3. Evanno method  
This function calculates the Evanno derivatives, tables and figures. The output from `summariseRunsStructure()` can be provided as input.

```coffee
#perform evanno method
evannoMethodStructure(df1)
```
![Evanno Method](screenshots/Fig2.jpg)  
__Fig 1.__ Evanno Method

### 4. Convert to dataframe  
STRUCTURE and TESS run files can be converted to R dataframes using this function. If multiple files are selected, then a list of dataframes is returned.

```coffee
#convert STRUCTURE run files to R dataframe
runsToDfStructure(files=flist)

#convert TESS run files to R dataframe
runsToDfTess(files=flist1)
```
### 5. Generate CLUMPP output  
This function can be used to create files for use with CLUMPP. The function creates a combined file and paramfile in separate directories by K.

```coffee
#convert STRUCTURE runs for CLUMPP
clumppExportStructure(files=flist)

#convert TESS runs for CLUMPP
clumppExportTess(files=flist1)
```
![CLUMPP results and the contents of each folder](screenshots/Fig3.jpg) 
__Fig 2.__ Folders created from CLUMPP export and the contents of each folder.

![CLUMPP results aligned file merged file and misc file](screenshots/Fig4.jpg)  
__Fig 3.__ Folder showing CLUMPP results: aligned file, merged file and misc file.

### 6. Collect CLUMPP output files  
The CLUMPP output files are created in multiple folders. This function helps to collect aligned files, merged files or both from multiple directories into a single directory. 

To follow this function in this tutorial, you will need to manually run CLUMPP.exe in each directory.

```coffee
#collect CLUMPP outputs into single directory
collectClumppOutput(prefix="STRUCTUREpop", filetype="both")  
collectClumppOutput(prefix="TESSpop", filetype="both")
```

### 7. Plot run files  
This function is used to plot barplots from STRUCTURE files, TESS files, combined files, aligned files or merged files.

* To plot separate files from STRUCTURE/TESS files  
```coffee
#plot separate figures
plotRuns(files=flist[1:2]) 
#plot separate figures
plotRuns(files=flist[1:2], imgoutput="sep")
```

* To plot joined files from STRUCTURE/TESS files  
```coffee
#plot joined figures
plotRuns(files=flist[1:2], imgoutput="join")
```

![plotRuns example 1](screenshots/Fig5.jpg)  
__Fig 4.__ Left: Single run plotted separately. Right: Two runs joined together in one image  

* To plot with populations labels  
```coffee
#read labels for STRUCTURE
pops<-read.delim(system.file("files/structurepoplabels.txt",package="pophelper"),header=F)

#plot separately with labels
plotRuns(files=flist[1:2], imgoutput="sep", poplab=pops$V1) 
#plot joined with labels
plotRuns(files=flist[1:2], imgoutput="join", poplab=pops$V1) 

#create TESS labels
labs1 <- factor(c(rep("PopA",30),rep("PopB",45)))

#plot TESS runs with labels
plotRuns(files=flist1[1:2], imgoutput="sep", poplab=labs1) 
plotRuns(files=flist1[1:2], imgoutput="join", poplab=labs1) 
```

![plotRuns example 2](screenshots/Fig6.jpg)  
__Fig 5.__ Left: Single run plotted separately with pop labels. Right: Two runs joined together in one image with pop labels.

* To plot only joined files from table files (combined/aligned/merged)  
```coffee
#read table files
tabs1<-c(system.file("files/STRUCTUREpop_K4-combined.txt",package="pophelper"),
          system.file("files/STRUCTUREpop_K4-combined-aligned.txt",package="pophelper"),
          system.file("files/STRUCTUREpop_K4-combined-merged.txt",package="pophelper"))

#plot table files
plotRuns(files=tabs1, imgoutput="tab")
```

![PlotRuns example 3](screenshots/Fig7.jpg)  
__Fig 6.__ Left: Combined files (Three STRUCTURE runs for K=4). Middle: Aligned files (Three STRUCTURE runs for K=4 aligned using CLUMPP). Right: Merged file (Three runs for K=4 merged into one table/figure using CLUMPP).

### 8. Plot Multiline  
This function is also used to create barplots from STRUCTURE, TESS or table files. The output is created as A4 format by default. The barplot is broken down to multiple rows to enable easier identification of individuals. The number of samples per line (`spl`) and number of lines per page (`lpp`) can be defined manually if required.

```coffee
#plot multiline
plotMultiline(files=flist[1]) 
plotMultiline(files=flist1[1]) 

#plotmultiline with custom setting
plotMultiline(files=flist[1], spl=75, lpp=10)
plotMultiline(files=flist1[1], spl=75, lpp=11)
```
![plotMultiline example](screenshots/Fig11.jpg)  
__Fig 7.__ Left: `plotMultiline` default output. Right: Modified output.

#### 8.1 Sample plot outputs

![Sample](screenshots/Fig12.jpg)  

__Fig 8.__ Multiline plots with (left) standard colours, (middle) `rich.colors()` from `gplots` package and (right) `brewer.pal(x,"Spectral")` from `RColorBrewer` package.


### 9. Collect TESS runs
TESS run files are generated from TESS into multiple folders. These file can be collect into a single folder using this function. 

```coffee
#collect TESS runs from multiple directories into one
collectRunsTess(runsdir = choose.dir())
```

## List of Functions  
### STRUCTURE
```coffee
tabulateRunsStructure()  
summariseRunsStructure()  
evannoMethodStructure()  
runsToDfStructure()  
clumppExportStructure()  
collectClumppOutput()  
plotRuns()
PlotMultiline()
```

### TESS
```coffee
collectRunsTess()  
tabulateRunsTess()
summariseRunsTess()
runsToDfTess()  
clumppExportTess()  
collectClumppOutput()  
plotRuns()  
plotMultiline()  
```
## References
[Evanno, G., Regnaut, S., and Goudet, J. (2005). Detecting the number of clusters of individuals using the software STRUCTURE: a simulation study. Molecular ecology, 14(8), 2611-2620](http://onlinelibrary.wiley.com/doi/10.1111/j.1365-294X.2005.02553.x/abstract)

[François, O., and Durand, E. (2010). Spatially explicit Bayesian clustering models in population genetics. Molecular Ecology Resources, 10(5), 773-784.](http://onlinelibrary.wiley.com/doi/10.1111/j.1755-0998.2010.02868.x/abstract)

[Jakobsson, M., and Rosenberg, N. A. (2007). CLUMPP: a cluster matching and permutation program for dealing with label switching and multimodality in analysis of population structure. Bioinformatics, 23(14), 1801-1806](http://bioinformatics.oxfordjournals.org/content/23/14/1801.short)

[Pritchard, J. K., Stephens, M., & Donnelly, P. (2000). Inference of population structure using multilocus genotype data. Genetics, 155(2), 945-959.](http://www.genetics.org/content/155/2/945.short)  

  
### Disclaimer

Please make sure you verify all your results by eye atleast once per batch. And feel free to email me if there are any issues which I am sure there will be since all possibilities have not been exhaustively tested.

The `pophelper` R package is offered free and without warranty of any kind, either expressed or implied. I will not be held liable to you for any damage arising out of the use, modification or 
inability to use this program. `pophelper` R package can be used, redistributed and/or modified freely 
for non-commercial purposes subject to the original source being properly cited. Licensed under GPL-3.

(c) 2014 Roy M Francis | roy.m.francis@outlook.com
