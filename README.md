# pophelper 1.0.0

`pophelper` is an R package to analyse output files generated from population analysis programs such as STRUCTURE and TESS. The `pophelper` package can be used to tabulate runs, summarise runs, perform the Evanno method, export files for CLUMPP and generate barplot figures. A brief introduction is provided here. For more detailed demonstration, refer the vignette.

## Installation  
You can install `pophelper` from `github` using the `devtools` package

```coffee
require(devtools)
install_github('pophelper', 'royfrancis')
require(pophelper)
```

## List of Functions  
A list of important functions are shown below. 

### Tabulate runs  
Select multiple STRUCTURE or TESS runs and tabulate them into a table.

```coffee
flist<-choose.files()
df<-tabulateRunsStructure(files=flist)
tabulateRunsTess()
```
### Summarise runs  
In case of STRUCTURE files, the tabulated runs can be further condensed by repeats.

```coffee
df1<-summariseRunsStructure(df)
```

### Evanno method  
This function calculates the Evanno derivatives, tables and figures. The output from `summariseRunsStructure()` can be provided as input.

```coffee
evannoMethodStructure(df1)
```
[Evanno Method](/Vignettes/evannoMethodStructure.png).

#### Convert to dataframe  
STRUCTURE and TESS run files can be converted to R dataframes using this function.

```coffee
runsToDfStructure(files=flist)
runsToDfTess()
```
#### Generate CLUMPP output  
This function can be used to create files for use with CLUMPP. The function creates a combined file and paramfile in separate directories by K.

```coffee
clumppExportStructure(files=flist)  
clumppExportTess()
```
[Folders created from clumpp export and the contents of each folder.](/Vignettes/Fig3.jpg).
[Folder showing CLUMPP results: aligned file, merged file and misc file.](/Vignettes/Fig4.jpg).

#### Collect CLUMPP output files  
The CLUMPP output files are created in multiple folders. This function helps to collect aligned files, merged files or both from multiple directories into a single directory.

```coffee
collectClumppOutput(prefix="STRUCTUREpop", filetype="both")  
collectClumppOutput(prefix="TESSpop", filetype="both")
```
#### Plot run files  
This function is used to plot barplots from STRUCTURE files, TESS files, combined files, aligned files or merged files.

To plot separate files from STRUCTURE/TESS files  
```coffee
plotRuns(files=flist)  
plotRuns(files=flist, imgoutput="sep")
```

To plot joined files from STRUCTURE/TESS files  
`plotRuns(files=flist, imgoutput="join")`

[Left: Single run plotted separately. Right: Two runs joined together in one image.](/Vignettes/Fig5.jpg)

To plot with populations labels  

```coffee
plotRuns(files=flist, imgoutput="sep", poplab=pops$V1)  
plotRuns(files=flist, imgoutput="join", poplab=pops$V1)  
```
[Left: Single run plotted separately with pop labels. Right: Two runs joined together in one image with pop labels.](/Vignettes/Fig6.jpg)

To plot only joined files from table files (combined/aligned/merged)  
`plotRuns(files=flist, imgoutput="tab")`

[Left: Combined files (Three STRUCTURE runs for K=4). Middle: Aligned files (Three STRUCTURE runs for K=4 aligned using CLUMPP). Right: Merged file (Three runs for K=4 merged into one table/figure using CLUMPP)](/Vignettes/Fig7.jpg)

#### Plot Multiline  
This function is also used to create barplots from STRUCTURE, TESS or table files. The output is created as A4 format by default. The barplot is broken down to multiple rows to enable easier identification of individuals. The number of samples per line (`spl`) and number of lines per page (`lpp`) can be defined manually.

```coffee
plotMultiline(files=flist[1])  
plotMultiline(files=flist[1], spl=75, lpp=10)
```
[Left: Default output. Right: Modified output](/Vignettes/Fig11.jpg)

#### Collect TESS runs
TESS run files are generated in multiple folders. These file can be collect into a single folder using this function.  
`collectRunsTess(runsdir = choose.dir())`

#### End of Document.
