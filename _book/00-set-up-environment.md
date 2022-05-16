# Setting Up Your Working Environment

In order to replicate all of the analyses in this document, you will need to install a large number of packages and programs beyond R. If you just want to pick and choose which code chunks to try you do not need to install all of these packages and we call the specific packages where they are used in each section of this document. If you'd rather have a full working enviornment ahead of time, you will need to.

## Installing R Packages

In this appendix we rely on a number of pre-existing R packages. In order to use these packages in a new installation of R and R-studio, you first need to install them. Note that you will only need to do this once on a new installation of R. To install packages, you can click on the "Packages" tab in the window in the bottom right of R studio, then click the "Install" button at the top and type the names of the packages separated by commas. Alternatively you can install packages from the console by simply typing "install.packages("nameofpackagehere")" without the outer quotation marks. 

install.packages(c("statnet","tnet"))

We use a number of R packages in the modules here and in the book for manipulating and analyzing network data and for other general analyses and procedures. The most frequently used network packages include:

* [igraph (Csardi and Nepusz 2006)](https://igraph.org/) - analytical routines for simple graphs and graph analysis
* [statnet (Krivitsky et al. 2020)](http://statnet.org/) - A suite of packages designed for the management and statistical analysis of networks
* [intergraph (Bojanowski 2015)](https://cran.r-project.org/web/packages/intergraph/intergraph.pdf) - a set of routines for coercing objects between common network formats in R
* [ggraph (Pederson 2021)](https://CRAN.R-project.org/package=ggraph) - a powerful graph visualization package that is based off of the ggplot2 plotting format

In order to install all of the packages that will be used in this appendix, you can run the following chunk of code. Specific packages will be initialized along with the specific sections where they are required. Note that the code below will not reinstall packages already installed in your current version of R.


```r
packages <- c("igraph", "statnet", "intergraph", "tnet", "ggplot2",
              "deldir","rjson","d3r","cccd","RBGL","graph","BiocGenerics",
              "networkD3","visNetwork","GISTools","rgeos","maptools","sp",
              "ndtv","gridExtra","png","scales","ape","graphlayouts",
              "igraphdata","ggrepel","ggsn","tidyverse","edgebundle",
              "superheat","ggplotify","ggforce","colorspace","ggmap","sf",
              "dplyr","ggpubr","ggraph","ggplot2","reshape2","multinet",
              "RColorBrewer","Rcpp","vegan","intergraph","geosphere",
              "networkDynamic","scatterplot3d","patchwork")

install.packages(setdiff(packages, rownames(installed.packages())))  

devtools::install_github("liamgilbey/ggwaffle")

if(!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("RBGL")

```

This version of the book was built with R version 4.2.0 (2022-04-22 ucrt) and the following packages:


|package        |version |source                       |
|:--------------|:-------|:----------------------------|
|animation      |2.7     |CRAN (R 4.2.0)               |
|ape            |5.6-2   |CRAN (R 4.2.0)               |
|cccd           |1.6     |CRAN (R 4.2.0)               |
|colorspace     |2.0-3   |CRAN (R 4.2.0)               |
|d3r            |1.0.0   |CRAN (R 4.2.0)               |
|deldir         |1.0-6   |CRAN (R 4.2.0)               |
|devtools       |2.4.3   |CRAN (R 4.2.0)               |
|dplyr          |1.0.9   |CRAN (R 4.2.0)               |
|edgebundle     |0.3.1   |CRAN (R 4.2.0)               |
|ergm           |4.2.1   |CRAN (R 4.2.0)               |
|geosphere      |1.5-14  |CRAN (R 4.2.0)               |
|ggforce        |0.3.3   |CRAN (R 4.2.0)               |
|ggmap          |3.0.0   |CRAN (R 4.2.0)               |
|ggplot2        |3.3.6   |CRAN (R 4.2.0)               |
|ggplotify      |0.1.0   |CRAN (R 4.2.0)               |
|ggpubr         |0.4.0   |CRAN (R 4.2.0)               |
|ggraph         |2.0.5   |CRAN (R 4.2.0)               |
|ggrepel        |0.9.1   |CRAN (R 4.2.0)               |
|ggsn           |0.5.0   |CRAN (R 4.2.0)               |
|GISTools       |0.7-4   |Github (liamgilbey/ggwaffle) |
|graph          |1.74.0  |Bioconductor                 |
|graphlayouts   |0.8.0   |CRAN (R 4.2.0)               |
|gridExtra      |2.3     |CRAN (R 4.2.0)               |
|igraph         |1.3.1   |CRAN (R 4.2.0)               |
|igraphdata     |1.0.1   |CRAN (R 4.2.0)               |
|intergraph     |2.0-2   |CRAN (R 4.2.0)               |
|knitr          |1.39    |CRAN (R 4.2.0)               |
|maptools       |1.1-4   |CRAN (R 4.2.0)               |
|mgcv           |1.8-40  |CRAN (R 4.2.0)               |
|multinet       |4.0.1   |CRAN (R 4.2.0)               |
|ndtv           |0.13.2  |CRAN (R 4.2.0)               |
|network        |1.17.1  |CRAN (R 4.2.0)               |
|networkD3      |0.4     |CRAN (R 4.2.0)               |
|networkDynamic |0.11.2  |CRAN (R 4.2.0)               |
|patchwork      |1.1.1   |CRAN (R 4.2.0)               |
|png            |0.1-7   |CRAN (R 4.2.0)               |
|RBGL           |1.72.0  |Bioconductor                 |
|RColorBrewer   |1.1-3   |CRAN (R 4.2.0)               |
|Rcpp           |1.0.8.3 |CRAN (R 4.2.0)               |
|reshape2       |1.4.4   |CRAN (R 4.2.0)               |
|rgeos          |0.5-9   |CRAN (R 4.2.0)               |
|rjson          |0.2.21  |CRAN (R 4.2.0)               |
|scales         |1.2.0   |CRAN (R 4.2.0)               |
|sf             |1.0-7   |CRAN (R 4.2.0)               |
|sna            |2.6     |CRAN (R 4.2.0)               |
|sp             |1.4-7   |CRAN (R 4.2.0)               |
|statnet        |2019.6  |CRAN (R 4.2.0)               |
|superheat      |0.1.0   |CRAN (R 4.2.0)               |
|tergm          |4.0.2   |CRAN (R 4.2.0)               |
|tibble         |3.1.7   |CRAN (R 4.2.0)               |
|tidyverse      |1.3.1   |CRAN (R 4.2.0)               |
|tnet           |3.0.16  |CRAN (R 4.2.0)               |
|tsna           |0.3.5   |CRAN (R 4.2.0)               |
|vegan          |2.6-2   |CRAN (R 4.2.0)               |
|visNetwork     |2.1.0   |CRAN (R 4.2.0)               |

## Suggested Workspace Setup

In order to follow along with the examples in the appendix it will be easiest if you set up your R working directory in a similar format to that used in creating it. Specifically, we suggest you create a new working directory and create an R studio project tied to that specific directory. Further, we suggest that you create a sub-folder of that working directory called "data" and place all of the files you downloaded above or from any other place in this appendix in that folder. Note that if you chose the "Just Give Me Everything" download you will have a .zip file that already contains a sub-folder called "data" so be sure you're not double nesting your folders (you want "working_directory/data" not "working_directory/data/data").
