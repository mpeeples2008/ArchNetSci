packages <- c("igraph", "statnet", "intergraph", "tnet", "ggplot2",
              "deldir","rjson","d3r","cccd","RBGL","graph","BiocGenerics",
              "networkD3","visNetwork","GISTools","rgeos","maptools","sp",
              "ndtv","gridExtra","png","scales","ape","graphlayouts",
              "igraphdata","ggrepel","ggsn","tidyverse","edgebundle",
              "superheat","ggplotify","ggforce","colorspace","ggmap","sf",
              "dplyr","ggpubr","ggraph","ggplot2","reshape2","multinet",
              "RColorBrewer","Rcpp","vegan","intergraph","geosphere",
              "networkDynamic","scatterplot3d","patchwork","BiocManager","devtools")

install.packages(packages)  

devtools::install_github("liamgilbey/ggwaffle")

BiocManager::install("RBGL")