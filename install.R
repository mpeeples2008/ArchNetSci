packages <- c("tidyverse", "ape", "devtools", "igraph", "statnet", "intergraph",
              "tnet", "ggplot2", "rjson", "d3r", "cccd", "networkD3", "visNetwork",
              "GISTools", "rgeos", "maptools", "sf", "igraphdata", "ggrepel",
              "ggsn", "tidyverse", "superheat", "ggplotify", "ggforce", "colorspace",
              "ggmap", "dplyr", "ggpubr", "ggraph", "reshape2", "multinet",
              "RColorBrewer", "Rcpp", "deldir", "vegan", "geosphere", "networkDynamic",
              "scatterplot3d", "patchwork", "concaveman", "latticeExtra",
              "orca", "pracma", "netdiffuseR", "graphkernels")

install.packages(setdiff(packages, rownames(installed.packages())))

devtools::install_github("liamgilbey/ggwaffle")
devtools::install_github("QiliShi/NetworkSim")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("RBGL")
