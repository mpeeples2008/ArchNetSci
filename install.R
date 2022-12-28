packages <- c("igraph", "statnet", "intergraph", "ggraph",
              "reshape2", "ggmap", "vegan", "sf", "devtools")

install.packages(setdiff(packages, rownames(installed.packages())))
devtools::install_github("liamgilbey/ggwaffle")
devtools::install_github("QiliShi/NetworkSim")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("RBGL")
