packages <- c("igraph", "statnet", "intergraph", "ggraph",
              "reshape2", "ggmap",  "vegan")

install.packages(setdiff(packages, rownames(installed.packages())))
