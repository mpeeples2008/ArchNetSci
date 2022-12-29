packages <- c("igraph", "statnet", "intergraph", "ggraph",
              "reshape2", "ggmap", "vegan", "sf")

install.packages(setdiff(packages, rownames(installed.packages())))
