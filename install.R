packages <- c("igraph", "statnet", "intergraph", "ggraph",
              "reshape2", "ggmap", "vegan", "sf", "multinet",
              "ggpubr", "Matrix", "concaveman", "ggplotify")

install.packages(setdiff(packages, rownames(installed.packages())))
