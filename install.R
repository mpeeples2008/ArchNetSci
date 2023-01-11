packages <- c("igraph", "statnet", "intergraph", "ggraph",
              "reshape2", "ggmap", "vegan", "sf", "multinet",
              "ggpubr", "concaveman", "ggplotify")

install.packages(setdiff(packages, rownames(installed.packages())))
