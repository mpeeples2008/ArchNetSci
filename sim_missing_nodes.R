sim_missing_nodes <- function(net,
                              nsim = 1000,
                              props = c(0.9, 0.8, 0.7, 0.6, 0.5,
                                        0.4, 0.3, 0.2, 0.1),
                              met = NA,
                              missing_probs = NA) {
  # insert measure of interest in line below
  if (met == "degree") {
  met_orig <- igraph::degree(net)} 
  else
  {if (met == "betweenness") {
      met_orig <- igraph::betweenness(net)} 
    else {
        if (met == "eigenvector") {
          met_orig <- igraph::eigen_centrality(net)$vector
        }
      }
    }
  output <- matrix(NA, nsim, length(props))
  colnames(output) <- as.character(props)
  for (j in 1:length(props)) {
    for (i in 1:nsim) {
      if (is.na(missing_probs)[1]) {
      sub_samp <- sample(seq(1, vcount(net)),
                         size = round(vcount(net) * props[j], 0))
      sub_net <- igraph::induced_subgraph(net, sort(sub_samp))} 
      else {
        sub_samp <- sample(seq(1, vcount(net), prob = missing_probs),
                           size = round(vcount(net) * props[j], 0))
        sub_net <- igraph::induced_subgraph(net, sort(sub_samp))
      }
      # insert measure of interest in line below (same as above)
      if (met == "degree") {
        temp_stats <- igraph::degree(sub_net)} 
      else
        {if (met == "betweenness") {
          temp_stats <- igraph::betweenness(sub_net)} 
          else {
            if (met == "eigenvector") {
              temp_stats <- igraph::eigen_centrality(sub_net)$vector
            }
          }
        }
      output[i, j] <- suppressWarnings(cor(temp_stats,
                                           met_orig[sort(sub_samp)],
                                           method = 'spearman'))
    }
  }
   return(output)
}

# Run the function
set.seed(5609)
dg_test <- sim_missing_nodes(net = chaco_net, met = "degree", missing_probs=mis)

# Visuzlize the results as a boxplot.
# Melt wide data format into long data format
df <- melt(as.data.frame(dg_test))

ggplot(data = df) +
  geom_boxplot(aes(x = variable, y = value)) +
  xlab("Sub-Sample Size as Proportion of Original") +
  ylab(expression("Spearman's" ~ rho)) +
  theme_bw() 