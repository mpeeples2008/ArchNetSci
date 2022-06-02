# Define function for assessing and retaining edges based on edge
# weight probabilities
edge_prob <- function(net, nsim = 1000, probs) {
  net_list <- list()
  for (i in 1:nsim) {
    sub_set <- NULL
    for (j in 1:ecount(net)) {
      temp <- rbinom(1, 1, prob = probs[j])
      if (temp == 1) {
        sub_set <- c(sub_set, j)
      }
    }
    net_list[[i]] <-
      igraph::delete_edges(net, which(!(seq(1, ecount(
        net
      ))
      %in% sub_set)))
  }
  return(net_list)
}



# Define function for assessing statistic of interest
compile_stat <- function(net_list, met) {
  out <- matrix(NA, vcount(net_list[[1]]), length(net_list))
  for (i in seq_len(length(net_list))) {
    # Select measure of interest based on met and calculate(same as above)
    if (met == "degree") {
      out[, i] <- igraph::degree(net_list[[i]])
    }
    else  {
      if (met == "betweenness") {
        out[, i] <- igraph::betweenness(net_list[[i]])
      }
      else {
        if (met == "eigenvector") {
          out[, i] <- igraph::eigen_centrality(net_list[[i]])$vector
        }
      }
    }
  }
  return(out)
}
