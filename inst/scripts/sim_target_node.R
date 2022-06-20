sim_target_node <- function(net,
                            nsim = 1000,
                            prop = 0.8,
                            target = NA,
                            met = NA,
                            missing_probs = NA) {
  # Initialize required library
  require(reshape2)

  if (FALSE %in% (is.numeric(prop) & (prop > 0) & (prop <= 1))) {
    stop("Variable prop must be numeric and be between 0 and 1",
         call. = F)
  }

  # Create vector for output
  output <- NULL

  # Iterate over each value of props and then each value from 1 to nsim
  for (i in 1:nsim) {
    # Find target node number
    target_number <- which(V(net)$name == target)

    prob_check <- missing_probs[-target_number]

    # Run code in brackets if missing_probs is NA
    if (is.na(missing_probs)[1]) {
      sub_samp <- sample(setdiff(1:vcount(net), target_number),
                         size = round(vcount(net) * (1 - prop), 0))
      sub_net <- igraph::induced_subgraph(net,
                                          sort(setdiff(1:vcount(net),
                                                       sub_samp)))
    }
    # Run code in brackets if missing_probs contains values
    else {
      sub_samp <- sample(
        setdiff(1:vcount(net), target_number),
        prob = prob_check,
        size = round(vcount(net) * (1 - prop), 0)
      )
      sub_net <- igraph::induced_subgraph(net,
                                          sort(setdiff(1:vcount(net),
                                                       sub_samp)))
    }
    # Select measure of interest based on met and calculate(same as above)
    if (met == "degree") {
      temp_stats <- igraph::degree(sub_net)
    }
    else   {
      if (met == "betweenness") {
        temp_stats <- igraph::betweenness(sub_net)
      }
      else {
        if (met == "eigenvector") {
          temp_stats <- igraph::eigen_centrality(sub_net)$vector
        }
      }
    }
    # Record output as rank order of target node
    output[i] <-
      which(names(sort(temp_stats, decreasing = TRUE)) == target)
  }
  # Return output
  return(output)
}
