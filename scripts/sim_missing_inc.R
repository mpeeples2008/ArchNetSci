sim_missing_inc <- function(net,
                            inc,
                            nsim = 1000,
                            props = c(0.9, 0.8, 0.7, 0.6, 0.5,
                                      0.4, 0.3, 0.2, 0.1),
                            missing_probs = NA,
                            met = NA) {
  # Initialize required library
  require(reshape2)
  
  props <- as.vector(props)
  
  if (FALSE %in% (is.numeric(props) & (props > 0) & (props <= 1))) {
    stop("Variable props must be numeric and be between 0 and 1",
         call. = F)
  }
  
  # Select measure of interest based on variable met and calculate
  if (!(met %in% c("degree", "betweenness", "eigenvector"))) {
    stop(
      "Argument met must be either degree, betweenness, or eigenvector. 
      Check function call.",
      call. = F
    )
  }
  else {
    if (met == "degree") {
      met_orig <- igraph::degree(net)
    }
    else
    {
      if (met == "betweenness") {
        met_orig <- igraph::betweenness(net)
      }
      else {
        if (met == "eigenvector") {
          met_orig <- igraph::eigen_centrality(net)$vector
        }
      }
    }
  }
  
  # Create data frame for out put and name columns
  output <- matrix(NA, nsim, length(props))
  colnames(output) <- as.character(props)
  
  # Iterate over each value of props and then each value from 1 to nsim
  for (j in 1:length(props)) {
    for (i in 1:nsim) {
      # Run code in brackets if missing_probs == NA
      if (is.na(missing_probs)[1]) {
        # Create a sub-sample of the incidence matrix rows 
        # at random
        sub_samp <- sample(seq(1, nrow(inc)),
                           size = round(nrow(inc) * props[j], 0))
        # Create an adjacency matrix by matrix algebra
        sub_adj <- t(inc[sub_samp,]) %*% inc[sub_samp,]
        # create a network object from the resulting adjacency matrix
        sub_net <- igraph::graph_from_adjacency_matrix(sub_adj,
                                                       mode = "undirected",
                                                       diag = FALSE)
      }
      # Run code in brackets if missing_probs contains values
      else {
        # Create a sub-sample of the incidence matrix rows based
        # on probabilities in missing_probs
        sub_samp <- sample(seq(1, nrow(inc)),
                           size = round(nrow(inc) * props[j], 0),
                           prob = missing_probs)
        # Create an adjacency matrix by matrix algebra
        sub_adj <- t(inc[sub_samp,]) %*% inc[sub_samp,]
        # create a network object from the resulting adjacency matrix
        sub_net <- igraph::graph_from_adjacency_matrix(sub_adj,
                                                       mode = "undirected",
                                                       diag = FALSE)
      }
      # Select measure of interest based on met and calculate(same as above)
      if (met == "degree") {
        temp_stats <- igraph::degree(sub_net)
      }
      else
      {
        if (met == "betweenness") {
          temp_stats <- igraph::betweenness(sub_net)
        }
        else {
          if (met == "eigenvector") {
            temp_stats <- igraph::eigen_centrality(sub_net)$vector
          }
        }
      }
      # Record output for row and column by calculating spearman's rho between
      # met_orig and each temp_stats iteration.
      output[i, j] <- suppressWarnings(cor(temp_stats,
                                           met_orig,
                                           method = 'spearman'))
    }
  }
  # Return output as data.frame 
  df_output <- suppressWarnings(melt(as.data.frame(output)))
  return(df_output)
}

