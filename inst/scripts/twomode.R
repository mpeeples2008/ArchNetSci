require(igraph)

betweenness_twomode <- function(net) {
  n1 <- length(which(V(net)$type == FALSE))
  n2 <- length(which(V(net)$type == TRUE))
  temp_bw <- igraph::betweenness(net, directed = FALSE)
  s_v <- round((n1 - 1) / n2, 0)
  t_v <- (n1 - 1) %% n2
  p_v <- round((n2 - 1) / n1, 0)
  r_v <- (n1 - 1) %% n2
  bw_v1 <-
    0.5 * (n2^2 * (s_v + 1)^2 + n2 * (s_v + 1) *
             (2 * t_v - s_v - 1) - t_v * (2 * s_v - t_v + 3))
  bw_v2 <-
    0.5 * (n1^2 * (p_v +1)^2 + n1 * (p_v +1) * (2 * r_v - p_v - 1) *
             r_v * (2 * p_v - r_v + 3))
  bw_n1 <- temp_bw[which(V(net)$type == FALSE)] / bw_v2
  bw_n2 <- temp_bw[which(V(net)$type == TRUE)] / bw_v1
  return(c(bw_n1, bw_n2))
}

degree_twomode <- function(net) {
  n1 <- length(which(V(net)$type == FALSE))
  n2 <- length(which(V(net)$type == TRUE))
  temp_dg <- igraph::degree(net, mode = "in")
  dg_n1 <- temp_dg[which(V(net)$type == FALSE)] / n2
  dg_n2 <- temp_dg[which(V(net)$type == TRUE)] / n1
  return(c(dg_n1, dg_n2))
}
