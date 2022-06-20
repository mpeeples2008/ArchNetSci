# Create function for assessing impact of sampling error on
# weighted degree for similarity network
sim_samp_error <- function(cer, nsim = 1000) {
  sim_list <- list()
  for (i in 1:nsim) {
    data_sim <-  NULL
    # the for-loop below creates a random multinomial replicate
    # of the ceramic data
    for (j in seq_len(nrow(cer))) {
      data_sim <-
        rbind(data_sim, t(rmultinom(1, rowSums(cer)[j], prob = cer[j, ])))
    }
    # Convert simulated data to proportion, create similarity matrix,
    # calculate degree, and assess correlation
    temp_p <- prop.table(as.matrix(data_sim), margin = 1)
    sim_list[[i]] <- ((2 - as.matrix(vegan::vegdist(temp_p,
                                                    method = "manhattan"))) / 2)
  }
  return(sim_list)
}


sim_cor <- function(sim_nets, sim) {
  # change this line to use a different metric
  dg_orig <- rowSums(sim)
  dg_cor <- NULL
  for (i in seq_len(length(sim_nets))) {
    # change this line to use a different metric
    dg_temp <- rowSums(sim_nets[[i]])
    dg_cor[i] <-
      suppressWarnings(cor(dg_orig, dg_temp, method = "spearman"))
  }
  return(dg_cor)
}
