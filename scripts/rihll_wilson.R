rihll_wilson <-
  function(Oi = 0,
           Wj = 0,
           alpha,
           beta,
           dist_mat,
           K = 1,
           eps = 0.01) {

    if (Oi == 0) {
      Oi <- rep(1, nrow(dist_mat))
    }
    if (Wj == 0) {
      Wj <- rep(1, nrow(dist_mat))
    }

    Dj <- Wj

    end_condition <- 1
    iter <- 0

    det <- exp(-beta * dist_mat)

    while (!(end_condition < 1e-5) & iter < 10000) {
      Wj <- Dj
      Tij <-
        apply(det * Oi %o% Wj ^ alpha, 2, '/',
              (Wj ^ alpha %*% det))
      delta_W <- eps * (colSums(Tij) - (K * Wj))
      Dj <- delta_W + Wj
      iter <- iter  + 1
      end_condition <- sum((Dj - Wj) ^ 2)
    }

    terminal_sites <- NULL
    for (i in seq_len(length(Wj))) {
      terminal_sites[i] <- sum(Tij[-i, i]) > max(Tij[i,])
    }

    out <-
      list(
        Wj = Wj,
        Tij = Tij,
        iter = iter,
        terminals = terminal_sites
      )

    return(out)
  }
