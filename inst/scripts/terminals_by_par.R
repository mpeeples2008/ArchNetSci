
terminals_by_par <- function(alpha, beta, data, dist_mat) {

Oi <- rep(1, nrow(dist_mat))
Wj <- rep(1, nrow(dist_mat))
eps <- 0.01
K <- 1

Dj <- Wj

end_condition <- 1
iter <- 0

det <- exp(-beta * d)

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
for (i in 1:109) terminal_sites[i] <- sum(Tij[-i, i]) > max(Tij[i, ])
return(length(which(terminal_sites == TRUE)))
}
