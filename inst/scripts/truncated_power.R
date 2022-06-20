truncated_power <- function (V, d, a, y, B) {
  temp <- matrix(0, nrow(d), ncol(d))
  for (i in seq_len(nrow(d))) {
    for (j in seq_len(ncol(d))) {
      temp[i, j] <- V[i] ^ y * V[j] ^ y * d[i, j] ^ -a * exp(-d[i, j] / B)
      if (temp[i, j] == Inf) {
        temp[i, j] <- 0
      }
    }
  }
  return(temp)
}
