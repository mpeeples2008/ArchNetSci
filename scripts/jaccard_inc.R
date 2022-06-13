jaccard_inc <- function(dat) {
  out <- matrix(NA, nrow(dat), nrow(dat))
  for (i in seq_len(nrow(dat))) {
    for (j in seq_len(nrow(dat))) {
      out[i, j] <- jaccard(dat[i, ], dat[j, ])
    }
  }
  return(out)
}
