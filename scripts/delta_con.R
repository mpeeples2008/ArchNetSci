# This is a version of the DeltaCon function for small networks
# based on the port originally written by Baoxu(Dash) Shi (see citation below)
# This was modified from code available here:
# https://github.com/bxshi/rdsg/blob/master/R/delta_con.R

# Port of original DeltaCon to R
# Baoxu(Dash) Shi
# Data Sciense Group
# University of Notre Dame
#
# Citation
#   D. Koutra, J. T. Vogelstein, and C. Faloutsos:
#   DeltaCon: A Principled Massive-Graph Similarity Function.
#   SIAM 2013: 162–170.
#   D. Koutra, T. Ke, U. Kang, D. H. Chau, H. K. Pao, C. Faloutsos:
#   Unifying Guilt-by-Association Approaches: Theorems and Fast Algorithms.
#   ECML/PKDD (2) 2011: 245-2


library(Matrix)
library(SparseM)
library(pracma)

.MAX_POWER = 10
.p = 0.51

inverse_lbp <- function(graph) {

  nnodes <- nrow(graph)

  # Sparse identity matrix
  I = NULL
      I <- .sparseDiagonal(nnodes, x = 1, shape = "t")

  # Sparse degree-diagonal matrix, D[i,i] = sum(graph[i,])
  x = NULL
      x <- rowSums(graph)


  D = NULL
      D <- sparseMatrix(c(1:nnodes), c(1:nnodes), x = x, dims = c(nnodes, nnodes))

  # Compute about-half homophily factor to guarantee covergence
  c1 = sum(D) + 2
  c2 = sum(D^2) - 1
  h_h = sqrt((-c1 + sqrt(c1^2 + 4 * c2)) / (8 * c2))

  # Compute constant ah and ch
  ah = 4 * h_h^2 / (1 - 4 * h_h^2)
  ch = 2 * h_h / (1 - 4 * h_h^2)

  # Invert matrices M1 and M2
  M = NULL
    M = ch * graph  - ah * D



  # Calculate inverse of M
    inv_ = I
    mat_ = M
    pow = 1
      while(max(mat_) > 1e-09 && pow < .MAX_POWER) {
        inv_ = inv_ + mat_
        mat_ = mat_ %*% M
        pow = pow + 1
      }

   return(inv_)
}



delta_con <- function(graph1, graph2) {

  # Naive FaBP
  inv1 <- inverse_lbp(graph1) * (.p - 0.5)
  inv2 <- inverse_lbp(graph2) * (.p - 0.5)

  # Compute DeltaCon similarity score
  delta_con <- 1 / (1 + sqrt(sum( (sqrt(inv1) - sqrt(inv2))^2 )))
  return(delta_con)
}
