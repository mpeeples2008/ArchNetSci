jaccard <- function(a, b) {
  intersection <- sum(intersect(a, b))
  union <- sum(a) + sum(b) - intersection
  return(intersection / union)
}
