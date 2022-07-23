#' Rihll and Wilson "Retail" Gravity Model
#'
#' This function implements the "Retail" gravity model made popular in
#' archaeology by Rihll and Wilson (1987). This procedure assesses flows of
#' resources between geographic locations using a matrix of distances based on
#' some e measure of the cost of travel between those locations.
#'
#' @param Oi The estimate3d weight of flow or interactions out of the origins i.
#' @param Wj The estimated weight of flow or interaction into destinations j.
#' In most archaeological applications, this is used to represent some measure
#' of settlement size.
#' @param alpha This parameter defines the importance of resource flow into
#' destinations. Values greater than 1 indicate increasing returns to scale for
#' every unit of flow.
#' @param beta The decay parameter that describes the rate of decay in
#' interaction at increasing distance.
#' @param dist_mat A symmetric matrix of distances between all locations
#' considered in map units.
#' @param K This is the factor used to convert size W to the sum of flows Dj.
#' By default this is set to 1.
#' @param eps This is a control parameter that determines how quickly W can
#' change at each iterative step. The default value is 0.01
#'
#' @return This function returns a list which contains Wj or the final estimated
#' weights incident on each location; Tij which is the measure of flow between
#' all locations; iter which is the number of iterations the function went
#' through; and terminal_sites which includes the data for sites where the
#' total flow of inputs into the site Wj is bigger than the largest flow out.
#'
#'
#' @export
#'

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
