require(orca)

### GCD-11 function

netGCD <- function(net1,net2) {


net1a <- t(apply(igraph::get.edgelist(net1,names = F),1,as.integer))
net2a <- t(apply(igraph::get.edgelist(net2,names = F),1,as.integer))

orb1 <- orca::count4(net1a)[,-c(4,13,14,15)]
orb2 <- orca::count4(net2a)[,-c(4,13,14,15)]

orbcor1 <- cor(orb1,method="spearman")
orbcor2 <- cor(orb2,method="spearman")

orbcor1 <- orbcor1[upper.tri(orbcor1)]
orbcor2 <- orbcor2[upper.tri(orbcor2)]

out <- as.matrix(dist(t(cbind(orbcor1,orbcor2))))[2]

return(out)
}

