library(tidyverse)
library(dplyr)
library(igraph)
library(entropy)
net_euro_2004 <- function( data_file ) {
  print(data_file)
  adj_matrix <- as.matrix(read.csv(data_file,row.names=1, check.names=FALSE, na.strings = ""))
  net.2004 <- graph_from_adjacency_matrix(adj_matrix,mode="undirected")
  isolated <- which(degree(net.2004)==0)
  net.2004 <- delete.vertices(net.2004, isolated)
  E(net.2004)$weight <- 1
  net.2004.simplified <- igraph::simplify(net.2004, edge.attr.comb = list(weight = "sum"))
  V(net.2004.simplified)$diversity <- diversity(net.2004.simplified)
  V(net.2004.simplified)$betweenness <- betweenness(net.2004.simplified)
  V(net.2004.simplified)$bd <- V(net.2004.simplified)$diversity *  V(net.2004.simplified)$betweenness
  return(net.2004.simplified)  
}

grecia <- data.frame(game=character(),entropy=numeric())
grecia.nets <- list()
bd.ranking <- data.frame(player = character(), team = character(), game = character(), bd = numeric(), entropy = numeric(), betweenness = numeric())
for (i in c("grecia", "grecia-2", "grecia-3", "grecia-5", "grecia-6")) {
  this.net <- net_euro_2004(paste0("data/",i,".dl.csv"))
  grecia.nets[[i]] = this.net
  grecia <- rbind(grecia,data.frame(game=i,entropy=entropy(E(this.net)$weight)))
  bd.ranking <- rbind(bd.ranking,
                      data.frame(player=V(this.net)$name,
                                 team="Greece",
                                 game=rep(i,length(V(this.net)$name)),
                                 bd=V(this.net)$bd,
                                 entropy=V(this.net)$diversity,
                                 betweenness=V(this.net)$betweenness)
  )
}

portugal.1 <- net_euro_2004("data/portugal.dl.csv")
bd.ranking <- rbind(bd.ranking,data.frame(player=V(portugal.1)$name,game=rep("portugal",length(V(portugal.1)$name)),bd=V(portugal.1)$bd),entropy=V(portugal.1)$diversity,
                    betweenness=V(portugal.1)$betweenness)
portugal <- data.frame(game="portugal",entropy=entropy(E(portugal.1)$weight))
portugal.nets <- list()
portugal.nets[['portugal']] = portugal.1
for (i in 2:6) {
  this.net <- net_euro_2004(paste0("data/portugal-",i,".dl.csv"))
  game <- paste0("portugal-",i)
  portugal.nets[[game]] = this.net
  portugal <- rbind(portugal,data.frame(game=game,entropy=entropy(E(this.net)$weight)))
  bd.ranking <- rbind(bd.ranking,
                      data.frame(player=V(this.net)$name,
                                 team="Portugal",
                                 game=rep(game,length(V(this.net)$name)),bd=V(this.net)$bd,
                                            entropy=V(this.net)$diversity,
                                            betweenness=V(this.net)$betweenness))
}



