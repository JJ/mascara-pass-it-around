library(tidyverse)
library(dplyr)
library(igraph)
library(entropy)

source("scripts/net_euro_2004.R")

france <- data.frame(game=character(),entropy=numeric())
france.nets <- list()
bd.ranking <- data.frame(player = character(), team = character(), game = character(), bd = numeric(), entropy = numeric(), betweenness = numeric())
for (i in c("francia", "francia-2", "francia-3", "francia-4")) {
  this.net <- net_euro_2004(paste0("data/",i,".dl.csv"))
  france.nets[[i]] = this.net
  france <- rbind(france,data.frame(game=i,entropy=entropy(E(this.net)$weight)))
  bd.ranking <- rbind(bd.ranking,
                      data.frame(player=V(this.net)$name,
                                 team="france",
                                 game=rep(i,length(V(this.net)$name)),
                                 bd=V(this.net)$bd,
                                 entropy=V(this.net)$diversity,
                                 betweenness=V(this.net)$betweenness)
  )
}

plot(france.nets[[1]])
plot(france.nets[[2]],edge.width = E(france.nets[[2]])$weight,vertex.size = V(france.nets[[2]])$betweenness, vertex.color="red")