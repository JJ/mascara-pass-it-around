library(tidyverse)
library(dplyr)
library(igraph)
library(entropy)

source("scripts/net_euro_2004.R")

spain <- data.frame(game=character(),entropy=numeric())
spain.nets <- list()
bd.ranking <- data.frame(player = character(), team = character(), game = character(), bd = numeric(), entropy = numeric(), betweenness = numeric())
for (i in c("spain", "spain-2")) {
  this.net <- net_euro_2004(paste0("data/",i,".dl.csv"))
  spain.nets[[i]] = this.net
  spain <- rbind(spain,data.frame(game=i,entropy=entropy(E(this.net)$weight)))
  bd.ranking <- rbind(bd.ranking,
                      data.frame(player=V(this.net)$name,
                                 team="Greece",
                                 game=rep(i,length(V(this.net)$name)),
                                 bd=V(this.net)$bd,
                                 entropy=V(this.net)$diversity,
                                 betweenness=V(this.net)$betweenness)
  )
}

plot(spain.nets[[1]])