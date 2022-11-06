library(tidyverse)
library(dplyr)
library(igraph)
library(entropy)

source("scripts/net_euro_2004.R")

euro2004 <- data.frame(game=character(),entropy=numeric(),normEntropy=numeric())
euro2004.nets <- list()
bd.ranking <- data.frame(player = character(), team = character(), game = character(), bd = numeric(), entropy = numeric(), betweenness = numeric(),flow=numeric())
for (i in c(
           "portugal","portugal-2","portugal-3", "portugal-4", "portugal-5", "portugal-6",
          "grecia","grecia-2","grecia-3", 
           "grecia-5", "grecia-6",
            "francia", "francia-2", "francia-4",
            "spain", "spain-2")) {
  this.net <- net_euro_2004(paste0("data/",i,".dl.csv"))
  euro2004.nets[[i]] = this.net
  euro2004 <- rbind(euro2004,data.frame(game=i,entropy=entropy(E(this.net)$weight)))
  team <- strsplit(i, "-")[[1]][1]
  print(team)
  bd.ranking <- rbind(bd.ranking,
                      data.frame(player=V(this.net)$name,
                                 team=team,
                                 game=rep(i,length(V(this.net)$name)),
                                 bd=V(this.net)$bd,
                                 entropy=V(this.net)$diversity,
                                 betweenness=V(this.net)$betweenness,
                                 flow=V(this.net)$flow)
  )
}

save(euro2004.nets,file="data/euro-2004.RData")
save(euro2004,file="data/euro-2004-player.RData")
bd.ranking$fd <- bd.ranking$flow * bd.ranking$entropy
save(bd.ranking, file="data/euro-2004-betw-flow-diversity.RData")
