library(tidyverse)
library(dplyr)
library(igraph)
library(entropy)
source("scripts/net_euro_2004.R")

grecia <- data.frame(game=character(),entropy=numeric(),normEntropy=numeric())
grecia.nets <- list()
grecia.norm <- c()
bd.ranking <- data.frame(player = character(), team = character(), game = character(), bd = numeric(), entropy = numeric(), betweenness = numeric())

entropies <- function( a.vector ) {
  states <- length(a.vector)
  print(a.vector)
  entropy <- entropy(a.vector,method="ML",unit="log")
  print(entropy)
  normEntropy <- entropy/log(states)
  print(normEntropy)
  return(c(entropy,normEntropy))
}

for (i in c("grecia", "grecia-2", "grecia-3", "grecia-5", "grecia-6")) {
  this.net <- net_euro_2004(paste0("data/",i,".dl.csv"))
  grecia.nets[[i]] = this.net
  these.entropies <- entropies(E(this.net)$weight)   
  grecia <- rbind(grecia,data.frame(game=i,
                                    entropy=these.entropies[[1]],
                                    normEntropy=these.entropies[[2]]))
    
  states <- length(unique(E(this.net)$weight))
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
these.entropies <- entropies(E(portugal.1)$weight)   
bd.ranking <- rbind(bd.ranking,
                    data.frame(player=V(portugal.1)$name,
                               team="Portugal",
                               game=rep("portugal",length(V(portugal.1)$name)),
                               bd=V(portugal.1)$bd,
                               entropy=V(portugal.1)$diversity,
                               betweenness=V(portugal.1)$betweenness)
)
portugal <- data.frame(game="portugal",entropy=these.entropies[[1]], normEntropy=these.entropies[[2]])
portugal.nets <- list()
portugal.nets[['portugal']] = portugal.1
for (i in 2:6) {
  this.net <- net_euro_2004(paste0("data/portugal-",i,".dl.csv"))
  game <- paste0("portugal-",i)
  portugal.nets[[game]] = this.net
  these.entropies <- entropies(E(this.net)$weight)
  portugal <- rbind(portugal,data.frame(game=game,
                                        entropy=these.entropies[[1]],
                                        normEntropy=these.entropies[[2]]))
  bd.ranking <- rbind(bd.ranking,
                      data.frame(player=V(this.net)$name,
                                 team="Portugal",
                                 game=rep(game,length(V(this.net)$name)),bd=V(this.net)$bd,
                                            entropy=V(this.net)$diversity,
                                            betweenness=V(this.net)$betweenness))
}

ggplot(bd.ranking[ bd.ranking$game=="portugal" | bd.ranking$game=="grecia",], aes(x=entropy,y=betweenness,shape=team,color=game,label=player))+geom_point()+geom_text(hjust=1,vjust=-1)
ggplot(bd.ranking[ bd.ranking$game=="portugal-6" | bd.ranking$game=="grecia-6",], aes(x=entropy,y=betweenness,shape=team,color=game,label=player))+geom_point()+geom_text(hjust=1,vjust=-1)

adj_matrix <- as.matrix(read.csv("data/grecia-2.dl.csv",row.names=1, check.names=FALSE, na.strings = ""))
net.2004 <- graph_from_adjacency_matrix(adj_matrix,mode="undirected")
isolated <- which(degree(net.2004)==0)
net.2004 <- delete.vertices(net.2004, isolated)
greece.vs.spain.cluster <-  cluster_edge_betweenness(net.2004,directed=F)
plot(greece.vs.spain.cluster,net.2004)
