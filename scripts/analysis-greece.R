library(tidyverse)
library(dplyr)
library(igraph)
library(entropy)

source("net_euro_2004.R")

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
bd.ranking <- rbind(bd.ranking,
                    data.frame(player=V(portugal.1)$name,
                               team="Portugal",
                               game=rep("portugal",length(V(portugal.1)$name)),
                               bd=V(portugal.1)$bd,
                               entropy=V(portugal.1)$diversity,
                               betweenness=V(portugal.1)$betweenness)
)
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

ggplot(bd.ranking[ bd.ranking$game=="portugal" | bd.ranking$game=="grecia",], aes(x=entropy,y=betweenness,shape=team,color=game,label=player))+geom_point()+geom_text(hjust=1,vjust=-1)
ggplot(bd.ranking[ bd.ranking$game=="portugal-6" | bd.ranking$game=="grecia-6",], aes(x=entropy,y=betweenness,shape=team,color=game,label=player))+geom_point()+geom_text(hjust=1,vjust=-1)

plot(grecia.nets[[2]],edge.width = E(grecia.nets[[2]])$weight,vertex.size = V(grecia.nets[[2]])$betweenness, vertex.color="white", vertex.border.color="blue")