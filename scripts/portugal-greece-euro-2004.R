library(igraph)
library(dplyr)
library(ggplot2)
library(ggthemes)

load("data/euro-2004.RData")
load("data/euro-2004-betw-flow-diversity.RData")
load("data/euro-2004-player.RData")

portugal <- euro2004.nets[["portugal-6"]]
greece <- euro2004.nets[["grecia-6"]]

V(portugal)$fd = V(portugal)$flow * V(portugal)$diversity
plot(portugal,edge.width = E(portugal)$weight,vertex.size = V(portugal)$fd, vertex.color="red",vertex.frame.color="green")
V(greece)$fd = V(greece)$flow * V(greece)$diversity
plot(greece,edge.width = E(greece)$weight,vertex.size = V(greece)$fd, vertex.color="white",vertex.frame.color="blue")
print(mean(V(portugal)$flow))
print(mean(V(greece)$flow))
diversity.portugal <- data.frame( rank=1:length(V(portugal)$diversity),entropy=sort(V(portugal)$diversity))
diversity.greece <- data.frame( rank=1:length(V(greece)$diversity),entropy=sort(V(greece)$diversity))
ggplot()+geom_point(data=diversity.portugal,shape=21,stroke=2,color='darkgreen',fill='red',aes(x=rank,y=entropy,size=5))  +geom_point(data=diversity.greece,shape=21,stroke=2,fill='blue',color='white',aes(x=rank,y=entropy),size=5)

ggplot(bd.ranking[ bd.ranking$game=="portugal-6" | bd.ranking$game=="grecia-6",], aes(x=entropy,y=flow,shape=team,color=game,label=player))+geom_point()+geom_text(hjust=1,vjust=-1)+theme_tufte()+xlim(0.65,1)


euro2004$team <- as.factor(as.character(lapply( euro2004$game, function(x) strsplit(x,"-")[[1]][1])))
ggplot(euro2004, aes(x=reorder(game,entropy),y=entropy,color=team))+geom_point()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab("Game")

luis.figo <- bd.ranking[ bd.ranking$player =="_LUIS_FIGO", ]
luis.figo$game <- as.factor(as.character(luis.figo$game))
ggplot(luis.figo, aes(x=betweenness,y=flow,size=entropy,color=game))+geom_point()
