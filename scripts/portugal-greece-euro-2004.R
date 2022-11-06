library(igraph)

load("data/euro-2004.RData")

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
ggplot()+geom_point(data=diversity.portugal,color='red',aes(x=rank,y=entropy))+geom_point(data=diversity.greece,color='blue',aes(x=rank,y=entropy))
