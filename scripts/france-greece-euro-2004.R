library(igraph)

load("data/euro-2004.RData")

france <- euro2004.nets[["francia-4"]]
greece <- euro2004.nets[["grecia-3"]]

plot(france,edge.width = E(france)$weight,vertex.size = V(france)$flow, vertex.color="blue",vertex.frame.color="red")
plot(greece,edge.width = E(greece)$weight,vertex.size = V(greece)$flow, vertex.color="white",vertex.frame.color="blue")
print(mean(V(france)$flow))
print(mean(V(france)$flow))
