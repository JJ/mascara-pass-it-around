library(tidyverse)
library(dplyr)
library(igraph)
library(entropy)
net_euro_2004 <- function( data_file ) {
  print(data_file)
  adj_matrix <- as.matrix(read.csv(data_file,fileEncoding="latin1",row.names=1, check.names=FALSE, na.strings = ""))
  net.2004 <- graph_from_adjacency_matrix(adj_matrix,mode="undirected")
  isolated <- which(degree(net.2004)==0)
  net.2004 <- delete.vertices(net.2004, isolated)
  E(net.2004)$weight <- 1
  net.2004.simplified <- igraph::simplify(net.2004, edge.attr.comb = list(weight = "sum"))
  net.2004.simplified$diversity <- diversity(net.2004.simplified)
  return(net.2004.simplified)  
}


grecia <- data.frame(game=character(),entropy=numeric())
grecia.nets <- c()
for (i in c("grecia", "grecia-2", "grecia-3", "grecia-5", "grecia-6")) {
  this.net <- net_euro_2004(paste0("data/",i,".dl.csv"))
  grecia.nets <- c(grecia.nets,this.net)
  grecia <- rbind(grecia,data.frame(game=i,entropy=entropy(E(this.net)$weight)))
}

portugal.1 <- net_euro_2004("data/portugal.dl.csv")
portugal <- data.frame(game="portugal",entropy=entropy(E(portugal.1)$weight))
portugal.nets <- c()
