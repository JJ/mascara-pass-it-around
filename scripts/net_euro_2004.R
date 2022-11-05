library(igraph)
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