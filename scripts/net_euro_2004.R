library(igraph)
library(CINNA)

adjacency_for_file <- function( file_name ) {
  return(as.matrix(read.csv(file_name,row.names=1, check.names=FALSE, na.strings = "")))
}

net_euro_2004 <- function( data_file ) {
  print(data_file)
  adj_matrix <- adjacency_for_file( data_file )
  net.2004 <- graph_from_adjacency_matrix(adj_matrix,mode="undirected")
  isolated <- which(degree(net.2004)==0)
  net.2004 <- delete.vertices(net.2004, isolated)
  E(net.2004)$weight <- 1
  net.2004.simplified <- igraph::simplify(net.2004, edge.attr.comb = list(weight = "sum"))
  V(net.2004.simplified)$diversity <- diversity(net.2004.simplified)
  V(net.2004.simplified)$betweenness <- betweenness(net.2004.simplified)
  centralities <- calculate_centralities(net.2004.simplified)
  V(net.2004.simplified)$flow <- centralities$`Flow Betweenness Centrality`
  V(net.2004.simplified)$bd <- V(net.2004.simplified)$diversity *  V(net.2004.simplified)$betweenness
  return(net.2004.simplified)  
}

tsallis <- function( adj_matrix, ratio ) {
  pass.network <- c(adj_matrix)
  total <- sum(pass.network)
  normalized.pass.network <- pass.network/total
  partial <- 0
  for ( i in 1:length(normalized.pass.network)) {
    partial <- partial - normalized.pass.network[i] ** ratio
  }
  print(partial)
  print((1 + partial)/(ratio -1))
  return((1 + partial)/(ratio -1))
}