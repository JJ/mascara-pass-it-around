library(igraph)


net_euro_2004 <- function( data_file ) {
  adj_matrix <- as.matrix(read.csv(data_file,fileEncoding="latin1",row.names=1, check.names=FALSE, na.strings = ""))
  net.2004 <- graph_from_adjacency_matrix(adj_matrix,mode="undirected")
  isolated <- which(degree(net.2004)==0)
  net.2004 <- delete.vertices(net.2004, isolated)
  plot(net.2004)
}

net_euro_2004("data/39-3.csv")
