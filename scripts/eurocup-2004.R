library(igraph)


net_euro_2004 <- function( data_file ) {
  adj_matrix <- as.matrix(read.csv(data_file,fileEncoding="latin1",row.names=1, check.names=FALSE, na.strings = ""))
  net.2004 <- graph_from_adjacency_matrix(adj_matrix,mode="undirected")
  isolated <- which(degree(net.2004)==0)
  net.2004 <- delete.vertices(net.2004, isolated)
  E(net.2004)$weight <- 1
  net.2004.simplified <- simplify(net.2004,
                                  edge.attr.comb = list(weight = "sum"))
  plot(net.2004.simplified,edge.width = E(net.2004.simplified)$weight)
}

for (i in c("data/39-3.csv","data/95-3.csv","data/110-3.csv","data/122-3.csv")) {
  net_euro_2004(i)
}
