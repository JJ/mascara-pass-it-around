PREFIX <- "data/euro-2004/"
TSALLIS_RATIO <- 0.2

source("scripts/net_euro_2004.R")

matches <- read.csv(paste0(PREFIX,"partidos.csv"))
                    
euro2004 <- data.frame(game=character(),entropy=numeric(),tsallisEntropy=numeric())
euro2004.nets <- list()
bd.ranking <- data.frame(player = character(), team = character(), game = character(), bd = numeric(), entropy = numeric(), betweenness = numeric(),flow=numeric())
for (i in c(matches$Team1,matches$Team2)) {
  file_name <- paste0(PREFIX,i,".dl.csv")
  this.net <- net_euro_2004( file_name )
  euro2004.nets[[i]] = this.net
  
  tsallis.entropy <- tsallis( adjacency_for_file(file_name ), TSALLIS_RATIO )
  euro2004 <- rbind(euro2004,data.frame(game=i,
                                        entropy=entropy(E(this.net)$weight),
                                        tsallisEntropy=tsallis.entropy))
  team <- strsplit(i, "-")[[1]][1]
  bd.ranking <- rbind(bd.ranking,
                      data.frame(player=V(this.net)$name,
                                 team=team,
                                 game=rep(i,length(V(this.net)$name)),
                                 bd=V(this.net)$bd,
                                 entropy=V(this.net)$diversity,
                                 betweenness=V(this.net)$betweenness,
                                 flow=V(this.net)$flow)
  )
}

save(euro2004.nets,file=paste0(PREFIX,"euro-2004.RData"))
save(euro2004,file=paste0(PREFIX,"euro-2004-player.RData"))
bd.ranking$fd <- bd.ranking$flow * bd.ranking$entropy
save(bd.ranking, file=paste0(PREFIX,"euro-2004-betw-flow-diversity.RData"))
