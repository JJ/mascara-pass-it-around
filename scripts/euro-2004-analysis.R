library(ggplot2)
library(ggthemes)

PREFIX="data/euro-2004/"
load(paste0(PREFIX,"euro-2004.RData"))
load(paste0(PREFIX,"euro-2004-betw-flow-diversity.RData"))
load(paste0(PREFIX,"euro-2004-player.RData"))

euro2004$team <- as.factor(as.character(lapply( euro2004$game, function(x) strsplit(x,"-")[[1]][1])))
euro2004$stage <- as.factor(as.character(lapply( euro2004$game, function(x) strsplit(x,"-")[[1]][2])))

ggplot(euro2004, aes(x=stage,y=tsallisEntropy,color=team))+geom_point()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab("Game")

matches <- read.csv(paste0(PREFIX,"partidos.csv"))

entropy.diffs <- data.frame(match=character(),scorediff=numeric(),entropydiff=numeric())
for (match in 1:nrow(matches)) {
  team1 <- matches[match,"Team1"]
  team2 <- matches[match,"Team2"]
  entropy1 <- unique(euro2004[euro2004$game==team1,]$tsallisEntropy)
  entropy2 <- unique(euro2004[euro2004$game==team2,]$tsallisEntropy)
  entropy.diffs <- rbind(entropy.diffs,
                         data.frame(match=paste0(team1,"-",team2),
                                    scorediff=matches[match,"Goals1"]-matches[match,"Goals2"],
                                    entropydiff=entropy1-entropy2)
                         )
}

ggplot(entropy.diffs,aes(x=entropydiff,y=scorediff))+geom_point()

