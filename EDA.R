# Load Data into Memory (result, trian, test)
source("6372_Project2_HOF/ImportData.R")
library(reshape2)
library(ggplot2)
library(GGally)
# install.packages("ggpubr")
library(ggpubr)
library(pheatmap)
library(RColorBrewer)
library(tree)
library(tidyr)



# Batting Stats Compared to HOF
pairs(result[,cols.Batting], col=result$HallOfFame_inducted)
pairs(result[,c(33,cols.Batting.avg)], col=result$HallOfFame_inducted)

#Plot More
# All batting is too much
ggpairsAlLBatting <- ggpairs(result[,cols.Batting], diag=list(continuous="density", discrete="bar"), axisLabels="show")
ggsave("6372_Project2_HOF/batting matrix.png",plot = ggpairsAlLBatting, type = png())

# Better Representation using just data
ggpairsBattingAvg <- ggpairs(result[,c(33,cols.Batting.avg)], diag=list(continuous="densityDiag", discrete="barDiag"), axisLabels="show")
# Inlcude BattingAvg and Hall Fields
ggpairsBatting_w_Hall <- ggpairs(result[,c(33,cols.BattingAvg_w_Hall)], diag=list(continuous="density", discrete="bar"), axisLabels="show")


# Heat Map
batting.stats <- result[,cols.Batting]
# Remove Batting_
names(batting.stats) <- substr(names(batting.stats), 9, nchar(names(batting.stats)))
names(batting.stats)
# Update Order of Columns, cause OCD
batting.stats <- batting.stats[, c(1:4,18,5:17)]

cormat <- round(cor(batting.stats),2)
melted_cormat <- melt(cormat)
head(melted_cormat)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()

ggsave("6372_Project2_HOF/batting heatmap.png",plot = last_plot(), type = png())

batting.stats$Inducted <- result$HallOfFame_inducted
#Matrix of hits, runs, rbi, walks, singles, doubles, and HR
ggpairsAlLBatting <- ggpairs(
    mapping = ggplot2::aes(color = Inducted),
    batting.stats[,c(2:9,12,19)], 
    diag=list(continuous="density", discrete="bar"), 
    axisLabels="show")
ggsave("6372_Project2_HOF/batting matrix.png",plot = ggpairsAlLBatting, type = png())

# Fielding EDA
fielding.stats <- result[,cols.Fielding]
# Remove Batting_
names(fielding.stats) <- substr(names(fielding.stats), 10, nchar(names(fielding.stats)))
names(fielding.stats)

fielding.stats$Inducted <- result$HallOfFame_inducted
ggpairsFielding <- ggpairs(
    mapping = ggplot2::aes(color = Inducted),
    fielding.stats, 
    diag=list(continuous="density", discrete="bar"), 
    axisLabels="show"
)
ggsave("6372_Project2_HOF/fielding matrix.png",plot = ggpairsFielding, type = png())

# Games Played vs. Hall
boxplot( G ~ Inducted, data = fielding.stats)
library(dplyr)

names(mtcars)

summ <- fielding.stats %>% 
    group_by(Inducted) %>% 
    summarize(mean = mean(G), median = median(G), sd = sd(G))

boxplotGamesPlayed <- ggplot(fielding.stats, aes(x = Inducted, y = G)) + 
    geom_boxplot() + 
    geom_label(
        data = summ, 
        aes(
            x = Inducted, 
            y = mean, 
            label = paste("Mean: ", round(mean, 1), "\nMedian: ", median, "\nSD: ", round(sd, 1))
        )
    )
ggsave("6372_Project2_HOF/Games Played Boxplot.png",plot = boxplotGamesPlayed, type = png())

# Means plot for position vs. hall compared to different stats
# Means Plot
mysummary<-function(x){
    result<-c(length(x),mean(x),sd(x),sd(x)/length(x))
    names(result)<-c("N","Mean","SD","SE")
    return(result)
}

# standardize the fielding by per game stat
fielding.stats$InnOuts.per <- fielding.stats$InnOuts / fielding.stats$G
fielding.stats$PO.per <- fielding.stats$PO / fielding.stats$G
fielding.stats$A.per <- fielding.stats$A / fielding.stats$G
fielding.stats$E.per <- fielding.stats$E / fielding.stats$G
fielding.stats$DP.per <- fielding.stats$DP / fielding.stats$G
fielding.stats$PB.per <- fielding.stats$PB / fielding.stats$G
fielding.stats$WP.per <- fielding.stats$WP / fielding.stats$G
fielding.stats$SB.per <- fielding.stats$SB / fielding.stats$G
fielding.stats$CS.per <- fielding.stats$CS / fielding.stats$G


fielding.stats$position <- result$position
# Means Plot for Inns Out vs. Inducted by Position
sumstats<-aggregate(InnOuts.per~Inducted*position,data=fielding.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotInnsOut <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Time played per game")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotInnsOut

# Means Plot for Put Out vs. Inducted by Position
sumstats<-aggregate(PO.per~Inducted*position,data=fielding.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotPO <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Putouts per Game")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotPO

# Means Plot for Assists vs. Inducted by Position
sumstats<-aggregate(A.per~Inducted*position,data=fielding.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotA <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Assists per Game")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotA

# Means Plot for Errors vs. Inducted by Position
sumstats<-aggregate(E.per~Inducted*position,data=fielding.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotE <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Errors per game")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotE

# Means Plot for Double Plays vs. Inducted by Position
sumstats<-aggregate(DP.per~Inducted*position,data=fielding.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotDP <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Double Plays per Game")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotDP

# Means plot for Passed Balls
sumstats<-aggregate(PB.per~Inducted*position,data=fielding.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotPB <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Pased Balls per Game")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotPB

# Means plot for Wild Pitches
sumstats<-aggregate(WP.per~Inducted*position,data=fielding.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotWP <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Wild Pitches (by catchers) per Game")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotWP

# Means plot for Stolen Bases
sumstats<-aggregate(SB.per~Inducted*position,data=fielding.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotSB <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Opponent Stolen Bases (by catchers) per Game")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotSB

# Means plot for Caught Stealing
sumstats<-aggregate(CS.per~Inducted*position,data=fielding.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotCS <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Opponents Caught Stealing (by catcher) per Game")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotCS

arrangeFieldingStats <- ggarrange(
    meansPlotPO, 
    meansPlotA, 
    meansPlotE, 
    meansPlotDP,
    labels = c("Putouts", "Assists", "Errors", "Double Plays" ),
    ncol = 2, 
    nrow = 2
)
ggsave("6372_Project2_HOF/Fielding Means Plots.png",plot = arrangeFieldingStats, type = png())

arrangeCatcherStats <- ggarrange(
    meansPlotPB,
    meansPlotWP,
    meansPlotSB,
    meansPlotCS,
    labels = c("Passed Balls", "Wild Pitches", "Stolen Bases", "Caught Stealing" ),
    ncol = 2, 
    nrow = 2
)
ggsave("6372_Project2_HOF/Catcher Fielding Means Plots.png",plot = arrangeCatcherStats, type = png())


# Box plot for catchers on PS, WP, SB, CS
# Box plot for Passed Balls
boxPlotPB <- ggplot(
        fielding.stats[fielding.stats$position=="c",],
        aes(x=Inducted,y=PB.per,group=Inducted,colour=Inducted)
    )+
    ylab("Pased Balls per Game")+
    geom_boxplot()
boxPlotPB

# Box plot for Wild Pitches
boxPlotWP <- ggplot(
        fielding.stats[fielding.stats$position=="c",],
        aes(x=Inducted,y=WP.per,group=Inducted,colour=Inducted)
    )+
    ylab("Wild Pitches (by catchers) per Game")+
    geom_boxplot()
boxPlotWP

# Box plot for Stolen Bases
boxPlotSB <- ggplot(
        fielding.stats[fielding.stats$position=="c",],
        aes(x=Inducted,y=SB.per,group=Inducted,colour=Inducted)
    )+
    ylab("Opponent Stolen Bases (by catchers) per game")+
    geom_boxplot()
boxPlotSB

# Box plot for Caught Stealing
boxPlotCS <- ggplot(
        fielding.stats[fielding.stats$position=="c",],
        aes(x=Inducted,y=CS.per,group=Inducted,colour=Inducted)
    )+
    ylab("Opponents Caught Stealing (by catcher) per Game")+
    geom_boxplot()
boxPlotCS

arrangeCatcherStats <- ggarrange(
    boxPlotPB,
    boxPlotWP,
    boxPlotSB,
    boxPlotCS,
    labels = c("Passed Balls", "Wild Pitches", "Stolen Bases", "Caught Stealing" ),
    ncol = 2, 
    nrow = 2
)
ggsave("6372_Project2_HOF/Catcher Fielding Box Plots.png",plot = arrangeCatcherStats, type = png())


# Cluster Analysis

batting.avg.stats <- result[,cols.Batting.avg]
batting.avg.stats$Inducted <- result$HallOfFame_inducted
batting.avg.stats$position <- result$position
# Means plot for Batting Average
sumstats<-aggregate(Batting_Average~Inducted*position,data=batting.avg.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotBattingAvg <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Batting Average")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotBattingAvg

# Means plot for walks per at bat
sumstats<-aggregate(Walks_Average~Inducted*position,data=batting.avg.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotWalksAvg <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Walks per plate apperance")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotWalksAvg

# Means plot for Runs at bat
sumstats<-aggregate(Runs_Average~Inducted*position,data=batting.avg.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotRunAvg <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Runs per plate apperance")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotRunAvg

# Means plot for Runs at bat
sumstats<-aggregate(RBI_Average~Inducted*position,data=batting.avg.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotRBIAvg <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("RBI per plate apperance")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotRBIAvg

# Means plot for Slugging Percentage
sumstats<-aggregate(Slugging~Inducted*position,data=batting.avg.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotSlugging <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Slugging Percentage")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotSlugging

# Means plot for Slugging Percentage
sumstats<-aggregate(OPS~Inducted*position,data=batting.avg.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotOPS <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("On-base Percentage")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotOPS

# Means plot for Slugging Percentage
sumstats<-aggregate(ISO~Inducted*position,data=batting.avg.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotISO <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("Isolated Power")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotISO

# Means plot for Slugging Percentage
sumstats<-aggregate(wOBA~Inducted*position,data=batting.avg.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotWOBA <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
    ylab("weighted on-base average")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotWOBA

arrangeBattingAvgStats <- ggarrange(
    meansPlotBattingAvg,
    meansPlotWalksAvg,
    meansPlotRunAvg,
    meansPlotRBI,
    meansPlotSlugging,
    meansPlotOPS,
    meansPlotISO,
    meansPlotWOBA,
    labels = c("Batting Average","Walks per AB", "Runs per AB", "RBI per AB", "Slugging %", "On-base %", "Isolated Power", "Weighted On-Base %"),
    ncol = 2, 
    nrow = 4
)
ggsave("6372_Project2_HOF/Batting Average Stats Means Plot.png",plot = arrangeBattingAvgStats, type = png())


# PCA
pc.result<-prcomp(result[,cols.Batting.no.cor],scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$Inducted<-result$HallOfFame_inducted

pc.result
summary(pc.result)
n <- dim(result[,cols.Batting.no.cor])[2]

#Scree plot
eigenvals<-(pc.result$sdev)^2
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
screePlotData <- data.frame( factor = 1:n, eigenvals = eigenvals/sum(eigenvals), cumulative.prop = cumulative.prop)
screePlotData <- melt(screePlotData, id = "factor")
PCAScreePlot <- ggplot(data = screePlotData,
    aes(x=factor, y=value, colour=variable)) +
    ylab("Prop. Var. Explained") +
    geom_line(aes(linetype=variable), size=1) + 
    theme(legend.position="none") +
    scale_x_continuous("Factors", 1:n, 1:n) +
    ggtitle("PCA Screen Plot")
PCAScreePlot

plotsPC1vPC2 <- ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(col=Inducted), size=2)+
    ggtitle("PC1 Vs. PC2") + 
    theme(legend.position="none")

plotsPC1vPC3 <- ggplot(data = pc.scores, aes(x = PC1, y = PC3)) +
    geom_point(aes(col=Inducted), size=2)+
    ggtitle("PC1 Vs. PC3") + 
    theme(legend.position="none")

plotsPC2vPC3 <- ggplot(data = pc.scores, aes(x = PC2, y = PC3)) +
    geom_point(aes(col=Inducted), size=2)+
    ggtitle("PC2 Vs. PC3") + 
    theme(legend.position="none")

arrangePCAAnalysis <- ggarrange(
    PCAScreePlot,
    plotsPC1vPC2,
    plotsPC1vPC3,
    plotsPC2vPC3,
    # labels = c("Scree Plot", "PC1 Vs. PC2", "PC1 Vs. PC3", "PC2 Vs. PC3"),
    ncol = 2, 
    nrow = 2
)
ggsave("6372_Project2_HOF/PCA Analysis.png",plot = arrangePCAAnalysis, type = png())



#hierarchical cluster

x<-t(result[,c(33, cols.Batting.avg)])
colnames(x)<- ifelse(result$HallOfFame_inducted=="Y",1,0)
heatMapBatting <- pheatmap(
    x,
    annotation_col=data.frame(Inducted=result$HallOfFame_inducted),
    scale="row",
    legend=T,
    # kmeans_k = 2,
    color=colorRampPalette(c("blue","white", "red"),space = "rgb")(100)
)
heatMapBatting
ggsave("6372_Project2_HOF/Batting Cluster.png",plot = heatMapBatting, type = png())

# Decision Tree
short.tree<-tree(HallOfFame_inducted~.,result[,c(cols.Inducted, 33, cols.Batting.avg)])
summary(short.tree)
plot(short.tree)
text(short.tree)

# Load rpart and rpart.plot
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)
# Create a decision tree model
tree <- rpart(HallOfFame_inducted~.,result[,c(cols.Inducted, cols.Batting.no.cor, cols.Fielding)], cp=.02)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#Awards Data
head(result[,cols.Awards])

sum(result$HallOfFame_inducted == "Y" & result$Awards_GoldGlove >= 6)
result$AllStarAwards <- result$Awards_BaseballMagazineAllStar + result$Awards_TSNAllStar
boxplot( AllStarAwards~HallOfFame_inducted, data = result)

# Interaction Terms for Positions and awards
# 
# 
# 
#   

awards.stats <- result.post.1961[,cols.Awards]
awards.stats$Inducted <- result.post.1961$HallOfFame_inducted
awards.stats$position <- result.post.1961$position

# Means plot for TSN Allstar
sumstats<-aggregate(Awards_TSNAllStar~Inducted*position,data=awards.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotTSNAwards <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
  ylab("TSN Allstar Award")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotTSNAwards

# Means plot for Awards_GoldGlove
sumstats<-aggregate(Awards_GoldGlove~Inducted*position,data=awards.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotGoldGlove <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
  ylab("Gold Glove")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotGoldGlove

# Means plot for Awards_HutchAward
sumstats<-aggregate(Awards_HutchAward~Inducted*position,data=awards.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotHutch <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
  ylab("Hutch Award")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotHutch
# To Many Zeros, remove

# Means plot for AllstarGames
sumstats<-aggregate(AllstarGames~Inducted*position,data=awards.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotAllstarGames <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
  ylab("Allstar Apperances")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotAllstarGames

# Means plot for Awards_TSNMajorLeaguePlayerOfTheYear
sumstats<-aggregate(Awards_TSNMajorLeaguePlayerOfTheYear~Inducted*position,data=awards.stats,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
meansPlotTSNPlayer <- ggplot(sumstats,aes(x=position,y=Mean,group=Inducted,colour=Inducted))+
  ylab("TSN Major League Player of the Year")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)
meansPlotTSNPlayer
summary(result$Awards_TSNMajorLeaguePlayerOfTheYear)

# arrangeBattingAvgStats <- ggarrange(
#   meansPlotBattingAvg,
#   meansPlotWalksAvg,
#   meansPlotRunAvg,
#   meansPlotRBI,
#   meansPlotSlugging,
#   meansPlotOPS,
#   meansPlotISO,
#   meansPlotWOBA,
#   labels = c("Batting Average","Walks per AB", "Runs per AB", "RBI per AB", "Slugging %", "On-base %", "Isolated Power", "Weighted On-Base %"),
#   ncol = 2, 
#   nrow = 4
# )
# ggsave("6372_Project2_HOF/Batting Average Stats Means Plot.png",plot = arrangeBattingAvgStats, type = png())
# 
 