# Load Data into Memory (result, trian, test)
source("6372_Project2_HOF/ImportData.R")

# Batting Stats Compared to HOF
pairs(result[,cols.Batting], col=result$HallOfFame_inducted)
pairs(result[,c(33,cols.Batting.avg)], col=result$HallOfFame_inducted)

#Plot More
library(GGally)
# All batting is too much
ggpairsAlLBatting <- ggpairs(result[,cols.Batting], diag=list(continuous="density", discrete="bar"), axisLabels="show")
# Better Representation using just data
ggpairsBattingAvg <- ggpairs(result[,c(33,cols.Batting.avg)], diag=list(continuous="densityDiag", discrete="barDiag"), axisLabels="show")
# Inlcude BattingAvg and Hall Fields
ggpairsBatting_w_Hall <- ggpairs(result[,c(33,cols.BattingAvg_w_Hall)], diag=list(continuous="density", discrete="bar"), axisLabels="show")
