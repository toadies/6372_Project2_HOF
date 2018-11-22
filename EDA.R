# Load Data into Memory (result, trian, test)
source("6372_Project2_HOF/ImportData.R")

# Batting Stats Compared to HOF
pairs(result[,cols.batting], col=result$HallOfFame_inducted)


#Examine the correlation between the continous predictors
pairs(result[,32:36], col=result$HallOfFame_inducted)

# Hitting versus positions
pairs(Batters[,c(34,35,38,39,54:62)],col=Batters$HallOfFame_inducted)

# Fielding versus positions
pairs(Batters[,c(70:76)],col=Batters$HallOfFame_inducted)