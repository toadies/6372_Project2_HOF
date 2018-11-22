# Load Data into Memory (result, trian, test)
source("6372_Project2_HOF/ImportData.R")

# Batting Stats Compared to HOF
pairs(result[,cols.Batting], col=result$HallOfFame_inducted)
pairs(result[,c(33,cols.Batting.avg)], col=result$HallOfFame_inducted)

