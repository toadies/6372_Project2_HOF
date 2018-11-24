result <- read.csv("6372_Project2_HOF/data/result.csv")
train <- read.csv("6372_Project2_HOF/data/train.csv")
test <- read.csv("6372_Project2_HOF/data/test.csv")

#Columes
cols.Inducted <- 6
cols.HallOfFame <- c(2:5)
cols.Batting.avg <- c(100:105)
cols.Batting <- c(32:48,99)
cols.Fielding <- c(49:59)
cols.Awards <- c(50:89)
cols.Positions <- c(90:98)
# Add Data For GGPairs
cols.BattingAvg_w_Hall <- c(2:5,100:105)

names(result)

result[,cols.Inducted]
