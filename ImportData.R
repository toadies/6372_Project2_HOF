#setwd("/Users/gbourzik/Documents/GitHub/")
result <- read.csv("6372_Project2_HOF/data/result.csv")
train <- read.csv("6372_Project2_HOF/data/train.csv")
test <- read.csv("6372_Project2_HOF/data/test.csv")
names(result)



names(result)
#Columes
cols.Inducted <- 6
cols.HallOfFame <- c(2:5)
cols.Batting.avg <- c(100:107)
cols.Batting.standardize <- c(108:123)
cols.Batting <- c(32:48,99)
cols.Batting.no.cor <- c(99, 36:48)
cols.Fielding <- c(51:59)
cols.Awards <- c(60:89)
cols.Positions <- c(90:98)
# Add Data For GGPairs
cols.BattingAvg_w_Hall <- c(2:5,100:105)
