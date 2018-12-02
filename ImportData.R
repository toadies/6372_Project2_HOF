#setwd("/Users/gbourzik/Documents/GitHub/")
result <- read.csv("6372_Project2_HOF/data/result.csv")
train <- read.csv("6372_Project2_HOF/data/train.csv")
test <- read.csv("6372_Project2_HOF/data/test.csv")
names(result)

result$nameLast <- as.character(result$nameLast)
result$nameFirst <- as.character(result$nameFirst)

#Columes
cols.Inducted <- 6
cols.HallOfFame <- c(2:5)
cols.Batting.avg <- c(100:104)
cols.Batting.standardize <- c(105:120)
cols.Batting <- c(32:48,99)
cols.Batting.no.cor <- c(99,34, 36:48)
cols.Fielding <- c(51:59)
cols.Awards <- c(60:89,121)
cols.Positions <- c(90:98)
# Add Data For GGPairs
cols.BattingAvg_w_Hall <- c(2:5,100:105)
cols.KNNData <- c(cols.Batting, cols.Batting.avg, cols.Fielding, cols.Awards)

result.truth <- result$HallOfFame_inducted
train.truth <- train$HallOfFame_inducted
test.truth <- test$HallOfFame_inducted

result.post.1961 <- read.csv("6372_Project2_HOF/data/result.post.1961.csv")
train.post.1961 <- read.csv("6372_Project2_HOF/data/train.post.1961.csv")
test.post.1961 <- read.csv("6372_Project2_HOF/data/test.post.1961.csv")
names(result.post.1961)

result.post.1961$nameLast <- as.character(result.post.1961$nameLast)
result.post.1961$nameFirst <- as.character(result.post.1961$nameFirst)
dim(train.post.1961)
