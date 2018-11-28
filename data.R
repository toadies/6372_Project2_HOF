# Read the Data

Batters <- read.csv("6372_Project2_HOF/data/Batters.csv", na.strings = "NULL")


# Identify position most appeared
Batters$position <- colnames(Batters[,53:61])[max.col(Batters[,53:61],ties.method="first")]
Batters$position <- as.character(Batters$position)
Batters$position <- substr(Batters$position, 15, nchar(Batters$position))

# Remove Pitchers
Batters <- Batters[Batters$position!="p",]

# Set Positions
Batters$position.c <- ifelse(Batters$position=="c", 1, 0)
Batters$position.1b <- ifelse(Batters$position=="1b", 1, 0)
Batters$position.2b <- ifelse(Batters$position=="2b", 1, 0)
Batters$position.3b <- ifelse(Batters$position=="3b", 1, 0)
Batters$position.ss <- ifelse(Batters$position=="ss", 1, 0)
Batters$position.lf <- ifelse(Batters$position=="lf", 1, 0)
Batters$position.cf <- ifelse(Batters$position=="cf", 1, 0)
Batters$position.rf <- ifelse(Batters$position=="rf", 1, 0)

# Get last year played
Batters$finalGame <- as.Date(Batters$finalGame)
Batters$finalYear <- as.numeric(format(Batters$finalGame, "%Y"))

Batters$Batting_1B <-  Batters$Batting_H - Batters$Batting_2B - Batters$Batting_3B - Batters$Batting_HR

# Batting Stats
Batters$Batting_Average <- Batters$Batting_H / Batters$Batting_AB
Batters$Walks_Average <- Batters$Batting_BB / Batters$Batting_AB
Batters$Runs_Average <- Batters$Batting_R / Batters$Batting_AB
Batters$RBI_Average <- Batters$Batting_RBI / Batters$Batting_AB
Batters$Slugging <- (1*Batters$Batting_1B + 2*Batters$Batting_2B + + 3*Batters$Batting_3B + 4*Batters$Batting_HR) /  Batters$Batting_AB 
Batters$OPS <- (Batters$Batting_H + Batters$Batting_BB + Batters$Batting_HBP) / (Batters$Batting_AB + Batters$Batting_BB + Batters$Batting_HBP + Batters$Batting_SF)
# Batters$OPSPlus - Can't Do because we need league OPS each year and Park factors Blah
Batters$ISO <- Batters$Slugging - Batters$Batting_Average
Batters$wOBA <- (0.69*(Batters$Batting_BB-Batters$Batting_IBB) +
                     0.722*Batters$Batting_HBP+
                     0.0888*Batters$Batting_1B+
                     1.271*Batters$Batting_2B+
                     1.616*Batters$Batting_3B+
                     2.101*Batters$Batting_HR) / 
    (Batters$Batting_AB+
         Batters$Batting_BB-
         Batters$Batting_IBB+
         Batters$Batting_SF+
         Batters$Batting_HBP)

Batters$R.perAB <- Batters$Batting_R / Batters$Batting_AB
Batters$H.perAB <- Batters$Batting_H / Batters$Batting_AB
Batters$singles.perAB <- Batters$Batting_1B / Batters$Batting_AB
Batters$doubles.perAB <- Batters$Batting_2B / Batters$Batting_AB
Batters$triples.perAB <- Batters$Batting_3B / Batters$Batting_AB
Batters$HR.perAB <- Batters$Batting_HR / Batters$Batting_AB
Batters$RBI.perAB <- Batters$Batting_RBI / Batters$Batting_AB
Batters$SB.perAB <- Batters$Batting_SB / Batters$Batting_AB
Batters$CS.perAB <- Batters$Batting_CS / Batters$Batting_AB
Batters$BB.perAB <- Batters$Batting_BB / Batters$Batting_AB
Batters$SO.perAB <- Batters$Batting_SO / Batters$Batting_AB
Batters$IBB.perAB <- Batters$Batting_IBB / Batters$Batting_AB
Batters$HBP.perAB <- Batters$Batting_HBP / Batters$Batting_AB
Batters$SH.perAB <- Batters$Batting_SH / Batters$Batting_AB
Batters$SF.perAB <- Batters$Batting_SF / Batters$Batting_AB
Batters$GIDP.perAB <- Batters$Batting_GIDP / Batters$Batting_AB

dim(Batters)
sum(Batters$HallOfFame_inducted == "Y")

# Eligible for Ballot
# 10 Years in Baseball
result <- Batters[Batters$Batting_total_seasons>=10 | Batters$HallOfFame_inducted=="Y",]
dim(result)
sum(result$HallOfFame_inducted == "Y")

# Last game 5 years ago (since last season is 2016 in this data)
result <- result[result$finalYear <= 2011 | result$HallOfFame_inducted=="Y",]
dim(result)
sum(result$HallOfFame_inducted == "Y")

# Remove Apperances
result <- result[,-c(49:65,116)]
names(result)

#Create a Training & Test Set
n <- nrow(result)
set.seed(123) 
index<-sample( 1:nrow(result),floor(n * .6) )
train<-result[index,]
test<-result[-index,]

write.csv(result, "6372_Project2_HOF/data/result.csv", row.names=FALSE)
write.csv(train, "6372_Project2_HOF/data/train.csv", row.names=FALSE)
write.csv(test, "6372_Project2_HOF/data/test.csv", row.names=FALSE)


dim(train)
dim(test)
