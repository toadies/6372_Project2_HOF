library(glmnet)
library(MASS)
library(caret)
library(pROC)
library(ROCR)
library(pheatmap)
library(randomForest)
library(class)
library(kknn)
library(dplyr)

#GB SetWD
source("6372_Project2_HOF/ImportData.R")

# Create a very simple model to compare againist
glm.simple <- glm(HallOfFame_inducted~Batting_H,data = train, family = binomial)
summary(glm.simple)

train$simple.prob <- predict.glm(glm.simple,train[,-cols.Inducted],type="response")
train$simple.predicted <- ifelse(train$simple.prob>.5,"Y","N")
confusionMatrix(table(train$simple.pred, train$HallOfFame_inducted))


# Test Model
test$simple.prob <- predict.glm(glm.simple,test[,-cols.Inducted],type="response")
test$simple.predicted <- ifelse(test$simple.prob>.5,"Y","N")
confusionMatrix(table(test$simple.pred, test$HallOfFame_inducted))

# Let's get our data for the LASSO
train.full <- train[,c(cols.Inducted, cols.Batting, cols.Batting.avg, cols.Awards)]

x <- model.matrix(HallOfFame_inducted~.,train.full)
y <- ifelse(train$HallOfFame_inducted=="Y",1,0)

set.seed(123)
glm.lasso <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse")
plot(glm.lasso)

#min value of lambda
lambda_min <- glm.lasso$lambda.min
#best value of lambda
lambda_1se <- glm.lasso$lambda.1se
#regression coefficients
glm.lasso.coef <- coef(glm.lasso,s=lambda_1se)
data.frame(name = glm.lasso.coef@Dimnames[[1]][glm.lasso.coef@i + 1], coefficient = glm.lasso.coef@x)

# Get column indecis
cols.lasso.coef <- glm.lasso.coef@i
cols.lasso.coef <- cols.lasso.coef[-1] # Remove the intercept

train.reduce = train.full[,cols.lasso.coef]
train.reduce$HallOfFame_inducted <- train.full$HallOfFame_inducted

#Assess Model
glm.manual <- glm(HallOfFame_inducted~.,data = train.reduce, family = binomial)
summary(glm.manual)

# Clean up the model to try to prevent overfitting
# RBIs are not useful
rbiColIndex <- which(colnames(train.reduce)=="Batting_RBI")
train.reduce <- train.reduce[,-rbiColIndex]
names(train.reduce)

# Rerun
glm.manual <- glm(HallOfFame_inducted~.,data = train.reduce, family = binomial)
summary(glm.manual)

# Exclude Awards_LouGehrigMemorialAward
# only 57 awarded from 1985 to 2011
louColIndex <- which(colnames(train.reduce)=="Awards_LouGehrigMemorialAward")
train.reduce <- train.reduce[,-louColIndex]
names(train.reduce)

train.final <- train.reduce

# Rerun
glm.final <- glm(HallOfFame_inducted~.,data = train.final, family = binomial)
summary(glm.final)

glm.final$coefficients
exp(cbind(coef(glm.final), confint(glm.final)))

# Test Model
test$final.prob <- predict.glm(glm.final,test[,-cols.Inducted],type="response")
test$final.predicted <- ifelse(test$final.prob>.5,"Y","N")
confusionMatrix(table(test$final.predicted, test$HallOfFame_inducted))

########### Sanitity checks if other variables are impactful #############
#
## Add Homeruns
train.final.incl.hr <- train.final
train.final.incl.hr$Batting_HR <- train.full$Batting_HR
names(train.final.incl.hr)

### Rerun
glm.incl.hr <- glm(HallOfFame_inducted~.,data = train.final.incl.hr, family = binomial)
summary(glm.incl.hr)

### Test Model
test$incl.hr.prob <- predict.glm(glm.incl.hr,test[,-cols.Inducted],type="response")
test$incl.hr.predicted <- ifelse(test$incl.hr.prob>.5,"Y","N")
confusionMatrix(table(test$incl.hr.predicted, test$HallOfFame_inducted))

#### No change in model, remove HRs

## Replace Triples with Homeruns since that is a big player in Hall voters
tripleColIndex <- which(colnames(train.final.incl.hr)=="Batting_3B")
train.final.excl.3b <- train.final.incl.hr[,-tripleColIndex]
names(train.final.excl.3b)

### Rerun
glm.excl.3b <- glm(HallOfFame_inducted~.,data = train.final.excl.3b, family = binomial)
summary(glm.excl.3b)

#### Significant but negative? That's not good, let's research why
is_outlier <- function(x) {
  return(x > quantile(x, 0.75) + 1.5 * IQR(x))
}
result$HR.outlier <- is_outlier(result$Batting_HR)
boxplotHRs <- ggplot(result, aes(x=HallOfFame_inducted, y=Batting_HR)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_text(aes(label=ifelse(HR.outlier,nameLast,"")), position = 'jitter')

boxplotHRs
ggsave("6372_Project2_HOF/Homerun Outliers.png",plot = boxplotHRs, type = png(), height = 10)

#### THEY ARE ALL CHEATERS!

# Can we improve our model by including an interaction with positions?
train.final.positions <- train.final
train.final.positions$position.c <- train$position.c
train.final.positions$position.1b <- train$position.1b
train.final.positions$position.2b <- train$position.2b
train.final.positions$position.3b <- train$position.3b
train.final.positions$position.ss <- train$position.ss
train.final.positions$position.lf <- train$position.lf
train.final.positions$position.rf <- train$position.rf
train.final.positions$position.cf <- train$position.cf

train.final.positions$position.of <- ifelse(train$position.lf == 1 | train$position.cf == 1 | train$position.rf == 1, 1, 0)
train.final.positions$position.if <- ifelse(train.final.positions$positions.of == 1, 0 , 1)
names(train.final.positions)

glm.final.positions <- glm(HallOfFame_inducted~
                             Batting_R+
                             Batting_3B+
                             Batting_Average+
                             AllstarGames+
                             TotalAllStarAwards+
                             position.c
                           ,data = train.final.positions, family = binomial)
summary(glm.final.positions)
# Test Model
test$final.positions.prob <- predict.glm(glm.final.positions,test[,-cols.Inducted],type="response")
test$final.positions.predicted <- ifelse(test$final.positions.prob>.5,"Y","N")
confusionMatrix(table(test$final.positions.predicted, test$HallOfFame_inducted))

# No interaction terms helps by position.  Including Catcher alone improves the model
View( glm.final.positions$coefficients )
View( exp(cbind(coef(glm.final.positions), confint(glm.final.positions))) )

#### FINAL MODEL ######
train.final$position.c <- train$position.c
# Rerun
glm.final <- glm(HallOfFame_inducted~.,data = train.final, family = binomial)
summary(glm.final)

glm.final$coefficients
exp(cbind(coef(glm.final), confint(glm.final)))

# Test Model
test$final.prob <- predict.glm(glm.final,test[,-cols.Inducted],type="response")
test$final.predicted <- ifelse(test$final.prob>.5,"Y","N")
confusionMatrix(table(test$final.predicted, test$HallOfFame_inducted))

# How did we originally do?
train$final.positions.prob <- predict.glm(glm.final.positions,train[,-cols.Inducted],type="response")
train$final.positions.predicted <- ifelse(train$final.positions.prob>.5,"Y","N")
confusionMatrix(table(train$final.positions.predicted, train$HallOfFame_inducted))

# Specificity is at 60% and 60% on test and training
# Let's find out why?

train.inHOF <- train[train$HallOfFame_inducted == "Y" & train$final.positions.predicted == "N",c(19,20,26,27,100,37,39,89,121,91)]
test.inHOF <- test[test$HallOfFame_inducted == "Y" & test$final.positions.predicted == "N",c(19,20,26,27,100,37,39,89,121,91)]
train.inHOF$train <- "Y"
test.inHOF$train <- "N"
review.inHOF <- rbind(train.inHOF, test.inHOF)

review.inHOF$debut <- as.Date(review.inHOF$debut)
review.inHOF$debutYear <- as.numeric(format(review.inHOF$debut, "%Y"))
review.inHOF$finalGame <- as.Date(review.inHOF$finalGame)
review.inHOF$finalYear <- as.numeric(format(review.inHOF$finalGame, "%Y"))

names(review.inHOF)

review.inHOF$nameFirst <- as.character(review.inHOF$nameFirst)
review.inHOF$nameLast <- as.character(review.inHOF$nameLast)

review.inHOF[review.inHOF$debutYear>=1961,]

df = data.frame(player = paste(review.inHOF$nameFirst, review.inHOF$nameLast, sep=""),
                start = review.inHOF$debutYear, 
                end = review.inHOF$finalYear
            )
plotTimeSpent <- ggplot() + 
  geom_rect(data=df,aes(xmin=start, xmax=end,ymin=player, ymax=player, color=player),size=1) +
  theme(legend.position="none") +
  geom_vline(xintercept = 1961)
plotTimeSpent
ggsave("6372_Project2_HOF/timeframe of players not predicted.png",plot = plotTimeSpent, type = png(), height = 10)

# Check LogisticRegression.post.1961.R for new analysis
# Most LASSO models performed poorly, with the exception it wanted to pull in Award Stats over batting statistics
# Let's run a consfusion matrix on just post 1961 players
result.post.1961$final.positions.prob <- predict.glm(glm.final.positions,result.post.1961[,-cols.Inducted],type="response")
result.post.1961$final.positions.predicted <- ifelse(result.post.1961$final.positions.prob>.5,"Y","N")
confusionMatrix(table(result.post.1961$final.positions.predicted, result.post.1961$HallOfFame_inducted))



# Assumptions
### VIF
car::vif(model)

probabilities <- predict(glm.final, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Y", "N")
head(predicted.classes)

# Select only numeric predictors
mydata <- train.final %>%
  dplyr::select_if(is.numeric) 

# mydata <- mydata[,-c(3)]

predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

train.transform <- train.final
# train.transform$sqBattingAvg <- train.transform$Batting_Average^2
# train.transform$eBattingAvg <- exp(train.transform$Batting_Average)
# train.transform$lBattingAvg <- log(train.transform$Batting_Average)
train.transform$lBatting_3B <- log(train.transform$Batting_3B + 1)
# train.transform$sqRuns <- train.transform$Batting_R^2
train.transform$lTotalAllstarAwards <- log(train.transform$TotalAllStarAwards + 1)
# train.transform$lAllstarGames <- log(trian.transform$AllstarGames + 1)
# train.transform$sqAllstarGames <- train.transform$AllstarGames^2

names(train.transform)
glm.transform <- glm(HallOfFame_inducted~
                  Batting_R+
                  lBatting_3B+
                  AllstarGames+
                  lTotalAllstarAwards+
                  position.c
             ,data = train.transform, family = binomial)
summary(glm.transform)
# Predict the probability (p) of diabete positivity
probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Y", "N")
head(predicted.classes)

# Select only numeric predictors
mydata <- train.transform %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

test.transform <- test
test.transform$sqBattingAvg <- test.transform$Batting_Average^2
test.transform$eBattingAvg <- exp(test.transform$Batting_Average)
test.transform$sqRuns <- test.transform$Batting_R^2
test.transform$lBatting_3B <- log(test.transform$Batting_3B + 1)
test.transform$lTotalAllstarAwards <- log(test.transform$TotalAllStarAwards + 1)
test.transform$lAllstarGames <- log(test.transform$AllstarGames + 1)
test.transform$sqAllstarGames <- test.transform$AllstarGames^2

# Test Model
test.transform$final.transform.prob <- predict.glm(model,test.transform[,-cols.Inducted],type="response")
test.transform$final.transform.predicted <- ifelse(test.transform$final.transform.prob>.5,"Y","N")
confusionMatrix(table(test.transform$final.transform.predicted, test.transform$HallOfFame_inducted))

# influential values
plot(glm.final.positions, which = 4, id.n = 3)
train[c(99,124,261),c(6,19,20,26,27,100,37,39,89,121,91)]
glm.final.positions.data <- augment(glm.final.positions) %>% 
  mutate(index = 1:n()) 

glm.final.positions.data %>% top_n(3, .cooksd)

ggplot(glm.final.positions.data, aes(index, .std.resid)) + 
  geom_point(aes(color = HallOfFame_inducted), alpha = .5) +
  theme_bw()
