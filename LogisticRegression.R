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

# Rerun coef variables to remove penatlties caused by LASSO
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

# Assumptions
# perform lack of fit)
library(MKmisc)
HLgof.test(fit = fitted(glm.manual), obs = train$HallOfFame_inducted)

