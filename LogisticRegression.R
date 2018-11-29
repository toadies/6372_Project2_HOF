library(glmnet)
library(MASS)
library(caret)
library(pROC)
library(ROCR)
library(pheatmap)
library(randomForest)
library(class)
library(kknn)

#GB SetWD
source("6372_Project2_HOF/ImportData.R")

train.full <- train[,c(cols.Inducted, cols.Batting, cols.Batting.avg, cols.Awards)]

x <- model.matrix(HallOfFame_inducted~.,train.full)
#convert class to numerical variable
y <- ifelse(train$HallOfFame_inducted=="Y",1,0)

glm.lasso <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse")
#plot result
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

# ACK Runs & RBIS? remove Runs
# exclude runs, since RBI is related
runsColIndex <- which(colnames(train.reduce)=="Batting_R")
train.reduce <- train.reduce[,-runsColIndex]
names(train.reduce)

# Rerun
glm.manual <- glm(HallOfFame_inducted~.,data = train.reduce, family = binomial)
summary(glm.manual)

# Exclude Awards_LouGehrigMemorialAward
# only 57 awarded from 1985 to 2011
louColIndex <- which(colnames(train.reduce)=="Awards_LouGehrigMemorialAward")
train.reduce <- train.reduce[,-louColIndex]
names(train.reduce)

# Rerun
glm.manual <- glm(HallOfFame_inducted~.,data = train.reduce, family = binomial)
summary(glm.manual)

glm.manual$coefficients
exp(cbind(coef(glm.manual), confint(glm.manual)))

# Test Model
lasso_prob <- predict.glm(glm.manual,test[,-cols.Inducted],type="response")
lasso_predict <- rep("N",nrow(test))
lasso_predict[lasso_prob>.5] <- "Y"

# confusion matrix (Updated)
confusionMatrix(table(test$HallOfFame_inducted, lasso_predict))

# ROC Curves (Updated)
roccurve <- roc(test$HallOfFame_inducted ~ lasso_prob)
plot(roccurve)
auc(roccurve)

# Add Homeruns since that is a big player in Hall voters
train.reduce$Batting_HR <- train.full$Batting_HR
names(train.reduce)
# Rerun
glm.manual <- glm(HallOfFame_inducted~.,data = train.reduce, family = binomial)
summary(glm.manual)

glm.manual$coefficients
exp(cbind(coef(glm.manual), confint(glm.manual)))

# Test Model
lasso_prob <- predict.glm(glm.manual,test[,-cols.Inducted],type="response")
lasso_predict <- rep("N",nrow(test))
lasso_predict[lasso_prob>.5] <- "Y"

# confusion matrix (Updated)
confusionMatrix(table(test$HallOfFame_inducted, lasso_predict))

# ROC Curves (Updated)
roccurve <- roc(test$HallOfFame_inducted ~ lasso_prob)
plot(roccurve)
auc(roccurve)

# Replace Triples with Homeruns since that is a big player in Hall voters
tripleColIndex <- which(colnames(train.reduce)=="Batting_3B")
train.reduce <- train.reduce[,-tripleColIndex]
names(train.reduce)
# Rerun
glm.manual <- glm(HallOfFame_inducted~.,data = train.reduce, family = binomial)
summary(glm.manual)

glm.manual$coefficients
exp(cbind(coef(glm.manual), confint(glm.manual)))

# Test Model
lasso_prob <- predict.glm(glm.manual,test[,-cols.Inducted],type="response")
lasso_predict <- rep("N",nrow(test))
lasso_predict[lasso_prob>.5] <- "Y"

# confusion matrix (Updated)
confusionMatrix(table(test$HallOfFame_inducted, lasso_predict))

# ROC Curves (Updated)
roccurve <- roc(test$HallOfFame_inducted ~ lasso_prob)
plot(roccurve)
auc(roccurve)


# Assumptions
# perform lack of fit
install.packages("generalhoslem")
library(generalhoslem)
# https://cran.r-project.org/web/packages/generalhoslem/generalhoslem.pdf
?logitgof
plot(glm.manual$residuals)

