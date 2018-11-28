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

# Assumptions
# perform lack of fit
install.packages("generalhoslem")
library(generalhoslem)
# https://cran.r-project.org/web/packages/generalhoslem/generalhoslem.pdf
?logitgof
plot(glm.manual$residuals)

#################### K Nearest Neighbor #################### 

#Knn
#train[,c(cols.Inducted, cols.Batting.avg)]
set.seed(123)
#knn.train = train(Attrition~., data=emp_train[,c(col.CatKNN)], method="knn", trControl=control, tuneGrid=grid1)
knn.train = train(HallOfFame_inducted~., data=train[,c(cols.Inducted, cols.KNNData)], method="knn")
knn.test = knn(train[,c(cols.Inducted, cols.KNNData)][,-1], test[,c(cols.Inducted, cols.KNNData)][,-1], train[,c(cols.Inducted, cols.KNNData)][,1], k=27)
knnPrediction <-confusionMatrix(table(knn.test, test$HallOfFame_inducted))
knnPrediction

# K Weighted
set.seed(123)
kknn.train = train.kknn(HallOfFame_inducted~., data=train[,c(cols.Inducted, cols.KNNData)], kmax=30, distance = 2)
prediction <- predict(kknn.train, test[,c(cols.Inducted, cols.KNNData)][,-1])
kWeightedPrediction <- confusionMatrix(table(test[,c(cols.Inducted, cols.KNNData)][,1],prediction))
kWeightedPrediction <-confusionMatrix(table(knn.test, test$HallOfFame_inducted))
kWeightedPrediction
#graphics.off() 
#par(mar=c(5,5,5,5))
plot(kknn.train)